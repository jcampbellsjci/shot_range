library(scales)
library(tidyverse)

combo <- read_csv("data/per_game.csv")
combo_totals <- read_csv("data/totals.csv")


#### Calculating New Stat Values ####

# Calculating value of a shot by player
point_values_player <- combo_totals %>%
  mutate_at(.vars = vars(fg_pct_restricted_area,
                         `fg_pct_in_the_paint_(non-ra)`,
                         `fg_pct_mid-range`,
                         ft_pct),
            .funs = list(value = ~ . * 2)) %>%
  mutate_at(.vars = vars(fg_pct_left_corner_3,
                         fg_pct_right_corner_3,
                         fg_pct_above_the_break_3,
                         fg_pct_backcourt),
            .funs = list(value = ~ . * 3))

# We'll calculate new point values if mid-range shots had different values
new_values_player <- point_values_player %>%
  mutate_at(.vars = vars(fgm_restricted_area,
                         `fgm_in_the_paint_(non-ra)`),
            .funs = list(pts = ~ . * 2)) %>%
  mutate_at(.vars = vars(fgm_left_corner_3,
                         fgm_right_corner_3,
                         fgm_above_the_break_3,
                         fgm_backcourt),
            .funs = list(pts = ~ . * 3)) %>%
  mutate(ftm_pts = ftm,
         `fgm_mid-range_pts` = `fgm_mid-range` * 2,
         `fgm_mid-range_new_pts` = `fgm_mid-range` * 2.5) %>%
  mutate(old_pts = rowSums(
    select(., fgm_restricted_area_pts:`fgm_mid-range_pts`)),
         new_pts = rowSums(
           select(., fgm_restricted_area_pts:ftm_pts,
                  `fgm_mid-range_new_pts`))) %>%
  select(player_id:age, gp, old_pts, new_pts,
         fgm_restricted_area_pts:`fgm_mid-range_new_pts`) %>%
  mutate(old_ppg = old_pts / gp,
         new_ppg = new_pts / gp) %>%
  mutate(ppg_difference = new_ppg - old_ppg,
         ppg_percent_inc = 1 - (old_ppg / new_ppg))

write_csv(new_values_player %>%
            select(player_name, team_abbreviation, old_ppg, new_ppg,
                   ppg_difference, ppg_percent_inc),
          "data/player_new_values.csv")


#### Summary Statistics ####

# Let's see how team points would change
# This would lead to a 2% increase in points
new_values_player %>%
  summarize(old_pts = sum(old_pts, na.rm = T),
            new_pts = sum(new_pts, na.rm = T)) %>%
  mutate(percent_increase = 1 - (old_pts / new_pts))
# Let's see this by team
# Note that players' points are being grouped with one team
# A player who was traded won't have points split
new_values_player %>%
  group_by(team_abbreviation) %>%
  summarize(old_pts = sum(old_pts, na.rm = T),
            new_pts = sum(new_pts, na.rm = T)) %>%
  mutate(percent_increase = 1 - (old_pts / new_pts)) %>%
  arrange(desc(percent_increase))

# Looking at percentage of points by area for each team
team_percentages <- new_values_player %>%
  group_by(team_abbreviation) %>%
  summarize_at(.vars = vars(old_pts,
                            fgm_restricted_area_pts:`fgm_mid-range_pts`),
               .funs = list(~ sum(., na.rm = T))) %>%
  group_by(team_abbreviation) %>%
  mutate_at(.vars = vars(fgm_restricted_area_pts:`fgm_mid-range_pts`),
               .funs = list(percentage = ~ . / old_pts)) %>%
  ungroup()

team_percentages %>%
  mutate(team_abbreviation = fct_reorder(
    team_abbreviation, `fgm_mid-range_pts_percentage`)) %>%
  gather(fgm_restricted_area_pts_percentage:`fgm_mid-range_pts_percentage`,
         key = shot_area, value = percentage) %>%
  ggplot(aes(x = team_abbreviation, y = percentage, fill = shot_area)) +
  geom_col(position = "fill") +
  coord_flip() +
  theme(legend.position = "top") +
  guides(fill = guide_legend(nrow = 4, byrow = TRUE)) +
  labs(x = "Percentage of Points", y = "Team", fill = "Shot Location")

write_csv(
  team_percentages %>%
    select(team_abbreviation,
           fgm_restricted_area_pts_percentage:`fgm_in_the_paint_(non-ra)_pts_percentage`,
           `fgm_mid-range_pts_percentage`,
           fgm_left_corner_3_pts_percentage:fgm_above_the_break_3_pts_percentage,
           ftm_pts_percentage),
  "data/team_pts_by_location.csv")
          
# Plotting the top % increases for players
new_values_player %>%
  filter(gp > (.7 * 65)) %>%
  mutate(ppg_inc_perc = 1 - (old_ppg / new_ppg)) %>%
  top_n(n = 10, wt = ppg_inc_perc) %>%
  mutate(player_name = fct_reorder(player_name, ppg_inc_perc)) %>%
  ggplot(aes(x = player_name, y = ppg_inc_perc)) +
  geom_col() +
  scale_y_continuous(labels = percent) +
  coord_flip() +
  labs(y = "PPG Percent Increase", x = "Player")
new_values_player %>%
  filter(gp > (.7 * 65)) %>%
  mutate(ppg_inc_perc = 1 - (old_ppg / new_ppg)) %>%
  top_n(n = -10, wt = ppg_inc_perc) %>%
  mutate(player_name = fct_reorder(player_name, -ppg_inc_perc)) %>%
  ggplot(aes(x = player_name, y = ppg_inc_perc)) +
  geom_col() +
  scale_y_continuous(labels = percent) +
  coord_flip() +
  labs(y = "PPG Percent Increase", x = "Player")
# Curious the location breakdown of some of these really low players
# Either shooting in the restricted area or from 3
combo %>%
  filter(player_name %in% (new_values_player %>%
                             filter(gp > (.7 * 65)) %>%
                             mutate(ppg_inc_perc =
                                      1 - (old_ppg / new_ppg)) %>%
                             top_n(n = -10,
                                   wt = ppg_inc_perc))$player_name) %>%
  gather(contains("fga"), key = location, value = avg_attempts) %>%
  ggplot(aes(x = player_name, y = avg_attempts, fill = location)) +
  geom_col(position = "fill") +
  scale_y_continuous(labels = percent) +
  coord_flip() +
  guides(fill = guide_legend(ncol = 2, bycol = TRUE)) +
  theme(legend.position = "top") +
  labs(x = "Player", y = "Percent of Attempts", fill = "Shot Location")
# Doing same thing for top
combo %>%
  filter(player_name %in% (new_values_player %>%
                             filter(gp > (.7 * 65)) %>%
                             mutate(ppg_inc_perc =
                                      1 - (old_ppg / new_ppg)) %>%
                             top_n(n = 10,
                                   wt = ppg_inc_perc))$player_name) %>%
  gather(contains("fga"), key = location, value = avg_attempts) %>%
  ggplot(aes(x = player_name, y = avg_attempts, fill = location)) +
  geom_col(position = "fill") +
  scale_y_continuous(labels = percent) +
  coord_flip() +
  guides(fill = guide_legend(ncol = 2, bycol = TRUE)) +
  theme(legend.position = "top") +
  labs(x = "Player", y = "Percent of Attempts", fill = "Shot Location")
