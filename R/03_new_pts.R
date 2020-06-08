library(tidyverse)


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


#### Summary Statistics ####

# Let's see how team points would change
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
