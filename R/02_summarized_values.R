library(tidyverse)


#### Calculating Totals ####

# We'll then calculate grand totals and find associated FG%
league_totals <- combo_totals %>%
  summarize_at(.vars = vars(fgm_restricted_area, fga_restricted_area,
                            `fgm_in_the_paint_(non-ra)`,
                            `fga_in_the_paint_(non-ra)`,
                            `fgm_mid-range`, `fga_mid-range`,
                            fgm_left_corner_3, fga_left_corner_3,
                            fgm_right_corner_3, fga_right_corner_3,
                            fgm_above_the_break_3, fga_above_the_break_3,
                            ftm, fta),
               .funs = ~ sum(., na.rm = T)) %>%
  mutate(fgp_restricted_area = fgm_restricted_area / fga_restricted_area,
         `fgp_in_the_paint_(non-ra)` = 
           `fgm_in_the_paint_(non-ra)` / `fga_in_the_paint_(non-ra)`,
         `fgp_mid-range` = `fgm_mid-range` / `fga_mid-range`,
         fgp_right_corner_3 = fgm_right_corner_3 / fga_right_corner_3,
         fgp_left_corner_3 = fgm_left_corner_3 / fga_left_corner_3,
         fgp_above_the_break_3 = 
           fgm_above_the_break_3 / fga_above_the_break_3,
         ftp = ftm / fta)


#### Value of Points ####

# Higher fg% leads to higher PPG to a point
# Most efficient players aren't scoring a ton
combo %>%
  ggplot(aes(x = fg_pct, y = pts)) +
  geom_point()

# Pattern doesn't change based on shot location
combo %>%
  gather(fg_pct_restricted_area,
         `fg_pct_in_the_paint_(non-ra)`,
         `fg_pct_mid-range`,
         fg_pct_left_corner_3,
         fg_pct_right_corner_3,
         fg_pct_above_the_break_3,
         ft_pct, key = fgp_type, value = fgp_value) %>%
  ggplot(aes(x = fgp_value, y = pts)) +
  geom_point() +
  facet_wrap(~ fgp_type)

# Let's look at point values for each shot range
point_values <- league_totals %>%
  mutate_at(.vars = vars(fgp_restricted_area,
                         `fgp_in_the_paint_(non-ra)`,
                         `fgp_mid-range`,
                         ftp),
            .funs = list(value = ~ . * 2)) %>%
  mutate_at(.vars = vars(fgp_left_corner_3,
                         fgp_right_corner_3,
                         fgp_above_the_break_3),
            .funs = list(value = ~ . * 3)) %>%
  select(fgp_restricted_area_value:fgp_above_the_break_3_value)
point_values %>%
  gather(fgp_restricted_area_value:fgp_above_the_break_3_value,
         key = location, value = value) %>%
  ggplot(aes(x = value, y = location)) +
  geom_segment(aes(x = 0, y = location,
                   xend = value, yend = location)) +
  geom_point(pch = 21, fill = "white", col = "black", size = 4)

# We'll take a look at adjusting the value of mid-range shots
point_values_adjusted <- league_totals %>%
  mutate_at(.vars = vars(fgp_restricted_area,
                         `fgp_in_the_paint_(non-ra)`,
                         ftp),
            .funs = list(value = ~ . * 2)) %>%
  mutate(`fgp_mid-range_value` = `fgp_mid-range` * 2.5) %>%
  mutate_at(.vars = vars(fgp_left_corner_3,
                         fgp_right_corner_3,
                         fgp_above_the_break_3),
            .funs = list(value = ~ . * 3)) %>%
  select(fgp_restricted_area_value:fgp_above_the_break_3_value)
point_values_adjusted %>%
  gather(fgp_restricted_area_value:fgp_above_the_break_3_value,
         key = location, value = value) %>%
  ggplot(aes(x = value, y = location)) +
  geom_segment(aes(x = 0, y = location,
                   xend = value, yend = location)) +
  geom_point(pch = 21, fill = "white", col = "black", size = 4)
