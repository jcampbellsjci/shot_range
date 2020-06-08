# The purpose of this script is to pull data
# All data is coming from the stats page on NBA.com

library(rjson)
library(httr)
library(tidyverse)


#### Pulling raw data ####

# Specifying headers for nba stat website
headers <- c(
  `Connection` = 'keep-alive',
  `Accept` = 'application/json, text/plain, */*',
  `x-nba-stats-token` = 'true',
  `User-Agent` = 'Mozilla/5.0 (Macintosh; Intel Mac OS X 10_15_2) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/79.0.3945.130 Safari/537.36',
  `x-nba-stats-origin` = 'stats',
  `Sec-Fetch-Site` = 'same-origin',
  `Sec-Fetch-Mode` = 'cors',
  `Referer` = 'http://stats.nba.com/%referer%/',
  `Accept-Encoding` = 'gzip, deflate, br',
  `Accept-Language` = 'en-US,en;q=0.9'
)

# Pulling shot range data
# This will have FG numbers for different areas of the court
shot_range_url <- "https://stats.nba.com/stats/leaguedashplayershotlocations?College=&Conference=&Country=&DateFrom=&DateTo=&DistanceRange=By+Zone&Division=&DraftPick=&DraftYear=&GameScope=&GameSegment=&Height=&LastNGames=0&LeagueID=00&Location=&MeasureType=Base&Month=0&OpponentTeamID=0&Outcome=&PORound=0&PaceAdjust=N&PerMode=PerGame&Period=0&PlayerExperience=&PlayerPosition=&PlusMinus=N&Rank=N&Season=2019-20&SeasonSegment=&SeasonType=Regular+Season&ShotClockRange=&StarterBench=&TeamID=0&VsConference=&VsDivision=&Weight="
shot_range_raw <- GET(shot_range_url, add_headers(.headers = headers))$content %>%
  rawToChar() %>%
  fromJSON()

# Also pulling totals from shot range data
range_totals_url <- "https://stats.nba.com/stats/leaguedashplayershotlocations?College=&Conference=&Country=&DateFrom=&DateTo=&DistanceRange=By+Zone&Division=&DraftPick=&DraftYear=&GameScope=&GameSegment=&Height=&LastNGames=0&LeagueID=00&Location=&MeasureType=Base&Month=0&OpponentTeamID=0&Outcome=&PORound=0&PaceAdjust=N&PerMode=Totals&Period=0&PlayerExperience=&PlayerPosition=&PlusMinus=N&Rank=N&Season=2019-20&SeasonSegment=&SeasonType=Regular+Season&ShotClockRange=&StarterBench=&TeamID=0&VsConference=&VsDivision=&Weight="
range_totals_raw <- GET(range_totals_url, add_headers(.headers = headers))$content %>%
  rawToChar() %>%
  fromJSON()

# Pulling traditional data
# Mainly want PPG from here
traditional_url <- "https://stats.nba.com/stats/leaguedashplayerstats?College=&Conference=&Country=&DateFrom=&DateTo=&Division=&DraftPick=&DraftYear=&GameScope=&GameSegment=&Height=&LastNGames=0&LeagueID=00&Location=&MeasureType=Base&Month=0&OpponentTeamID=0&Outcome=&PORound=0&PaceAdjust=N&PerMode=PerGame&Period=0&PlayerExperience=&PlayerPosition=&PlusMinus=N&Rank=N&Season=2019-20&SeasonSegment=&SeasonType=Regular+Season&ShotClockRange=&StarterBench=&TeamID=0&TwoWay=0&VsConference=&VsDivision=&Weight="
traditional_raw <- GET(traditional_url, add_headers(.headers = headers))$content %>%
  rawToChar() %>%
  fromJSON()
traditional_totals_url <- "https://stats.nba.com/stats/leaguedashplayerstats?College=&Conference=&Country=&DateFrom=&DateTo=&Division=&DraftPick=&DraftYear=&GameScope=&GameSegment=&Height=&LastNGames=0&LeagueID=00&Location=&MeasureType=Base&Month=0&OpponentTeamID=0&Outcome=&PORound=0&PaceAdjust=N&PerMode=Totals&Period=0&PlayerExperience=&PlayerPosition=&PlusMinus=N&Rank=N&Season=2019-20&SeasonSegment=&SeasonType=Regular+Season&ShotClockRange=&StarterBench=&TeamID=0&TwoWay=0&VsConference=&VsDivision=&Weight="
traditional_totals_raw <- GET(traditional_totals_url, add_headers(.headers = headers))$content %>%
  rawToChar() %>%
  fromJSON()


#### Cleaning raw data ####

# Pulling column names for shot range data
# This requires a bit of cleaning
# There are two headers so have to combine them
column_names_shot_range <- tolower(c(
  shot_range_raw$resultSets$headers[[2]]$columnNames[1:5],
  gsub(" ", "_", paste(
    shot_range_raw$resultSets$headers[[2]]$columnNames[6:8],
    sort(rep(shot_range_raw$resultSets$headers[[1]]$columnNames, 3)),
    sep = "_")))) %>%
  .[c(1:5, 21:23, 12:14, 18:20, 15:17, 24:26, 6:11)]

# Making any element that is NULL NA
shot_range_list <- map(shot_range_raw$resultSets$rowSet,
                       function(x)
                         map(x, function(y) ifelse(is.null(y), NA, y)))
range_totals_list <- map(range_totals_raw$resultSets$rowSet,
                         function(x)
                           map(x, function(y) ifelse(is.null(y), NA, y)))

# Setting names of each element
shot_range_list <- map(shot_range_list,
                       ~ set_names(., column_names_shot_range))
range_totals_list <- map(range_totals_list,
                         ~ set_names(., column_names_shot_range))

# Binding elements into a tibble
shot_range_df <- map_df(shot_range_list,
                        ~ bind_rows(.))
range_totals_df <- map_df(range_totals_list,
                          ~ bind_rows(.))

# Setting column names for traditional data set
traditional_list <- map(traditional_raw$resultSets[[1]]$rowSet,
                        ~ set_names(., tolower(
                          traditional_raw$resultSets[[1]]$headers)))
traditional_totals_list <- map(traditional_totals_raw$resultSets[[1]]$rowSet,
                               ~ set_names(., tolower(
                                 traditional_totals_raw$resultSets[[1]]$headers)))

# Binding traditional list into a tibble
traditional_df <- map_df(traditional_list,
                         ~ bind_rows(.))
traditional_totals_df <- map_df(traditional_totals_list,
                                ~ bind_rows(.))

# Combining shot range and traditional data
combo <- shot_range_df %>%
  inner_join(traditional_df %>%
               select(player_id, pts, gp, fg_pct, ftm, fta, ft_pct)) %>%
  select(-c(fgm_backcourt:fg_pct_backcourt))
combo_totals <- range_totals_df %>%
  inner_join(traditional_totals_df %>%
               select(player_id, pts, gp, fgm, fga, fg_pct,
                      fg3m, fg3a, fg3_pct, ftm, fta, ft_pct))
