# The purpose of this code is to find the YoY correlation of Ridge-Regressed Rebounding Rate using data from the 2021-2022
# and 2022-2023 seasons. The data comes from nbashotcharts.com and can be found there or in repo csv files. 

library(tidyverse)
library(hoopR)

# Necessary Datasets (in CSV files. Make sure to have numeric cols as numeric)

FourFactorsRAPM2022
FourFactorsRAPM2023

# Data Manipulation

FourFactorsRAPM2022_reb <- FourFactorsRAPM2022 %>%
  select(playerId, playerName, season, RA_ORBD, RA_ORBD__Off, RA_ORBD__Def) %>%
  rename(RA_ORBD22 = RA_ORBD, RA_ORBD__Off22 = RA_ORBD__Off, RA_ORBD__Def22 = RA_ORBD__Def)

FourFactorsRAPM2023_reb <- FourFactorsRAPM2023 %>%
  select(playerId, playerName, season, RA_ORBD, RA_ORBD__Off, RA_ORBD__Def) %>%
  rename(RA_ORBD23 = RA_ORBD, RA_ORBD__Off23 = RA_ORBD__Off, RA_ORBD__Def23 = RA_ORBD__Def) 

# Getting player total minutes

min_23 <- hoopR::nba_leaguedashplayerstats(season = "2022-23")

min_23 <- min_23[["LeagueDashPlayerStats"]]

min_22 <- hoopR::nba_leaguedashplayerstats(season = "2021-22")

min_22 <- min_22[["LeagueDashPlayerStats"]]

min_22$MIN <- as.numeric(min_22$MIN)
min_23$MIN <- as.numeric(min_23$MIN)

min_22$PLAYER_ID <- as.numeric(min_22$PLAYER_ID)
min_23$PLAYER_ID <- as.numeric(min_23$PLAYER_ID)

min_22 <- min_22 %>%
  filter(MIN >= 500) %>%
  select(PLAYER_ID, MIN)

min_23 <- min_23 %>%
  filter(MIN >= 500) %>%
  select(PLAYER_ID, MIN)

# Adding Minutes Data to Dataframes

FourFactorsRAPM2022_reb <- merge(FourFactorsRAPM2022_reb, min_22, by.x = "playerId", by.y = "PLAYER_ID")
FourFactorsRAPM2023_reb <- merge(FourFactorsRAPM2023_reb, min_23, by.x = "playerId", by.y = "PLAYER_ID")

# Joining Dataframes from '22 and '23 (only for players with 500 min in each year)

matching_rows <- intersect(FourFactorsRAPM2022_reb$playerId, FourFactorsRAPM2023_reb$playerId)

FourFactorsRAPM2223_reb <- inner_join(FourFactorsRAPM2022_reb, FourFactorsRAPM2023_reb, by = c("playerId", "playerName"))


# YoY correlation of Ridge-Regressed Rebounding Rate

cor(FourFactorsRAPM2223_reb$RA_ORBD22, FourFactorsRAPM2223_reb$RA_ORBD23)
#0.42

cor(FourFactorsRAPM2223_reb$RA_ORBD__Off22, FourFactorsRAPM2223_reb$RA_ORBD__Off23)
#0.45

cor(FourFactorsRAPM2223_reb$RA_ORBD__Def22, FourFactorsRAPM2223_reb$RA_ORBD__Def23)
#0.35

# Plots of YoY relationship for OReb and DReb

ggplot(FourFactorsRAPM2223_reb, aes(x = RA_ORBD__Off22, y = RA_ORBD__Off23)) +
  geom_point() +
  geom_smooth(method = "lm") +
  labs(title = "Year to Year Correlation of Offensive Rebounding Impact",
       x = "2022 OReb Impact", y = "2023 OReb Impact") +
  coord_cartesian(xlim = c(-2, 3.2), ylim = c(-2, 3)) +
  theme(plot.title = element_text(hjust = 0.5))

ggplot(FourFactorsRAPM2223_reb, aes(x = RA_ORBD__Def22, y = RA_ORBD__Def23)) +
  geom_point() +
  geom_smooth(method = "lm") +
  labs(title = "Year to Year Correlation of Defensive Rebounding Impact",
       x = "2022 DReb Impact", y = "2023 DReb Impact") +
  coord_cartesian(xlim = c(-1.5, 1.9), ylim = c(-1.5, 1.9)) +
  theme(plot.title = element_text(hjust = 0.5))
