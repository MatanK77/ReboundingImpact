# This code uses two initial datasets. FourFactorsRAPM is from nbashotcharts.com and contains ridge-regressed rebounding rate for the 3 seasons 2020-2023.
# X20_23_4000_Minutes is from Basketball Reference and contains players with 4000+ minutes in those years (as well as some other useful information.
# Using this data along with nba tracking data from the nba api (acquired via the HoopR package), ridge-regressed rebounding rate is modeled. 
# Offensive rebounding rate is modeled using tracking data + height, while defensive rebounding rate is modeled with the inclusion of player clusters (k-means).
 

# Some Packages

library(tidyverse)
library(mudata2)
library(hoopR)
library(mgcv)
library(RColorBrewer)

# Necessary Datasets (found in csv files in repo)

FourFactorsRAPM
X20_23_4000_Minutes



# Merge the Datasets 

df_fourfactors_4000min <- merge(FourFactorsRAPM, X20_23_4000_Minutes, by.x = "playerName", by.y = "Player")

# Select the Useful Columns

df_reb_rapm_4000min <- df_fourfactors_4000min %>%
  select(playerName, playerId, Age, `MP▼`, Pos ,RA_ORBD, RA_ORBD_Rank, RA_ORBD__Def, RA_ORBD__Def_Rank, RA_ORBD__Off, RA_ORBD__Off_Rank)

# Get Rebound Tracking Data Using HoopR
Q <- nba_leaguedashptstats(league_id = "00", per_mode = "Totals", player_or_team = "Player", pt_measure_type = "Rebounding", season = "2022-23")

rebounding_tracking_22_23 <- Q[["LeagueDashPtStats"]]

rebounding_tracking_22_23 <- rebounding_tracking_22_23 %>%
  mutate_at(c(5:35), as.numeric)
rebounding_tracking_22_23$PLAYER_ID <- as.numeric(rebounding_tracking_22_23$PLAYER_ID)

# Get League Hustle Stats (Box Outs)
Z <- nba_leaguehustlestatsplayer(league_id = "00", per_mode = "Totals", season = "2022-23")

boxout_hustle_22_23 <- Z[["HustleStatsPlayer"]]

boxout_hustle_22_23 <- boxout_hustle_22_23 %>%
  mutate_at(c(5:28), as.numeric)

boxout_hustle_22_23$PLAYER_ID <- as.numeric(boxout_hustle_22_23$PLAYER_ID)

# Repeat these two dataframes for the '21-22 season and '20-21 season
A <- nba_leaguedashptstats(league_id = "00", per_mode = "Totals", player_or_team = "Player", pt_measure_type = "Rebounding", season = "2021-22")

rebounding_tracking_21_22 <- A[["LeagueDashPtStats"]]

rebounding_tracking_21_22 <- rebounding_tracking_21_22 %>%
  mutate_at(c(5:35), as.numeric)
rebounding_tracking_21_22$PLAYER_ID <- as.numeric(rebounding_tracking_21_22$PLAYER_ID)

B <- nba_leaguedashptstats(league_id = "00", per_mode = "Totals", player_or_team = "Player", pt_measure_type = "Rebounding", season = "2020-21")

rebounding_tracking_20_21 <- B[["LeagueDashPtStats"]]

rebounding_tracking_20_21 <- rebounding_tracking_20_21 %>%
  mutate_at(c(5:35), as.numeric)
rebounding_tracking_20_21$PLAYER_ID <- as.numeric(rebounding_tracking_20_21$PLAYER_ID)

C <- nba_leaguehustlestatsplayer(league_id = "00", per_mode = "Totals", season = "2021-22")

boxout_hustle_21_22 <- C[["HustleStatsPlayer"]]

boxout_hustle_21_22 <- boxout_hustle_21_22 %>%
  mutate_at(c(5:28), as.numeric)

boxout_hustle_21_22$PLAYER_ID <- as.numeric(boxout_hustle_21_22$PLAYER_ID)

D <- nba_leaguehustlestatsplayer(league_id = "00", per_mode = "Totals", season = "2020-21")

boxout_hustle_20_21 <- D[["HustleStatsPlayer"]]

boxout_hustle_20_21 <- boxout_hustle_20_21 %>%
  mutate_at(c(5:28), as.numeric)

boxout_hustle_20_21$PLAYER_ID <- as.numeric(boxout_hustle_20_21$PLAYER_ID) 

# Add Columns for the Season to each df

rebounding_tracking_22_23 <- rebounding_tracking_22_23 %>%
  mutate(Season = "2022-2023", .after = PLAYER_ID)

rebounding_tracking_21_22 <- rebounding_tracking_21_22 %>%
  mutate(Season = "2021-2022", .after = PLAYER_ID)

rebounding_tracking_20_21 <- rebounding_tracking_20_21 %>%
  mutate(Season = "2020-2021", .after = PLAYER_ID)

boxout_hustle_22_23 <- boxout_hustle_22_23 %>%
  mutate(Season = "2022-2023", .after = PLAYER_ID)

boxout_hustle_21_22 <- boxout_hustle_21_22 %>%
  mutate(Season = "2021-2022", .after = PLAYER_ID)

boxout_hustle_20_21 <- boxout_hustle_20_21 %>%
  mutate(Season = "2020-2021", .after = PLAYER_ID)

# Join the 3 years of each df type together (boxout_hustle and rebounding_tracking)
 
rebounding_tracking_20_23 <- rbind(rebounding_tracking_22_23, rebounding_tracking_21_22, rebounding_tracking_20_21)

boxout_hustle_20_23 <- rbind(boxout_hustle_22_23, boxout_hustle_21_22, boxout_hustle_20_21)

# Make summary statistics for each large df, grouped by player_id

reb_tracking_data <- rebounding_tracking_20_23 %>%
  group_by(PLAYER_ID, PLAYER_NAME) %>%
  summarise(Min = sum(MIN),
            OReb = sum(OREB),
            Contested_OReb = sum(OREB_CONTEST),
            Uncontested_OReb = sum(OREB_UNCONTEST),
            `Contested% OReb` = sum(OREB_CONTEST)/sum(OREB),
            OReb_Chances = sum(OREB_CHANCES),
            `OReb_Chance%` = sum(OREB)/sum(OREB_CHANCES),
            Defered_OReb_Chances = sum(OREB_CHANCE_DEFER),
            `Adjusted_OReb_Chance%` = sum(OREB)/(sum(OREB_CHANCES) - sum(OREB_CHANCE_DEFER)),
            OReb_Distance = sum(AVG_OREB_DIST * (OREB/sum(OREB))),
            DReb = sum(DREB),
            Contested_DReb = sum(DREB_CONTEST),
            Uncontested_DReb = sum(DREB_UNCONTEST),
            `Contested% DReb` = sum(DREB_CONTEST)/sum(DREB),
            DReb_Chances = sum(DREB_CHANCES),
            `DReb_Chance%` = sum(DREB)/sum(DREB_CHANCES),
            Defered_DReb_Chances = sum(DREB_CHANCE_DEFER),
            `Adjusted_DReb_Chance%` = sum(DREB)/(sum(DREB_CHANCES) - sum(DREB_CHANCE_DEFER)),
            DReb_Distance = sum(AVG_DREB_DIST * (DREB/sum(DREB)))
            
  )

boxout_data <- boxout_hustle_20_23 %>%
  group_by(PLAYER_ID, PLAYER_NAME) %>%
  summarise(Min = sum(MIN),
            Offensive_boxouts = sum(OFF_BOXOUTS),
            Defensive_boxouts = sum(DEF_BOXOUTS),
            Boxouts = sum(BOX_OUTS),
            Team_Rebound_When_Boxout = sum(BOX_OUT_PLAYER_TEAM_REBS),
            Player_Rebound_When_Boxout = sum(BOX_OUT_PLAYER_REBS))

# Full Dataset gets put together into one Dataframe
df_reb_rapm_4000min <- df_reb_rapm_4000min %>%
  rename(Min = `MP▼`, PLAYER_ID = playerId, PLAYER_NAME = playerName)

partial_dataset <- merge(boxout_data, reb_tracking_data, by.x = "PLAYER_ID", by.y = "PLAYER_ID")

full_dataset <- merge(partial_dataset, df_reb_rapm_4000min, by.x = "PLAYER_ID", by.y = "PLAYER_ID")

# Making Stats on a Per 36 Minute Basis
full_dataset_pm <- full_dataset %>%
  mutate(Offensive_boxouts_pm = (Offensive_boxouts/Min)*36,
         Defensive_boxouts_pm = (Defensive_boxouts/Min)*36,
         OReb_pm = (OReb/Min)*36,
         Contested_OReb_pm = (Contested_OReb/Min)*36,
         Uncontested_OReb_pm = (Uncontested_OReb/Min)*36,
         DReb_pm = (DReb/Min)*36,
         Contested_DReb_pm = (Contested_DReb/Min)*36,
         Uncontested_DReb_pm = (Uncontested_DReb/Min)*36,
  )

# Adding Height to Data

height<- hoopR::nba_leaguedashplayerbiostats(season = "2022-23")
height <- height[["LeagueDashPlayerBioStats"]]
height <- height %>%
  select(PLAYER_ID,PLAYER_HEIGHT_INCHES)

full_dataset_pm <- merge(full_dataset_pm, height, by = "PLAYER_ID")

full_dataset_pm$PLAYER_HEIGHT_INCHES <- as.numeric(full_dataset_pm$PLAYER_HEIGHT_INCHES)

# Testing a linear model for ORBD_impact that includes position data. I also tested various other modeling methods, including
# tree-based models. Ultimately, they performed as well or worse than simple multiple linear regression and due to the ease of 
# interpreting linear models, it was a pretty easy decision to just use a linear model.

linear_model_oreb <- lm(RA_ORBD__Off ~ Pos + Contested_OReb_pm + Uncontested_OReb_pm + Offensive_boxouts_pm + `Adjusted_OReb_Chance%` +
                          OReb_Distance, data = full_dataset_pm)

summary(linear_model_oreb)

# Now removing unnecessary variables that aren't close to p = 0.05

linear_model_oreb_simple <- lm(RA_ORBD__Off ~ Pos + Contested_OReb_pm + Uncontested_OReb_pm , data = full_dataset_pm)

summary(linear_model_oreb_simple)

# ORBD_impact can also be estimated simply without positional data (also removing non-key variables). 
# Position data can be conflicting from source to source etc and is categorical v numeric. 
# Using player height instead is a reasonable choice.

linear_model_oreb_noPos_simple <- lm(RA_ORBD__Off ~ Contested_OReb_pm + Uncontested_OReb_pm +
                                       PLAYER_HEIGHT_INCHES, data = full_dataset_pm)

summary(linear_model_oreb_noPos_simple)

# For defensive rebounding, models in general seem to do a worse job fitting rebounding impact. As a result, instead of using simple positions
# in the model, I created some clusters to estimate player defensive role using tracking data. I also tried this approach with offensive
# rebounding, but the clusters didn't have as much impact so they were left out to make the OReb model simpler.

# Clusters:
# First importing some basic player data

basic_player_data_22_23 <- nba_leaguedashplayerstats(per_mode = "Per100Possessions", season = "2022-23")

basic_player_data_22_23 <- basic_player_data_22_23[["LeagueDashPlayerStats"]]

basic_player_data_22_23 <- basic_player_data_22_23 %>%
  mutate_at(c(6:66), as.numeric)

basic_player_data_22_23$PLAYER_ID <- as.numeric(basic_player_data_22_23$PLAYER_ID)

# Filter for a minimum number of GP and filter for stats used for clustering

basic_data_for_kmeans <- basic_player_data_22_23 %>%
  filter(GP >= 30) %>%
  select(PLAYER_NAME, PLAYER_ID,FGA, FG_PCT, FG3A, FTA, FT_PCT, OREB, DREB, AST, TOV, STL, BLK, PF, PTS)

# Now getting the defensive contest and boxout data for clustering

contested_data <- boxout_hustle_20_23 %>%
  group_by(PLAYER_ID, PLAYER_NAME) %>%
  summarise(Min = sum(MIN),
            Defensive_boxouts = sum(DEF_BOXOUTS),
            Defensive_boxouts_pm = Defensive_boxouts * 48 / Min,
            contested_2pt = sum(CONTESTED_SHOTS_2PT),
            contested_2pt_pm = contested_2pt * 48 / Min,
            contested_3pt = sum(CONTESTED_SHOTS_3PT),
            contested_3pt_pm = contested_3pt * 48 / Min)

# Merging the boxscore data and defensive tracking data

defensive_cluster_data <- merge(basic_data_for_kmeans, contested_data, by.x = "PLAYER_ID", by.y = "PLAYER_ID")

# These are the statistics that will be used to decide clusters

defensive_cluster_data <- defensive_cluster_data  %>%
  select(PLAYER_ID, PLAYER_NAME.x, Min, DREB, STL, BLK, Defensive_boxouts_pm, contested_2pt_pm, contested_3pt_pm)

# Scale Data for clustering

defensive_cluster_data[,c(4:9)] = scale(defensive_cluster_data[,c(4:9)])

# Testing the appropriate amount of clusters

set.seed(225)
clusters <- 20
within_sum_squares_error <- numeric(clusters)

for (i in 1:clusters) {
  kmeans.out <- kmeans(defensive_cluster_data[,c(4:9)], centers = i, nstart = 22)
  within_sum_squares_error[i] <- kmeans.out$tot.withinss
}

# Plot within sum of squares error v clusters

wsse_tibble <- tibble(clusters = 1:clusters, wsse =  within_sum_squares_error) 

ggplot(wsse_tibble, aes(x = clusters, y = wsse, group = 1)) +
  geom_point(size = 4)+
  geom_line() + 
  labs(xlab = "Amount of Clusters", title = "Scree Plot") + 
  scale_x_continuous(breaks = c(1, 5, 10, 15, 20, 25))

# Going to use 8 clusters
k <- 8
set.seed(227)
kmeans.out <- kmeans(defensive_cluster_data[,c(4:9)], centers = 8, nstart = 20)

# Putting clusters into table

kmeans_clusters <- as.data.frame(kmeans.out$centers)

kmeans_clusters <- kmeans_clusters %>%
  mutate(Cluster = c("C1", "C2", "C3", "C4", "C5", "C6", "C7", "C8"), .before = DREB)

kmeans_clusters_transposed <- data.frame(t(kmeans_clusters[-1]))

# Visualizing Clusters

kmeans_clusters_longer <- pivot_longer(kmeans_clusters, cols = c(2:7), names_to = "Statistic", values_to = "Standard_Devs")

ggplot(data = kmeans_clusters_longer, aes(x = Standard_Devs, y = Statistic, color = Cluster)) +
  geom_point(size = 3) +
  labs(title = "Strengths and Weaknesses of Each Defensive Cluster", x = "Standard Deviations", y = element_blank()) +
  scale_color_brewer(palette = "Dark2") 

# Players and their Clusters
players_and_cluster <- tibble(name = defensive_cluster_data$PLAYER_NAME, cluster = kmeans.out$cluster, PLAYER_ID = defensive_cluster_data$PLAYER_ID)

players_and_cluster$cluster <- as.character(players_and_cluster$cluster)

# Adding player cluster results to rebound tracking data

full_dataset_pm_clusters <- merge(players_and_cluster, full_dataset_pm)

# Linear Model For Defensive Rebounding Using Clusters
linear_model_dreb_clusters <- lm(RA_ORBD__Def ~ cluster + Contested_DReb_pm + Uncontested_DReb_pm + Defensive_boxouts_pm + `Adjusted_DReb_Chance%` +
                                    DReb_Distance, data = full_dataset_pm_clusters)

summary(linear_model_dreb_clusters)

# Remove some Vars. The key statistics in DReb are Boxouts and Contested Rebounds.
linear_model_dreb_clusters_simple <- lm(RA_ORBD__Def ~ cluster + Contested_DReb_pm + Defensive_boxouts_pm, data = full_dataset_pm_clusters)

summary(linear_model_dreb_clusters_simple)

# Adding Predicted Rebound Impact to Datasets

full_dataset_pm_clusters$predicted_oreb_impact <- predict(linear_model_oreb_simple, full_dataset_pm_clusters)

full_dataset_pm_clusters$predicted_dreb_impact <- predict(linear_model_dreb_clusters_simple, full_dataset_pm_clusters)

# Cleaning up full_dataset_pm_clusters

full_dataset_pm_clusters <- full_dataset_pm_clusters %>%
  select(PLAYER_ID, name, Pos, cluster, PLAYER_HEIGHT_INCHES, Min, Offensive_boxouts, Defensive_boxouts, Boxouts, OReb,
         Contested_OReb, Uncontested_OReb, `Contested% OReb`, OReb_Chances, `OReb_Chance%`,
         `Adjusted_OReb_Chance%`, OReb_Distance, DReb, Contested_DReb, Uncontested_DReb, `Contested% DReb`,
         DReb_Chances, `DReb_Chance%`, `Adjusted_DReb_Chance%`, DReb_Distance, RA_ORBD, RA_ORBD_Rank,
         RA_ORBD__Off, RA_ORBD__Off_Rank, RA_ORBD__Def, RA_ORBD__Def_Rank, Offensive_boxouts_pm,
         Defensive_boxouts_pm, OReb_pm, Contested_OReb_pm, Uncontested_OReb_pm, DReb_pm, Contested_DReb_pm,
         Uncontested_DReb_pm, predicted_oreb_impact, predicted_dreb_impact)

# Summary of Ridge Regressed Rebounding and Predicted Impact

rebound_impact <- full_dataset_pm_clusters %>%
  mutate(predicted_reb_impact = predicted_oreb_impact + predicted_dreb_impact) %>%
  select(name, cluster, RA_ORBD, predicted_reb_impact, RA_ORBD__Off, predicted_oreb_impact, RA_ORBD__Def, predicted_dreb_impact)
