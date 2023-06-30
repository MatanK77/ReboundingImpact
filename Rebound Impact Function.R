# This function takes a given season as an input (format of "YYYY-yy) along with 3 models: a Ridge-Regressed Offensive Rebounding model,
# a RR DReb model and an XGBoost DReb Model. The construction of these models can be found elsewhere in the repo (Rebound Impact Modeling
# and XGBoost DReb Impact). It outputs a leaderboard for that given season in predicted rebounding impact. It can be used for any season where
# tracking data is available but was modeled on 2020-2023 data (The NBA seems to have changed boxout definitions prior to then, so models have
# to be trained on prior data to be accurate). In order to run the function, make sure to also have the kmeans.out clustering object from the 
# "Rebound Impact Modeling" script in the Global Environment.





reb_impact_function_withxg <- function(season, orebmodel, drebmodel, drebmodelxgboost) {
 
   # Get Rebound Tracking Data Using HoopR
  Q_func <- nba_leaguedashptstats(league_id = "00", per_mode = "Totals", player_or_team = "Player", pt_measure_type = "Rebounding", season = season)
  
  rebounding_tracking_func <- Q_func[["LeagueDashPtStats"]]
  
  rebounding_tracking_func <- rebounding_tracking_func %>%
    mutate_at(c(5:35), as.numeric)
  rebounding_tracking_func$PLAYER_ID <- as.numeric(rebounding_tracking_func$PLAYER_ID)
  
  # Get League Hustle Stats (Box Outs)
  Z_func <- nba_leaguehustlestatsplayer(league_id = "00", per_mode = "Totals", season = season)
  
  boxout_hustle_func <- Z_func[["HustleStatsPlayer"]]
  
  boxout_hustle_func <- boxout_hustle_func %>%
    mutate_at(c(5:28), as.numeric)
  
  boxout_hustle_func$PLAYER_ID <- as.numeric(boxout_hustle_func$PLAYER_ID)
  
  reb_tracking_data_func <- rebounding_tracking_func %>%
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
  
  boxout_data_func <- boxout_hustle_func %>%
    group_by(PLAYER_ID, PLAYER_NAME) %>%
    summarise(Min = sum(MIN),
              Offensive_boxouts = sum(OFF_BOXOUTS),
              Defensive_boxouts = sum(DEF_BOXOUTS),
              Boxouts = sum(BOX_OUTS),
              Team_Rebound_When_Boxout = sum(BOX_OUT_PLAYER_TEAM_REBS),
              Player_Rebound_When_Boxout = sum(BOX_OUT_PLAYER_REBS))
  
  
  full_dataset_func <- merge(boxout_data_func, reb_tracking_data_func, by.x = "PLAYER_ID", by.y = "PLAYER_ID") %>%
    rename(Min = Min.x)
  
  full_dataset_pm_func <- full_dataset_func %>%
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
  
  height<- hoopR::nba_leaguedashplayerbiostats(season = season)
  height <- height[["LeagueDashPlayerBioStats"]]
  height <- height %>%
    select(PLAYER_ID,PLAYER_HEIGHT_INCHES)
  
  full_dataset_pm_func <- merge(full_dataset_pm_func, height, by = "PLAYER_ID")
  
  full_dataset_pm_func$PLAYER_HEIGHT_INCHES <- as.numeric(full_dataset_pm_func$PLAYER_HEIGHT_INCHES)
  
  
  # Adding Basic Player Data
  basic_player_data_func <- nba_leaguedashplayerstats(per_mode = "Per100Possessions", season = season)
  
  basic_player_data_func <- basic_player_data_func[["LeagueDashPlayerStats"]]
  
  basic_player_data_func <- basic_player_data_func %>%
    mutate_at(c(6:66), as.numeric)
  
  basic_player_data_func$PLAYER_ID <- as.numeric(basic_player_data_func$PLAYER_ID)
  
  # Filter for a minimum number of GP and filter for stats used for clustering
  
  basic_data_for_kmeans_func <- basic_player_data_func %>%
    filter(GP >= 30) %>%
    select(PLAYER_NAME, PLAYER_ID,FGA, FG_PCT, FG3A, FTA, FT_PCT, OREB, DREB, AST, TOV, STL, BLK, PF, PTS)
  
  contested_data_func <- boxout_hustle_func %>%
    group_by(PLAYER_ID, PLAYER_NAME) %>%
    summarise(Min = sum(MIN),
              Defensive_boxouts = sum(DEF_BOXOUTS),
              Defensive_boxouts_pm = Defensive_boxouts * 48 / Min,
              contested_2pt = sum(CONTESTED_SHOTS_2PT),
              contested_2pt_pm = contested_2pt * 48 / Min,
              contested_3pt = sum(CONTESTED_SHOTS_3PT),
              contested_3pt_pm = contested_3pt * 48 / Min)
  
  # Merging the boxscore data and defensive tracking data
  
  defensive_cluster_data_func <- merge(basic_data_for_kmeans_func, contested_data_func, by.x = "PLAYER_ID", by.y = "PLAYER_ID")
  
  
  # These are the statistics that will be used to decide clusters
  
  defensive_cluster_data_func <- defensive_cluster_data_func  %>%
    select(PLAYER_ID, PLAYER_NAME.x, Min, DREB, STL, BLK, Defensive_boxouts_pm, contested_2pt_pm, contested_3pt_pm) %>%
    drop_na()
  
  # Scale Data for clustering
  
  defensive_cluster_data_func[,c(4:9)] = scale(defensive_cluster_data_func[,c(4:9)])
  
  
  # Assign the new data points to the existing clusters based on the previously obtained centroids
  
  defensive_cluster_data_func$cluster <- kmeans(defensive_cluster_data_func[, c(4:9)], centers = kmeans.out$centers)$cluster
  
  
  # Players and their Clusters
  players_and_cluster2_func <- tibble(name = defensive_cluster_data_func$PLAYER_NAME, 
                                      cluster = defensive_cluster_data_func$cluster, PLAYER_ID = defensive_cluster_data_func$PLAYER_ID)
  
  players_and_cluster2_func$cluster <- as.character(players_and_cluster2_func$cluster)
  
  # Adding player cluster IDs to rebound tracking data
  
  full_dataset_pm_clusters2_func <- merge(players_and_cluster2_func, full_dataset_pm_func)
  
  
  # Add Predictions to Df
  
  full_dataset_pm_clusters2_func$predicted_oreb_impact <- predict(orebmodel, full_dataset_pm_clusters2_func)
  
  full_dataset_pm_clusters2_func$predicted_dreb_impact <- predict(drebmodel, full_dataset_pm_clusters2_func)
  
  
  #Xg dataset
  full_dataset_pm_clusters2_forxg_func <- full_dataset_pm_clusters2_func %>%
    select(cluster, PLAYER_HEIGHT_INCHES, `Adjusted_DReb_Chance%`, DReb_Distance, Defensive_boxouts_pm, Contested_DReb_pm, 
           Uncontested_DReb_pm)
  
  full_dataset_pm_clusters2_forxg_func$cluster <- factor(full_dataset_pm_clusters2_forxg_func$cluster, levels = 1:8)
  
  dummy_func <- dummyVars(" ~ .", data= full_dataset_pm_clusters2_forxg_func)
  full_dataset_pm_clusters2_forxg_func <- data.frame(predict(dummy_func, newdata = full_dataset_pm_clusters2_forxg_func)) 
  
  
  #Predict
  
  full_pred_func <- predict(drebmodelxgboost, newdata = as.matrix(full_dataset_pm_clusters2_forxg_func))
  
  full_pred_df_func <- as_tibble(full_pred_func)
  
  full_dataset_pm_clusters2_func <- full_dataset_pm_clusters2_func %>%
    mutate(pred_dreb_xg = full_pred_df_func$value)
  
  
  # Summary of RAPM Rebounding and Predicted Impact
  
  rebound_impact_func <- full_dataset_pm_clusters2_func %>%
    mutate(predicted_reb_impact = predicted_oreb_impact + predicted_dreb_impact) %>%
    select(PLAYER_NAME.x, cluster, Min, predicted_reb_impact, predicted_oreb_impact, predicted_dreb_impact, pred_dreb_xg) %>%
    rename(name = PLAYER_NAME.x)
  
  return(rebound_impact_func)
}



a_2122<- reb_impact_function_withxg(season = "2021-22", orebmodel = linear_model_oreb_noPos_simple, drebmodel = linear_model_dreb_clusters_simple,
                                    drebmodelxgboost = xgb_model)

a_2223 <- reb_impact_function_withxg(season = "2022-23", orebmodel = linear_model_oreb_noPos_simple, drebmodel = linear_model_dreb_clusters_simple,
                                     drebmodelxgboost = xgb_model)

a_2122<- reb_impact_function_withxg(season = "2021-22", orebmodel = linear_model_oreb_noPos_simple, drebmodel = linear_model_dreb_clusters_simple,
                                    drebmodelxgboost = xgb_model)
