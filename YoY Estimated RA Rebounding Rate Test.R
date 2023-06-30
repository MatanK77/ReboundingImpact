# This code uses two dataframes, a_2122 and a_2223 (which contain estimated ridge-regressed rebounding rate),  created using the Rebound Impact Function 
# and tests/plots their YoY correlation.


library(tidyverse)


# Testing Rebounding Models

matching_rows2 <- intersect(a_2122$name, a_2223$name)

a_2122 <- a_2122 %>%
  rename(playerName = name, cluster_22 = cluster, min_22 = Min, predicted_reb_impact_22 = predicted_reb_impact,
         predicted_dreb_impact_22 = predicted_dreb_impact, predicted_oreb_impact_22 = predicted_oreb_impact, pred_dreb_xg_22 = pred_dreb_xg)

a_2223 <- a_2223 %>%
  rename(playerName = name, cluster_23 = cluster, min_23 = Min, predicted_reb_impact_23 = predicted_reb_impact,
         predicted_dreb_impact_23 = predicted_dreb_impact, predicted_oreb_impact_23 = predicted_oreb_impact, pred_dreb_xg_23 = pred_dreb_xg)

a_2122 <- a_2122 %>% 
  filter(min_22 >= 500)

a_2223 <- a_2223 %>% 
  filter(min_23 >= 500)

rebound_impact_test <- inner_join(a_2122, a_2223, by = c("playerName"))

cor(rebound_impact_test$predicted_reb_impact_22, rebound_impact_test$predicted_reb_impact_23)

cor(rebound_impact_test$predicted_oreb_impact_22, rebound_impact_test$predicted_oreb_impact_23)

cor(rebound_impact_test$predicted_dreb_impact_22, rebound_impact_test$predicted_dreb_impact_23)

cor(rebound_impact_test$pred_dreb_xg_22, rebound_impact_test$pred_dreb_xg_23)



ggplot(rebound_impact_test, aes(x = predicted_oreb_impact_22, y = predicted_oreb_impact_23)) +
  geom_point() +
  geom_smooth(method = "lm") +
  labs(title = "Year to Year Correlation of Estimated Offensive Rebounding Impact",
       x = "2022 Estimated OReb Impact", y = "2023 Estimated OReb Impact") +
  scale_y_continuous(breaks = c(-2, -1, 0, 1, 2, 3, 4, 5, 6)) +
  scale_x_continuous(breaks = c(-2, -1, 0, 1, 2, 3, 4, 5, 6)) +
  theme(plot.title = element_text(hjust = 0.5))

ggplot(rebound_impact_test, aes(x = predicted_dreb_impact_22, y = predicted_dreb_impact_23)) +
  geom_point() +
  geom_smooth(method = "lm") +
  labs(title = "Year to Year Correlation of Estimated Defensive Rebounding Impact",
       x = "2022 Estimated DReb Impact", y = "2023 Estimated DReb Impact") +
  scale_y_continuous(breaks = c(-3,-2, -1, 0, 1, 2, 3)) +
  scale_x_continuous(breaks = c(-3,-2, -1, 0, 1, 2, 3)) +
  theme(plot.title = element_text(hjust = 0.5))

ggplot(rebound_impact_test, aes(x = pred_dreb_xg_22, y = pred_dreb_xg_23)) +
  geom_point() +
  geom_smooth(method = "lm") +
  labs(title = "YoY Correlation of Estimated D-Reb Impact with XGBoost",
       x = "2022 Estimated DReb Impact", y = "2023 Estimated DReb Impact") +
  scale_y_continuous(breaks = c(-1.5, -1, -0.5, 0, 0.5, 1, 1.5)) +
  scale_x_continuous(breaks = c(-1.5, -1, -0.5, 0, 0.5, 1, 1.5)) +
  theme(plot.title = element_text(hjust = 0.5))

