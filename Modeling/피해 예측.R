library(dplyr)
library(sf)
library(spgwr)
library(GWmodel)
library(caret)
df <- readxl::read_excel("C:/Users/82109/OneDrive/고려대/grad/DAB/1009데이터.xlsx")
df <- st_as_sf(df,wkt="geometry",crs=5179)


df_yes <- df %>% filter(flood_index != 0)
df_no <- df %>% filter(flood_index == 0)

#####침수가 일어나지 않은 격자에 대한 피해지수 예측 코드
yes_cent <- st_centroid(df_yes)
filtered_sp1 <- as(yes_cent, "Spatial")
no_cent <- st_centroid(df_no)
filtered_sp <- as(no_cent, "Spatial")

predicted_y_values <- gwr.predict(flood_index ~ elevation + river_dist + rmg_dist + sewer_dist + slope_dist,
                                  data = filtered_sp1, predictdata= filtered_sp, bw=53, kernel="gaussian",adaptive=FALSE)
gwr_coefficients <- as.data.frame(predicted_y_values$SDF)
df_no$pred_y <- gwr_coefficients$prediction

filtered_sp$elevation<-as.numeric(filtered_sp$elevation)

# 침수 피해 등급 나누기(quantile)
predicted_sf <- df_damage_scaled%>%
  mutate(damage_grade_quantile = cut(pred_y, 
                                     breaks = quantile(pred_y, probs = seq(0, 1, by = 0.25)),  # 사분위수 기준으로 나누기
                                     labels = c("Q1", "Q2", "Q3", "Q4"),  # 사분위수 기준 등급 이름 설정
                                     include.lowest = TRUE))
ggplot(df_yes_clean) +
  geom_sf(aes(fill = damage_grade_quantile)) +  # damage_grade 열을 기준으로 색상 설정
  scale_fill_manual(values = c("Q1" = "lightblue", "Q2" = "yellow", "Q3" = "orange", "Q4" = "red")) +
  labs(title = "Flood Damage Grade Map", fill = "Damage Grade") +
  theme_minimal()
