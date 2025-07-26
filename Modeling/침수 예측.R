library(dplyr)
library(sf)
library(spgwr)
library(GWmodel)
library(writexl)
library(readxl)
library(ggplot2)

df <- readxl::read_excel("C:/Users/82109/OneDrive/고려대/grad/DAB/1009데이터.xlsx")
df <- st_as_sf(df,wkt="geometry",crs=5181)
df$elevation<-as.numeric(df$elevation)

df_new <- st_centroid(df)
df_new_sp <- as(df_new, "Spatial")

predicted_y_values<-gwr.predict(flood_index ~ elevation + river_dist + rmg_dist + 
                                  sewer_dist + slope_dist,data=df_cent,predictdata=df_new_sp,bw=761.4911,
                                kernel="gaussian",adaptive=FALSE)
gwr_coef<-as.data.frame(predicted_y_values$SDF)
df$pred_y<-gwr_coef$prediction

predicted_sf <- df %>%
  mutate(grade_quantile = cut(pred_y, 
                              breaks = quantile(pred_y, probs = seq(0, 1, by = 0.25)),  # 사분위수 기준으로 나누기
                              labels = c("Q1", "Q2", "Q3", "Q4"),  # 사분위수 기준 등급 이름 설정
                              include.lowest = TRUE))
ggplot(predicted_sf) +
  geom_sf(aes(fill = grade_quantile)) +  # damage_grade 열을 기준으로 색상 설정
  scale_fill_manual(values = c("Q1" = "lightblue", "Q2" = "yellow", "Q3" = "orange", "Q4" = "red")) +
  labs(title = "Flood Damage Grade Map", fill = "Damage Grade") +
  theme_minimal()


# 클러스터별 회귀계수 뽑기
clust0 <- read_excel("C:/Users/82109/OneDrive/고려대/grad/DAB/cluster_0.xlsx")
clust1 <- read_excel("C:/Users/82109/OneDrive/고려대/grad/DAB/cluster_1.xlsx")
clust2 <- read_excel("C:/Users/82109/OneDrive/고려대/grad/DAB/cluster_2.xlsx")

flood_cl0 <- df %>% filter(grid_id %in% clust0$grid_id_1)
flood_cl1 <- df %>% filter(grid_id %in% clust1$grid_id_1)
flood_cl2 <- df %>% filter(grid_id %in% clust2$grid_id_1)

cent0 <- st_centroid(flood_cl0)
cent0$X <- st_coordinates(cent0)[,1]
cent0$Y <- st_coordinates(cent0)[,2]

cent1 <- st_centroid(flood_cl1)
cent1$X <- st_coordinates(cent1)[,1]
cent1$Y <- st_coordinates(cent1)[,2]

cent2 <- st_centroid(flood_cl2)
cent2$X <- st_coordinates(cent2)[,1]
cent2$Y <- st_coordinates(cent2)[,2]

coef0 <- gwr_coef %>% filter(coords.x1 %in% cent0$X & coords.x2 %in% cent0$Y)
coef1 <- gwr_coef %>% filter(coords.x1 %in% cent1$X & coords.x2 %in% cent1$Y)
coef2 <- gwr_coef %>% filter(coords.x1 %in% cent2$X & coords.x2 %in% cent2$Y)

coef.mean0 <- coef0 %>% summarise(elevation = mean(elevation_coef),
                                  river_dist = mean(river_dist_coef),
                                  rmg_dist = mean(rmg_dist_coef),
                                  sewer_dist = mean(sewer_dist_coef),
                                  slope_dist = mean(slope_dist_coef))
coef.mean1 <- coef1 %>% summarise(elevation = mean(elevation_coef),
                                  river_dist = mean(river_dist_coef),
                                  rmg_dist = mean(rmg_dist_coef),
                                  sewer_dist = mean(sewer_dist_coef),
                                  slope_dist = mean(slope_dist_coef))
coef.mean2 <- coef2 %>% summarise(elevation = mean(elevation_coef),
                                  river_dist = mean(river_dist_coef),
                                  rmg_dist = mean(rmg_dist_coef),
                                  sewer_dist = mean(sewer_dist_coef),
                                  slope_dist = mean(slope_dist_coef))
flood_clust_coef <- rbind(coef.mean0, coef.mean1, coef.mean2)
flood_clust_coef
