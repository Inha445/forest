#-------------------------------------------------------------------------
# Date: 2018-05
# Title: fire - gangwondo
# Contens : 1) 지역 - 격자 (4*4)
#           2) 날짜 : 일별
#           3) 기상 : 종관기상
#
# Issue   : 1) imbalanced 문제 . 1이 1% 인 경우에도 under / over가 작동?
#
#-------------------------------------------------------------------------
rm(list=ls())
gc()

#0. LOAD LIBRARY ============================================================

library(ggplot2)
library(dplyr)
require(gridExtra)
require(reshape2)
library(raster)
library(googleway)
library(pracma)  
library(knitr)

#1. READ DATA================================================================
setwd("C:\\Users\\Jihyeon\\Desktop\\공모전 - 화재")
fire <- read.csv("data\\fire_gw_gps2.csv", stringsAsFactors = FALSE)

day_weth_gw <- read.csv("data\\day_weth_gw.csv", stringsAsFactors = FALSE)

gisang <- read.csv("data\\기상관측센터.csv", stringsAsFactors = FALSE)
gisang <- subset(gisang, 지점 %in% unique(day_weth_gw$지점))
gisang <- data.frame(gisang %>% group_by(지점) %>% slice(1))
gisang <- subset(gisang, select = c(지점, 지점명, 위도, 경도))

weth_gw <- left_join(day_weth_gw, gisang)


#2. gisang gps 다시! ================================================================
# 
# re_fire <- subset(fire, is.na(fire$lat))
# re_fire <- rbind(re_fire , fire[404,])  #fire[404, ] : lat < 37 , 강원도에 포함 X 잘못긁어옴!
# 
# fire_add<-subset(re_fire, select = c(발생장소_시도,발생장소_시군구,발생장소_읍면,
#                                             발생장소_동리,발생장소_지번))
# add_tmp <-c()
# for(i in 1:nrow(fire_add)){
#   add_tmp[i]<-paste(fire_add[i,1],fire_add[i,2],fire_add[i,3],fire_add[i,4])
# }
# 
# key <- "AIzaSyB5Ph-aX57EyfNfZPribmSjI28aFYqpxWM"
# 
# gps <- list()
# for(i in 1:length(add_tmp)){
#   gps[[i]] <- google_geocode(address=add_tmp[i], key= key)$results
# }
# 
# geometry <- lapply(gps, function(x) x$geometry$location)
# geo_null <- as.vector(sapply(geometry , function(x) is.null(x)))
# idx <- which(geo_null)
# for(i in 1:length(idx)){
#   geometry[[idx[i]]] <- data.frame(lat=NA, lng=NA)}
# 
# #중복 처리
# idx        <- unlist(lapply(geometry, nrow))
# num_idx    <- rep(1:length(gps), idx)
# geo_df     <- bind_rows(geometry)
# geo_df$idx <- num_idx
# geo_df     <- data.frame(geo_df %>% group_by(idx) %>% slice(1))
# 
# re_fire$lat <- geo_df$lat
# re_fire$lng <- geo_df$lng
# 
# # 그래도 안되는것은 일일이 해주기!! 
# re_fire$lat[1]<- google_geocode(address="강원도 정선군 남면 낙동리", key= key)$results$geometry$location$lat
# re_fire$lng[1]<- google_geocode(address="강원도 정선군 남면 낙동리", key= key)$results$geometry$location$lng
# 
# re_fire$lat[6]<- google_geocode(address="강원도 삼척시 미로면 상사전리", key= key)$results$geometry$location$lat
# re_fire$lng[6]<- google_geocode(address="강원도 삼척시 미로면 상사전리", key= key)$results$geometry$location$lng
# 
# re_fire$lat[7]<- google_geocode(address="강원도 정선군 남면 낙동리", key= key)$results$geometry$location$lat
# re_fire$lng[7]<- google_geocode(address="강원도 정선군 남면 낙동리", key= key)$results$geometry$location$lng
# 
# re_fire$lat[8]<- google_geocode(address="강원도 양양군 손양면 삽존리", key= key)$results$geometry$location$lat
# re_fire$lng[8]<- google_geocode(address="강원도 양양군 손양면 삽존리", key= key)$results$geometry$location$lng
# 
# 
# 
# fire[rownames(re_fire),] <- re_fire
# write.csv(fire , "data\\fire_gw_gps2.csv", row.names = FALSE)


#3. gisang gps plot ==============================================================
korea <- shapefile("data\\shape\\TL_SCCO_SIG.shp")
korea <- fortify(korea,region='SIG_CD')
korea_gw <- subset(korea, substr(id,1,2) == "42")

gisang <- read.csv("data\\기상관측센터.csv", stringsAsFactors = FALSE)
gisang <- subset(gisang, select = c(지점, 지점명, 위도, 경도))
gisang <- data.frame(gisang %>% group_by(지점) %>% slice(1))

gisang_sm  <- subset(gisang, 지점 %in% c(90,93,95,100,101,104,105,106,114,121,211,212,216,217))


# 격자로 나누기 -----
x_inter <- seq(min(korea_gw$long),max(korea_gw$long), length=5)
y_inter <- seq(min(korea_gw$lat),max(korea_gw$lat), length=5)

seq(min(korea_gw$long),max(korea_gw$long), length=9)
seq(min(korea_gw$lat),max(korea_gw$lat), length=9)

ggplot() + 
  geom_polygon(data=korea_gw, aes(x=long, y=lat, group=group), fill='white', color='black') +
  geom_point(data=gisang_sm, aes(x=경도, y=위도),col="red",size=3)+
  geom_point(data=fire, aes(x=lng,y=lat), col="darkgrey")+
  geom_vline(xintercept = x_inter)+
  geom_hline(yintercept = y_inter)+
  annotate("text", x=127.3815, y = 38.41610, label = "1", col="blue", size = 10) +
  annotate("text", x=127.9488, y = 38.41610, label = "2", col="blue", size = 10) +
  annotate("text", x=128.5160, y = 38.41610, label = "3", col="blue", size = 10) +
  annotate("text", x=127.3815, y = 38.01959, label = "1", col="blue", size = 10) +
  annotate("text", x=127.9488, y = 38.01959, label = "2", col="blue", size = 10) +
  annotate("text", x=128.5160, y = 38.01959, label = "3", col="blue", size = 10) +
  annotate("text", x=129.0833, y = 38.01959, label = "3", col="blue", size = 10) +
  annotate("text", x=127.3815, y = 37.62307, label = "4", col="blue", size = 10) +
  annotate("text", x=127.9488, y = 37.62307, label = "4", col="blue", size = 10) +
  annotate("text", x=128.5160, y = 37.62307, label = "5", col="blue", size = 10) +
  annotate("text", x=129.0833, y = 37.62307, label = "6", col="blue", size = 10) +
  annotate("text", x=127.9488, y = 37.22656, label = "7", col="blue", size = 10) +
  annotate("text", x=128.5160, y = 37.22656, label = "8", col="blue", size = 10) +
  annotate("text", x=129.0833, y = 37.22656, label = "9", col="blue", size = 10)



ggplot() + 
  geom_polygon(data=korea_gw, aes(x=long, y=lat, group=group), fill='white', color='black') +
  geom_point(data=gisang_sm, aes(x=경도, y=위도),col="red",size=3)+
  geom_point(data=fire, aes(x=lng,y=lat), col="darkgrey")
geom_vline(xintercept = x_inter) +
  geom_hline(yintercept = y_inter)




# 4. gisang + fire join ==============================================================
x_inter <- seq(min(korea_gw$long),max(korea_gw$long), length=5)
y_inter <- seq(min(korea_gw$lat),max(korea_gw$lat), length=5)

area <- ifelse(fire$lng <= x_inter[2] & fire$lat >= y_inter[3] & fire$lat <= y_inter[5], "1",
               ifelse(fire$lng >= x_inter[2] & fire$lng <= x_inter[3] & fire$lat >= y_inter[3] & fire$lat <= y_inter[5], "2",
                      ifelse(fire$lng >= x_inter[3] & fire$lng <= x_inter[5] & fire$lat >= y_inter[3] & fire$lat <= y_inter[5], "3",
                             ifelse(fire$lng >= x_inter[1] & fire$lng <= x_inter[3] & fire$lat >= y_inter[2] & fire$lat <= y_inter[3], "4",
                                    ifelse(fire$lng >= x_inter[3] & fire$lng <= x_inter[4] & fire$lat >= y_inter[2] & fire$lat <= y_inter[3], "5",
                                           ifelse(fire$lng >= x_inter[4] & fire$lng <= x_inter[5] & fire$lat >= y_inter[2] & fire$lat <= y_inter[3], "6",
                                                  ifelse(fire$lng >= x_inter[2] & fire$lng <= x_inter[3] & fire$lat >= y_inter[1] & fire$lat <= y_inter[2], "7",
                                                         ifelse(fire$lng >= x_inter[3] & fire$lng <= x_inter[4] & fire$lat >= y_inter[1] & fire$lat <= y_inter[2], "8",
                                                                ifelse(fire$lng >= x_inter[4] & fire$lng <= x_inter[5] & fire$lat >= y_inter[1] & fire$lat <= y_inter[2], "9", "그외")))))))))

fire$area  <- area

area <- ifelse(gisang_sm$경도 <= x_inter[2] & gisang_sm$위도 >= y_inter[3] & gisang_sm$위도 <= y_inter[5], "1",
               ifelse(gisang_sm$경도 >= x_inter[2] & gisang_sm$경도 <= x_inter[3] & gisang_sm$위도 >= y_inter[3] & gisang_sm$위도 <= y_inter[5], "2",
                      ifelse(gisang_sm$경도 >= x_inter[3] & gisang_sm$경도 <= x_inter[5] & gisang_sm$위도 >= y_inter[3] & gisang_sm$위도 <= y_inter[5], "3",
                             ifelse(gisang_sm$경도 >= x_inter[1] & gisang_sm$경도 <= x_inter[3] & gisang_sm$위도 >= y_inter[2] & gisang_sm$위도 <= y_inter[3], "4",
                                    ifelse(gisang_sm$경도 >= x_inter[3] & gisang_sm$경도 <= x_inter[4] & gisang_sm$위도 >= y_inter[2] & gisang_sm$위도 <= y_inter[3], "5",
                                           ifelse(gisang_sm$경도 >= x_inter[4] & gisang_sm$경도 <= x_inter[5] & gisang_sm$위도 >= y_inter[2] & gisang_sm$위도 <= y_inter[3], "6",
                                                  ifelse(gisang_sm$경도 >= x_inter[2] & gisang_sm$경도 <= x_inter[3] & gisang_sm$위도 >= y_inter[1] & gisang_sm$위도 <= y_inter[2], "7",
                                                         ifelse(gisang_sm$경도 >= x_inter[3] & gisang_sm$경도 <= x_inter[4] & gisang_sm$위도 >= y_inter[1] & gisang_sm$위도 <= y_inter[2], "8",
                                                                ifelse(gisang_sm$경도 >= x_inter[4] & gisang_sm$경도 <= x_inter[5] & gisang_sm$위도 >= y_inter[1] & gisang_sm$위도 <= y_inter[2], "9", "그외")))))))))
gisang_sm$area  <- area
weth_df <- left_join(weth_gw, gisang_sm)

ggplot() + 
  geom_polygon(data=korea_gw, aes(x=long, y=lat, group=group), fill='white', color='black') +
  geom_point(data=gisang_sm, aes(x=경도, y=위도, col=area),size=5)+
  geom_point(data=fire, aes(x=lng, y=lat, col=area))+
  geom_vline(xintercept = x_inter)+
  geom_hline(yintercept = y_inter)+
  annotate("text", x=127.3815, y = 38.41610, label = "1", col="blue", size = 10) +
  annotate("text", x=127.9488, y = 38.41610, label = "2", col="blue", size = 10) +
  annotate("text", x=128.5160, y = 38.41610, label = "3", col="blue", size = 10) +
  annotate("text", x=127.3815, y = 38.01959, label = "1", col="blue", size = 10) +
  annotate("text", x=127.9488, y = 38.01959, label = "2", col="blue", size = 10) +
  annotate("text", x=128.5160, y = 38.01959, label = "3", col="blue", size = 10) +
  annotate("text", x=129.0833, y = 38.01959, label = "3", col="blue", size = 10) +
  annotate("text", x=127.3815, y = 37.62307, label = "4", col="blue", size = 10) +
  annotate("text", x=127.9488, y = 37.62307, label = "4", col="blue", size = 10) +
  annotate("text", x=128.5160, y = 37.62307, label = "5", col="blue", size = 10) +
  annotate("text", x=129.0833, y = 37.62307, label = "6", col="blue", size = 10) +
  annotate("text", x=127.9488, y = 37.22656, label = "7", col="blue", size = 10) +
  annotate("text", x=128.5160, y = 37.22656, label = "8", col="blue", size = 10) +
  annotate("text", x=129.0833, y = 37.22656, label = "9", col="blue", size = 10)  

# weth_df 정리 ------------
sapply(weth_df, function(x) round(sum(is.na(x))/length(x),3))

#시각변수 제외
weth_df <- subset(weth_df, select = - grep("시각", colnames(weth_df)))
#풍향변수 제외
weth_df <- subset(weth_df, select = - grep("풍향", colnames(weth_df)))
weth_df <- subset(weth_df, select = - c(지점, 지점명, 위도, 경도))

str(weth_df)
weth_df <- data.frame(weth_df %>% group_by(일시, area) %>% summarize_all(function(x) mean(x, na.rm=TRUE)))

fire$date <- as.Date(paste0(fire$발생일시_년,"-",fire$발생일시_월,"-",fire$발생일시_일))
fire_df <- data.frame(fire %>% group_by(date, area) %>% summarize(y = TRUE,
                                                                  y_count = length(sido) ,
                                                                  y2_ha = sum(피해면적_합계)))

weth_df <- rename(weth_df, "date" = "일시")
weth_df$date <- as.Date(weth_df$date)


tot_df <- left_join(weth_df, fire_df)
tot_df$y[is.na(tot_df$y)]             <- 0
tot_df$y_count[is.na(tot_df$y_count)] <- 0
tot_df$y2_ha[is.na(tot_df$y2_ha)]     <- 0

subset(tot_df, y == 1)

#imbalanced 문제가 심각함.
table(tot_df$y)
566/(45745+566)

lapply(split(tot_df, area), function(x) sum(x$y == 1)/length(x$y))

knit("RGitHub.Rmd")
# write.csv(tot_df, "tot_df.csv", row.names = FALSE)



