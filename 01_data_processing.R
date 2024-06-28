
# LOAD REQUIRED PACKAGES AND FUNCTIONS -----------------------------------------
if (!require("pacman")) install.packages("pacman")
pkgs = c("readr","dplyr", "sf","ggplot2", "tidyr", "lubridate", "readxl", "stats", "hhh4addon",
         "hhh4contacts", "data.table", "reshape2") 
pacman::p_load(pkgs, character.only = T)

setwd("C:/Users/Huanjingshi/Documents/WPSDrive/16131756/WPS云盘/GDCDC/GDCDC2023/HHH4-EV71")

# read data#
hfmd_surv<- read_excel("data/original/广东省手足口病哨点监测数据_全变量_仅哨点监测2012_2019.xlsx", 
                       col_types = c("numeric", "text", "text", 
                                     "text", "text", "text", "text", "text", 
                                     "text", "text", "numeric", "text", 
                                     "text", "text", "text", "text", "date", 
                                     "date", "date", "date", "text", "numeric", 
                                     "numeric", "numeric", "numeric", 
                                     "numeric", "text", "numeric", "numeric", 
                                     "numeric", "numeric", "text", "numeric", 
                                     "numeric", "text", "numeric", "numeric", 
                                     "numeric")) %>%
  dplyr::mutate(采样日期 = lubridate::ymd(采样日期),
                发病日期 = lubridate::ymd(发病日期)) 

# 清洗手足口病病原学诊断结果数据：2^3-1 + 1 = 8种结果
names(hfmd_surv)
unique(hfmd_surv$result)
table(hfmd_surv$result)

# # hfmd_surv$result <- ifelse(hfmd_surv$result %in% )
# CA16:
# CA6
# EV71
# CA16:
# CA6
# EV71
# library(stringr)
# vec <- c("apple", "banana", "cherry", "bananax")
# char_to_check <- "banana"
# str_detect(vec, char_to_check)


# 筛选eV71,去结果空值者
hfmd_surv1 <- hfmd_surv %>% dplyr::filter(one == 1) %>% #one == 1==>筛选ev71
  dplyr::select(采样地市, 发病日期, 采样日期, ev71阳性, one) %>%
  dplyr::mutate(ev71阳性 = as.numeric(ev71阳性),
                one = as.numeric(one)) %>%
  dplyr::filter(one=="1")


#计算各个城市发病日期到采样日期的时间间隔
sum(is.na(hfmd_surv1$发病日期))
sum(is.na(hfmd_surv1$采样日期))

day_gap <- hfmd_surv1[!is.na(hfmd_surv1$发病日期), ] %>% 
  dplyr::mutate(gap = 采样日期-发病日期) %>%
  dplyr::group_by(采样地市) %>% summarise(mean_gap = mean(gap)) 

#反推hfmd_surv数据中缺失的发病日期
summary(as.numeric(hfmd_surv1$发病日期)) 
summary(as.numeric(hfmd_surv1$采样日期))

hfmd_surv1 <- hfmd_surv1 %>% left_join(day_gap, by = c("采样地市"))
summary(hfmd_surv1$发病日期)

hfmd_surv1$发病日期 <- ifelse(is.na(hfmd_surv1$发病日期),
                          hfmd_surv1$采样日期 - hfmd_surv1$mean_gap,
                          hfmd_surv1$发病日期)

hfmd_surv1$发病日期 <- lubridate::as_date(hfmd_surv1$发病日期)
# hfmd_surv1$发病日期 <- as.Date(hfmd_surv1$发病日期, origin = "1970-01-01")

summary((hfmd_surv1$发病日期))
summary((hfmd_surv1$采样日期))

hfmd_surv1 <- hfmd_surv1 %>%
  dplyr::mutate(发病日期 = lubridate::ymd(发病日期))

day_gap_final <- hfmd_surv1 %>% 
  dplyr::mutate(gap = 采样日期-发病日期) %>%
  dplyr::group_by(采样地市) %>% summarise(mean_gap = mean(gap)) 
(day_gap_final)

#查看时间范围
summary(lubridate::ymd(hfmd_surv1$发病日期))

#创建地市时间序列
gd21city <- read_excel("data/original/21地市信息.xlsx") %>%
  dplyr::select(nameC, nameE, region = Region, region2 = Region2)

gdmap <- st_read("data/original/gdmap.gpkg")
nameC <- gdmap$nameC %>% sort()
dates <- seq(as.Date("2012-01-01"), as.Date("2019-12-31"), by = "1 day")
gdts <- expand.grid(nameC = nameC, date = dates) %>% arrange(nameC) %>%
  dplyr::left_join(gd21city)


#合并hfmd_surv1和时间序列
hfmd_surv2 <- gdts %>% 
  left_join(.,hfmd_surv1, by = c("nameC" = "采样地市", "date" = "发病日期"))%>%#应该是发病日期??
  dplyr::mutate(year = surveillance::isoWeekYear(date)$ISOYear,
                month = lubridate::month(date),
                year1 = lubridate::year(date),
                weekiso =  surveillance::isoWeekYear(date)$ISOWeek,
                weekepi = lubridate::isoweek(date)) 

hfmd_surv2$ev71阳性[is.na(hfmd_surv2$ev71阳性)] <- 0
hfmd_surv2$one[is.na(hfmd_surv2$one)] <- 0

sum(hfmd_surv2$ev71阳性);sum(hfmd_surv2$one)


####按date来计算####
hfmd_surv3 <- hfmd_surv2 %>% dplyr::group_by(nameC, date) %>%
  dplyr::summarise(ev71 = sum(ev71阳性), Nsample = sum(one)) %>% 
  dplyr::mutate(nameC = paste0("city21_", nameC)) %>%
  dplyr::select(date, ev71, Nsample, nameC)

hfmd_surv4_1 <- hfmd_surv2 %>% dplyr::group_by(region, date) %>%
  dplyr::summarise(ev71 = sum(ev71阳性), Nsample = sum(one)) %>% 
  dplyr::mutate(nameC = paste0("region4_", region)) %>% ungroup() %>% 
  dplyr::select(-region)
hfmd_surv4_2 <- hfmd_surv2 %>% dplyr::group_by(region2, date) %>%
  dplyr::summarise(ev71 = sum(ev71阳性), Nsample = sum(one))%>% 
  dplyr::mutate(nameC = paste0("region2_", region2)) %>% ungroup() %>% 
  dplyr::select(-region2)
hfmd_surv4_3 <- hfmd_surv2 %>% dplyr::group_by(date) %>%
  dplyr::summarise(ev71 = sum(ev71阳性), Nsample = sum(one))%>% 
  dplyr::mutate(nameC = "Provincial") %>% ungroup()

names(hfmd_surv3) == names(hfmd_surv4_1) 
names(hfmd_surv4_1) ==names(hfmd_surv4_2)
hfmd_surv5 <- rbind(hfmd_surv3,hfmd_surv4_1,hfmd_surv4_2,hfmd_surv4_3)
unique(hfmd_surv5$nameC)

# 
# Nsample <- hfmd_surv3 %>% dplyr::group_by(nameC, year =lubridate::year(date)) %>%
#   dplyr::summarise(Nsample = sum(Nsample)) %>% dplyr::filter(year == 2017) %>%
#   dplyr::arrange(Nsample) %>%
#   dplyr::left_join(gd21city)
# View(Nsample)


# caculate ev71p
hfmd_surv3 <- hfmd_surv5
hfmd_surv3$ev71p <- ifelse(hfmd_surv3$Nsample == 0, 0,
                           hfmd_surv3$ev71 / hfmd_surv3$Nsample)
summary(hfmd_surv3)
sum(is.na(hfmd_surv3))
#
psych::describe(hfmd_surv3$ev71p)


# ####2.1滑动三日平均:用循环####
# # data0 <- hfmd_surv3
# hfmd_surv3$ev71m3 <- 0
# # data0$Nsamplem3 <- 0
# # data0
# # data0 <- data0 %>% rename(nameC = nameC)
# 
# ##生成每个城市点的list##
# cities <-unique(hfmd_surv3$nameC) %>% as.data.frame()
# colnames(cities) <- "nameC" #城市列表
# 
# data <- lapply(cities$nameC, function(x){
#   hfmd_surv3[hfmd_surv3$nameC == x, ]
# })
# names(data) <- cities$nameC
# 
# # data <- data1
# ##滑动3天 # 这里的平滑好像算错了，需要循环每个城市，然后计算rate
# 
# ####2.建立模型循环####
# 
# for(i in seq(length(data))) {
#   # 输出当前城市
#   print(paste("df",cities$nameC[i],sep = "_"))
# 
#   # 提取单个城市数据
#   sub <- data[[i]] %>% dplyr::arrange(nameC, date)
# 
#   #ev71
#   # sub$ev71m3 <- 0
#   # sub$Nsamplem3 <- 0
# 
#   #ev71
#   # sub$ev71m3 <- TTR::SMA(sub$ev71, n = 3)
#   # sub$Nsamplem3 <- TTR::SMA(sub$Nsample, n = 3)
# 
#   #
#   for (j in 3:nrow(sub)) {
#     sub[j,"ev71m3"] <- (sub[j, "ev71"] + sub[j-1,"ev71"] + sub[j-2, "ev71"])/3
#     sub[j,"Nsamplem3"] <- (sub[j,"Nsample"] + sub[j-1,"Nsample"] + sub[j-2,"Nsample"])/3
#   }
# 
#   ##替换各城市首1-2周数据
#   sub[1, "ev71m3"] <- sub[1, "ev71"];
#   sub[1, "Nsamplem3"] <- sub[1, "Nsample"]
#   sub[2, "ev71m3"] <- (sub[1, "ev71"] + sub[2, "ev71"])/2
#   sub[2, "Nsamplem3"] <- (sub[1, "Nsample"] + sub[2, "Nsample"])/2
# 
#   ##替换各城市首1-2月数据
#   assign(paste("df",cities$nameC[i],sep = "_"), sub)
# }
# 
# dfall <- rbind(df_东莞市, df_中山市, df_云浮市, df_佛山市, df_广州市, df_惠州市, df_揭阳市, df_梅州市, df_汕头市, df_汕尾市, df_江门市, df_河源市,
#                df_深圳市, df_清远市, df_湛江市, df_潮州市, df_珠海市, df_肇庆市, df_茂名市, df_阳江市, df_韶关市)
# sum(is.na(dfall))
# sum(dfall$ev71);sum(dfall$ev71m3) #3083;3083
# sum(dfall$Nsample);sum(dfall$Nsamplem3)#30940;30935.5
# 
# dfall$ev71pm3 <- dfall$ev71m3/dfall$Nsamplem3
# dfall$ev71pm3 <- ifelse(is.nan(dfall$ev71pm3), 0,dfall$ev71pm3 )
# summary(dfall$ev71pm3)
# summary(dfall)
# 
# 
# dfall_daily1 <- dfall %>% dplyr::select(nameC, date, ev71p, ev71pm3)
# 
# sum(dfall_daily1$ev71p)#1590.865
# sum(dfall_daily1$ev71pm3)#2849.448


# #### 2.2滑动3天平均:用TTR:SMA(结果和2.1一样)####
# 
# hfmd_surv3$ev71m3 <- 0
# 
# ##生成每个城市点的list##
# cities <-unique(hfmd_surv3$nameC) %>% as.data.frame()
# colnames(cities) <- "nameC" 
# 
# data <- lapply(cities$nameC, function(x){
#   hfmd_surv3[hfmd_surv3$nameC == x, ]
# })
# names(data) <- cities$nameC
# 
# # 建立模型循环#
# 
# lag = 7 #90   30   7
# 
# result_list <- list()
# 
# for(i in seq(length(data))) {
#   # 输出当前城市
#   print(paste("df",cities$nameC[i],sep = "_"))
# 
#   # 提取单个城市数据
#   sub <- data[[i]] %>% dplyr::arrange(nameC, date)
# 
#   #用TTR::SMA求移动平均（simple moving average)
#   sub$ev71m3 <- TTR::SMA(sub$ev71, n = lag)
#   sub$Nsamplem3 <- TTR::SMA(sub$Nsample, n = lag)
# 
#   ##存储结果sub
#   result_list[[i]] <- sub
# }
# 
# final_d <- do.call(rbind, result_list)
# 
# 
# d7 <- final_d %>% dplyr::mutate(ev71p_w4 = ifelse(Nsamplem3 == 0, 0, ev71m3/Nsamplem3)) %>%
#   ungroup() %>% dplyr::select(nameC, date, d7 = ev71p_w4)
# 
# d30 <- final_d %>% dplyr::mutate(ev71p_w4 = ifelse(Nsamplem3 == 0, 0, ev71m3/Nsamplem3)) %>%
#   ungroup() %>% dplyr::select(nameC, date, d30 = ev71p_w4)
# 
# d90 <- final_d %>% dplyr::mutate(ev71p_w4 = ifelse(Nsamplem3 == 0, 0, ev71m3/Nsamplem3)) %>%
#   ungroup() %>% dplyr::select(nameC, date, d90 = ev71p_w4)
# 
# 
# ev71p_all <- d7 %>% dplyr::left_join(d30) %>% dplyr::left_join(d90)
# 
# ev71p_all[is.na(ev71p_all)] <- 0
# 
# saveRDS(ev71p_all,"data/processed/ev71p_all_1220_发病日期.rds")


#2017年5月前，13个地市（潮州，佛山，广州，河源，东莞，惠州，江门，清远，
# 汕头，韶关，孕妇，湛江，珠海）的病原学监测数据都存在非常大的问题....
ev71p_all <- readRDS("data/processed/ev71p_all_1220_发病日期.rds")
unique(ev71p_all$nameC)
# ev71p_all <- readRDS("data/processed/ev71p_all_1220.rds")
ev71p_all$nameC <- gsub("city21_","",ev71p_all$nameC)
unique(ev71p_all$nameC)

ev71p_city <- ev71p_all %>% ungroup() %>% dplyr::left_join(gd21city[ ,c("nameC", "nameE")]) %>%
  dplyr::mutate(year = lubridate::year(date)) %>%
  dplyr::filter(nameC %in% as.character(gd21city$nameC)) %>%
  dplyr::filter(year == 2017) %>%
  #               date >= as.Date("2012-01-01"),date <= as.Date("2019-12-01")) %>% 
  # tidyr::spread(key = "lag_pct", value = value) %>%
  ggplot(aes(x = date)) +
  geom_line(aes(y = d90 * 100), linetype = 1, size = .3) +
  facet_wrap(~ nameE, scales = "free_y") +
  scale_fill_brewer(type = "q", palette= 7)+
  scale_x_date(date_breaks = "1 month", date_labels = "%m",
               expand = c(0, 1)) +
  scale_y_continuous(expand = c(0.01, 0)) +
  labs(x = "", y = "Estimated EV71 proportion", fill = "") +
  theme_bw(base_size = 14) +
  theme(legend.position = "top", strip.text = element_text())
ev71p_city

# 确定2016年哪些城市需要插值
gdcase1 <- hfmd_surv2 %>% dplyr::mutate(year = lubridate::year(date)) %>%
  dplyr::filter(year == 2016) %>% dplyr::group_by(region, region2, nameC, nameE, year) %>% 
  dplyr::summarise(Nsample = sum(one)) %>% dplyr::arrange(Nsample)
# View(gdcase1)
units <- head(gdcase1,12) %>% ungroup() %>% dplyr::select(region, nameC, nameE) %>% 
  group_by(region, nameC, nameE)  %>% dplyr::arrange(region)
nameC2016 <- as.character(units$nameC)
nameC2016


#长型数据

ev71p_long <- ev71p_all %>% tidyr::gather(key = "lag_pct",value = "value", -(nameC:date)) %>%
  dplyr::mutate(year = lubridate::year(date))

unique(ev71p_long$nameC)
# perl <- c("region2_Non-Pearl River Delta", "region2_Pearl River Delta")
# region4 <- c("region4_Eastern Region", "region4_Mountainous Region",
#              "region4_Pearl River Delta", "region4_Western Region")
# 
# ev71p_perl <- ev71p_long %>% ungroup() %>%
#   dplyr::filter(nameC %in% perl, date >= as.Date("2016-01-01")) %>%
#   tidyr::spread(key = "lag_pct", value = value) %>%
#   ggplot(aes(x = date)) +
#   geom_line(aes(y = d90 * 100), linetype = 1, size = .3) +
#   facet_wrap(~ nameC, scales = "free_y") +
#   scale_fill_brewer(type = "q", palette= 7)+
#   scale_x_date(date_breaks = "3 month", date_labels = "%y%b",
#                expand = c(0, 1)) +
#   scale_y_continuous(expand = c(0.01, 0)) +
#   labs(x = "", y = "Estimated EV71 proportion", fill = "") +
#   theme_bw(base_size = 14) +
#   theme(legend.position = "top", strip.text = element_text())
# ev71p_perl
# 
# ev71p_region4 <- ev71p_long %>% ungroup() %>%
#   dplyr::filter(nameC %in% region4,
#                 date >= as.Date("2016-01-01"),
#                 date <= as.Date("2017-12-31")) %>%
#   tidyr::spread(key = "lag_pct", value = value) %>%
#   ggplot(aes(x = date)) +
#   geom_line(aes(y = d90 * 100), linetype = 1, size = .3) +
#   facet_wrap(~ nameC, scales = "free_y") +
#   scale_fill_brewer(type = "q", palette= 7)+
#   scale_x_date(date_breaks = "3 month", date_labels = "%y%b",
#                expand = c(0, 1)) +
#   scale_y_continuous(expand = c(0.01, 0)) +
#   labs(x = "", y = "Estimated EV71 proportion", fill = "") +
#   theme_bw(base_size = 14) +
#   theme(legend.position = "top", strip.text = element_text())
# ev71p_region4


# 筛选出2016年数据

names(ev71p_2016)
ev71p_2016 <- ev71p_long %>% dplyr::filter(date < as.Date("2017-07-01")) %>%
  dplyr::filter(!(nameC %in% nameC2016)) %>%
  tidyr::spread(key = "nameC", value = "value") %>%
  # dplyr::mutate(汕头市 = `region4_Eastern Region`,
  #               潮州市 = `region4_Eastern Region`,
  #               河源市 = `region4_Mountainous Region`,
  #               清远市 = `region4_Mountainous Region`,
  #               韶关市 = `region4_Mountainous Region`,
  #               佛山市 = `region4_Pearl River Delta`,
  #               惠州市 = `region4_Pearl River Delta`,
  #               江门市 = `region4_Pearl River Delta`,
  #               珠海市 = `region4_Pearl River Delta`,
  #               广州市 = `region4_Pearl River Delta`,
  #               云浮市 = `region4_Western Region`,
  #               湛江市 = `region4_Western Region`)

  dplyr::mutate(汕头市 = `region2_Non-Pearl River Delta`,
                潮州市 = `region2_Non-Pearl River Delta`,
                河源市 = `region2_Non-Pearl River Delta`,
                清远市 = `region2_Non-Pearl River Delta`,
                韶关市 = `region2_Non-Pearl River Delta`,
                佛山市 = `region2_Pearl River Delta`,
                惠州市 = `region2_Pearl River Delta`,
                江门市 = `region2_Pearl River Delta`,
                珠海市 = `region2_Pearl River Delta`,
                广州市 = `region2_Pearl River Delta`,
                云浮市 = `region2_Non-Pearl River Delta`,
                湛江市 = `region2_Non-Pearl River Delta`)
# 
# dplyr::mutate(汕头市 = Provincial,
#               潮州市 = Provincial,
#               河源市 = Provincial,
#               清远市 = Provincial,
#               韶关市 = Provincial,
#               佛山市 = Provincial,
#               惠州市 = Provincial,
#               江门市 = Provincial,
#               珠海市 = Provincial,
#               广州市 = Provincial,
#               云浮市 = Provincial,
#               湛江市 = Provincial)


ev71p_2016_cz <- ev71p_2016 %>% tidyr::gather(key = "nameC", value = "value", -date, -lag_pct, -year) %>%
  dplyr::select(nameC, date, lag_pct, value, year)
summary(ev71p_2016_cz$year)

ev71p_final <- ev71p_long %>% dplyr::filter(date >= as.Date("2017-07-01")) %>% 
  rbind(ev71p_2016_cz) %>% #dplyr::arrange(nameC, year, lag_pct,date) %>%
  dplyr::filter(nameC %in% as.character(gd21city$nameC)) %>%
  dplyr::select(-year) %>% tidyr::spread(key = lag_pct, value = value) %>%
  dplyr::left_join(gd21city[ ,c("nameC", "nameE")], by = "nameC") %>%
  dplyr::arrange(nameE, date)
summary(ev71p_final$date)

ev71pplot <- ev71p_final %>% 
  # dplyr::filter(date >= "2012-01-01" ) %>% #& date <= "2017-12-31"
  dplyr::mutate(year = lubridate::year(date)) %>%
  # dplyr::filter(year  == 2018) %>%
  ggplot(aes(x = date)) +
  geom_line(aes(y = d90 * 100), linetype = 1, size = .3) +
  facet_wrap(~ nameE, scales = "free_y") +
  scale_fill_brewer(type = "q", palette= 7)+
  scale_x_date(date_breaks = "6 month", date_labels = "%m",
               expand = c(0, 1)) +
  scale_y_continuous(expand = c(0.01, 0)) +
  labs(x = "", y = "Estimated EV71 proportion", fill = "") +
  theme_bw(base_size = 14) +
  theme(legend.position = "top", strip.text = element_text())
ev71pplot


openxlsx::write.xlsx(ev71p_final, "data/processed/ev71p_daily_1211.xlsx")
# openxlsx::write.xlsx(ev71p_final, "data/processed/ev71p_daily_1211_region2.xlsx")

sum(dfall_daily1$ev71p)#和2.1完全一致
# sum(dfall_daily1$ev71pm3)#和2.1完全一致

####2.2滑动7天平均:用TTR:SMA(结果和2.1一样)####
# data0 <- hfmd_surv3
hfmd_surv3$ev71m3 <- 0
hfmd_surv3$Nsamplem3 <- 0
# data0
# data0 <- data0 %>% rename(nameC = nameC)

##生成每个城市点的list##
cities <-unique(hfmd_surv3$nameC) %>% as.data.frame()
colnames(cities) <- "nameC" #城市列表

data <- lapply(cities$nameC, function(x){
  hfmd_surv3[hfmd_surv3$nameC == x, ]
})
names(data) <- cities$nameC

#建立模型循环
for(i in seq(length(data))) {
  # 输出当前城市
  print(paste("df",cities$nameC[i],sep = "_"))

  # 提取单个城市数据
  sub <- data[[i]] %>% dplyr::arrange(nameC, date)

  #ev71
  # sub$ev71m3 <- 0
  # sub$Nsamplem3 <- 0

  #用TTR::SMA求移动平均（simple moving average)
  sub$ev71m3 <- TTR::SMA(sub$ev71, n = 7)
  sub$Nsamplem3 <- TTR::SMA(sub$Nsample, n = 7)

  #
  # for (j in 3:nrow(sub)) {
  #   sub[j,"ev71m3"] <- (sub[j, "ev71"] + sub[j-1,"ev71"] + sub[j-2, "ev71"])/3
  #   sub[j,"Nsamplem3"] <- (sub[j,"Nsample"] + sub[j-1,"Nsample"] + sub[j-2,"Nsample"])/3
  # }

  ##替换各城市首1-2周数据
  sub[1, "ev71m3"] <- sub[1, "ev71"];
  sub[1, "Nsamplem3"] <- sub[1, "Nsample"]
  sub[2, "ev71m3"] <- (sub[1, "ev71"] + sub[2, "ev71"])/2
  sub[2, "Nsamplem3"] <- (sub[1, "Nsample"] + sub[2, "Nsample"])/2
  sub[3, "ev71m3"] <- (sub[1, "ev71"]+ sub[2, "ev71"]+ sub[3, "ev71"])/3
  sub[3, "Nsamplem3"] <- (sub[1, "Nsample"]+ sub[2, "Nsample"]+ sub[3, "Nsample"])/3
  sub[4, "ev71m3"] <- (sub[1, "ev71"] + sub[2, "ev71"]+ sub[3, "ev71"]+ sub[4, "ev71"])/4
  sub[4, "Nsamplem3"] <- (sub[1, "Nsample"] + sub[2, "Nsample"]+ sub[3, "Nsample"]+ sub[4, "Nsample"])/4
  sub[5, "ev71m3"] <- (sub[1, "ev71"] + sub[2, "ev71"]+ sub[3, "ev71"]+ sub[4, "ev71"]+ sub[5, "ev71"])/5
  sub[5, "Nsamplem3"] <- (sub[1, "Nsample"] + sub[2, "Nsample"]+ sub[3, "Nsample"]+ sub[4, "Nsample"]+ sub[5, "Nsample"])/5
  sub[6, "ev71m3"] <- (sub[1, "ev71"] + sub[2, "ev71"] + sub[3, "ev71"] + sub[4, "ev71"] + sub[5, "ev71"] + sub[6, "ev71"])/6
  sub[6, "Nsamplem3"] <- (sub[1, "Nsample"] + sub[2, "Nsample"]+ sub[3, "Nsample"]+ sub[4, "Nsample"]+ sub[5, "Nsample"]+ sub[6, "Nsample"])/6
  ##替换各城市首1-2月数据
  assign(paste("df",cities$nameC[i],sep = "_"), sub)
}

ev71p_daily_lag7 <- rbind(df_东莞市, df_中山市, df_云浮市, df_佛山市, df_广州市, df_惠州市, df_揭阳市, df_梅州市, df_汕头市, df_汕尾市, df_江门市, df_河源市,
               df_深圳市, df_清远市, df_湛江市, df_潮州市, df_珠海市, df_肇庆市, df_茂名市, df_阳江市, df_韶关市)

ev71p_daily_lag7$ev71pm3 <- ifelse(ev71p_daily_lag7$Nsamplem3 == 0, 0,
                                   ev71p_daily_lag7$ev71m3/ev71p_daily_lag7$Nsamplem3)
sum(is.na(ev71p_daily_lag7))


sum(ev71p_daily_lag7$ev71);sum(ev71p_daily_lag7$ev71m3)
sum(ev71p_daily_lag7$Nsample);sum(ev71p_daily_lag7$Nsamplem3)
sum(ev71p_daily_lag7$ev71p);sum(ev71p_daily_lag7$ev71pm3)

ev71p_d7 <- ev71p_daily_lag7 %>% dplyr::select(nameC, date, ev71p, ev71p_d7 = ev71pm3) %>%
  dplyr::left_join(ev71p_d3)

# openxlsx::write.xlsx(ev71p_d7, "data/processed/ev71p_daily_1211.xlsx")



####2.2滑动30/90天平均:用TTR:SMA(结果和2.1一样)####

# data0 <- hfmd_surv3
hfmd_surv3$ev71m3 <- 0
hfmd_surv3$Nsamplem3 <- 0
# data0
# data0 <- data0 %>% rename(nameC = nameC)

##生成每个城市点的list##
cities <-unique(hfmd_surv3$nameC) %>% as.data.frame()
colnames(cities) <- "nameC" #城市列表

data <- lapply(cities$nameC, function(x){
  hfmd_surv3[hfmd_surv3$nameC == x, ]
})
names(data) <- cities$nameC


result_list <- list()


k = 30
for(i in seq(length(data))) {
  # 输出当前城市
  print(paste("df",cities$nameC[i],sep = "_"))
  
  # 提取单个城市数据
  sub <- data[[i]] %>% dplyr::arrange(nameC, date)
  
  #用TTR::SMA求移动平均（simple moving average)
  sub$ev71m3 <- TTR::SMA(sub$ev71, n = k)
  sub$Nsamplem3 <- TTR::SMA(sub$Nsample, n = k)
  
  ##存储结果sub
  result_list[[i]] <- sub
  
}

# 使用do.call和rbind合并所有数据框
final_d30 <- do.call(rbind, result_list)

ev71p_d30 <- final_d30 %>% 
  dplyr::mutate(ev71p_w4 = ifelse(Nsamplem3 == 0, 0, 
                                  ev71m3/Nsamplem3)) %>% ungroup() %>%
  dplyr::select(nameC, date, ev71p_d0 = ev71p, ev71p_d30 = ev71p_w4)


k = 90
for(i in seq(length(data))) {
  # 输出当前城市
  print(paste("df",cities$nameC[i],sep = "_"))
  
  # 提取单个城市数据
  sub <- data[[i]] %>% dplyr::arrange(nameC, date)
  
  #用TTR::SMA求移动平均（simple moving average)
  sub$ev71m3 <- TTR::SMA(sub$ev71, n = k)
  sub$Nsamplem3 <- TTR::SMA(sub$Nsample, n = k)
  
  ##替换各城市首1-11周数据
  result_list[[i]] <- sub
  
}

# 使用do.call和rbind合并所有数据框
final_d90 <- do.call(rbind, result_list)

ev71p_d90 <- final_d90 %>% 
  dplyr::mutate(ev71p_w4 = ifelse(Nsamplem3 == 0, 0, 
                                  ev71m3/Nsamplem3)) %>% ungroup() %>%
  dplyr::select(nameC, date, ev71p_d0 = ev71p, ev71p_d90 = ev71p_w4)


ev71p_d90
ev71p_d30
ev71p_d7

ev71p <- ev71p_d7 %>% dplyr::left_join(ev71p_d30) %>% dplyr::left_join(ev71p_d90) %>%
        dplyr::select(-ev71p_d0) 
ev71p[is.na(ev71p)] <- 0
ev71p

evev71pct_plot <- ev71p %>%
  dplyr::mutate(date = lubridate::ymd(date)) %>%  left_join( ., cityname) %>%
  dplyr::select(date, country = nameE, ev71p, -nameC) %>% arrange(country, date, ev71p) %>%
  ggplot(aes(x = date)) +
  geom_line(aes(y = ev71p), linetype = 1, size = .3) +
  # geom_point(aes(y = observed), size = 1) +
  facet_wrap(~ country, scales = "free_y") +
  # scale_fill_brewer(type = "q", palette= 7)+
  scale_x_date(date_breaks = "3 month", date_labels = "%b",
               expand = c(0, 1)) +
  scale_y_continuous(expand = c(0.01, 0)) +
  labs(x = "", y = "Reported cases", fill = "") +
  theme_bw(base_size = 14) +
  theme(legend.position = "top", strip.text = element_text())
evev71pct_plot

openxlsx::write.xlsx(ev71p, "data/processed/ev71p_daily_1211.xlsx")

####按周计算ev71构成比####
names(hfmd_surv2)
hfmd_week <- hfmd_surv2 %>% dplyr::select(nameC, date, ev71阳性, one) %>%
  dplyr::mutate(year = surveillance::isoWeekYear(date)$ISOYear,
                # month = lubridate::month(date),
                week =  surveillance::isoWeekYear(date)$ISOWeek) %>%
  dplyr::group_by(nameC, year, week) %>% 
  dplyr::summarise(ev71阳性 = sum(ev71阳性), one = sum(ev71阳性))

summary(hfmd_week)
hfmd_week$ev71p <- ifelse(hfmd_week$one == 0, 0, 
                          hfmd_week$ev71阳性/hfmd_week$one)
summary(hfmd_week$ev71p)
sum(hfmd_week$ev71阳性)

#####按周计算移动平均4周、8周、12周#####

##生成每个城市点的list##
cities <-unique(hfmd_week$nameC) %>% as.data.frame()
colnames(cities) <- "nameC" #城市列表

data <- lapply(cities$nameC, function(x){
  hfmd_week[hfmd_week$nameC == x, ]
})
names(data) <- cities$nameC

####4周####

final_w4 <- data.frame()
k = 4

for(i in seq(length(data))) {
  # 输出当前城市
  city <- cities$nameC[i]
  print(city)
  
  # 提取单个城市数据
  sub <- data[[i]] %>% dplyr::arrange(nameC, year, week)
  
  #ev71
  sub$ev71_w4 <- TTR::SMA(sub$ev71阳性, n = k) 
  sub$N_w4 <- TTR::SMA(sub$one, n = k)
  
  ##替换各城市首1-2周数据
  sub[1, "ev71_w4"] <- sub[1, "ev71阳性"]
  sub[1, "N_w4"] <- sub[1, "one"]
  sub[2, "ev71_w4"] <- (sub[1, "ev71阳性"] + sub[2, "ev71阳性"])/2
  sub[2, "N_w4"] <- (sub[1, "one"] + sub[2, "one"])/2
  sub[3, "ev71_w4"] <- (sub[1, "ev71阳性"] + sub[2, "ev71阳性"] + sub[3, "ev71阳性"])/3
  sub[3, "N_w4"] <- (sub[1, "one"] + sub[2, "one"] + sub[3, "one"])/3
  
  
  final_w4 <- rbind(final_w4, sub)
}

final_w4

ev71p_w4 <- final_w4 %>% 
  dplyr::mutate(ev71p_w4 = ifelse(N_w4 == 0, 0, 
                                  ev71_w4/N_w4)) %>% ungroup() %>%
  dplyr::select(nameC, year, week, w0 = ev71p, w4 = ev71p_w4)


####8周####

final_w8 <- data.frame()
k = 8

for(i in seq(length(data))) {
  # 输出当前城市
  print(paste("df",cities$nameC[i],sep = "_"))
  
  # 提取单个城市数据
  sub <- data[[i]] %>% dplyr::arrange(nameC, year, week)
  
  #ev71
  sub$ev71_w4 <- TTR::SMA(sub$ev71阳性, n = k) 
  sub$N_w4 <- TTR::SMA(sub$one, n = k)
  
  ##替换各城市首1-7周数据
  sub[1, "ev71_w4"] <- sub[1, "ev71阳性"]
  sub[1, "N_w4"] <- sub[1, "one"]
  
  sub[2, "ev71_w4"] <- (sub[1, "ev71阳性"] + sub[2, "ev71阳性"])/2
  sub[2, "N_w4"] <- (sub[1, "one"] + sub[2, "one"])/2
  
  sub[3, "ev71_w4"] <- (sub[1, "ev71阳性"] + sub[2, "ev71阳性"] + sub[3, "ev71阳性"])/3
  sub[3, "N_w4"] <- (sub[1, "one"] + sub[2, "one"] + sub[3, "one"])/3
  
  sub[4, "ev71_w4"] <- (sub[1, "ev71阳性"] + sub[2, "ev71阳性"] + sub[3, "ev71阳性"] + sub[4, "ev71阳性"])/4
  sub[4, "N_w4"] <- (sub[1, "one"] + sub[2, "one"] + sub[3, "one"] + sub[4, "one"] )/4
  
  sub[5, "ev71_w4"] <- (sub[1, "ev71阳性"] + sub[2, "ev71阳性"] + sub[3, "ev71阳性"] + sub[4, "ev71阳性"] + sub[5, "ev71阳性"])/5
  sub[5, "N_w4"] <- (sub[1, "one"] + sub[2, "one"] + sub[3, "one"] + sub[4, "one"] + sub[5, "one"] )/5
  
  sub[6, "ev71_w4"] <- (sub[1, "ev71阳性"] + sub[2, "ev71阳性"] + sub[3, "ev71阳性"] + sub[4, "ev71阳性"] + sub[5, "ev71阳性"] + sub[6, "ev71阳性"])/6
  sub[6, "N_w4"] <- (sub[1, "one"] + sub[2, "one"] + sub[3, "one"] + sub[4, "one"] + sub[5, "one"] + sub[6, "one"] )/6
  
  sub[7, "ev71_w4"] <- (sub[1, "ev71阳性"] + sub[2, "ev71阳性"] + sub[3, "ev71阳性"] + sub[4, "ev71阳性"] + sub[5, "ev71阳性"] + sub[6, "ev71阳性"] + sub[7, "ev71阳性"])/7
  sub[7, "N_w4"] <- (sub[1, "one"] + sub[2, "one"] + sub[3, "one"] + sub[4, "one"] + sub[5, "one"] + sub[6, "one"] + sub[7, "one"] )/7
  
  
  final_w8 <- rbind(final_w8, sub)

}

final_w8

ev71p_w8 <- final_w8 %>% 
  dplyr::mutate(ev71p_w4 = ifelse(N_w4 == 0, 0, 
                                  ev71_w4/N_w4)) %>% ungroup() %>%
  dplyr::select(nameC, year, week, w0 = ev71p, w8 = ev71p_w4)

####12周####

result_list <- list()

k = 12
for(i in seq(length(data))) {
  # 输出当前城市
  print(paste("df",cities$nameC[i],sep = "_"))
  
  # 提取单个城市数据
  sub <- data[[i]] %>% dplyr::arrange(nameC, year, week)
  
  #ev71
  sub$ev71_w4 <- TTR::SMA(sub$ev71阳性, n = k) 
  sub$N_w4 <- TTR::SMA(sub$one, n = k)
  
  ##替换各城市首1-11周数据
  sub[1, "ev71_w4"] <- sub[1, "ev71阳性"]
  sub[1, "N_w4"] <- sub[1, "one"]
  
  sub[2, "ev71_w4"] <- (sub[1, "ev71阳性"] + sub[2, "ev71阳性"])/2
  sub[2, "N_w4"] <- (sub[1, "one"] + sub[2, "one"])/2
  
  sub[3, "ev71_w4"] <- (sub[1, "ev71阳性"] + sub[2, "ev71阳性"] + sub[3, "ev71阳性"])/3
  sub[3, "N_w4"] <- (sub[1, "one"] + sub[2, "one"] + sub[3, "one"])/3
  
  sub[4, "ev71_w4"] <- (sub[1, "ev71阳性"] + sub[2, "ev71阳性"] + sub[3, "ev71阳性"] + sub[4, "ev71阳性"])/4
  sub[4, "N_w4"] <- (sub[1, "one"] + sub[2, "one"] + sub[3, "one"] + sub[4, "one"] )/4
  
  sub[5, "ev71_w4"] <- (sub[1, "ev71阳性"] + sub[2, "ev71阳性"] + sub[3, "ev71阳性"] + sub[4, "ev71阳性"] + sub[5, "ev71阳性"])/5
  sub[5, "N_w4"] <- (sub[1, "one"] + sub[2, "one"] + sub[3, "one"] + sub[4, "one"] + sub[5, "one"] )/5
  
  sub[6, "ev71_w4"] <- (sub[1, "ev71阳性"] + sub[2, "ev71阳性"] + sub[3, "ev71阳性"] + sub[4, "ev71阳性"] + sub[5, "ev71阳性"] + sub[6, "ev71阳性"])/6
  sub[6, "N_w4"] <- (sub[1, "one"] + sub[2, "one"] + sub[3, "one"] + sub[4, "one"] + sub[5, "one"] + sub[6, "one"] )/6
  
  sub[7, "ev71_w4"] <- (sub[1, "ev71阳性"] + sub[2, "ev71阳性"] + sub[3, "ev71阳性"] + sub[4, "ev71阳性"] + sub[5, "ev71阳性"] + sub[6, "ev71阳性"] + sub[7, "ev71阳性"])/7
  sub[7, "N_w4"] <- (sub[1, "one"] + sub[2, "one"] + sub[3, "one"] + sub[4, "one"] + sub[5, "one"] + sub[6, "one"] + sub[7, "one"] )/7
 
  sub[8, "ev71_w4"] <- (sub[1, "ev71阳性"] + sub[2, "ev71阳性"] + sub[3, "ev71阳性"] + sub[4, "ev71阳性"] + sub[5, "ev71阳性"] + sub[6, "ev71阳性"] + sub[7, "ev71阳性"]+ sub[8, "ev71阳性"])/8
  sub[8, "N_w4"] <- (sub[1, "one"] + sub[2, "one"] + sub[3, "one"] + sub[4, "one"] + sub[5, "one"] + sub[6, "one"] + sub[7, "one"] + sub[8, "one"] )/8
  
  sub[9, "ev71_w4"] <- (sub[1, "ev71阳性"] + sub[2, "ev71阳性"] + sub[3, "ev71阳性"] + sub[4, "ev71阳性"] + sub[5, "ev71阳性"] + sub[6, "ev71阳性"] + sub[7, "ev71阳性"] + sub[8, "ev71阳性"]+ sub[9, "ev71阳性"])/9
  sub[9, "N_w4"] <- (sub[1, "one"] + sub[2, "one"] + sub[3, "one"] + sub[4, "one"] + sub[5, "one"] + sub[6, "one"] + sub[7, "one"] + sub[8, "one"]+ sub[9, "one"] )/9
  
  sub[10, "ev71_w4"] <- (sub[1, "ev71阳性"] + sub[2, "ev71阳性"] + sub[3, "ev71阳性"] + sub[4, "ev71阳性"] + sub[5, "ev71阳性"] + sub[6, "ev71阳性"] + sub[7, "ev71阳性"] + sub[8, "ev71阳性"]+ sub[9, "ev71阳性"]+ sub[10, "ev71阳性"])/10
  sub[10, "N_w4"] <- (sub[1, "one"] + sub[2, "one"] + sub[3, "one"] + sub[4, "one"] + sub[5, "one"] + sub[6, "one"] + sub[7, "one"] + sub[8, "one"]+ sub[9, "one"]+ sub[10, "one"] )/10
 
  sub[11, "ev71_w4"] <- (sub[1, "ev71阳性"] + sub[2, "ev71阳性"] + sub[3, "ev71阳性"] + sub[4, "ev71阳性"] + sub[5, "ev71阳性"] + sub[6, "ev71阳性"] + sub[7, "ev71阳性"] + sub[8, "ev71阳性"]+ sub[9, "ev71阳性"]+ sub[10, "ev71阳性"]+ sub[11, "ev71阳性"])/11
  sub[11, "N_w4"] <- (sub[1, "one"] + sub[2, "one"] + sub[3, "one"] + sub[4, "one"] + sub[5, "one"] + sub[6, "one"] + sub[7, "one"] + sub[8, "one"]+ sub[9, "one"]+ sub[10, "one"] + sub[11, "one"] )/11
  
  result_list[[i]] <- sub

}

# 使用do.call和rbind合并所有数据框
final_w12 <- do.call(rbind, result_list)

ev71p_w12 <- final_w12 %>% 
  dplyr::mutate(ev71p_w4 = ifelse(N_w4 == 0, 0, 
                                  ev71_w4/N_w4)) %>% ungroup() %>%
  dplyr::select(nameC, year, week, w0 = ev71p, w12 = ev71p_w4)

####合并4周、8周、12周####
ev71p_w4
ev71p_w8
ev71p_w12
ev71p_week <- ev71p_w4 %>% left_join(ev71p_w8) %>% left_join(ev71p_w12)
colSums(ev71p_week[,4:7])

openxlsx::write.xlsx(ev71p_week, "data/processed/ev71p_week_1211.xlsx")


