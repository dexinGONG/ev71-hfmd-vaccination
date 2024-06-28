
# LOAD REQUIRED PACKAGES AND FUNCTIONS -----------------------------------------
pkgs = c("surveillance", "dplyr", "sp", "sf", "hhh4addon","formattable", "dplyr", 
         "tidyr", "ggplot2", "writexl", "openxlsx") 
pacman::p_load(pkgs, character.only = T) # load/install packages

# set working directory
setwd("/Users/zhongjingxin/GONGDEXIN/GDCDC/HHH4-EV71") 

# LOAD DATA --------------------------------------------------------------------
#### Modelling using weekly data ####
startday <- as.Date("2016-01-01") # set the start day for the analysis

all_input_daily <- readRDS("data/processed/all_input_daily.rds") %>% ungroup() %>%
  dplyr::filter(date >= startday) # only keep data after 2016-01-01

all_input_week1 <- readRDS("week/data/processed/all_input_week.rds") %>%
  dplyr::filter(date >= startday)

# convert year-week to date format
isoweek <- all_input_daily %>% dplyr::select(date, country) %>%
  dplyr::mutate(year = surveillance::isoWeekYear(date)$ISOYear,
                week =  surveillance::isoWeekYear(date)$ISOWeek) # convert date to ISO week format

isoweek1 <- isoweek %>% group_by(country, year, week) %>% summarise(date = max(date)) %>%
  dplyr::mutate(year2 = surveillance::isoWeekYear(date)$ISOYear,
                week2 =  surveillance::isoWeekYear(date)$ISOWeek) # convert date to ISO week format, add a new variable date for the last day of the week (easier for plot)

isoweek2 <- isoweek1[ , c("country", "year", "week", "date")] # only keep the necessary columns


##### Case data 病例数据 #####
counts <- all_input_week1 %>% ungroup() %>%
  dplyr::select(date, country, case) %>%
  arrange(country, date) %>%
  tidyr::spread(key = country, value = case) # convert to matrix
sum(counts[-1]) 

##### Vaccination coverage 疫苗覆盖率 #####
vac0 <- all_input_week1 %>% ungroup() %>% # unvaccinated rate
  dplyr::select(date, country, vac0) %>%
  dplyr::mutate(vac0 = vac0 * 100) %>%
  arrange(country, date) %>%
  tidyr::spread(key = country, value = vac0) %>%
  dplyr::select(-date) %>%
  as.matrix() 

vac <- (100 - vac0) %>% as.matrix() # vaccination coverage

##### Vaccine doses 疫苗接种剂次#####
dose <- all_input_daily %>% ungroup() %>%
  dplyr::select(date, country, dose) %>%
  arrange(country, date) %>%
  tidyr::spread(key = country, value = dose) %>%
  dplyr::select(-date) %>%
  as.matrix()

##### Population mobility index: Gaode actual immigration index matrix 高德指数矩阵 #####

AMI <- all_input_week1 %>% ungroup() %>%
  dplyr::select(date, country, AMI) %>%
  arrange(country, date) %>%
  tidyr::spread(key = country, value = AMI) %>%
  dplyr::select(-date) %>%
  as.matrix()

##### Gaode index matrices 高德指数矩阵 #####
gaode_sum <- readRDS("data/processed/gaode_sum.rds")[-1] %>% as.matrix()
gaode_mean <- readRDS("data/processed/gaode_mean.rds")[-1] %>% as.matrix()

# create a function to process covariate 定义一个通用函数来处理其他协变量数据
process_data <- function(data, group_cols, value_col, join_data = NULL) {
  result <- data %>%
    dplyr::select(all_of(group_cols), all_of(value_col)) %>%
    dplyr::group_by(across(all_of(group_cols))) %>%
    dplyr::summarise(value = mean(!!sym(value_col), na.rm = TRUE)) %>%
    dplyr::arrange(across(all_of(group_cols))) 
  
  if (!is.null(join_data)) {
    result <- result %>%
      left_join(join_data, by = group_cols)
  }
  
  result %>%
    ungroup() %>%
    dplyr::select(-date) %>%
    tidyr::spread(key = group_cols[3], value = value) %>%
    dplyr::select(-all_of(group_cols[1:2])) %>%
    as.matrix()
}


#### Process each dataset 处理各个数据集 ####
# cpop: cpop_frac (Proportion: Child population in a city / Total child population in Guangdong Province)（儿童比例：某城市儿童人口/广东省总儿童人口）
cpop <- process_data(all_input_daily, c("year", "week", "country"), "cpop", isoweek2) *100 

# cprop: Composition ratio (Child population in a city / Total population in a city) # cprop: 儿童构成比（某城市儿童人口/某城市总人口） 
cprop <- process_data(all_input_daily, c("year", "week", "country"), "cprop", isoweek2) *100 

# population: City population # population: 城市人口数 
population <- process_data(all_input_daily, c("year", "week", "country"), "population", isoweek2)

# cpopulation: Child population # cpopulation: 儿童人口数 
cpopulation <- process_data(all_input_daily, c("year", "week", "country"), "cpopulation", isoweek2)

# pdensity: Population density # pdensity: 人口密度 
pdensity <- process_data(all_input_daily, c("year", "week", "country"), "pdensity", isoweek2)

# area: City area # area: 城市面积 
area <- process_data(all_input_daily, c("year", "week", "country"), "area", isoweek2)

# GDP: Gross Domestic Product # GDP: 国内生产总值 
gdp <- process_data(all_input_daily, c("year", "week", "country"), "gdp", isoweek2)

# gdppercap: GDP per capita # gdppercap: 人均GDP 
gdppercap <- process_data(all_input_week1,c("year", "week", "country"), "gdppercap", isoweek2)

# urban: Urbanization rate # urban: 城镇化率 
urban <- process_data(all_input_week1, c("year", "week", "country"), "urban", isoweek2) * 100

# temp_mean: 平均气温 # temp_mean: Mean temperature
temp_mean <- process_data(all_input_week1, c("year", "week", "country"), "temp_mean", isoweek2)

# rain_mean: 降雨量 # rain_mean: Rainfall
rain_mean <- process_data(all_input_week1, c("year", "week", "country"), "rain", isoweek2)

# rh_mean: 相对湿度 # rh_mean: Relative humidity
rh_mean <- process_data(all_input_week1, c("year", "week", "country"), "rh", isoweek2)

rh_mean <- process_data(all_input_week1, c("year", "week", "country"), "rh", isoweek2)

# Other calculations # 其他计算 

# cpopulation_sd: Standardized child population # cpopulation_sd: 标准化儿童人口数 
cpopulation_sd <- (cpopulation - mean(cpopulation))/sd(cpopulation)

# cpopulation_scale: Scaled child population # cpopulation_scale: 缩放儿童人口数
cpopulation_scale <- scale(cpopulation, center = FALSE, scale = apply(cpopulation, 2, function(x) diff(range(x))))

# cpopulation_norm: 归一化儿童人口数 # cpopulation_norm: Normalized child population
cpopulation_norm <- normalize(cpopulation, min = 0, max = 100, na.rm = FALSE)

# cpdensity: 儿童人口密度 # cpdensity: Child population density
cpdensity <- cpopulation / area

# temp_mean_sd: 标准化平均气温 # temp_mean_sd: Standardized mean temperature
temp_mean_sd <- (temp_mean - mean(temp_mean))/sd(temp_mean)

# rain_mean_sd: 标准化降雨量 # rain_mean_sd: Standardized rainfall
rain_mean_sd <- (rain_mean - mean(rain_mean))/sd(rain_mean)

# rh_mean_sd: 标准化相对湿度 # rh_mean_sd: Standardized relative humidity
rh_mean_sd <- (rh_mean - mean(rh_mean))/sd(rh_mean)


### estabilish structured time series, sts 构建结构时间序列数据(epi_sts)

gd <- st_read("data/processed/geodata/africa.gpkg") # read map #读取广东省21地市地图

# Check that the order of cases and countries in the map are the same
all(colnames(counts[-1]) == gd$country)

map <- as(gd, "Spatial") # convert to Spatial object
row.names(map) <- as.character(gd$country) # set row names to country names

##### Create adj mat and neighbours order 创建邻接矩阵和毗连阶乘 
gd_adjmat <- poly2adjmat(map) # create adjacency matrix
gd_nbOrder <- nbOrder(gd_adjmat, maxlag = Inf) # create neighbours order

maxlag <- max(gd_nbOrder) # maximum number of neighbours for a city某城市的最大毗邻城市数目

# set start date of sts 设定sts初始日期
startday <- counts$date[1] 

# create sts object
epi_sts <- sts(observed = counts[-1],
               start = c(lubridate::year(startday),
                         lubridate::epiweek(startday)),
               # start = c(surveillance::isoWeekYear(startday)$ISOYear,
               #           surveillance::isoWeekYear(startday)$ISOWeek),
               frequency = 52,
               # population = cpopulation, 
               neighbourhood = gd_nbOrder,
               map = map) 

# #### epi curve 病例时间序列图
# plot(epi_sts, type = observed ~ time,  # aggregate counts over all units
#      main = "Weekly number of EV71 HMFD cases in Guangdong Province")
# #### spatial distribution map病例空间分布图
# plot(epi_sts, type = observed ~ unit,  # plot counts for each unit
#      main = "Weekly number of EV71 HMFD cases in Guangdong Province")
# plot(epi_sts, type = observed ~ unit, # plot counts for each unit
#      # population = epi_sts@map$population / 100000,  #names(epi_sts@map);广东地图没人口数
#      labels = list(font = 1), colorkey = list(space = "right"),
#      sp.layout = layout.scalebar(epi_sts@map, corner = c(0.05, 0.05),
#      scale = 50, labels = c("0", "50 km"), height = 0.03)) # add scale bar

# MODEL模型 --------------------------------------------------------------------

##### 设定建模区间
start_day <- startday + 7*4 # “startday”为sts初始日期，往后推几周，以后续考虑滞后效应
end_day <- "2019-08-25" # 建模(拟合)最后一天。腾出时间以后续预测

fit_start <- which(counts$date == start_day) 
fit_end <- which(counts$date == end_day) 

##### 内部拟合时间区间 ???待核查内部拟合、外部拟合、预测以及训练集、测试集等概念
TRAIN <- c(fit_start :(fit_end))   

##### 外部拟合时间区间 ???待核查内部拟合、外部拟合、预测以及训练集、测试集等概念
preddays <- 3
# TEST <- (fit_end + 1): (fit_end + preddays)

data = list(vac = vac,vac0= vac0,cpopulation = cpopulation,
            population = population, cpdensity = cpdensity,
            gdppercap = gdppercap, urban = urban,area = area,
            rain_mean = rain_mean, temp_mean = temp_mean,rh_mean = rh_mean,
            rain_mean_sd = rain_mean_sd, temp_mean_sd = temp_mean_sd, rh_mean_sd = rh_mean_sd)
#AMI = AMI, gaode_sum = gaode_sum, gaode_mean = gaode_mean,


#### BASIC MODEL 基础模型####
f_end <- ~ 1 
f_ar <- ~ 1  
f_ne <- ~ 1 

#### Basic Model 基础模型（空间联系权重用neighbourhood(epi_sts)，即城市间毗邻个数）
epi_sts_basic <- list(end = list(f = f_end), 
                      ar = list(f = f_ar), 
                      ne = list(f = f_ne, weights = W_powerlaw(maxlag = maxlag, normalize = TRUE) ), 
                      #W_powerlaw(maxlag = 9, normalize = TRUE) #neighbourhood(epi_sts) #neighbourhood(epi_sts))
                      family = "NegBin1",  # "NegBin1" "Poisson" "NegBin2"
                      optimizer = list(stop = list(tol=1e-5, niter=500), 
                                       regression = list(method="nlminb")), # "nlminb" "optim"
                      subset = TRAIN, 
                      data = data,
                      keep.terms = TRUE) 

fit_basic <- hhh4(stsObj = epi_sts,control = epi_sts_basic) 


# Update the weights of the ne part 更新ne部分的权重（空间联系权重用0-1矩阵，即城市若毗邻定义为1，否则为0）
fit_basic_binary <- update(fit_basic,
                           ne = list(f = ~1, weights = gd_adjmat))  
#(use the 0-1 matrix gd_adjmat for spatial connection weights,
# i.e., if the cities are adjacent, it is defined as 1; otherwise, it is 0

# update weights using Power Law estimation, normalize = TRUE # 用PowerLaw作空间权重
fit_basic_powerlaw <- update(fit_basic,
                             ne = list(f = ~1, weights = W_powerlaw(maxlag = maxlag, normalize = TRUE)))

# update weights using Power Law estimation, normalize = FALSE # 用PowerLaw作空间权重
fit_basic_powerlaw_nonorm <- update(fit_basic,
                                    ne = list(f = ~1, weights = W_powerlaw(maxlag = maxlag, normalize = FALSE)))

# update weights using Populaiton Mobility Index # 用人口流动指数作空间权重
fit_basic_gaode <- update(fit_basic,
                          ne = list(f = ~1, weights = gaode_mean))

# update weights using log-transformed Populaiton Mobility Index # 用log(人口流动指数)作空间权重
log_gaode <- log(gaode_sum)
log_gaode[is.infinite(log_gaode)] <- 0

fit_basic_gaode_log <- update(fit_basic,
                              ne = list(f = ~1, weights = log_gaode))

# model evaluation using AIC as the criterion 对比不同权重下的AIC,log(gaode)最好
AIC(fit_basic, fit_basic_binary,fit_basic_powerlaw, fit_basic_powerlaw_nonorm, fit_basic_gaode, fit_basic_gaode_log) 

# fit_basic_gaode_log turns to be the best basic model 替换fit_basic 为fit_basic_binary和fit_basic_powerlaw
fit_basic <- fit_basic_gaode_log #fit_basic_gaode#fit_basic_powerlaw#fit_basic #fit_basic_binary #fit_basic_powerlaw

# Function to Calculate Coefficients and Confidence Intervals 计算系数和置信区间的函数
calculate_coefs_and_CIs <- function(fit_basic, nterms) {
  coefs <- exp(coef(fit_basic)[1:nterms])
  CIs <- exp(confint(fit_basic)[1:nterms, ])
  id_log <- c(grep("over", names(coefs)), grep("neweights.d", names(coefs)))
  coefs[id_log] <- log(coefs[id_log])
  CIs[id_log, ] <- log(CIs[id_log, ])
  round(cbind(coefs, CIs), 3)
}

# Function to calculate prediction performance scores 计算预测性能得分的函数
calculate_fit_scores <- function(fit_basic, tp, type, which.start = NULL) {
  forecast <- oneStepAhead(fit_basic, tp = tp, type = type, which.start = which.start) #oneStepAhead_hhh4lag
  colMeans(scores(forecast))
}

# Function to generate the final summary table生成最终摘要表格的函数
generate_final_summary_table <- function(tab, fit_basic, fitScores) {
  rbind(
    cbind(Params = rownames(tab), tab),
    c("", "", "", ""),
    c("AIC", round(AIC(fit_basic), 2), "", ""),
    c("", "", "", ""),
    names(fitScores),
    round(as.numeric(fitScores), 3)
  )
}


# Function to calculate p-values# 计算 p 值的函数
calculate_pvalues <- function(fit_basic, nterms) {
  beta_hat <- fit_basic$coefficients[1:nterms]
  sd_hat <- fit_basic$se[1:nterms]
  stopifnot(all.equal(names(beta_hat), names(sd_hat)))
  zscores <- beta_hat / sd_hat
  pvalues <- 2 * pnorm(abs(zscores), lower.tail = FALSE)
  pvalue <- data.frame(
    name = names(pvalues),
    pvalue = round(pvalues, 3)
  )
  pvalue
}


# set the number of terms in the model 设置模型中的项数
num <- 1 #  if powerlaw is used, num would be defined as 2
nterms <- terms(fit_basic)$nGroups + num
tab <- calculate_coefs_and_CIs(fit_basic, nterms)

# Calculate and print prediction performance scores 计算和打印预测性能得分
tp <- c(fit_end, fit_end + preddays)
fitScores_final <- calculate_fit_scores(fit_basic, tp, type = "final")
fitScores_final

fitScores_rolling <- calculate_fit_scores(fit_basic, tp, type = "rolling", which.start = "final")
fitScores_rolling


# Generate the final summary table 生成最终摘要表格
tab <- generate_final_summary_table(tab, fit_basic, fitScores_final)


# Calculate p-values and generate a table 计算 p 值并生成包含 p 值的表格
pvalue <- calculate_pvalues(fit_basic, nterms)
tab_p_value_epi_sts_basic <- tab %>%
  as.data.frame() %>%
  left_join(pvalue, by = c("Params" = "name"))


# Write to CSV file 写入 CSV 文件
write.xlsx(tab_p_value_epi_sts_basic, "output/tables/basic_model_powerlaw.xlsx")


####covariates: not included vs. offset vs. covariates 协变量:不纳入vs.offset vs.协变量

## Data Transformation 数据变换
cpopulation_log <- log(cpopulation)
gdppercap_log <- log(gdppercap)
cprop <- as.matrix(cprop)
rain_mean <- rain_mean + 1

## Define variable list 定义变量列表
variables <- c("vac0", "cpopulation_log","gdppercap_log", #vac会报错
               "cpop", "cprop",  "urban",
               "temp_mean",  "rh_mean", "rain_mean") #"rain_mean"无法纳入，可能需变换
#右边变量报错(Error in nlminb等)："cpdensity", "pdensity","gdp", "area", #

# Define Soptions and SmodelGrid 定义Soptions和SmodelGrid
Soptions <- c("none", "Soffset", "Scovar")
SmodelGrid <- expand.grid(end = Soptions, ar = Soptions, ne = Soptions)
row.names(SmodelGrid) <- do.call("paste", c(SmodelGrid, list(sep = "|")))

# Create a list to store AIC results 创建一个存储AIC结果的列表 
AIC_results <- list()

# Main loop: iterate through each variable for modeling and AIC calculation主循环，遍历变量建模和AIC计算
for (var in variables) {
  Sprop <- get(var)  # Get the current variable
  epi_sts_covar <- apply(X = SmodelGrid, MARGIN = 1, FUN = function(options) {
    updatecomp <- function(comp, option) switch(option,
                                                "none" = comp,
                                                "Soffset" = list(offset = comp$offset * Sprop),
                                                "Scovar" = list(f = update(comp$f, ~. + Sprop)))
    update(fit_basic,
           end = updatecomp(fit_basic$control$end, options[1]),
           ar = updatecomp(fit_basic$control$ar, options[2]),
           ne = updatecomp(fit_basic$control$ne, options[3]),
           subset = TRAIN,
           optimizer = list(stop = list(tol=1e-5, niter=500), 
                            regression = list(method="nlminb")), 
           keep.terms = TRUE,
           data = list(Sprop = Sprop))
  })
  
  # Calculate AIC for the models in epi_sts_covar计算当前变量的AIC值
  tryCatch({
    aics_covar <- sapply(epi_sts_covar, AIC, simplify = FALSE)
    aics_season_covar_sorted <- do.call(rbind, lapply(aics_covar, function(x) cbind(x = x)))
    
    # Store the AIC results in the results list将AIC结果存储到结果列表中
    AIC_results[[var]] <- as.data.frame(aics_season_covar_sorted)
    AIC_results[[var]]$comp <- gsub("`", "", row.names(AIC_results[[var]]))
  }, error = function(e) {
    warning(paste("Error in AIC calculation for variable", var, ":", e$message))
  })
}

# Create a new Excel workbook 创建一个新的Excel工作簿
wb <- openxlsx::createWorkbook()

# Loop through AIC_results and add each result to the Excel workbook 循环遍历AIC_results，将每个结果添加到Excel工作簿中
for (var in names(AIC_results)) {
  addWorksheet(wb, var)
  writeData(wb, var, AIC_results[[var]])
}

# Save the workbook
saveWorkbook(wb, "AIC_Results.xlsx", overwrite = TRUE)

# # 假设epi_sts_season和TRAIN已经定义
# # 这里是主循环，遍历每个变量进行建模和AIC计算
# for (var in variables) {
#   Sprop <- get(var)  # 获取当前变量
#   epi_sts_covar <- apply(X = SmodelGrid, MARGIN = 1, FUN = function(options) {
#     updatecomp <- function(comp, option) switch(option,
#                                                 "unchanged" = list(),
#                                                 "Soffset" = list(offset = comp$offset * Sprop),
#                                                 "Scovar" = list(f = update(comp$f, ~. + (Sprop))))
#     update(fit_basic,
#            end = updatecomp(fit_basic$control$end, options[1]),
#            ar = updatecomp(fit_basic$control$ar, options[2]),
#            ne = updatecomp(fit_basic$control$ne, options[3]),
#            subset = TRAIN,
#            optimizer = list(stop = list(tol=1e-5, niter=500), 
#                             regression = list(method="nlminb")), 
#            keep.terms = TRUE,
#            data = list(Sprop = Sprop))
#   })
# 
#   # 计算当前变量的AIC值
#   aics_covar <- do.call(AIC, lapply(names(epi_sts_covar), as.name),
#                                envir = as.environment(epi_sts_covar))
#   aics_season_covar_sorted <- aics_covar[order(aics_covar[, "AIC"]), ]
# 
#   # 将AIC结果存储到结果列表中
#   AIC_results[[var]] <- as.data.frame(aics_season_covar_sorted)
#   AIC_results[[var]]$comp <- gsub("`", "", row.names(AIC_results[[var]]))
# }
# 
# # 创建一个新的Excel工作簿
# wb <- openxlsx::createWorkbook()
# 
# # 循环遍历AIC_results，将每个结果添加到Excel工作簿中
# for (var in names(AIC_results)) {
#   addWorksheet(wb, var)
#   writeData(wb, var, AIC_results[[var]])
# }
# 
# # 保存工作簿到文件
# saveWorkbook(wb, "output/tables/AIC_Sprop.xlsx", overwrite = TRUE)


# MODEL 2 -----------------------------------------------------------------
# Define variable list 后续继续查找，尝试纳入其他变量


#####Find the model with the lowest AIC: different variable combinations寻找AIC最低的模型：不同变量组合####
demographic_factors <- c("log(cpopulation)","vac","log(gdppercap)", "temp_mean","rain_mean","rh_mean")

# Weather_factors <- c()

# Create an empty data frame to store the results创建一个空数据框来存储结果
result_df <- data.frame(demographic_factor = character(),
                        loglikelihood = numeric(),
                        stringsAsFactors = FALSE)
# Initialize an empty string vector to store variable names初始化个空字符串向量累积变量名
variable_accumulator <- c()
# Iterate through each variable in demographic_factors 遍历 demographic_factors 中的每个变量
for (factor in demographic_factors) {
  # Update the variable accumulator 更新变量累积器
  variable_accumulator <- c(variable_accumulator, factor)
  
  # Construct a new f_end formula 构建新的 f_end 公式
  new_f_end_formula <- paste("~ 1 + ", paste(variable_accumulator, collapse = " + "))
  new_f_end <- as.formula(new_f_end_formula)
  
  new_f_ar_formula <- paste("~ 1 + ", paste(variable_accumulator, collapse = " + "))
  new_f_ar <- as.formula(new_f_ar_formula)
  
  new_f_ne_formula <- paste("~ 1 + ", paste(variable_accumulator, collapse = " + "))
  new_f_ne <- as.formula(new_f_ne_formula)
  
  model_basic <- fit_basic
  
  # Update f_end in model_basic 更新 model_basic 中的 f_end
  model_basic$control$end$f <- new_f_end
  model_basic$control$ar$f <- new_f_ar
  model_basic$control$ne$f <- new_f_ne
  # Update the model 更新拟合模型
  fit <- update(fit_basic,
                ar = list(f = update(formula(model_basic)$ar, new_f_ar)),
                ne = list(f = update(formula(model_basic)$ne, new_f_ne)),
                end = list(f = update(formula(model_basic)$end, new_f_end))
  )
  
  # Extract the AIC提取对数似然
  AIC <- AIC(fit)
  
  # Add the results to the data frame将结果添加到结果数据框中
  
  result_df <- rbind(result_df, data.frame(demographic_factor = new_f_end_formula,
                                           AIC = AIC))
}
result_df

write_xlsx(result_df, "output/tables/AIC_BasicModel逐个加协变量.xlsx")

##### Basic Model + demographic factors #####

# Update the model 更新模型
epi_sts_demographics <- update(fit_basic,
        ar = list(f = update(formula(fit_basic)$ar, ~. + (vac) + log(gdppercap) + log(cpopulation))),
        ne = list(f = update(formula(fit_basic)$ne, ~. + (vac) + log(gdppercap) + log(cpopulation))),
        end = list(f = update(formula(fit_basic)$end,  ~. + (vac) + log(gdppercap) + log(cpopulation)))) 

options(scipen = 999, digits = 3) # Set the number of decimal 设置小数点位数

fit_basic <- epi_sts_demographics # set the epi_sts_demographics as the new model 设置新模型

# set the number of terms in the model 设置模型中的项数
nterms <- terms(fit_basic)$nGroups + num
tab <- calculate_coefs_and_CIs(fit_basic, nterms)

# Calculate and print prediction performance scores 计算和打印预测性能得分
tp <- c(fit_end, fit_end + preddays)
fitScores_final <- calculate_fit_scores(fit_basic, tp, type = "final")
fitScores_final

fitScores_rolling <- calculate_fit_scores(fit_basic, tp, type = "rolling", which.start = "final")
fitScores_rolling

# Generate the final summary table 生成最终摘要表格
tab <- generate_final_summary_table(tab, fit_basic, fitScores_final)

# Calculate p-values and generate a table 计算 p 值并生成包含 p 值的表格
pvalue <- calculate_pvalues(fit_basic, nterms)
tab_p_value_epi_sts_demographics <- tab %>%
  as.data.frame() %>%
  left_join(pvalue, by = c("Params" = "name"))

# Write to CSV file 写入 CSV 文件
write.xlsx(tab_p_value_epi_sts_demographics, "output/tables/basic_model_powerlaw_demographics.xlsx")


#### Figure S2: Plot seasonality ####

# plot(fit, type = observed ~ unit, total = TRUE,
#      hide0s = TRUE, par.settings = NULL, legend = FALSE)
# dev.new()
plot(fit, type = "maps",  #和后面的图矛盾？？？
     which = c("epi.own", "epi.neighbours", "endemic"),
     prop = TRUE, labels = list(cex = 0.6))

# plot(fit, type = "season", components = "end", main = "")
# plot(fit, type = "season", components = "ar", main = "")
# plot(fit, type = "season", components = "ne", main = "")


##### Basic Model + weather factors #########

epi_sts_demographics_weather <- update(epi_sts_demographics,
        end = list(f = update(formula(epi_sts_demographics)$end,  ~. + rain_mean_sd + temp_mean_sd + rh_mean_sd)),
        ar = list(f = update(formula(epi_sts_demographics)$ar, ~. + rain_mean_sd + temp_mean_sd + rh_mean_sd)),
        ne = list(f = update(formula(epi_sts_demographics)$ne, ~. + rain_mean_sd + temp_mean_sd + rh_mean_sd)))

fit_basic <- epi_sts_demographics_weather

plot(fit_basic, type = "fitted", total = TRUE,  #Overall fitting
     hide0s = TRUE, par.settings = NULL, legend = FALSE) -> fitted_components

fitted_components$Overall
colSums(fitted_components$Overall)[3:5] / sum(fitted_components$Overall[,1])

# set the number of terms in the model 设置模型中的项数
nterms <- terms(fit_basic)$nGroups + num
tab <- calculate_coefs_and_CIs(fit_basic, nterms)

# Calculate and print prediction performance scores 计算和打印预测性能得分
tp <- c(fit_end, fit_end + preddays)
fitScores_final <- calculate_fit_scores(fit_basic, tp, type = "final")
fitScores_final

fitScores_rolling <- calculate_fit_scores(fit_basic, tp, type = "rolling", which.start = "final")
fitScores_rolling

# Generate the final summary table 生成最终摘要表格
tab <- generate_final_summary_table(tab, fit_basic, fitScores_final)

# Calculate p-values and generate a table 计算 p 值并生成包含 p 值的表格
pvalue <- calculate_pvalues(fit_basic, nterms)
tab_p_value_epi_sts_demographics_weather <- tab %>%
  as.data.frame() %>%
  left_join(pvalue, by = c("Params" = "name"))
tab_p_value_epi_sts_demographics_weather

# Write to CSV file 写入 CSV 文件和最终模型
write.xlsx(tab_p_value_epi_sts_demographics_weather, "output/tables/basic_model_powerlaw_demographics_weather.xlsx")
saveRDS(epi_sts_demographics_weather, "output/models/epi_sts_demographics_weather.rds")


#### Test different number of seasonal waves寻找最佳季节效应 ####

f.season0 <- addSeason2formula(f = ~ 1 + vac + log(gdppercap) + log(cpopulation) + rain_mean_sd + temp_mean_sd + 
                                 rh_mean_sd , S=0, period= epi_sts@freq) 
f.season1 <- update(f.season0, addSeason2formula(f =~.,S=1))
f.season2 <- update(f.season0, addSeason2formula(f =~.,S=2))

f.season1 <- addSeason2formula(f = ~ 1 + vac + log(gdppercap) + log(cpopulation) + rain_mean_sd + temp_mean_sd +
                                 rh_mean_sd , S=1, period= epi_sts@freq) # one wave
f.season1 <- addSeason2formula(f = ~ 1 + vac + log(gdppercap) + log(cpopulation) + rain_mean_sd + temp_mean_sd +
                                 rh_mean_sd , S=1, period= epi_sts@freq) # one wave
f.season2 <- addSeason2formula(f = ~ 1 + vac + log(gdppercap) + log(cpopulation) + rain_mean_sd + temp_mean_sd +
                                 rh_mean_sd, S=2, period= epi_sts@freq) # two Waves

# 检查不同季节波数对模型的影响
Soptions <- c("none","oneSwave", "twoSwave")

# 创建模型网格
SmodelGrid <- expand.grid(end = Soptions, ar = Soptions, ne = Soptions) # 3^3 = 27 models
row.names(SmodelGrid) <- do.call("paste", c(SmodelGrid, list(sep = "|"))) # 生成模型名称

# 更新模型
epi_sts_demographics_weather_season <- apply(X = SmodelGrid, MARGIN = 1, FUN = function (options) {
  updatecomp <- function (comp, option) switch(option,
                                               "none"=list(f =f.season0),
                                               "oneSwave" = list(f =f.season1),
                                               "twoSwave" = list(f = f.season2))
  update(fit_basic,
         end = updatecomp(fit_basic$control$end,  options[1]),
         ar = updatecomp(fit_basic$control$ar, options[2]),
         ne = updatecomp(fit_basic$control$ne, options[3]),
         data = list(f.season0 = f.season0, f.season1 = f.season1, f.season2 = f.season2),
         subset = TRAIN,
         optimizer = list(stop = list(tol=1e-5, niter=50000),
                          regression = list(method="nlminb")),
         keep.terms = TRUE)
})

# 计算AIC值
aics_basic_season <- lapply(epi_sts_demographics_weather_season, AIC)
aics_basic_season_df <- do.call(rbind, aics_basic_season) %>% as.data.frame()
aics_basic_season_df <- aics_basic_season_df %>% 
  dplyr::mutate(comp = as.vector(row.names(aics_basic_season_df))) %>%
  dplyr::rename(AIC = V1)
aics_basic_season_sorted <- aics_basic_season_df[order(aics_basic_season_df[, "AIC"]), ]

aics_basic_season_sorted <- aics_basic_season_sorted %>%
  dplyr::filter(!is.na(AIC)) %>% as.data.frame


# 将AIC结果转换为数据框
AIC_season <- as.data.frame(aics_basic_season_sorted)

AIC_season$comp <- gsub("`", "", AIC_season$comp)

# names(epi_sts_demographics_weather_season)
# row.names(aics_basic_season_sorted)

epi_sts_demographics_weather_season1 <- epi_sts_demographics_weather_season[row.names(aics_basic_season_sorted)]

epi_seasonPreds <- lapply(epi_sts_demographics_weather_season1, oneStepAhead,
                          tp = tp, type = "rolling", which.start = "final")


# range(TEST)-1

#get score values and calibration tests and select best model
#if rps and logS are different, we chose by logS

source('week/code/HPAI_hhh4_custom_functions.R')
SCORES <- c("rps", "logs")

epi_seasonPreds_scores <- lapply(epi_seasonPreds, scores, which = SCORES, individual = TRUE)
scores_season <- t(sapply(epi_seasonPreds_scores, colMeans, dims = 2)) %>% as.data.frame()
scores_season$comp <- row.names(scores_season)

AIC_scores_season <- AIC_season %>% dplyr::right_join(scores_season, by = "comp") %>%
  dplyr::select(End_Within_Between = comp, AIC, logs, rps) %>%
  dplyr::arrange(logs) 
# season => cov  logs:121 # rps:021 # AIC:222
# cov => season  logs:001 # rps:001 # AIC:022

write.xlsx(AIC_scores_season, paste0("output/tables/AIC_scores_season", ".xlsx"), rowNames = FALSE)


# names(epi_seasonPreds)
AIC_scores_season1 <- AIC_scores_season %>% dplyr::filter(!is.na(logs))

epi_seasonPreds1 <- epi_seasonPreds[AIC_scores_season1$End_Within_Between]

best_models <- selectBestModel(epi_seasonPreds, metrics = SCORES, #error
                               verbose = TRUE, plot = FALSE)
# mu > 0 are not all TRUE

final_mod_name <- substr(best_models$logs, 1, nchar(best_models$logs))
# final_mod_name <- "twoSwave|twoSwave|twoSwave"#按AIC标准来则是`twoSwave|twoSwavetwoSwave`
# end（第一个） = 1wave时，demographic模型时候不收敛； 但是AIC 18617.69，3个疫苗都有保护作用；
# ar (第二个) = 1wave时，epi_sts_season_demographics_weather的AIC为 18019, df=33；ri可收敛;疫苗反向作用
# ne (第三个) = 1wave时，epi_sts_season_demographics_weather的AIC为 17897, df=33; ri不收敛;疫苗end.vac 1.06  0.965  1.165 
# 所有都等于2wave时候，AIC 17926.22；end.vac    1.051  0.972   1.136


#create a new seasonal model, using the best model chosen above
epi_sts_demographics_weather_season <- epi_sts_demographics_weather_season1[[final_mod_name]]

fit_basic <- epi_sts_demographics_weather_season
# fit_basic <- hhh4(epi_sts, control = epi_sts_season)

# set the number of terms in the model 设置模型中的项数
nterms <- terms(fit_basic)$nGroups + num
tab <- calculate_coefs_and_CIs(fit_basic, nterms)

# Calculate and print prediction performance scores 计算和打印预测性能得分
tp <- c(fit_end, fit_end + preddays)
fitScores_final <- calculate_fit_scores(fit_basic, tp, type = "final")
fitScores_final

fitScores_rolling <- calculate_fit_scores(fit_basic, tp, type = "rolling", which.start = "final")
fitScores_rolling

# Generate the final summary table 生成最终摘要表格
tab <- generate_final_summary_table(tab, fit_basic, fitScores_final)

# Calculate p-values and generate a table 计算 p 值并生成包含 p 值的表格
pvalue <- calculate_pvalues(fit_basic, nterms)
tab_p_value_demographics_weather_season <- tab %>%
  as.data.frame() %>%
  left_join(pvalue, by = c("Params" = "name"))
tab_p_value_demographics_weather_season


# Write to CSV file 写入 CSV 文件
writexl::write_xlsx(tab_p_value_demographics_weather_season, "output/tables/basic_model_powerlaw_weather_season.xlsx")
saveRDS(epi_sts_demographics_weather_season, "output/models/epi_sts_demographics_weather_season.rds")



### Plot seasonality ####
plot(fit, type = "maps",  #和后面的图矛盾？？？
     which = c("epi.own", "epi.neighbours", "endemic"),
     prop = TRUE, labels = list(cex = 0.6))

plot(fit_basic, type = "season", components = "end", main = "")
# plot(fit_basic, type = "season", components = "ar", main = "")
# plot(fit_basic, type = "season", components = "ne", main = "")




# 作图:21个地市三个成分流行地图 ------------------------------------------------

设置字体和编码
windowsFonts(myFont = windowsFont("SimSun"))  # 设置字体为宋体
par(mfrow = c(3, 1))
# 绘制地图
plot(fit, type = "maps",
     which = "epi.own",
     prop = TRUE, labels = list(cex = 2.5),
     cex.main = 20, main = "时间自相关成分")


##### 随机效应 ####
epi_sts_demographics_weather_season_ri <- update(epi_sts_demographics_weather_season,
         end = list(f = update(formula(epi_sts_demographics_weather_season)$end,  ~. + ri() - 1)),
         ar = list(f = update(formula(epi_sts_demographics_weather_season)$ar, ~. + ri() - 1)),
         ne = list(f = update(formula(epi_sts_demographics_weather_season)$ne, ~. + ri() - 1)))


fit_basic <- epi_sts_demographics_weather_season_ri

# set the number of terms in the model 设置模型中的项数
summary(fit_basic)
nterms <- terms(fit_basic)$nGroups + num
tab <- calculate_coefs_and_CIs(fit_basic, nterms)

# Calculate and print prediction performance scores 计算和打印预测性能得分
tp <- c(fit_end, fit_end + preddays)
fitScores_final <- calculate_fit_scores(fit_basic, tp, type = "final")
fitScores_final

fitScores_rolling <- calculate_fit_scores(fit_basic, tp, type = "rolling", which.start = "final")
fitScores_rolling

# Generate the final summary table 生成最终摘要表格
tab <- generate_final_summary_table(tab, fit_basic, fitScores_final)

# Calculate p-values and generate a table 计算 p 值并生成包含 p 值的表格
pvalue <- calculate_pvalues(fit_basic, nterms)
tab_p_value_demographics_weather_season_ri <- tab %>%
  as.data.frame() %>%
  left_join(pvalue, by = c("Params" = "name"))

# Write to CSV file 写入 CSV 文件
write.csv(tab_p_value_demographics_weather_season_ri, "output/tables/basic model_powerlaw_season_demographics_weather_ri.csv")


AIC_all_model <- AIC(fit_basic_binary,fit_basic_powerlaw, fit_basic_gaode, fit_basic_gaode_log,
    # epi_sts_season,# epi_sts_basic,
    epi_sts_demographics,
    epi_sts_demographics_weather,
    epi_sts_demographics_weather_season_ri,
    epi_sts_demographics_weather_season)

writexl::write_xlsx(AIC_all_model, "output/tables/AIC_all_model.xlsx")
