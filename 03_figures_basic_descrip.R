
# LOAD REQUIRED PACKAGES AND FUNCTIONS -----------------------------------------
if (!require("pacman")) install.packages("pacman")
pkgs = c("EpiEstim","surveillance", "dplyr", "sp", "sf", "hhh4addon","formattable", "dplyr", 
         "tidyr", "ggplot2", "hhh4addon", "RColorBrewer") 
pacman::p_load(pkgs, character.only = T)


setwd("/Users/zhongjingxin/GONGDEXIN/GDCDC/HHH4-EV71")
# setwd("C:/Users/Huanjingshi/Documents/WPSDrive/16131756/WPS云盘/GDCDC/GDCDC2023/HHH4-EV71/")


source("R/functions.R")

# LOAD DATA --------------------------------------------------------------------

startday <- as.Date("2016-01-03")
end_day <- as.Date("2019-08-25")

# Rt at the provincial level
cases_prov <- readRDS("week/data/processed/all_input_week.rds")  %>%
  dplyr::filter(date >= startday) %>% ungroup() %>% 
  dplyr::select(date, country, case) %>% dplyr::group_by(date) %>%
  dplyr::summarise(case = sum(case))

plot_cases_prov <- cases_prov %>% 
  ggplot(aes(x = date, y = case)) +
  geom_line() +
  labs(x = "", y = "EV71 HFMD cases") +
  scale_x_date(date_labels = "%Y", 
               limits = c(as.Date("2016-01-01"), as.Date("2019-12-29")),
               expand = c(0, 0)) +
  theme_bw(base_size = 12)

Rt_prov <- EpiEstim::estimate_R(incid = cases_prov$case,
                     method = "parametric_si",
                     config = make_config(list(
                       mean_si = 3.7, std_si = 2.6,
                       t_start=2:(nrow(cases_prov)-10),
                       t_end=(2+10):nrow(cases_prov))))

#提取实时Rt和95%置信区
eR <- Rt_prov$R$`Mean(R)`

Rt_prov <- data.frame(R=Rt_prov$R$`Mean(R)`,
                   RL=Rt_prov$R$`Quantile.0.025(R)`,
                   RH=Rt_prov$R$`Quantile.0.975(R)`,
                   date=cases_prov$date[(nrow(cases_prov)-length(eR)+1):nrow(cases_prov)])

Rt_prov1 <- rbind(data.frame(R=rep(NA,11), RL=rep(NA,11),RH=rep(NA,11),date=cases_prov$date[1:11]),Rt_prov)

Rt_prov1 %>% 
  dplyr::filter(date >= as.Date("2016-01-01")) %>%
  ggplot(aes(x = date, y = R)) +
  geom_line() +
  # facet_wrap(~ country, scales = "free_y") +
  labs(x = "", y = "Rt") +
  scale_x_date(date_labels = "%Y", 
               limits = c(as.Date("2016-01-01"), as.Date("2019-12-29")),
               expand = c(0, 0)) +
  theme_bw(base_size = 12)

all_input_daily <- readRDS("week/data/processed/all_input_week.rds") %>%
  dplyr::filter(date >= startday)


# Cases at country level
cases <- all_input_daily %>% 
  ungroup() %>% 
  dplyr::select(date, country, case) 


# Shapefile
africa <- st_read("data/processed/geodata/africa.gpkg") %>%
  dplyr::rename(name = country)

# DATA PREPARATION -------------------------------------------------------------

all_input <- all_input_daily %>%
  dplyr::rename(COUNTRY = country, time = date,
                observed = case, vac = vac, temp = temp_mean, sh = rh)

all_input$rp100k <- (all_input$observed / all_input$population) * 100000

# first_day <- all_input$time[1]
# last_day <- tail(all_input$time, 1)

counts_mat <- cases %>% tidyr::spread(key = country, value = case)

unitsname <- colSums(counts_mat[-1]) %>% sort(decreasing = TRUE) %>% head(4) %>% names()
unitsname_vac <- c("Guangzhou", "Shenzhen", "Zhaoqing", "Zhongshan")
unitsname_rpk <- c("Zhuhai", "Huizhou", "Shanwei", "Zhanjiang")

# Select units to be analyzed
unitsname <- unitsname_rpk

# Filter and summarize data
df <- all_input %>%
  dplyr::filter(COUNTRY %in% unitsname) %>%
  dplyr::group_by(COUNTRY) %>%
  dplyr::summarise(time = min(time), value = mean(rp100k))

options(scipen = 999)
# 
# # Function to create plots
# create_plot <- function(data, unitsname, start_day, end_day, variables, labels, y_free = TRUE) {
#   data %>%
#     ungroup() %>%
#     dplyr::select(COUNTRY, time, all_of(variables)) %>%
#     dplyr::mutate(vac = 100 * vac) %>%
#     dplyr::filter(COUNTRY %in% unitsname) %>%
#     tidyr::gather(key = "var", value = "value", -COUNTRY, -time) %>%
#     ggplot(aes(x = time, y = value)) +
#     geom_rect(data = df, aes(xmin = start_day, xmax = end_day, ymin = -Inf, ymax = Inf), fill = "grey", alpha = 0.1) +
#     geom_rect(data = df, aes(xmin = end_day, xmax = as.Date("2019-12-29"), ymin = -Inf, ymax = Inf), fill = "orange", alpha = 0.1) +
#     geom_line(aes(col = COUNTRY)) +
#     facet_grid(rows = vars(factor(var, levels = variables, labels = labels)), scales = ifelse(y_free, "free_y", "fixed"), labeller = label_parsed) +
#     labs(x = "", y = "", col = "") +
#     ggsci::scale_color_nejm() +
#     scale_x_date(date_breaks = "1 year", date_labels = "%Y", limits = c(as.Date("2016-01-01"), as.Date("2019-12-29")), expand = c(0, 1)) +
#     theme_bw(base_size = 10) +
#     theme(legend.position = "top")
# }
# 
# # Define variables and labels for plots
# variables_1 <- c("rp100k", "vac", "AMI", "temp", "rain", "sh")
# labels_1 <- c(expression("Cases per" ~ 100000), expression("Vaccination coverage"('%')), "Traffic~mobility~\n~index",
#               expression("Temperature" ~ (C^o)), "Rain~(mm)", expression("Relative Humidity" ~ ('%')))
# 
# variables_2 <- c("rp100k", "vac", "cpopulation", "gdppercap", "temp", "rain", "sh")
# labels_2 <- c(expression("Cases per" ~ 100000), expression("Vaccination coverage"('%')), "child~population", "GDP~per~capital",
#               expression("Temperature" ~ (C^o)), "Rain~(mm)", expression("Relative Humidity" ~ ('%')))
# 
# # Create plots
# plot_1 <- create_plot(all_input, unitsname, start_day = as.Date("2016-01-03"), end_day = as.Date("2019-08-25"), variables_1, labels_1)
# plot_2 <- create_plot(all_input, unitsname, start_day = as.Date("2016-01-03"), end_day = as.Date("2019-08-25"), variables_2, labels_2)
# 
# # Save plots
# ggsave("figs/figure1A input1.pdf", plot_1, width = 7, height = 9)
# ggsave("figs/figure1B input2.pdf", plot_2, width = 7, height = 9)
# 
# # Additional plots for individual variables
# create_individual_plot <- function(data, variable, label, y_label, unitsname) {
#   data %>%
#     # dplyr::filter(COUNTRY %in% unitsname) %>%
#     ggplot(aes(x = time, y = !!sym(variable))) +
#     geom_line() +
#     facet_wrap(~ COUNTRY, scales = "free_y") +
#     labs(x = "", y = y_label) +
#     scale_x_date(date_labels = "%Y", expand = c(0, 0)) +
#     theme_bw(base_size = 12)
# }
# 
# # Create and save individual plots
# unitsname <- unique(all_input$COUNTRY)
# 
# plot_cases <- create_individual_plot(all_input, "rp100k", "Daily cases per 100,000", "Daily cases per 100,000", unitsname)
# ggsave('figs/SI_figureS1 plot_cases.pdf', width = 18, height = 10)
# 
# plot_vac <- create_individual_plot(all_input, "vac", "Vaccination coverage (%)", "Vaccination coverage (%) since Jan 2016", unitsname)
# ggsave('figs/SI_figureS1 plot_vac.pdf', width = 18, height = 10)
# 
# plot_dose <- create_individual_plot(all_input, "dose", "Number of doses administered since Jan 2016", "Number of doses administered since Jan 2016", unitsname)
# ggsave('figs/SI_figureS1 plot_dose.pdf', width = 18, height = 10)
# 
# plot_mobility <- create_individual_plot(subset(all_input, all_input$year == 2019), "AMI", "Actual population mobility index in 2019", "Actual population mobility index in 2019", unitsname)
# ggsave('figs/SI_figureS1 plot_mobility.pdf', width = 18, height = 10)
# 
# plot_cpopulation <- create_individual_plot(all_input, "cpopulation", "Child population", "Child population", unitsname)
# ggsave('figs/SI_figureS1 plot_cpopulation.pdf', width = 18, height = 10)
# 
# plot_cprop <- create_individual_plot(all_input, "cprop", "Child proportion (%)", "Child proportion (%)", unitsname)
# ggsave('figs/SI_figureS1 plot_cprop.pdf', width = 18, height = 10)
# 
# plot_pdensity <- create_individual_plot(all_input, "pdensity", expression("Population~density" ~ (`per` ~ km^2)), "Population density (per km^2)", unitsname)
# ggsave('figs/SI_figureS1 plot_pdensity.pdf', width = 18, height = 10)
# 
# plot_gdppercap <- create_individual_plot(all_input, "gdppercap", "GDP per capita (US Dollar)", "GDP per capita (US Dollar)", unitsname)
# ggsave('figs/SI_figureS1 plot_gdppercap.pdf', width = 18, height = 10)
# 
# plot_urban <- create_individual_plot(all_input, "urban", "Urban ratio (%)", "Urban ratio (%)", unitsname)
# ggsave('figs/SI_figureS1 plot_urban.pdf', width = 18, height = 10)
# 
# plot_temp <- create_individual_plot(all_input, "temp", expression("Temperature" ~ (C^o)), "Temperature (C^o)", unitsname)
# ggsave('figs/SI_figureS1 plot_temp.pdf', width = 18, height = 10)
# 
# plot_rain <- create_individual_plot(all_input, "rain", "Rain (mm)", "Rain (mm)", unitsname)
# ggsave('figs/SI_figureS1 plot_rain.pdf', width = 18, height = 10)
# 
# plot_rh <- create_individual_plot(all_input, "sh", "Relative Humidity (%)", "Relative Humidity (%)", unitsname) #all_input里面是sh名字
# ggsave('figs/SI_figureS1 plot_rh.pdf', width = 18, height = 10)

# LOAD MODEL --------------------------------------------------------------------

# fit <- readRDS("output/models/epi_sts_season_demographics_weather.rds")
fit <- readRDS("output/models/epi_sts_demographics_weather_season.rds")


# MAP WITH PREDICTIVE SCORES ---------------------------------------------------

fit_end <- fit$control$subset[length(fit$control$subset)]
tp <- c(fit_end, fit_end + 3)

# Perform one-step-ahead forecast
forecast <- oneStepAhead_hhh4lag(fit, tp = tp, type = "final")

# Compute log scores
fitScores <- colMeans(scores(forecast, which = "logs", individual = TRUE))
logs <- tibble(name = names(fitScores), score = as.numeric(fitScores))


# Calibration test for logs
calibrationTest(fit, which = "logs")

# Prepare calibration test results
calib_test <- tibble(name = colnames(forecast$observed), z = NA, pvalue = NA)
size <- as.numeric(unique(exp(forecast$psi)))

for (i in 1:ncol(forecast$observed)) {
  test <- calibrationTest(x = forecast$observed[, i], mu = forecast$pred[, i], size = size, which = "logs")
  calib_test$z[i] <- test$statistic
  calib_test$pvalue[i] <- test$p.value
}

# Overall calibration test
calibrationTest(x = forecast$observed, mu = forecast$pred, size = size, which = "logs")

# MAP VISUALIZATION ------------------------------------------------------------

# Load shapefile
africa <- st_read("data/processed/geodata/africa.gpkg")

# Join calibration test results with shapefile data
africa <- africa %>% left_join(calib_test, by = c("country" = "name"))

# Define breaks for p-value visualization
breaks <- c(0, 0.01, 0.05, 0.10, 1)

# # Create map function for plotting
# create_map <- function(shape, x, palette, labs, breaks, digits, legend_title, panel_title) {
#   map(x = x, shape = shape, palette = palette, labs = labs, breaks = breaks, digits = digits, 
#       legend_title = legend_title, panel_title = panel_title)
# }
# 
# # Create and display the map
# pv_map <- create_map(africa, "pvalue", "RdBu", c("< 0.01", "0.01 - 0.05", "0.05 - 0.10", "> 0.10"), 
#                      breaks, 2, "P-value", "Calibration Test")
# 
# # Save the map
# tmap_save(pv_map, "figs/figureS13 map calibration.pdf", width = 7, height = 8)
# 
# 
# 
# 
# 
# forecast <- oneStepAhead_hhh4lag(fit, tp = tp, type = "final")
# # fitScores <- colMeans(scores(forecast, which = "rps", individual = T))
# fitScores <- colMeans(scores(forecast, which = "logs", individual = T))
# logs <- tibble(name = names(fitScores), score = as.numeric(fitScores))
# 
# fit$control$ar$lag <- 1
# fit$control$ne$lag <- 1
# # calibrationTest(fit, which = "rps")
# calibrationTest(fit, which = "logs")
# 
# calib_test <- tibble(name = colnames(forecast$observed),
#                      z = NA,
#                      pvalue = NA)
# size <- as.numeric(unique(exp(forecast$psi)))
# for (i in 1:ncol(forecast$observed)) {
#   test <- x <- calibrationTest(x = forecast$observed[, i], 
#                                mu = forecast$pred[, i], 
#                                # size = size, which = "rps")
#                                size = size, which = "logs")
#   calib_test$z[i] <- test$statistic
#   calib_test$pvalue[i] <- test$p.value
# }
# 
# calibrationTest(x = forecast$observed, 
#                 mu = forecast$pred, 
#                 size = size, which = "logs")
# 
# # calibrationTest(x = forecast$observed,
# #                 mu = forecast$pred,
# #                 size = size, which = "rps")
# 
# africa <- st_read("data/processed/geodata/africa.gpkg") 
# africa <- africa %>% 
#   left_join(calib_test, by = c("country" = "name"))
# 
# breaks <- c(0, 0.01, 0.05, 0.10, 1)
# map(x = "pvalue", shape = africa, 
#               palette = "RdBu",  #-Spectral
#               labs = c("< 0.01", "0.01 - 0.05", "0.05 - 0.10", "> 0.10"),
#               breaks = breaks, digits = 2, legend_title = "P-value", 
#               panel_title = "Calibration Test")
# 
# # tmap_save(pv_map, "figs/figureS13_calibration.pdf", width = 7, height = 8)




# Term contributions plot ------------------------------------------------------

nterms <- terms(fit)$nGroups
coefs <- coef(fit)[1:nterms]

# startday <- as.Date("2016-01-03")

# all_input_daily <- readRDS("week/data/processed/all_input_week.rds") %>% ungroup() %>%
#   dplyr::filter(date >= startday)


startday <- all_input_daily$date[1]
final_dates <- as.character(unique(all_input_daily$date));

# Define the function to calculate contributions
calculate_contributions <- function(var_name, var_label, data, coefs, se, type, start_date) {
  var_low <- coefs[var_name] - 1.96 * se[var_name]
  var_up <- coefs[var_name] + 1.96 * se[var_name]
  
  low95 <- (var_low * data) %>% 
    tidymat(type = type, var = var_label) %>%
    dplyr::rename(low95 = contrib)
  
  up95 <- (var_up * data) %>% 
    tidymat(type = type, var = var_label) %>%
    dplyr::rename(up95 = contrib)
  
  contrib <- (coefs[var_name] * data) %>% 
    tidymat(type = type, var = var_label) %>%
    left_join(low95, by = c("time", "type", "var", "country")) %>% 
    left_join(up95, by = c("time", "type", "var", "country"))
  
  return(contrib)
}

# Apply the function to different variables
vac_ar <- calculate_contributions("ar.vac", "Vaccination coverage", fit$control$data$vac, coefs, fit$se, "Demographics", "2016-08-01")
vac_ar$contrib <- ifelse(vac_ar$time <= as.Date("2016-08-01"),
                         vac_ar$contrib == NA, vac_ar$contrib)
vac_ar$low95 <- ifelse(vac_ar$time <= as.Date("2016-08-01"),
                       vac_ar$low95 == NULL, vac_ar$low95)
vac_ar$up95 <- ifelse(vac_ar$time <= as.Date("2016-08-01"),
                      vac_ar$up95 == NULL, vac_ar$up95)

cpop_ar <- calculate_contributions("ar.log(cpopulation)", "Child population", log(fit$control$data$cpopulation), coefs, fit$se, "Demographics", "2016-08-01")
gdppercap_ar <- calculate_contributions("ar.log(gdppercap)", "GDP per capital", log(fit$control$data$gdppercap), coefs, fit$se, "Demographics", "2016-08-01")
Rain_ar <- calculate_contributions("ar.rain_mean_sd", "Rain", fit$control$data$rain_mean_sd, coefs, fit$se, "Weather factors", "2016-08-01")
Temp_ar <- calculate_contributions("ar.temp_mean_sd", "Temperature", fit$control$data$temp_mean_sd, coefs, fit$se, "Weather factors", "2016-08-01")
Rh_ar <- calculate_contributions("ar.rh_mean_sd", "Relative humidity", fit$control$data$rh_mean_sd, coefs, fit$se, "Weather factors", "2016-08-01")


vac_ne <- calculate_contributions("ne.vac", "Vaccination coverage", fit$control$data$vac, coefs, fit$se, "Demographics", "2016-08-01")
vac_ne$contrib <- ifelse(vac_ne$time <= as.Date("2016-08-01"),
                         vac_ne$contrib == NA, vac_ne$contrib)
vac_ne$low95 <- ifelse(vac_ne$time <= as.Date("2016-08-01"),
                       vac_ne$low95 == NULL, vac_ne$low95)
vac_ne$up95 <- ifelse(vac_ne$time <= as.Date("2016-08-01"),
                      vac_ne$up95 == NULL, vac_ne$up95)

cpop_ne <- calculate_contributions("ne.log(cpopulation)", "Child population", log(fit$control$data$cpopulation), coefs, fit$se, "Demographics", "2016-08-01")
gdppercap_ne <- calculate_contributions("ne.log(gdppercap)", "GDP per capital", log(fit$control$data$gdppercap), coefs, fit$se, "Demographics", "2016-08-01")
Rain_ne <- calculate_contributions("ne.rain_mean_sd", "Rain", fit$control$data$rain_mean_sd, coefs, fit$se, "Weather factors", "2016-08-01")
Temp_ne <- calculate_contributions("ne.temp_mean_sd", "Temperature", fit$control$data$temp_mean_sd, coefs, fit$se, "Weather factors", "2016-08-01")
Rh_ne <- calculate_contributions("ne.rh_mean_sd", "Relative humidity", fit$control$data$rh_mean_sd, coefs, fit$se, "Weather factors", "2016-08-01")

vac_en <- calculate_contributions("end.vac", "Vaccination coverage", fit$control$data$vac, coefs, fit$se, "Demographics", "2016-08-01")
vac_en$contrib <- ifelse(vac_en$time <= as.Date("2016-08-01"),
                         vac_en$contrib == "", vac_en$contrib)
vac_en$low95 <- ifelse(vac_en$time <= as.Date("2016-08-01"),
                       vac_en$low95 == "", vac_en$low95)
vac_en$up95 <- ifelse(vac_en$time <= as.Date("2016-08-01"),
                      vac_en$up95 == "", vac_en$up95)

cpop_en <- calculate_contributions("end.log(cpopulation)", "Child population", log(fit$control$data$cpopulation), coefs, fit$se, "Demographics", "2016-08-01")
gdppercap_en <- calculate_contributions("end.log(gdppercap)", "GDP per capital", log(fit$control$data$gdppercap), coefs, fit$se, "Demographics", "2016-08-01")
Rain_en <- calculate_contributions("end.rain_mean_sd", "Rain", fit$control$data$rain_mean_sd, coefs, fit$se, "Weather factors", "2016-08-01")
Temp_en <- calculate_contributions("end.temp_mean_sd", "Temperature", fit$control$data$temp_mean_sd, coefs, fit$se, "Weather factors", "2016-08-01")
Rh_en <- calculate_contributions("end.rh_mean_sd", "Relative humidity", fit$control$data$rh_mean_sd, coefs, fit$se, "Weather factors", "2016-08-01")

# Combine contributions
contrib_ar <- bind_rows(cpop_ar, gdppercap_ar, Rain_ar, Temp_ar, Rh_ar) %>% dplyr::mutate(low95 = NA, up95 = NA) %>%
  bind_rows(vac_ar)
contrib_ar$contrib <- replace(contrib_ar$contrib, is.infinite(contrib_ar$contrib), NA)
# summary(contrib_ar)

contrib_ne <- bind_rows(cpop_ne, gdppercap_ne, Rain_ne, Temp_ne, Rh_ne) %>% dplyr::mutate(low95 = NA, up95 = NA) %>%
  bind_rows(vac_ne)
contrib_ne$contrib <- replace(contrib_ne$contrib, is.infinite(contrib_ne$contrib), NA)
# summary(contrib_ne)

contrib_en <- bind_rows(cpop_en, gdppercap_en, Rain_en, Temp_en, Rh_en) %>% dplyr::mutate(low95 = NA, up95 = NA) %>%
  bind_rows(vac_en)
contrib_en$contrib <- replace(contrib_en$contrib, is.infinite(contrib_en$contrib), NA)
# summary(contrib_en)

# Generate legends for plots
forlegend1 <- expand.grid(time = as.Date("2016-01-03"), 
                          type = "Demographics",
                          var = unique(contrib_ar$var[contrib_ar$type == "Weather factors"]),
                          country = unique(contrib_ar$country),
                          contrib = NA) %>% as_tibble()

forlegend2 <- expand.grid(time = as.Date("2016-01-03"), 
                          type = "Weather factors",
                          var = unique(contrib_ne$var[contrib_ne$type == "Demographics"]),
                          country = unique(contrib_ne$country),
                          contrib = NA) %>% as_tibble()

# plot 4 cities with the highest reported case incidence
unitsname <- unitsname_rpk

# plot
color4ci <- "lightpink"
A1_vac <- contrib_ar %>%
  dplyr::filter(country %in% unitsname, 
                time >= "2016-01-01") %>%
  ggplot(aes(x = time, y = contrib, color = var)) +
  # geom_vline(xintercept = as.Date("2016-08-01"), colour = "grey", size = 0.5) +  # 在x=0处添加一条灰色的粗竖线
  geom_line(aes(col = var)) +
  # geom_text(x = as.Date("2016-07-01"), y = -0.2,
  #           label = "Introduction of HFMD vaccines", color = "grey",
  #           size = 3, angle = 0, vjust = 0.5) +
  geom_ribbon(aes(ymin = low95, ymax = up95,
                  xmin = as.Date("2016-07-01"), xmax = as.Date("2019-12-29")),
              fill = color4ci, alpha = .3) +
  facet_grid(country ~ type, scales = "free_y") +
  labs(col = "", x = "",
       y = expression("Contribution to"~log(lambda[it]))) +
  # colorblindr::scale_color_OkabeIto(use_black = T) +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y",
               limits = c(as.Date("2016-01-01"), as.Date("2019-12-29")),
               expand = c(0, 1)) +
  theme_bw() +
  theme(legend.position = "bottom",
        strip.background.y = element_blank(),
        strip.text.y = element_blank())


B1_vac <- contrib_ne %>%
  bind_rows(forlegend1) %>%
  dplyr::filter(country %in% unitsname, 
                time >= "2016-01-01") %>%
  ggplot(aes(x = time, y = contrib, color = var)) +
  # geom_ribbon(aes(ymin = low95, ymax = up95, fill = type), alpha = .3) +
  geom_line(aes(col = var)) +
  geom_ribbon(aes(ymin = low95, ymax = up95,
                  xmin = as.Date("2016-07-01"), xmax = as.Date("2019-12-29")),
              fill = color4ci, alpha = .3) +
  facet_grid(country ~ type, scales = "free_y") +
  labs(col = "", x = "",
       y = expression("Contribution to"~log(phi[it]))) +
  # colorblindr::scale_color_OkabeIto(use_black = T) +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y",
               limits = c(as.Date("2016-01-01"), as.Date("2019-12-29")),
               expand = c(0, 1)) +
  theme_bw() +
  theme(legend.position = "bottom",
        strip.background.y = element_blank(),
        strip.text.y = element_blank())

C1_vac <- contrib_en %>%
  # na.omit() %>%
  # bind_rows(forlegend1) %>%
  dplyr::filter(country %in% unitsname, #type == type_vac,
                time >= "2016-01-01") %>%
  # ggplot(aes(x = time, y = contrib)) +
  ggplot(aes(x = time, y = contrib, color = var)) +
  geom_line(aes(col = var)) +
  geom_ribbon(aes(ymin = low95, ymax = up95,
                  xmin = as.Date("2016-07-01"), xmax = as.Date("2019-12-29")),
              fill = color4ci, alpha = .3) +
  facet_grid(country ~ type, scales = "free_y") +
  labs(col = "", x = "",
       y = expression("Contribution to"~log(nu[it]))) +
  # colorblindr::scale_color_OkabeIto(use_black = T) +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y",
               limits = c(as.Date("2016-01-01"), as.Date("2019-12-29")),
               expand = c(0, 1)) +
  theme_bw() +
  theme(legend.position = "bottom",
        strip.background.y = element_blank(),
        strip.text.y = element_blank())

#ALL Term contribution: all variable aggregated in a graph #
ggpubr::ggarrange(A1_vac, B1_vac, C1_vac, labels = c("A", "B", "C"),  #
                  widths = c(1,1,1), nrow = 1,
                  common.legend = T)

ggsave('figs/figure6A all term contribution.pdf',width = 10, height = 7)



# ar_CI <- contrib_ar %>% dplyr::filter(var == "Vaccination coverage") %>%
#   dplyr::select(time, type, country, "Vaccination Coverage low 95% CI" = low95,
#                 "Vaccination Coverage UP 95% CI" = up95) %>%
#   tidyr::gather(key = var, value = contrib, -time, -type,  -country ) %>%
#   dplyr::select(time, type, var, country, contrib)
# 
# 
# contrib_ar1 <- contrib_ar %>% dplyr::select(time, type, var, country, contrib) %>%
#   rbind(ar_CI)# %>%filter(var =="Vaccination Coverage low 95% CI" )



####分 social economical & weather ####
color4ci <- "lightblue1"
A1 <- contrib_ar %>% 
  dplyr::filter(country %in% unitsname,
                type == "Demographics", time >= "2016-01-01") %>% 
  dplyr::mutate(type = "Demographics",
                var = ifelse(var == "Vaccination coverage",
                             "Vaccination coverage (with 95%CI)", var)) %>%
  ggplot(aes(x = time, y = contrib, color = var)) +
  # geom_vline(xintercept = as.Date("2016-08-01"), colour = "grey", size = 0.5) +  # 在x=0处添加一条灰色的粗竖线
    geom_line(aes(col = var)) +
  geom_text(x = as.Date("2016-07-01"), y = -0.2,
            label = "Introduction of HFMD vaccines", color = "grey",
            size = 3, angle = 0, vjust = 0.5) +
  geom_ribbon(aes(ymin = low95, ymax = up95,
                  xmin = as.Date("2016-07-01"), xmax = as.Date("2019-12-29")),
              fill = color4ci, alpha = .7) +
  facet_grid(country ~ type, scales = "free_y") +
  labs(col = "", x = "",
       y = expression("Within-city transmission: contribution to"~log(lambda[it]))) +
  # colorblindr::scale_color_OkabeIto(use_black = T) +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y",
               limits = c(as.Date("2016-01-01"), as.Date("2019-12-29")),
               expand = c(0, 1)) +
  theme_bw() +
  theme(legend.position = "bottom",
        strip.background.y = element_blank(),
        strip.text.y = element_blank())

A2 <- contrib_ar %>% 
  dplyr::filter(country %in% unitsname,
                type == "Weather factors", time >= "2016-01-01") %>% 
  ggplot(aes(x = time, y = contrib, color = var)) +
  geom_line(aes(col = var)) +
  geom_ribbon(aes(ymin = low95, ymax = up95,
                  xmin = as.Date("2016-07-01"), xmax = as.Date("2019-12-29")),
              fill = color4ci, alpha = .7) +
  facet_grid(country ~ type, scales = "free_y") +
  labs(col = "", x = "",
       y = expression("Within-city transmission: contribution to"~log(lambda[it]))) +
  # colorblindr::scale_color_OkabeIto(use_black = T) +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y",
               limits = c(as.Date("2016-01-01"), as.Date("2019-12-29")),
               expand = c(0, 1)) +
  theme_bw() +
  # theme(legend.position = "bottom")
  theme(legend.position = "bottom",
        strip.background.y = element_blank(),
        strip.text.y = element_blank())

B1 <- contrib_ne %>% 
  dplyr::filter(country %in% unitsname,
                type == "Demographics", time >= "2016-01-01") %>% 
  dplyr::mutate(type = "Demographics",
                var = ifelse(var == "Vaccination coverage",
                             "Vaccination coverage (with 95%CI)", var)) %>%
  ggplot(aes(x = time, y = contrib, color = var)) +
  geom_line(aes(col = var)) +
  geom_ribbon(aes(ymin = low95, ymax = up95,
                  xmin = as.Date("2016-07-01"), xmax = as.Date("2019-12-29")),
              fill = color4ci, alpha = .7) +
  facet_grid(country ~ type, scales = "free_y") +
  labs(col = "", x = "", 
       y = expression("Between-city transmission: contribution to"~log(phi[it]))) +
  # colorblindr::scale_color_OkabeIto(use_black = T) +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y",
               limits = c(as.Date("2016-01-01"), as.Date("2019-12-29")),
               expand = c(0, 1)) +
  theme_bw() +
  theme(legend.position = "bottom",
        strip.background.y = element_blank(),
        strip.text.y = element_blank())

B2 <- contrib_ne %>% 
  dplyr::filter(country %in% unitsname,
                type == "Weather factors", time >= "2016-01-01") %>% 
  ggplot(aes(x = time, y = contrib, color = var)) +
  geom_line(aes(col = var)) +
  geom_ribbon(aes(ymin = low95, ymax = up95,
                  xmin = as.Date("2016-07-01"), xmax = as.Date("2019-12-29")),
              fill = color4ci, alpha = .7) +
  facet_grid(country ~ type, scales = "free_y") +
  labs(col = "", x = "",
       y = expression("Between-city transmission: contribution to"~log(phi[it]))) +
  # colorblindr::scale_color_OkabeIto(use_black = T) +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y",
               limits = c(as.Date("2016-01-01"), as.Date("2019-12-29")),
               expand = c(0, 1)) +
  theme_bw() +
  # theme(legend.position = "bottom")
  theme(legend.position = "bottom",
        strip.background.y = element_blank(),
        strip.text.y = element_blank())



C1 <- contrib_en %>% 
  dplyr::filter(country %in% unitsname,
                type == "Demographics", time >= "2016-01-01") %>% 
  dplyr::mutate(type = "Demographics",
                var = ifelse(var == "Vaccination coverage",
                             "Vaccination coverage (with 95%CI)", var)) %>%
  ggplot(aes(x = time, y = contrib, color = var)) +
  geom_line(aes(col = var)) +
  geom_ribbon(aes(ymin = low95, ymax = up95,
                  xmin = as.Date("2016-07-01"), xmax = as.Date("2019-12-29")),
              fill = color4ci, alpha = .7) +
  facet_grid(country ~ type, scales = "free_y") +
  labs(col = "", x = "", 
       y = expression("Endemic component: contribution to"~log(nu[it]))) +
  # colorblindr::scale_color_OkabeIto(use_black = T) +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y",
               limits = c(as.Date("2016-01-01"), as.Date("2019-12-29")),
               expand = c(0, 1)) +
  theme_bw() +
  theme(legend.position = "bottom",
        strip.background.y = element_blank(),
        strip.text.y = element_blank())
  theme(legend.position = "bottom")

C2 <- contrib_en %>% 
  dplyr::filter(country %in% unitsname,
                type == "Weather factors", time >= "2016-01-01") %>% 
  ggplot(aes(x = time, y = contrib, color = var)) +
  geom_line(aes(col = var)) +
  geom_ribbon(aes(ymin = low95, ymax = up95,
                  xmin = as.Date("2016-07-01"), xmax = as.Date("2019-12-29")),
              fill = color4ci, alpha = .7) +
  facet_grid(country ~ type, scales = "free_y") +
  labs(col = "", x = "",
       y = expression("Endemic component: contribution to"~log(nu[it]))) +
  # colorblindr::scale_color_OkabeIto(use_black = T) +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y",
               limits = c(as.Date("2016-01-01"), as.Date("2019-12-29")),
               expand = c(0, 1)) +
  theme_bw() +
  # theme(legend.position = "bottom")
  theme(legend.position = "bottom",
        strip.background.y = element_blank(),
        strip.text.y = element_blank())



## demographic impact ##
ggpubr::ggarrange(A1, B1, C1, labels = c("A",  "B",  "C"),
                  widths = c(1,1,1), nrow = 1,
                  common.legend = T)
ggsave('figs/figure6B demographics contribution.pdf',width = 10, height = 7)

## weather impact ##
ggpubr::ggarrange(A2,  B2,  C2, labels = c("A", "B",  "C"),
                  widths = c(1, 1, 1), nrow = 1,
                  common.legend = T)
ggsave('figs/figure6C weather contribution.pdf',width = 10, height = 7)

## demographic & weather impact ##
ggpubr::ggarrange(A1, A2, B1, B2, C1, C2, labels = c("A", "", "B", "", "C", ""),
                  widths = c(1, 1, 1), nrow = 1,
                  common.legend = T)
ggsave('figs/figure6D all term contribution.pdf',width = 10, height = 7)



# WEIGHTS ----------------------------------------------------------------------

# Gaode population mobility index高德指数均数矩阵

gaode_sum <- readRDS("data/processed/gaode_sum.rds")[-1] %>% as.matrix()
gaode_mean <- readRDS("data/processed/gaode_mean.rds")[-1] %>% as.matrix()

wmat <- gaode_mean %>% as_tibble() #gaode_sum #gaode_mean
colnames(gaode_mean)

wmat$from <-colnames(gaode_mean)


wmat <- wmat %>% 
  tidyr::gather(key = "to", value = "wji", -from) 

wmat$from <- factor(wmat$from, levels = sort(unique(wmat$from), decreasing = T))  

wmat$wji[wmat$wji == 0] <- NA
ggplot(wmat, aes(x = to, y = from)) + 
  geom_tile(aes(fill = wji), colour = "white") +
  scale_fill_viridis_c(direction = -1, na.value = "white") +
  scale_x_discrete(position = "top") +
  labs(x = "", y = "") + 
  coord_equal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 0))

ggsave('figs/SI_figure16A PowerLaw OD matrix map.pdf',width = 10, height = 7)


# PowerLaw estimated weights
wmat <- getNEweights(fit) %>% 
  as_tibble()
wmat$from <- names(wmat)
# rowSums(wmat)

wmat <- wmat %>% 
  tidyr::gather(key = "to", value = "wji", -from) 

wmat$from <- factor(wmat$from, levels = sort(unique(wmat$from), decreasing = T))  

wmat$wji[wmat$wji == 0] <- NA
ggplot(wmat, aes(x = to, y = from)) + 
  geom_tile(aes(fill = wji), colour = "white") +
  scale_fill_viridis_c(direction = -1, na.value = "white") +
  scale_x_discrete(position = "top") +
  labs(x = "", y = "") + 
  coord_equal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 0))

ggsave('figs/SI_figure16B PowerLaw OD matrix map.pdf',width = 10, height = 7)

# 
# rho <-  coef(fit)[["neweights.d"]]
# rho_low <- confint(fit)["neweights.d", 1]
# rho_up <- confint(fit)["neweights.d", 2]
# 
# maxlag <- 8
# what <- tibble(o = 1:maxlag, w = (1:maxlag) ^ -rho, wlow = (1:maxlag) ^ -rho_low, wup = (1:maxlag) ^ -rho_up)
# 
# ggplot(what, aes(x = o, y = w)) +
#   geom_ribbon(aes(ymin = wlow, ymax = wup), alpha = .2) +
#   geom_point() +
#   geom_line() +
#   scale_x_continuous(breaks = 1:maxlag) +
#   labs(x = "Adjacency order", y = "Non-normalized weights")
# 
# ggsave('figs/SI_figure16C Decay weights rho.pdf',width = 10, height = 7)
