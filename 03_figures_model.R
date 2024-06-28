
# LOAD REQUIRED PACKAGES AND FUNCTIONS -----------------------------------------
pkgs = c("surveillance", "dplyr", "sp", "sf", "hhh4addon","formattable", "dplyr", 
         "tidyr", "ggplot2", "hhh4addon") 
pacman::p_load(pkgs, character.only = T)

# set working directory
setwd("/Users/zhongjingxin/GONGDEXIN/GDCDC/HHH4-EV71") 

# LOAD DATA --------------------------------------------------------------------

# # Cases at country level
# startday <- as.Date("2016-01-03")
# end_day <- as.Date("2019-08-25")
# 
# all_input_daily <- readRDS("week/data/processed/all_input_week.rds") %>% ungroup() %>%
#   dplyr::filter(date >= startday)
# 
# counts <- all_input_daily %>% ungroup() %>% dplyr::select(date, country,  case) %>%
#   arrange(country,date) %>% tidyr::spread(key = country, value = case)

# Load final model 
fit <- readRDS("output/models/epi_sts_demographics_weather_season.rds")

# set model-fitting and inference period
inference_days <- fit$control$subset 
inference_dates <- counts$date[inference_days]
fit_start <- inference_days[1]
fit_end <- inference_days[length(inference_days)]

preddays <- 3 #7, 14
tp = c(fit_end, fit_end + preddays)

# read Shapefile 
africa <- st_read("data/processed/geodata/africa.gpkg") 

africa_full <- st_read("data/processed/geodata/africa.gpkg") %>%
  dplyr::rename(name = country) #names(africa_full)

# DATA PROCESSING --------------------------------------------------------------

cases_to_plot <- all_input_daily %>% dplyr::select(date, country, date, case) %>%
  rename(COUNTRY = country, time = date, observed = case)

colsums <- colSums(counts[-1])
colsums <- sort(colsums, decreasing = T)
units_case <- names(which(colsums > 1000)) %>% head(., 4)    #top 4 with highest hfmd
units_rpk <- c("Zhuhai", "Huizhou", "Shanwei", "Zhanjiang") #rpk
units_vac <- c("Guangzhou", "Shenzhen", "Zhaoqing", "Zhongshan") #vac


# Predicted mean
fit$terms <- terms(fit)
fitted <- meanHHH(theta = unname(fit$coefficients), model = fit$terms, 
                  subset = fit_start:fit_end, total.only = F)
fitted$inference_dates = inference_dates



# cbind(fitted$mean[1,1], fitted$epidemic[1,1], fitted$endemic[1,1], 
#       fitted$epi.own[1,1], fitted$epi.neighbours[1,1], 
#       fitted$ar.exppred[1,1], fitted$ne.exppred[1,1], fitted$end.exppred[1,1],
#       lubridate::as_date(fitted$inference_dates[1]))

# FIXED/RANDOM EFFECTS MAP 成分累积地图-----------------
# 内置模型时间序列图 -------------------------------------------------
par(mfrow = c(7, 3))
par(mar = c(1, 1, 1, 1))  # 调整边距
plot(fit, type = "fitted", units = colnames(fitted$mean),
     hide0s = TRUE, par.settings = NULL, legend = 1)
fitted_components <- plot(fit, type = "fitted", total = TRUE,
                          hide0s = TRUE, par.settings = NULL, legend = FALSE)

colSums(fitted_components$Overall)[3:5] / sum(fitted_components$Overall[,1])
epi.own.prop <- fitted_components$Overall[ ,c("epi.own")]/fitted_components$Overall[,1]
sd(epi.own.prop)
mean(epi.own.prop) 

mean_value <- mean(epi.own.prop)

# Calculate the 95% confidence interval
conf_interval <- mean_value + c(-1, 1) * qt(0.975, length(epi.own.prop) - 1) * sd(epi.own.prop) / sqrt(length(epi.own.prop))

# Output the results
mean_value
conf_interval


fitted_components$Overall



# CONTRIBUTION OVER TIME FOR SPECIFIC COUNTRIES作图：4 个城市 时空多成分模型时间序列图 --------------------------------
units <- units_rpk
  
ids <- match(units, colnames(fitted$mean))

results <- cbind(fitted$endemic[, ids[1]], fitted$epi.own[, ids[1]], fitted$epi.neighbours[, ids[1]]) %>% 
  rbind(cbind(fitted$endemic[, ids[2]], fitted$epi.own[, ids[2]], fitted$epi.neighbours[, ids[2]])) %>% 
  rbind(cbind(fitted$endemic[, ids[3]], fitted$epi.own[, ids[3]], fitted$epi.neighbours[, ids[3]])) %>% 
  rbind(cbind(fitted$endemic[, ids[4]], fitted$epi.own[, ids[4]], fitted$epi.neighbours[, ids[4]]))

results <- as.data.frame(results)
names(results) <- c("Endemic component", "Within-city transmission", "Between-city transmission")
results$cname <- rep(units, each = length(inference_days))
results$COUNTRY <- rep(units, each = length(inference_days))
results$time <- inference_dates

results %>%
  inner_join(cases_to_plot, by = c("COUNTRY", "time")) %>% 
  select(-COUNTRY) %>% 
  tidyr::gather(key = "Contribution", value = "value", -cname, -time, -observed) %>% 
  mutate(Contribution = factor(Contribution, 
                               levels = c("Between-city transmission", "Within-city transmission", "Endemic component"))) %>% 
  ggplot(aes(x = time)) +
  geom_area(aes(y = value, fill = Contribution), alpha = 1) +
  # geom_line(aes(y = observed), linetype = 1, size = .1, alpha = 0.6) +
  geom_point(aes(y = observed), size = 0.6, alpha = 0.6) +
  facet_wrap(~ cname, scales = "free_y", ncol = 1) +
  scale_fill_manual(values = c("orange","#91D1C2B2",  "red1"),
                    guide = guide_legend(title.position = "top",
                                         title.hjust = 0.5, 
                                         label.hjust = 0.5,
                                         title.theme = element_text(size = 9),
                                         label.theme = element_text(size = 9))) +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y", 
               limits = c(as.Date("2016-01-01"), as.Date("2019-12-29")),
               expand = c(0, 1)) +
  scale_y_continuous(expand = c(0.01, 0)) +
  labs(x = "", y = "Reported cases", fill = "") +
  theme_bw(base_size = 14) +
  theme(strip.text = element_text(face = "bold"),
      legend.position = "top", 
      legend.key=element_blank(),
      legend.text = element_text(size = 12),
      legend.key.size = unit(0.6, units = "cm"))

ggsave("figs/figure4A 4 city contrib.pdf", width = 6, height = 8, dpi = 320)



# CONTRIBUTION MAP -------------------------------------------------------------

# Model-components contributions over time
total <- apply(fitted$mean, 2, sum)
epidemic <- apply(fitted$epidemic, 2, sum)
sort(epidemic/total)
within <- apply(fitted$epi.own, 2, sum)
between <- apply(fitted$epi.neighbours, 2, sum)
endemic <- apply(fitted$endemic, 2, sum)

africa$within_prop <- within / total 
africa$between_prop <- between / total 
africa$endemic_prop <- endemic / total 
summary(africa)


africa <- africa %>% dplyr::rename(name = country)
africa_full <- africa_full %>% 
  full_join(st_drop_geometry(africa[c("name", "within_prop", "between_prop",
                                      "endemic_prop")]))
# 
# source("R/functions.R")
# 
# within <- map(x = "within_prop", shape = africa_full, palette = "-RdYlBu",
#               breaks = seq(0, 1, l = 11),
#               digits = 1, legend_title = "Contribution",
#               panel_title = "Within city contribution")
# 
# between <- map(x = "between_prop", shape = africa_full, palette = "-RdYlBu",
#                breaks = seq(0, 1, l = 11),
#                digits = 1, legend_title = "Contribution",
#                panel_title = "Between city contribution")
# 
# endemic <- map(x = "endemic_prop", shape = africa_full, palette = "-RdYlBu",
#                breaks = seq(0, 1, l = 11),
#                digits = 1, legend_title = "Contribution",
#                panel_title = "Endemic contribution")
# 
# reff_map_add_on <- tmap_arrange(within, between, endemic, ncol = 3)  #add-on

# tmap_save(within, "figs/3成分累积地图.pdf", width = 7, height = 8)


# start_day <- startday + 7*4
# end_day <- "2019-08-25"
# 
# fit_start <- which(counts$date == start_day)  #5
# fit_end <- which(counts$date == end_day)  #191
# 
# TRAIN <- c(fit_start:fit_end)
# TEST <- (fit_end + 1): fit_end      #58:63#356:361 # from week 44 to week 49 in 2022
# 
# 
# inference_days <- fit$control$subset
# inference_dates <- counts$date[inference_days]
# model_start_day <- inference_days[1]
# model_end_day <- inference_days[length(inference_days)]
# # end_day <- inference_days[length(inference_days)]
# # code_to_name <- tibble(COUNTRY = units, cname = units_names)
# 
# 
# # Predicted mean
# fit$terms <- terms(fit)
# fitted <- meanHHH(theta = unname(fit$coefficients), model = fit$terms, 
#                   subset = model_start_day:model_end_day, total.only = F)
# fitted$inference_dates = inference_dates
# saveRDS(fitted,"output/models/fitted_mean.rds")



# PREDICTIONS FOR SPECIFIC COUNTRIES -------------------------------------------
pred <- oneStepAhead_hhh4lag(result = fit, tp = c(fit_start, fit_end ) - 1, 
                             type = "final")

quants <- hhh4addon:::quantile.oneStepAhead(pred, 
                                            probs = c(0.025, 0.25, 0.5, 0.75, 0.975))

units_id <- match(units, africa$name)

quants <- quants[ , units_id, ]
quants <- rbind(quants[ , , 1], quants[ , , 2], quants[ , , 3], quants[ , , 4],
                quants[ , , 5]) 
quants <- as_tibble(quants)   # dim(quants); 930/5;nrow(quants)
quants$quantile <- rep(c("low95", "low50", "median", "up50", "up95"),
                       each = length(inference_days))  #跟“2019-06-30”或“08-31”有关
quants$time <- rep(inference_dates, time = 5)

code_to_name <- tibble(COUNTRY = units, cname = units)

fitted_mean <- predict(fit) %>% 
  as_tibble() %>%
  select(units) %>%
  mutate(time = inference_dates) %>% 
  tidyr::gather(key = "COUNTRY", value = "mean", -time)
  
quants_insample <- quants %>% 
  tidyr::gather(key = "COUNTRY", value = "value", -quantile, -time) %>%
  tidyr::spread(key = quantile, value = value) %>% 
  inner_join(code_to_name) %>% 
  inner_join(cases_to_plot, by = c("COUNTRY", "time")) %>% 
  inner_join(fitted_mean, by = c("COUNTRY", "time")) %>% 
  select(-COUNTRY) 


nsims <- 10000
sims <- simulate(fit, y.start = observed(fit$stsObj)[(fit_end - fit$lags + 1 ):(fit_end), ], 
                 # 若有滞后，则应为fit$max_lag；这里无滞后；将最后一个值fit_end作为模拟值的起始值
                 subset = fit_end:nrow(fit$stsObj), 
                 nsim = nsims, simplify = F)

sims <- do.call(rbind, lapply(sims, function(x) observed(x)[ ,units_id]))
sims <- as_tibble(sims)

pred_start <- as.Date(counts$date[fit_end]) # 预测起始值
pred_end <- pred_start + (nrow(fit$stsObj) - fit_end -1 )*7 #预测最后一天"2019-12-29"；不选31号和29号距离2天；跨年日期
sims$time <- rep(c(counts$date[fit_end:(nrow(fit$stsObj)-1)],as.Date("2019-12-31")), #补回去31号
                 time = nsims)


sims <- sims %>% 
  tidyr::gather(key = "COUNTRY", value = "value", -time) %>%
  group_by(COUNTRY, time) %>% 
  summarise(low95 = quantile(value, p = 0.025),
            low50 = quantile(value, p = 0.25),
            mean = mean(value), 
            median = quantile(value, p = 0.5),
            up50 = quantile(value, p = 0.75),
            up95 = quantile(value, p = 0.975)) %>% 
  ungroup() %>% 
  inner_join(code_to_name) %>%  
  inner_join(cases_to_plot, by = c("COUNTRY", "time")) %>% 
  select(-COUNTRY) 

sims <- bind_rows(quants_insample[quants_insample$time == pred_start -1, ], sims)
quants_insample$type <- "IS"
sims$type <- "OUS"


all_preds <- quants_insample %>% 
  bind_rows(sims)


ggplot(all_preds, aes(x = time)) +
  geom_ribbon(aes(ymin = low95, ymax = up95, fill = type), alpha = .3) +
  geom_ribbon(aes(ymin = low50, ymax = up50, fill = type), alpha = .5) +
  geom_line(aes(y = mean, col = type), size = 1) +
  geom_point(aes(y = observed, shape = type), size = 1) + 
  facet_wrap(~ cname, scales = "free_y", ncol = 1) +
  labs(x = "", y = "Reported Cases") +
  scale_fill_manual("50% - 95% CI", values = c("#4DBBD5B2", "orange"),
                    labels = c("Retrospective", "Forecast")) +
  scale_color_manual("Predicted mean", values = c("#4DBBD5B2", "orange"),
                     labels = c("Retrospective", "Forecast")) +
  scale_shape_manual("Reported cases", values = c(19, 21), 
                     labels = c("Retrospective", "Forecast")) +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y", 
               limits = c(as.Date("2016-01-01"), as.Date("2019-12-29")),
               expand = c(0, 1)) +
  theme_gray(base_size = 21) +
  theme(strip.text = element_text(face = "bold"),
        legend.position = "right", legend.key=element_blank(),
        legend.text = element_text(size = 14),
        legend.key.size = unit(1, units = "cm")) +
  guides(color = guide_legend(override.aes = list(size = 2)),
         fill = guide_legend(override.aes = list(size = 2)),
         shape = guide_legend(override.aes = list(size = 2)))

ggsave("figs/figure5 4 city restro pred.pdf", width = 20, height = 10, dpi = 320) #模拟和预测的区别？



# CONTRIBUTION OVER TIME FOR ALL COUNTRIES -------------------------------------

units <- colnames(fitted$mean)
units_names <- units

results <- cbind(fitted$endemic[, 1], fitted$epi.own[, 1], fitted$epi.neighbours[, 1])

for (i in 2:length(units)) {
    results <-  rbind(results, 
                      cbind(fitted$endemic[, i], 
                            fitted$epi.own[, i], 
                            fitted$epi.neighbours[, i])) 
}


results <- as.data.frame(results)
names(results) <- c("Endemic component", "Within-city transmission", "Between-city transmission")
results$cname <- rep(units, each = length(fit$control$subset))
results$COUNTRY <- rep(units, each = length(fit$control$subset))
results$time <- inference_dates


results %>%
  inner_join(cases_to_plot, by = c("COUNTRY", "time")) %>% 
  select(-COUNTRY) %>% 
  tidyr::gather(key = "Contribution", value = "value", -cname, -time, -observed) %>% 
  mutate(Contribution = factor(Contribution, 
                               levels = c("Between-city transmission", "Within-city transmission", "Endemic component"))) %>% 
  ggplot(aes(x = time)) +
  geom_area(aes(y = value, fill = Contribution), alpha = 1) +
  # geom_line(aes(y = observed), linetype = 1, size = 1) +
  geom_point(aes(y = observed), size = 0.8, alpha = 0.6) +
  facet_wrap(~ cname, scales = "free_y", ncol = 3) +
  scale_fill_manual(values = c("orange","#91D1C2B2",  "red1"),
                    guide = guide_legend(title.position = "top",
                                         title.hjust = 0.5, #label.position = "top",
                                         label.hjust = 0.5,
                                         title.theme = element_text(size = 20),
                                         label.theme = element_text(size = 20))) +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y", 
               limits = c(as.Date("2016-01-01"), as.Date("2019-12-29")),
               expand = c(0, 1)) +
  scale_y_continuous(expand = c(0.01, 0)) +
  labs(x = "", y = "Estimated EV71 HFMD cases", fill = "") +
  theme_bw(base_size = 20) +
  theme(legend.position = "top", strip.text = element_text(face = "bold"))

ggsave("figs/figureS11 all cities contrib.pdf", width = 3*2 * 5, height = 14, dpi = 320)
ggsave("figs/figureS11 all cities contrib v1.pdf", width = 3 * 10, height = 7 *6, dpi = 320)




# PREDICTIONS FOR 4 COUNTRIES ------------------------------------------------
pred <- oneStepAhead_hhh4lag(result = fit, tp = c(fit_start, fit_end) - 1, 
                             type = "final")

quants <- hhh4addon:::quantile.oneStepAhead(pred, 
                                            probs = c(0.025, 0.25, 0.5, 0.75, 0.975))

units_id <- match(units, africa$name)

quants <- quants[ , units_id, ]
quants <- rbind(quants[ , , 1], quants[ , , 2], quants[ , , 3], quants[ , , 4],
                quants[ , , 5]) 
quants <- as_tibble(quants)
quants$quantile <- rep(c("low95", "low50", "median", "up50", "up95"),
                       each = length(inference_days))
quants$time <- rep(inference_dates, time = 5)

code_to_name <- tibble(COUNTRY = units, cname = units_names)

fitted_mean <- predict(fit) %>% 
  as_tibble() %>% 
  select(units) %>%
  mutate(time = inference_dates) %>%
  tidyr::gather(key = "COUNTRY", value = "mean", -time)


quants_insample <- quants %>% 
  tidyr::gather(key = "COUNTRY", value = "value", -quantile, -time) %>%
  tidyr::spread(key = quantile, value = value) %>% 
  inner_join(code_to_name) %>% 
  inner_join(cases_to_plot, by = c("COUNTRY", "time")) %>% 
  inner_join(fitted_mean, by = c("COUNTRY", "time")) %>% 
  select(-COUNTRY) 

nsims <- 10000
sims <- simulate(fit, y.start = observed(fit$stsObj)[(fit_end - fit$lags + 1):fit_end, ], 
                 # 若有滞后，则应为fit$max_lag；这里无滞后；将最后一个值fit_end作为模拟值的起始值
                 subset = fit_end:nrow(fit$stsObj), 
                 nsim = nsims, simplify = F) 
sims <- do.call(rbind, lapply(sims, function(x) observed(x)[ ,units_id]))
sims <- as_tibble(sims)

pred_start <- as.Date(counts$date[fit_end]) # 预测起始值
pred_end <- pred_start + (nrow(fit$stsObj) - fit_end -1)*7 #预测最后一天"2019-12-29"；不选31号和29号距离2天；跨年日期
sims$time <- rep(c(counts$date[fit_end:(nrow(fit$stsObj)-1)],as.Date("2019-12-31")), #补回去31号
                 time = nsims)

sims <- sims %>% 
  tidyr::gather(key = "COUNTRY", value = "value", -time) %>%
  group_by(COUNTRY, time) %>% 
  summarise(low95 = quantile(value, p = 0.025),
            low50 = quantile(value, p = 0.25),
            mean = mean(value), 
            median = quantile(value, p = 0.5),
            up50 = quantile(value, p = 0.75),
            up95 = quantile(value, p = 0.975)) %>% 
  ungroup() %>% 
  inner_join(code_to_name) %>%  
  inner_join(cases_to_plot, by = c("COUNTRY", "time")) %>% 
  select(-COUNTRY) 

ggplot(sims, aes(x = time)) +
  geom_ribbon(aes(ymin = low95, ymax = up95), fill = "orange", alpha = .3) +
  geom_ribbon(aes(ymin = low50, ymax = up50), fill = "orange", alpha = .5) +
  geom_line(aes(y = mean), col = "orange", size = 1, linetype = 1) +
  geom_point(aes(y = observed)) + 
  facet_wrap(~ cname, scales = "free_y", ncol = 3) +
  scale_x_date(date_breaks = "1 month", date_labels = "%Y %b",
             limits = c(as.Date("2019-08-25"), as.Date("2019-12-31")),
             expand = c(0, 1),
             labels = c("August to December")) +
  scale_y_continuous(expand = c(0.01, 0)) +
  labs(x = "", y = "Reported cases") +
  theme_bw(base_size = 20) +
  theme(legend.position = "top", strip.text = element_text(face = "bold"))

ggsave("figs/figureS11 all cities 2019 sims.pdf", width = 6 * 5, height = 14, dpi = 320)
ggsave("figs/figureS11 all cities 2019 sims.pdf", width = 3 * 10, height = 7 *6, dpi = 320)

sims <- bind_rows(quants_insample[quants_insample$time == pred_start -1*7, ], sims)
quants_insample$type <- "IS"
sims$type <- "OUS"



all_preds <- quants_insample %>% 
  bind_rows(sims)

unique(all_preds$type)

ggplot(all_preds, aes(x = time)) +
  geom_ribbon(aes(ymin = low95, ymax = up95, fill = type), alpha = .3) +
  geom_ribbon(aes(ymin = low50, ymax = up50, fill = type), alpha = .5) +
  geom_line(aes(y = mean, col = type), size = 1) +
  geom_point(aes(y = observed, shape = type), size = 0.6) + 
  facet_wrap(~ cname, scales = "free_y", ncol = 3) +
  labs(x = "", y = "Reported Cases") +
  scale_fill_manual("50% - 95% CI", values = c("#4DBBD5B2", "orange"),
                    labels = c("Retrospective", "Forecast")) +
  scale_color_manual("Predicted mean", values = c("#4DBBD5B2", "orange"),
                     labels = c("Retrospective", "Forecast")) +
  scale_shape_manual("Reported cases", values = c(19, 21), 
                     labels = c("Retrospective", "Forecast")) +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y", 
               limits = c(as.Date("2016-01-01"), as.Date("2019-12-29")),
               expand = c(0, 1)) +
  theme_gray(base_size = 21) +
  theme(strip.text = element_text(face = "bold"),
        legend.position = "right", legend.key=element_blank(),
        legend.text = element_text(size = 21),
        legend.key.size = unit(1, units = "cm")) +
  guides(color = guide_legend(override.aes = list(size = 4)),
         fill = guide_legend(override.aes = list(size =4)),
         shape = guide_legend(override.aes = list(size = 4)))

ggsave("figs/figureS12 all cities restro pred.pdf", width = 6 * 5, height = 14, dpi = 320)

