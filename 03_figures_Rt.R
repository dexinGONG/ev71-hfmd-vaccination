
library(EpiEstim)
# set working directory
setwd("/Users/zhongjingxin/GONGDEXIN/GDCDC/HHH4-EV71")
# setwd("C:/Users/Huanjingshi/Documents/WPSDrive/16131756/WPS云盘/GDCDC/GDCDC2023/HHH4-EV71/")

names(all_input_daily)

all_input_daily <- readRDS("data/processed/all_input_daily.rds") %>% ungroup() %>%
  # dplyr::mutate(case = case / population, allhfmd = allhfmd/population) %>%
  dplyr::select(date, country, allhfmd, case) %>%
  dplyr::rename(nameE = country)

case_prov <- all_input_daily %>% group_by(date) %>% summarise(case = sum(case))

check_case_prov <- case_prov %>%
  filter(date >= as.Date("2019-01-01"), date <= as.Date("2019-04-01"))
View(check_case_prov)


plot_case_prov <- case_prov %>% 
  ggplot(aes(x = date, y = case)) + geom_line()
plot_case_prov

Rt_province <- EpiEstim::estimate_R(incid = case_prov$case,
                                    method = "parametric_si",
                                    config = make_config(list(
                                      mean_si = 3.7, 
                                      std_si = 2.6
                                      # t_start=2:(nrow(case_prov)-10),
                                      # t_end=(2+10):nrow(case_prov)
                                      )
                                      )
                                    )

#提取实时Rt和95%置信区
eR_province <- Rt_province$R$`Mean(R)`

data_province <- data.frame(R=Rt_province$R$`Mean(R)`,
                   RL=Rt_province$R$`Quantile.0.025(R)`,
                   RH=Rt_province$R$`Quantile.0.975(R)`,
                   date=seq.Date(from = as.Date(case_prov$date[nrow(case_prov)])-length(eR_province)+1,
                                 to = as.Date(case_prov$date[nrow(case_prov)]),by="day"))

# data_province <- rbind(data.frame(R=rep(NA,11),RL=rep(NA,11),RH=rep(NA,11),
#                          date=seq.Date(as.Date("2012-01-01"),as.Date("2012-01-11"),"day")),
#                        data_province)
# test_data_province <- data_province %>% filter(date >= as.Date("2019-01-01"), 
#                                                date <= as.Date("2019-04-01"))
# View(test_data_province)

Rt_prov <- data_province %>% 
  dplyr::filter(date >= as.Date("2012-01-01")) %>%
  ggplot(aes(x = date, y = R)) +
  geom_line() +
  # facet_wrap(~ country, scales = "free_y") +
  labs(x = "", y = "Rt") +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y",
               expand = c(0, 1)) +
  theme_bw(base_size = 12) + 
  geom_line(color = "blue") +  # Line for R_t
  geom_ribbon(aes(ymin = RL, ymax = RH), alpha = 0.2) +  # Shaded area for 95% CI
  labs(title = "Temporal Variation of Reproduction Number (Rt) with 95% CI",
       x = "Date",
       y = "Reproduction Number (Rt)")
Rt_prov

# Rt for 21 cities
for(i in seq(length(case_data))) {
  print(paste(cities$nameE[i],sep = "_"))
  
  # extract sub data
  sub <- case_data[[i]] %>% dplyr::arrange(date, nameE)
  
  # sub$case <- sub$allhfmd

  Rt <- EpiEstim::estimate_R(incid = sub$case,
                              method = "parametric_si",
                              config = make_config(list(
                                mean_si = 3.7, std_si = 2.6,
                                t_start=2:(nrow(sub)-10),
                                t_end=(2+10):nrow(sub))))
  
  #提取实时Rt和95%置信区
  eR <- Rt$R$`Mean(R)`
  
  data <- data.frame(R=Rt$R$`Mean(R)`,
                     RL=Rt$R$`Quantile.0.025(R)`,
                     RH=Rt$R$`Quantile.0.975(R)`,
                     date=seq.Date(from = as.Date(sub$date[nrow(sub)])-length(eR)+1,
                                   to = as.Date(sub$date[nrow(sub)]),by="day"))
  
  data <- rbind(data.frame(R=rep(NA,11),RL=rep(NA,11),RH=rep(NA,11),
                           date=seq.Date(as.Date("2012-01-01"),as.Date("2012-01-11"),"day")),
                data) %>% dplyr::mutate(country = paste(cities$nameE[i],sep = "_"))
  
  # # cumsum
  # sub$dose_cum <- apply(sub[, "dose", drop = FALSE], 2, cumsum)
  # 
  # # vac
  # sub$vac = sub$dose_cum / 2 / sub$echildren 
  # sub$vac_echildren_by_year = sub$dose_cum / 2 / sub$echildren_by_year 
  
  # save result
  result_list[[i]] <- data
  
}

Rt_all <- do.call(rbind, result_list)

Rt_all %>% 
  # dplyr::filter(date >= as.Date("2019-01-01")) %>%
  ggplot(aes(x = date, y = R)) +
  geom_line() +
  facet_wrap(~ country, scales = "free_y") +
  labs(x = "", y = "Rt") +
  scale_x_date(date_labels = "%y", expand = c(0, 0)) +
  theme_bw(base_size = 12)
# png(paste0('figs/Rt_all_', Sys.Date(), '.png'), width = 9, height = 5, units = 'in', res = 300)

