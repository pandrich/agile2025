
#The temperature using ERA5 spans 22 years from 2000-2022


# first we load the temp data
load(file = "/Data/Prepared climate/Neil_tempdata.RData")

#then create the heat indicators, by country and region

climate_all_temp <- d.temp_all %>%
  group_by(country, region) %>% 
  mutate(
    mean_temp_lr = mean(temp, na.rm = TRUE),
    sd_temp_lr = sd(temp, na.rm = TRUE)
  ) %>% 
  ungroup() %>% 
  mutate(
    sd_temp = (temp - mean_temp_lr) / sd_temp_lr #create a standardised longterm temp baseline
  ) %>%
  
  arrange(country, region, date) %>%
  
  # compute (daily) binary temperature indicators over the 22 years reference period
  mutate(
    temp_above1sd = ifelse(sd_temp > 1, 1L, 0L), 
    temp_above32 = ifelse(temp > 32, 1L, 0L), 
    temp_above36 = ifelse(temp > 36, 1L, 0L), 
    
  ) %>%
  # compute (monthly) values repeated on each daily row, regroup by year+month
  group_by(country, region, year, month) %>%

  summarise(
    days_above_1sd_month = sum(temp_above1sd, na.rm = TRUE ), 
    days_above_32_month = sum(temp_above32, na.rm = TRUE ), 
    days_above_36_month = sum(temp_above36, na.rm = TRUE ),

    max_temp_month = max(temp, na.rm = TRUE),
    .groups = "drop") %>%
  
  group_by(country, region) %>%
  mutate(
    sum_days_above_1sd_12m = roll_sumr(days_above_1sd_month, n = 12, na.rm = TRUE),
    sum_days_above_32_12m = roll_sumr(days_above_32_month, n = 12, na.rm = TRUE),
    sum_days_above_36_12m = roll_sumr(days_above_36_month, n = 12, na.rm = TRUE),

    mean_max_temp_12m = roll_meanr(max_temp_month, n = 12, na.rm = TRUE), #controls for seasonality and interview timing
    
    max_max_temp_12m = roll_maxr(max_temp_month, n = 12, na.rm = TRUE)) %>%
  ungroup() %>% 
  dplyr::select(
    -c(
      days_above_1sd_month,   #remove as unwanted
      days_above_32_month,
      days_above_36_month,
      max_temp_month
    )
  ) %>% 
  pivot_longer(
    cols = -c(country, region, year, month),
    names_to = "variable",
    values_to = "value"
  ) %>% 
  group_by(country, region, variable) %>% 
  mutate(
    mean_value = mean(value, na.rm = TRUE), #get the mean for each of the above indicators in "value"
    sd_value = sd(value, na.rm = TRUE),
  ) %>%
  ungroup() %>% 
  mutate(
    dev_value_mean = (value - mean_value) / mean_value, #this captures the proportional mean temperature
    dev_value_sd = (value - mean_value) / sd_value      #while this capture the standard deviation of temp from the norm (the 22 year reference period)
    
  ) %>% 
  pivot_wider(
    id_cols = c(country, region, year, month),
    names_from = "variable",
    values_from = c(value, dev_value_mean, dev_value_sd)
  )

#save it
save(climate_all_temp, file="Data/Prepared climate/temp complete.RData")

