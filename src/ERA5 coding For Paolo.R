#----- ERA5 Heat reanalysis project -----#
#-----The association between heat exposure and sexual violence in the past year -----#

#-24th of November 2025-#

#libraries:
library(dplyr)
library(lubridate)
library(ncdf4)
library(raster)

#----uploading the heat data

# loading climate data
temp_data <- brick("/Users/bothainaeltigani/Dropbox/2023_Violence against children/Data/Prepared climate/era5_t2m_max_2000-2022.nc", varname = "t2m_max") 
print(temp_data)

#ensure crs from temp_data matches crs in sfdf
#sfdf contains shapefiles of: Lesotho, Namibia, Zimbabwe, Mozambique and Zambia
st_crs(sfdf) <- st_crs(temp_data)
sfdf <- st_transform(sfdf, st_crs(temp_data))

#extract spatial variables and temperature data:
cru_temp_extract_all <- exact_extract(
  x = temp_data,
  y = sfdf, #this is the shapefiles
  fun = "mean", #weighted mean
  weights = "area",
  max_cells_in_memory = 366819840,
  append_cols = c("country","region", "REG_ID") #whatever you need from y
)

# reshaping data into long format
library(stringr)
d.temp_all <- cru_temp_extract_all %>%
  pivot_longer(
    cols = matches("^mean\\.X"),      # picks all mean.XYYYY.MM.DD columns
    names_to = "date_string",
    values_to = "tempK"
  ) %>%
  mutate(
    # extract the YYYY.MM.DD pattern robustly
    date_parsed = str_extract(date_string, "\\d{4}\\.\\d{2}\\.\\d{2}"),
    date = as.Date(date_parsed, format = "%Y.%m.%d"),
    year = as.integer(format(date, "%Y")),
    month = as.integer(format(date, "%m")),
    day = as.integer(format(date, "%d")),
    temp = tempK - 273.15 #convert from Kelvin to Celsius
  )

# summary of climate indicator
summary(d.temp_all)

# save file
save(d.temp_all, file = "/Users/bothainaeltigani/Dropbox/2023_Violence against children/Data/Prepared climate/Neil_tempdata.RData")

#--------------Preparing Heat indicators --------#

# temp data
load(file = "/Users/bothainaeltigani/Dropbox/2023_Violence against children/Data/Prepared climate/Neil_tempdata.RData")


climate_all_temp <- d.temp_all %>%
  group_by(country, region) %>% 
  mutate(
    mean_temp_lr = mean(temp, na.rm = TRUE),
    sd_temp_lr = sd(temp, na.rm = TRUE)
  ) %>% 
  ungroup() %>% 
  mutate(
    sd_temp = (temp - mean_temp_lr) / sd_temp_lr
  ) %>%
  #group by country region
  #sd mean
  #create new mutate() standardised temp variable ( temp - mean/ sd)
  arrange(country, region, date) %>%
 
   # compute daily binary and rolling (needs cross-year continuity)
  mutate(
    temp_above1sd = ifelse(sd_temp > 1, 1L, 0L), #this might have to be before the grouping
    temp_above32 = ifelse(temp > 32, 1L, 0L), #this might have to be before the grouping
    temp_above36 = ifelse(temp > 36, 1L, 0L), #this might have to be before the grouping
    
      ) %>%
  # I want monthly values repeated on each daily row, regroup by year+month
  group_by(country, region, year, month) %>%
  # mutate(
  #   days_above32_past365 = roll_sumr(temp_above32, n = 12, na.rm = TRUE),   #how many days are (>32) in the past year
  #   prop_above32_past365 = roll_meanr(temp_above32, n = 12, na.rm = TRUE),  #what proportion of year is days (>32) from 0-1
  #   
  #   monthly_avg_days_above32_past365 = mean(days_above32_past365, na.rm = TRUE),
  #   monthly_avg_prop_above32_past365 = mean(prop_above32_past365, na.rm = TRUE)
  summarise(
    days_above_1sd_month = sum(temp_above1sd, na.rm = TRUE ), #creating a per month variable
    days_above_32_month = sum(temp_above32, na.rm = TRUE ), #creating a per month variable
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
      days_above_1sd_month,
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
    mean_value = mean(value, na.rm = TRUE),
  ) %>%
  ungroup() %>% 
  mutate(
  dev_value = (value - mean_value) / mean_value
  ) %>% 
  pivot_wider(
    id_cols = c(country, region, year, month),
    names_from = "variable",
    values_from = c(value, dev_value)
  )
summary(climate_all_temp)

#save it
save(climate_all_temp, file="Data/Prepared climate/temp ERA5 complete.RData")

