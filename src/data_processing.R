build_spei <- function(rast_spei, adm_shapes, start_date) {
  print(paste0("Starting SPEI data processing"))
  rast_times <- terra::time(rast_spei)
  ids <- which(rast_times >= start_date)
  rast_spei <- rast_spei %>% terra::subset(ids)
  dates <- as.character(terra::time(rast_spei))
  df_spei <- (
    rast_spei
    %>% exactextractr::exact_extract(
      adm_shapes,
      fun = "weighted_mean",
      weights = "area",
      max_cells_in_memory = 176067000,
      append_cols = c("country", "adm_code", "adm_name"),
      progress = FALSE
    )
  )
  colnames(df_spei) <- c("country", "adm_code", "adm_name", dates)
  df_spei <- (
    df_spei
    %>% pivot_longer(
      cols = -c("country", "adm_code", "adm_name"),
      names_to = "date",
      values_to = "spei"
    )
    %>% mutate(
      date = as.Date(date, format = "%Y-%m-%d")
    )
  )
}

build_weighted_spei_country <- function(
    iso3, 
    rast_spei, 
    rast_pop, 
    adm_pop,
    adm_shapes, 
    start_date, 
    age_disag = FALSE
) {
  print(paste0("Starting processing SPEI data for: ", iso3))
  if (age_disag) {
    file_name = "age_weighted_spei_"
  } else {
    file_name = "weighted_spei_"
  }
  dfs_spei_times <- list()
  rast_times <- terra::time(rast_spei)
  ids <- which(rast_times >= start_date)
  start_id <- ids[1]
  rast_times <- rast_times[ids]
  thresholds <- round(quantile(seq_along(rast_times), probs = c(0, 0.25, 0.5, 0.75, 1)))
  for (i in seq_along(rast_times)) {
    if (i %in% thresholds) {
      thresh_idx <- which(thresholds == i)
      print(paste0(names(thresh_idx), " complete"))
    }
    rast_spei_time  <- (
      rast_spei[[paste0("spei_", start_id + i - 1)]]
      %>% terra::crop(terra::ext(unlist(coords[iso3])))
    )
    rast_spei_time_fine <- terra::resample(rast_spei_time, rast_pop)
    rast_spei_weighted <- rast_spei_time_fine * rast_pop
    dfs_spei_times[[i]] <- (
      rast_spei_weighted
      %>% exactextractr::exact_extract(
        adm_shapes,
        fun = function(values, coverage_fraction) sum(values * coverage_fraction, na.rm = TRUE),
        max_cells_in_memory = 176067000,
        append_cols = c("adm_code"),
        progress = FALSE
      )
      %>% merge(
        adm_pop,
        by = "adm_code",
        all.x = TRUE
      )
      %>% mutate(
        date = as.Date(rast_times[[i]], format = "%Y-%m-%d"),
        spei = (result / pop)
      )
    )
  }
  df_spei <- (
    bind_rows(dfs_spei_times)
    %>% select(-c(pop, result))
    %>% mutate(
      country = country_map[[iso3]]
    )
    %>% relocate(country)
  )
  df_spei %>% write_csv(file.path(proc_data_dir, paste0(file_name, iso3, ".csv")))
  df_spei
}


calculate_utci_12m_stats <- function(df_utci_daily, roll = FALSE, roll_days = 3, weights = "") {
  if (weights != "") {
    variable_name = paste0(weights, "_", "utci")
  } else {
    variable_name = "utci"
  }
  if (roll) {
    df_utci_daily <- (
      df_utci_daily
      %>% arrange(country, adm_code, adm_name, date)
      %>% group_by(country, adm_code, adm_name)
      %>% mutate(
        utci_max = slider::slide_index_dbl(
          .x = utci_max,
          .i = date,
          .f = ~ mean(.x, na.rm = TRUE),
          .before = ~ .x %m-% days(roll_days),
          .complete = TRUE
        )
      )
      %>% ungroup()
    )
    variable_name = paste0(variable_name, "_roll")
  }
  df_utci_12m <- (
    df_utci_daily
    %>% group_by(country, adm_code, adm_name)
    %>% mutate(
      hist_mean = mean(utci_max, na.rm = TRUE),
      hist_sd = sd(utci_max, na.rm = TRUE),
      hist_p85 = quantile(utci_max, 0.85, na.rm = TRUE),
      hist_p90 = quantile(utci_max, 0.9, na.rm = TRUE),
      hist_p95 = quantile(utci_max, 0.95, na.rm = TRUE)
    )
    %>% ungroup()
    %>% mutate(
      sd_clim = (utci_max - hist_mean) / hist_sd,
      above_1sd = as.numeric(sd_clim > 1),
      above_p85 = as.numeric(utci_max > hist_p85),
      above_p90 = as.numeric(utci_max > hist_p90),
      above_p95 = as.numeric(utci_max > hist_p95),
      above_32 = as.numeric(utci_max > 32)
    )
    %>% select(-c(hist_mean, hist_sd, sd_clim, hist_p85, hist_p90, hist_p95))
    %>% arrange(country, adm_code, adm_name, date)
    %>% group_by(country, adm_code, adm_name)
    %>% mutate(
      across(
        -c(date, utci_max),
        ~ (
          slider::slide_index_dbl(
            .x = (.),
            .i = date,
            .f = ~ sum(.x, na.rm = TRUE),
            .before = ~ .x %m-% months(12),
            .complete = TRUE
          )
        ),
        .names = "{.col}_12m"
      ),
      mean_12m = slider::slide_index_dbl(
        .x = utci_max,
        .i = date,
        .f = ~ mean(.x, na.rm = TRUE),
        .before = ~ .x %m-% months(12),
        .complete = TRUE
      ),
      max_12m = slider::slide_index_dbl(
        .x = utci_max,
        .i = date,
        .f = ~ max(.x, na.rm = TRUE),
        .before = ~ .x %m-% months(12),
        .complete = TRUE
      )
    )
    %>% ungroup()
    %>% select(country, adm_code, adm_name, date, contains("12m"))
    %>% filter(!is.na(above_1sd_12m))
    %>% pivot_longer(
      cols = -c(country, adm_code, adm_name, date),
      names_to = "variable",
      values_to = "value"
    ) 
    %>% group_by(country, adm_code, adm_name, variable)
    %>% mutate(
      mean_value = mean(value, na.rm = TRUE),
      sd_value = sd(value, na.rm = TRUE)
    )
    %>% ungroup()
    %>% mutate(
      dev_value = if_else(
        sd_value == 0,
        0,
        (value - mean_value) / sd_value
      ),
      variable = paste0(variable_name, "_", variable)
    )
    %>% filter(!is.na(value))
    %>% pivot_wider(
      id_cols = c(country, adm_code, adm_name, date),
      names_from = "variable",
      values_from = c(value, dev_value)
    )
  )
  df_utci_12m
}

calculate_utci_12m_stats_monthly_ave <- function(df_utci_daily, weights = "") {
  if (weights != "") {
    variable_name = paste0(weights, "_", "utci_max")
  } else {
    variable_name = "utci_max"
  }
  df_utci_12m <- (
    df_utci_daily
    %>% group_by(country, adm_code, adm_name, month(date))
    %>% mutate(
      hist_mean = mean(utci_max, na.rm = TRUE),
      hist_sd = sd(utci_max, na.rm = TRUE),
      p85 = quantile(utci_max, 0.85, na.rm = TRUE),
      p90 = quantile(utci_max, 0.9, na.rm = TRUE),
      p95 = quantile(utci_max, 0.95, na.rm = TRUE)
    )
    %>% ungroup()
    %>% mutate(
      sd_clim = (utci_max - hist_mean) / hist_sd,
      above_1sd = as.numeric(sd_clim > 1),
      above_p85 = as.numeric(utci_max > p85),
      above_p90 = as.numeric(utci_max > p90),
      above_p95 = as.numeric(utci_max > p95),
      above_32 = as.numeric(utci_max > 32)
    )
    %>% select(-c(`month(date)`, hist_mean, hist_sd, sd_clim, p85, p90, p95))
    %>% arrange(country, adm_code, adm_name, date)
    %>% group_by(country, adm_code, adm_name)
    %>% mutate(
      across(
        -c(date, utci_max),
        ~ (
          slider::slide_index_dbl(
            .x = (.),
            .i = date,
            .f = ~ sum(.x, na.rm = TRUE),
            .before = ~ .x %m-% months(12),
            .complete = TRUE
          )
        ),
        .names = "{.col}_12m"
      ),
    )
    %>% ungroup()
    %>% select(country, adm_code, adm_name, date, contains("12m"))
    %>% filter(!is.na(above_1sd_12m))
    %>% pivot_longer(
      cols = -c(country, adm_code, adm_name, date),
      names_to = "variable",
      values_to = "value"
    ) 
    %>% group_by(country, adm_code, adm_name, variable)
    %>% mutate(
      mean_value = mean(value, na.rm = TRUE),
      sd_value = sd(value, na.rm = TRUE)
    )
    %>% ungroup()
    %>% mutate(
      dev_value = (value - mean_value),
      variable = paste0(variable_name, "_", variable)
    )
    %>% filter(!is.na(value))
    %>% pivot_wider(
      id_cols = c(country, adm_code, adm_name, date),
      names_from = "variable",
      values_from = c(value, dev_value)
    )
  )
  df_utci_12m
}


calculate_temp_12m_stats <- function(df_temp_daily, roll = FALSE, roll_days = 3, weights = "") {
  if (weights != "") {
    variable_name = paste0(weights, "_temp")
  } else {
    variable_name = "temp"
  }
  if (roll) {
    df_temp_daily <- (
      df_temp_daily
      %>% arrange(country, adm_code, adm_name, date)
      %>% group_by(country, adm_code, adm_name)
      %>% mutate(
        temp_2m_max = slider::slide_index_dbl(
          .x = temp_2m_max,
          .i = date,
          .f = ~ mean(.x, na.rm = TRUE),
          .before = ~ .x %m-% days(roll_days),
          .complete = TRUE
        )
      )
      %>% ungroup()
    )
    variable_name = paste0(variable_name, "_roll")
  }
  df_temp_12m <- (
    df_temp_daily
    %>% group_by(country, adm_code, adm_name)
    %>% mutate(
      hist_mean = mean(temp_2m_max, na.rm = TRUE),
      hist_sd = sd(temp_2m_max, na.rm = TRUE),
      hist_p85 = quantile(temp_2m_max, 0.85, na.rm = TRUE),
      hist_p90 = quantile(temp_2m_max, 0.9, na.rm = TRUE),
      hist_p95 = quantile(temp_2m_max, 0.95, na.rm = TRUE)
    )
    %>% ungroup()
    %>% mutate(
      sd_temp = (temp_2m_max - hist_mean) / hist_sd,
      above_1sd = as.numeric(sd_temp > 1),
      above_p85 = as.numeric(temp_2m_max > hist_p85),
      above_p90 = as.numeric(temp_2m_max > hist_p90),
      above_p95 = as.numeric(temp_2m_max > hist_p95),
      above_32 = as.numeric(temp_2m_max > 32)
    )
    %>% select(-c(hist_mean, hist_sd, sd_temp, hist_p85, hist_p90, hist_p95))
    %>% arrange(country, adm_code, adm_name, date)
    %>% group_by(country, adm_code, adm_name)
    %>% mutate(
      across(
        -c(date, temp_2m_max),
        ~ (
          slider::slide_index_dbl(
            .x = (.),
            .i = date,
            .f = ~ sum(.x, na.rm = TRUE),
            .before = ~ .x %m-% months(12),
            .complete = TRUE
          )
        ),
        .names = "{.col}_12m"
      ),
      mean_12m = slider::slide_index_dbl(
        .x = temp_2m_max,
        .i = date,
        .f = ~ mean(.x, na.rm = TRUE),
        .before = ~ .x %m-% months(12),
        .complete = TRUE
      ),
      max_12m = slider::slide_index_dbl(
        .x = temp_2m_max,
        .i = date,
        .f = ~ max(.x, na.rm = TRUE),
        .before = ~ .x %m-% months(12),
        .complete = TRUE
      )
    )
    %>% ungroup()
    %>% select(country, adm_code, adm_name, date, contains("12m"))
    %>% filter(!is.na(above_1sd_12m))
    %>% pivot_longer(
      cols = -c(country, adm_code, adm_name, date),
      names_to = "variable",
      values_to = "value"
    ) 
    %>% group_by(country, adm_code, adm_name, variable)
    %>% mutate(
      mean_value = mean(value, na.rm = TRUE),
      sd_value = sd(value, na.rm = TRUE)
    )
    %>% ungroup()
    %>% mutate(
      dev_value = if_else(
        sd_value == 0,
        0,
        (value - mean_value) / sd_value
      ),
      variable = paste0(variable_name, "_", variable)
    )
    %>% filter(!is.na(value))
    %>% pivot_wider(
      id_cols = c(country, adm_code, adm_name, date),
      names_from = "variable",
      values_from = c(value, dev_value)
    )
  )
  df_temp_12m
}

calculate_temp_12m_stats_monthly_ave <- function(df_temp_daily, weights = "") {
  if (weights != "") {
    variable_name = paste0(weights, "_", "temp_2m_max")
  } else {
    variable_name = "temp_2m_max"
  }
  df_temp_12m <- (
    df_temp_daily
    %>% group_by(country, adm_code, adm_name, month(date))
    %>% mutate(
      hist_mean = mean(temp_2m_max, na.rm = TRUE),
      hist_sd = sd(temp_2m_max, na.rm = TRUE),
      p85 = quantile(temp_2m_max, 0.85, na.rm = TRUE),
      p90 = quantile(temp_2m_max, 0.9, na.rm = TRUE),
      p95 = quantile(temp_2m_max, 0.95, na.rm = TRUE)
    )
    %>% ungroup()
    %>% mutate(
      sd_clim = (temp_2m_max - hist_mean) / hist_sd,
      above_1sd = as.numeric(sd_clim > 1),
      above_p85 = as.numeric(temp_2m_max > p85),
      above_p90 = as.numeric(temp_2m_max > p90),
      above_p95 = as.numeric(temp_2m_max > p95),
      above_32 = as.numeric(temp_2m_max > 32)
    )
    %>% select(-c(`month(date)`, hist_mean, hist_sd, sd_clim, p85, p90, p95))
    %>% arrange(country, adm_code, adm_name, date)
    %>% group_by(country, adm_code, adm_name)
    %>% mutate(
      across(
        -c(date, temp_2m_max),
        ~ (
          slider::slide_index_dbl(
            .x = (.),
            .i = date,
            .f = ~ sum(.x, na.rm = TRUE),
            .before = ~ .x %m-% months(12),
            .complete = TRUE
          )
        ),
        .names = "{.col}_12m"
      )
    )
    %>% ungroup()
    %>% select(country, adm_code, adm_name, date, contains("12m"))
    %>% filter(!is.na(above_1sd_12m))
    %>% pivot_longer(
      cols = -c(country, adm_code, adm_name, date),
      names_to = "variable",
      values_to = "value"
    ) 
    %>% group_by(country, adm_code, adm_name, variable)
    %>% mutate(
      mean_value = mean(value, na.rm = TRUE),
      sd_value = sd(value, na.rm = TRUE)
    )
    %>% ungroup()
    %>% mutate(
      dev_value = value - mean_value,
      variable = paste0(variable_name, "_", variable)
    )
    %>% filter(!is.na(value))
    %>% pivot_wider(
      id_cols = c(country, adm_code, adm_name, date),
      names_from = "variable",
      values_from = c(value, dev_value)
    )
  )
  df_temp_12m
}


calculate_spei_12m_stats <- function(df_spei, weights = "") {
  if (weights != "") {
    variable_name = paste0(weights, "_", "spei")
  } else {
    variable_name = "spei"
  }
  df_spei_12m <- (
    df_spei
    %>% group_by(country, adm_code, adm_name)
    %>% mutate(
      hist_mean = mean(spei, na.rm = TRUE),
      hist_sd = sd(spei, na.rm = TRUE),
      p05 = quantile(spei, 0.05, na.rm = TRUE),
      p10 = quantile(spei, 0.1, na.rm = TRUE),
      p15 = quantile(spei, 0.15, na.rm = TRUE)
    )
    %>% ungroup()
    %>% mutate(
      sd_spei = (spei - hist_mean) / hist_sd,
      below_1sd = as.numeric(sd_spei < -1),
      below_p05 = as.numeric(spei < p05),
      below_p10 = as.numeric(spei < p10),
      below_p15 = as.numeric(spei < p15),
      below_m1 = as.numeric(spei < -1)
    )
    %>% select(-c(hist_mean, hist_sd, sd_spei, p05, p10, p15))
    %>% arrange(country, adm_code, adm_name, date)
    %>% group_by(country, adm_code, adm_name)
    %>% mutate(
      across(
        -c(date, spei),
        ~ (
          slider::slide_index_dbl(
            .x = (.),
            .i = date,
            .f = ~ sum(.x, na.rm = TRUE),
            .before = ~ .x %m-% months(12),
            .complete = TRUE
          )
        ),
        .names = "{.col}_12m"
      ),
    )
    %>% ungroup()
    %>% select(country, adm_code, adm_name, date, contains("12m"))
    %>% filter(!is.na(below_m1_12m))
    %>% pivot_longer(
      cols = -c(country, adm_code, adm_name, date),
      names_to = "variable",
      values_to = "value"
    ) 
    %>% group_by(country, adm_code, adm_name, variable)
    %>% mutate(
      mean_value = mean(value, na.rm = TRUE),
      sd_value = sd(value, na.rm = TRUE)
    )
    %>% ungroup()
    %>% mutate(
      dev_value = value - mean_value,
      # dev_value = if_else(
      #   sd_value == 0,
      #   0,
      #   (value - mean_value) / sd_value
      # ),
      variable = paste0(variable_name, "_", variable)
    )
    %>% filter(!is.na(value))
    %>% pivot_wider(
      id_cols = c(country, adm_code, adm_name, date),
      names_from = "variable",
      values_from = c(value, dev_value)
    )
  )
  df_spei_12m
}

calculate_spei_12m_stats_monthly_ave <- function(df_spei, weights = "") {
  if (weights != "") {
    variable_name = paste0(weights, "_", "spei")
  } else {
    variable_name = "spei"
  }
  df_spei_12m <- (
    df_spei
    %>% group_by(country, adm_code, adm_name, month(date))
    %>% mutate(
      hist_mean = mean(spei, na.rm = TRUE),
      hist_sd = sd(spei, na.rm = TRUE),
      p05 = quantile(spei, 0.05, na.rm = TRUE),
      p10 = quantile(spei, 0.1, na.rm = TRUE),
      p15 = quantile(spei, 0.15, na.rm = TRUE)
    )
    %>% ungroup()
    %>% mutate(
      sd_clim = (spei - hist_mean) / hist_sd,
      below_1sd = as.numeric(sd_clim < -1),
      below_p05 = as.numeric(spei < p05),
      below_p10 = as.numeric(spei < p10),
      below_p15 = as.numeric(spei < p15),
      below_m1 = as.numeric(spei < -1)
    )
    %>% select(-c(`month(date)`, hist_mean, hist_sd, sd_clim, p05, p10, p15))
    %>% arrange(country, adm_code, adm_name, date)
    %>% group_by(country, adm_code, adm_name)
    %>% mutate(
      across(
        -c(date, spei),
        ~ (
          slider::slide_index_dbl(
            .x = (.),
            .i = date,
            .f = ~ sum(.x, na.rm = TRUE),
            .before = ~ .x %m-% months(12),
            .complete = TRUE
          )
        ),
        .names = "{.col}_12m"
      ),
    )
    %>% ungroup()
    %>% select(country, adm_code, adm_name, date, contains("12m"))
    %>% filter(!is.na(below_m1_12m))
    %>% pivot_longer(
      cols = -c(country, adm_code, adm_name, date),
      names_to = "variable",
      values_to = "value"
    ) 
    %>% group_by(country, adm_code, adm_name, variable)
    %>% mutate(
      mean_value = mean(value, na.rm = TRUE),
      sd_value = sd(value, na.rm = TRUE)
    )
    %>% ungroup()
    %>% mutate(
      dev_value = value - mean_value,
      # dev_value = if_else(
      #   sd_value == 0,
      #   0,
      #   (value - mean_value) / sd_value
      # ),
      variable = paste0(variable_name, "_", variable)
    )
    %>% filter(!is.na(value))
    %>% pivot_wider(
      id_cols = c(country, adm_code, adm_name, date),
      names_from = "variable",
      values_from = c(value, dev_value)
    )
  )
  df_spei_12m
}