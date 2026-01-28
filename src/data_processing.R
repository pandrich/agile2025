calculate_utci_12m_stats <- function(df_utci_daily, weights = "") {
  if (weights != "") {
    variable_name = paste0(weights, "_", "utci_max")
  } else {
    variable_name = "utci_max"
  }
  df_utci_12m <- (
    df_utci_daily
    %>% group_by(country, adm_code, adm_name)
    %>% mutate(
      hist_mean = mean(utci_max, na.rm = TRUE),
      hist_sd = sd(utci_max, na.rm = TRUE)
    )
    %>% ungroup()
    %>% mutate(
      sd_clim = (utci_max - hist_mean) / hist_sd,
      above_1sd = as.numeric(sd_clim > 1),
      above_32 = as.numeric(utci_max > 32)
    )
    %>% select(-c(hist_mean, hist_sd, sd_clim))
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


calculate_temp_12m_stats <- function(df_temp_daily, weights = "") {
  if (weights != "") {
    variable_name = paste0(weights, "_", "temp_2m_max")
  } else {
    variable_name = "temp_2m_max"
  }
  df_temp_12m <- (
    df_temp_daily
    %>% group_by(country, adm_code, adm_name)
    %>% mutate(
      hist_mean = mean(temp_2m_max, na.rm = TRUE),
      hist_sd = sd(temp_2m_max, na.rm = TRUE)
    )
    %>% ungroup()
    %>% mutate(
      sd_clim = (temp_2m_max - hist_mean) / hist_sd,
      above_1sd = as.numeric(sd_clim > 1),
      above_32 = as.numeric(temp_2m_max > 32)
    )
    %>% select(-c(hist_mean, hist_sd, sd_clim))
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
      hist_sd = sd(spei, na.rm = TRUE)
    )
    %>% ungroup()
    %>% mutate(
      sd_clim = (spei - hist_mean) / hist_sd,
      below_1sd = as.numeric(sd_clim < -1),
      below_m1 = as.numeric(spei < -1)
    )
    %>% select(-c(hist_mean, hist_sd, sd_clim))
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