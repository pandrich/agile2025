library(httr)
library(readr)
library(dplyr)

root <- rprojroot::has_file("agile2025.Rproj")$make_fix_file()
data_dir <- root("data/raw/food_prices")
output_file <- file.path(data_dir, "wld_2021_rtfp_v02_m_maize_prices.csv")

# ---- SETTINGS ----
base_url <- "https://microdata.worldbank.org/api/tables/data/fcv/wld_2021_rtfp_v02_m"

fields <- paste(
  c(
    "ISO3",
    "country",
    "adm1_name",
    "adm2_name",
    "mkt_name",
    "DATES",
    "year",
    "month",
    "maize",
    "o_maize",
    "h_maize",
    "l_maize",
    "c_maize"
  ),
  collapse = ","
)

limit <- 1000
offset <- 0

countries <- c("MOZ", "ZMB", "ZWE", "LSO", "NAM")

all_data <- list()
page <- 1

# ---- HELPER: read API CSV safely from raw bytes ----
read_api_csv <- function(raw_content) {
  tmp <- tempfile(fileext = ".csv")
  writeBin(raw_content, tmp)
  
  df <- tryCatch(
    read.csv(
      tmp,
      fileEncoding = "UTF-16LE",
      stringsAsFactors = FALSE,
      check.names = FALSE
    ),
    error = function(e) NULL
  )
  
  if (is.null(df)) {
    df <- tryCatch(
      read.csv(
        tmp,
        fileEncoding = "UTF-16",
        stringsAsFactors = FALSE,
        check.names = FALSE
      ),
      error = function(e) NULL
    )
  }
  
  if (is.null(df)) {
    df <- tryCatch(
      read.csv(
        tmp,
        fileEncoding = "UTF-8",
        stringsAsFactors = FALSE,
        check.names = FALSE
      ),
      error = function(e) NULL
    )
  }
  
  if (is.null(df)) {
    stop("Failed to parse CSV response from API.")
  }
  
  as_tibble(df)
}

# ---- HELPER: fetch one page with retries ----
fetch_page <- function(url, offset, max_retries = 5) {
  for (attempt in seq_len(max_retries)) {
    cat(sprintf("Requesting offset %d, attempt %d/%d\n", offset, attempt, max_retries))
    
    res <- tryCatch(
      GET(url),
      error = function(e) NULL
    )
    
    if (!is.null(res) && status_code(res) == 200) {
      return(content(res, "raw"))
    }
    
    wait_seconds <- min(60, 2^(attempt - 1))
    cat(sprintf("Temporary failure at offset %d. Waiting %d seconds...\n", offset, wait_seconds))
    Sys.sleep(wait_seconds)
  }
  
  stop("Request failed at offset ", offset, " after ", max_retries, " attempts.")
}

# ---- PAGINATION LOOP ----
repeat {
  cat(sprintf("Fetching page %d (offset=%d)\n", page, offset))
  
  url <- paste0(
    base_url,
    "?fields=", fields,
    "&limit=", limit,
    "&offset=", offset,
    "&format=csv"
  )
  
  raw_content <- fetch_page(url, offset, max_retries = 5)
  df <- read_api_csv(raw_content)
  
  if (nrow(df) == 0) {
    break
  }
  
  all_data[[page]] <- df
  
  offset <- offset + limit
  page <- page + 1
}

# ---- COMBINE ----
full_data <- bind_rows(all_data)
cat("Total rows fetched:", nrow(full_data), "\n")

# ---- LOCAL FILTER ----
filtered_data <- full_data %>%
  filter(ISO3 %in% countries)

cat("Rows after filtering:", nrow(filtered_data), "\n")

# ---- SAVE ----
output_file <- "world_bank_maize_selected_countries.csv"
write_csv(filtered_data, output_file)

cat("Done. File saved as:", output_file, "\n")