rm(list = ls())
library(tidyverse)
library(lubridate)
library(rvest)
library(httr)

# Global variables
months <- c("January","February","March","April",
            "May","June","July","August",
            "September","October","November","December")

# Functions
na_locf <- function(x) {
  x_len <- length(x)
  
  if (x_len < 2)
    stop("na_locf: vector not length 2 or more")
  
  values <- x[!is.na(x)]
  if (is.na(x[1])) values <- c(NA, values)
  
  ind = 1
  ind_val <- rep(1, x_len)
  i <- 2
  while (i <= length(x)) {
    if (!is.na(x[i]))
      ind <- ind+1
    
    ind_val[i] <- ind
    
    i <- i+1
  }
  
  return(values[ind_val])
}

request_site <- function(url) {
  attempts <- 5
  attempt <- 0
  
  html <- NULL
  
  while (attempt < attempts) {
    html <- tryCatch(
      GET(url, timeout(30)),
      error = function(e) {Sys.sleep(30)}
    )
    
    if (!is.null(html))
      break;
    
    attempt <- attempt + 1
  }
  
  if (is.null(html))
    stop("failed after 3 attempsts")
  
  return(html)
}

survey_html_to_dataframe <- function(hws_html) {
  col_left <- hws_html %>%
    html_nodes("div.stats_col_left") %>%
    html_text()
  col_mid <- hws_html %>%
    html_nodes("div.stats_col_mid") %>%
    html_text()
  col_right <- hws_html %>%
    html_nodes("div.stats_col_right") %>%
    html_text()
  col_right2 <- hws_html %>%
    html_nodes("div.stats_col_right2") %>%
    html_text()
  
  df_data <- data.frame(
    category = col_left,
    name = str_trim(col_mid),
    percent = str_trim(col_right),
    change = str_trim(col_right2),
    stringsAsFactors = FALSE
  ) %>%
    mutate(is_data_row = if_else(category == "", 1L, 0L)) %>%
    mutate(category = str_trim(category)) %>%
    filter(category != "ITEM") %>%
    mutate(
      category = if_else(category == "", NA_character_, category),
      name = if_else(name == "", NA_character_, name),
      percent = if_else(percent == "", NA_character_, percent),
      change = if_else(change == "", NA_character_, change)
    ) %>%
    mutate(category = na_locf(category))
    # mutate(percent = as.numeric(gsub("%","",percent)),
    #        change = as.numeric(gsub("%","",change)))
  
  # Extract the month date of the survey data
  survey_date <- hws_html %>%
    html_nodes("h1") %>%
    html_text()
  
  survey_date <- str_extract(survey_date, paste0("(", paste0(months, collapse="|"), ")", " [0-9]{4}"))[1] %>%
    fast_strptime("%B %Y") %>%
    as.Date() %>% as.character()
  
  df_data <- df_data %>%
    mutate(report_date = survey_date)
}

get_current_data <- function(date, platform = "combined") {
  platforms = c("linux","mac","pc","combined")
  if (!(platform %in% platforms))
    stop(paste0("get_historic_data: supplied platform not one of ", paste0(platforms, collapse=", ")))
  
  url <- paste0(
    "https://store.steampowered.com/hwsurvey/",
    case_when(
      platform == "linux" ~ paste0("?platform=", "linux"),
      platform == "mac" ~ paste0("?platform=",   "mac"),
      platform == "pc" ~ paste0("?platform=",   "pc"),
      TRUE ~ ""
    )
  )
  
  hws <- request_site(url) %>%
    read_html()
  
  df_hws <- survey_html_to_dataframe(hws) %>%
    mutate(platform = platform) %>%
    mutate(scrape_date = as.character(date))
}

get_historic_data <- function(date, platform = "combined") {
  platforms = c("linux","mac","pc","combined")
  if (!(platform %in% platforms))
    stop(paste0("get_historic_data: supplied platform not one of ", paste0(platforms, collapse=", ")))
  
  url <- paste0(
    "https://web.archive.org/web/",
    gsub("-","",as.character(date)), "000000/https://store.steampowered.com/hwsurvey/",
    case_when(
      platform == "linux" ~ paste0("?platform=", "linux"),
      platform == "mac" ~ paste0("?platform=",   "mac"),
      platform == "pc" ~ paste0("?platform=",   "pc"),
      TRUE ~ ""
    )
  )
  
  hws <- request_site(url) %>%
    read_html()
  
  df_hws <- survey_html_to_dataframe(hws) %>%
    mutate(platform = platform) %>%
    mutate(scrape_date = as.character(date))
}


# Process historic data ----
df_combined <- NULL
df_linux    <- NULL
df_mac      <- NULL
df_win      <- NULL
for (mydate in as.character(seq(as.Date("2016-01-01"), as.Date("2020-04-25"), by = "weeks"))) {
  print(as.character(mydate))

  df_combined <- rbind(df_combined, get_historic_data(mydate))
  Sys.sleep(5)

  df_linux    <- rbind(df_linux, get_historic_data(mydate, "linux"))
  Sys.sleep(5)

  df_mac      <- rbind(df_mac, get_historic_data(mydate, "mac"))
  Sys.sleep(5)

  df_win      <- rbind(df_win, get_historic_data(mydate, "pc"))
  Sys.sleep(5)
}

df_combined <- df_combined %>% group_by(report_date) %>% filter(scrape_date == max(scrape_date))
df_linux    <- df_linux    %>% group_by(report_date) %>% filter(scrape_date == max(scrape_date))
df_mac      <- df_mac      %>% group_by(report_date) %>% filter(scrape_date == max(scrape_date))
df_win      <- df_win      %>% group_by(report_date) %>% filter(scrape_date == max(scrape_date))

save(list = c("df_combined","df_linux","df_mac","df_win"), file = "steam_survey_historic.rda")


# Refresh ----
# load("steam_survey_historic.rda")
# tmp_combined <- get_current_data(Sys.Date())
# tmp_linux    <- get_current_data(Sys.Date(), "linux")
# tmp_mac      <- get_current_data(Sys.Date(), "mac")
# tmp_win      <- get_current_data(Sys.Date(), "pc")
# 
# df_combined <- df_combined %>% ungroup() %>% rbind(tmp_combined)
# df_linux    <- df_linux %>% ungroup() %>% rbind(tmp_linux)
# df_mac      <- df_mac %>% ungroup() %>% rbind(tmp_mac)
# df_win      <- df_win %>% ungroup() %>% rbind(tmp_win)
# save(list = c("df_combined","df_linux","df_mac","df_win"), file = "steam_survey_historic_20200503.rda")
