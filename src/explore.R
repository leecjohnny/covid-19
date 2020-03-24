rm(list = ls())

library(tidyverse)
library(lubridate)
library(scales)
library(tidycensus)


# Data Source: Johns Hopkins University Center for Systems Science and Engineering (JHU CSSE)
# URL: https://github.com/CSSEGISandData/COVID-19

git_host_url <- "https://raw.githubusercontent.com"
repo_path <- "/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/"
filename <- "time_series_19-covid-%s.csv"

retrieve_jhu_data <- function(url) {
    read_csv(url) %>%
        pivot_longer(-(1:4), names_to = "date", values_to = "cases") %>%
        rename_all(tolower) %>%
        rename_all(~ str_replace(., "/", "_")) %>%
        mutate(date = mdy(date))
}

jhu_metrics <- tribble(
    ~metric_type,
    "Confirmed",
    "Deaths",
    "Recovered"
    ) %>%
    mutate(
        get_urls = sprintf(paste0(git_host_url, repo_path, filename), metric_type),
        data = map(get_urls, retrieve_jhu_data)
    ) %>%
    unnest(cols = data) %>%
    select(-get_urls) %>%
    ## "Recovered" time series currently does not match with daily reports
    filter(metric_type != "Recovered") %>%
    pivot_wider(names_from = metric_type, values_from = cases) %>%
    mutate(
        Active = Confirmed - Deaths
        ## - Recovered
    ) %>%
    pivot_longer(-(1:5), names_to = "metric_type", values_to = "cases")

# Column names
# > cbind(names(jhu_metrics))
#      [,1]
# [1,] "province_state"
# [2,] "country_region"
# [3,] "lat"
# [4,] "long"
# [5,] "date"
# [6,] "metric_type"
# [7,] "cases"

# Metric type names
# > jhu_metrics %>% distinct(metric_type)
# # A tibble: 4 x 1
#   metric_type
#   <chr>
# 1 Confirmed
# 2 Deaths
# 3 Active

# Data Source: COVID Tracking Project
# URL: https://covidtracking.com/api/

ctp_state_daily_csv_url <- "http://covidtracking.com/api/states/daily.csv"

ctp_metrics <- read_csv(ctp_state_daily_csv_url) %>%
    select(-dateChecked) %>%
    ## Coerce into same shape as JHU data
    rename(
        "Total Tested" = total,
        province_state = state
    ) %>%
    rename_at(vars(-date, -province_state), str_to_title) %>%
    pivot_longer(-(1:2), names_to = "metric_type", values_to = "cases") %>%
    mutate(
        date = ymd(date),
        country_region = "US",
        lat = NA,
        long = NA,
        ## Use full names as JHU
        province_state = coalesce(state.name[match(province_state, state.abb)], province_state)
    ) %>%
    select(
        province_state,
        country_region,
        lat,
        long,
        date,
        metric_type,
        cases
    )

# > cbind(names(ctp_metrics))
#      [,1]
# [1,] "province_state"
# [2,] "country_region"
# [3,] "lat"
# [4,] "long"
# [5,] "date"
# [6,] "metric_type"
# [7,] "cases"

# > ctp_metrics %>% distinct(metric_type)
# # A tibble: 6 x 1
#   metric_type
#   <chr>
# 1 Positive
# 2 Negative
# 3 Pending
# 4 Hospitalized
# 5 Death
# 6 Total Tested

us_covid_combined <- rbind(
    jhu_metrics %>% filter(country_region == "US"),
    ctp_metrics %>% filter(metric_type == "Total Tested")
)

## Get state populations
census_api_key(Sys.getenv("CENSUS_API_KEY"))
state_pop <- get_acs(geography = "state", variables = "B01003_001", year = 2018) %>%
    rename(province_state = NAME,
    population_num = estimate) %>%
    select(province_state, population_num)

compute_daily_change <- function(data) {
    data %>%
        group_by(date, metric_type) %>%
        summarise(n_total = sum(cases)) %>%
        group_by(metric_type) %>%
        arrange(date) %>%
        mutate(
            daily_move = n_total - lag(n_total),
            metric_type_str = paste0(metric_type, " (Latest: ", comma(last(daily_move)), ")")
        ) %>%
        ungroup()
}

compute_cumulative_daily <- function(data) {
    data %>%
        group_by(date, metric_type) %>%
        summarise(n_total = sum(cases)) %>%
        group_by(metric_type) %>%
        arrange(date) %>%
        mutate(metric_type_str = paste0(metric_type, ": ", comma(last(n_total)))) %>%
        ungroup()
}

plot_daily <- function(data, region_label, as_of_date, log_scale = FALSE) {
    data %>%
        ggplot(aes(x = date, y = daily_move, color = metric_type_str)) +
        geom_line(size = 1) +
        geom_point(
            data = . %>% filter(date == max(date)),
            size = 4
        ) +
        scale_x_date(date_breaks = "1 week", date_labels = "%b %d") +
        scale_y_continuous(
            labels = comma,
            trans = if_else(log_scale, "pseudo_log", "identity")) +
        labs(
            title = paste0(region_label, " Daily Case Growth (by type)"),
            subtitle = paste0("Data as of ", as_of_date),
            y = "# Cases",
            x = "Date",
            color = "Metric Type",
            caption = "Sources: JHU CSSE, COVID Tracking Project"
        ) +
        theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
        theme_classic()
}

plot_cumulative <- function(data, region_label, as_of_date, log_scale = FALSE) {
    data %>%
        ggplot(aes(x = date, y = n_total, color = metric_type_str)) +
        geom_line(size = 1) +
        geom_point(
            data = . %>% filter(date == max(date)),
            size = 4
        ) +
        scale_x_date(date_breaks = "1 week", date_labels = "%b %d") +
        scale_y_continuous(
            labels = comma,
            trans = if_else(log_scale, "pseudo_log", "identity")) +
        labs(
            title = paste0(region_label, " Total Case Growth (by type)"),
            subtitle = paste0("Data as of ", as_of_date),
            y = "# Cases",
            x = "Date",
            color = "Metric Type",
            caption = "Sources: JHU CSSE, COVID Tracking Project"
        ) +
        theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
        theme_classic()
}


## Latest date
latest_date <- jhu_metrics %>%
    slice(which.max(date)) %>%
    select(date) %>%
    pluck(1)

## Global Total Summary
jhu_metrics %>%
    group_by(metric_type) %>%
    filter(date == max(date)) %>%
    summarise(
        n_total = sum(cases)
    )

## US Total Summary

us_d <- us_covid_combined

us_d %>%
    group_by(metric_type) %>%
    filter(date == max(date)) %>%
    summarise(
        n_total = sum(cases)
    )

## Daily US Case Growth

## Normal Scale
compute_daily_change(us_d) %>%
    plot_daily("US", latest_date)
ggsave(paste0("../output/daily_us_case_growth_", latest_date, ".png"), width = 8, height = 6)

## Cumulative US Case Growth

## Normal Scale
compute_cumulative_daily(us_d) %>%
    plot_cumulative("US", latest_date)
ggsave(paste0("../output/total_us_cases_", latest_date, ".png"), width = 8, height = 6)

## Log Scale
compute_cumulative_daily(us_d) %>%
    plot_cumulative("US", latest_date, log_scale = TRUE)
ggsave(paste0("../output/total_us_cases_log_scale_", latest_date, ".png"), width = 8, height = 6)


cal_d <- us_covid_combined %>%
        filter(country_region == "US" & province_state == "California")

## CA Total Summary
cal_d %>%
    group_by(metric_type) %>%
    filter(date == max(date)) %>%
    summarise(
        n_total = sum(cases)
    )

## Daily California Case Growth
compute_daily_change(cal_d) %>%
    plot_daily("California", latest_date)
ggsave(paste0("../output/daily_california_case_growth_", latest_date, ".png"), width = 8, height = 6)

## Cumulative CA Case Growth

## Normal Scale
compute_cumulative_daily(cal_d) %>%
    plot_cumulative("California", latest_date)
ggsave(paste0("../output/total_california_cases_", latest_date, ".png"), width = 8, height = 6)

## Log Scale
compute_cumulative_daily(cal_d) %>%
    plot_cumulative("California", latest_date, log_scale = TRUE)
ggsave(paste0("../output/total_california_cases_log_scale_", latest_date, ".png"), width = 8, height = 6)

## Compute Testing per Million People (Top 10 States )
top_10_testing <- us_covid_combined %>%
    filter(metric_type == "Total Tested") %>%
    group_by(date, province_state) %>%
    summarise(n_total = sum(cases)) %>%
    ungroup() %>%
    left_join(state_pop) %>%
    mutate(testing_per_million = (n_total / population_num) * 1000000) %>%
    filter(!is.na(testing_per_million)) %>%
    arrange(desc(date), desc(testing_per_million)) %>%
    top_n(1, date) %>%
    top_n(10, testing_per_million) %>%
    pull(province_state)

us_covid_combined %>%
    filter(metric_type == "Total Tested") %>%
    group_by(date, province_state) %>%
    summarise(n_total = sum(cases)) %>%
    ungroup() %>%
    left_join(state_pop) %>%
    mutate(testing_per_million = (n_total / population_num) * 1000000) %>%
    filter(!is.na(testing_per_million) & province_state %in% top_10_testing) %>%
    ggplot(aes(x = date, y = testing_per_million, color = province_state)) +
    geom_line(size = 1) +
    geom_point(
        data = . %>% filter(date == max(date)),
        size = 2
    ) +
    scale_x_date(date_breaks = "1 week", date_labels = "%b %d") +
    scale_y_continuous(labels = comma) +
    labs(
        title = "Top 10 States Testing per Million People",
        subtitle = paste0("Data as of ", latest_date),
        y = "# Tests Completed per Million People",
        x = "Date",
        color = "State",
        caption = "Sources: JHU CSSE, COVID Tracking Project"
    ) +
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
    theme_classic()

