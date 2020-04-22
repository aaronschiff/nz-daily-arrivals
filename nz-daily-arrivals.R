# *****************************************************************************
# Setup ----

library(conflicted)
library(tidyverse)
library(lubridate)
library(here)
library(readxl)
library(my.r.functions)
library(janitor)
library(scales)

conflict_prefer("here", "here")
conflict_prefer("filter", "dplyr")

# *****************************************************************************


# *****************************************************************************
# Load data ---- 

data_file <- "daily-movements-across-nz-border-Jan-Mar-2019-2020-2020-04-22.xlsx"

# Daily movements data
movements_dat <- read_excel(path = here(paste0("data/", data_file)), 
                            sheet = "Data") %>%
  clean_names() %>%
  mutate(date = as_date(date))

# Daily arrivals totals
daily_arrivals_totals <- movements_dat %>%
  filter(direction_code == "A") %>%
  group_by(year, month, day, date) %>%
  summarise(arrivals = sum(total_movements)) %>%
  ungroup()

# *****************************************************************************


# *****************************************************************************
# Last 14 days chart ---

# Total daily arrivals in the most recent 14 days of 2020 and the same days
# in 2019 for comparison

latest_date <- max(daily_arrivals_totals$date)
first_date <- latest_date - duration(num = 13, units = "days")

last_14_days_arrivals <- daily_arrivals_totals %>%
  filter(date >= first_date, date <= latest_date) %>%
  rename(arrivals.2020 = arrivals) %>%
  select(-date) %>%
  left_join(y = daily_arrivals_totals %>%
              filter(year == 2019) %>%
              select(month, day, arrivals.2019 = arrivals), 
            by = c("month", "day")) %>%
  pivot_longer(cols = c(arrivals.2020, arrivals.2019), 
               names_to = "arrivals.year", 
               values_to = "arrivals") %>%
  separate(col = arrivals.year, into = c("junk", "year"), convert = TRUE) %>%
  select(-junk) %>%
  arrange(year, month, day) %>%
  group_by(year) %>%
  mutate(dayorder = row_number()) %>%
  mutate(xlabel = paste0(month.abb[month], " ", day))

chart_last_14_days_arrivals <- last_14_days_arrivals %>%
  mutate(ylabel = ifelse(arrivals > 1000, 
                         paste0(format_decimal_label(x = arrivals / 1000, dp = 1), "k"), 
                         format_decimal_label(x = arrivals, dp = 0))) %>%
  ggplot(mapping = aes(x = fct_reorder(.f = xlabel, .x = dayorder), 
                       y = arrivals, 
                       label = ylabel, 
                       fill = as.factor(year), 
                       colour = as.factor(year))) + 
  geom_col(position = position_dodge(), 
           size = 0) +
  my_geom_text(position = position_dodge(width = 1), 
               rel_size = 0.75, 
               vjust = -0.3, 
               hjust = 0.5) + 
  scale_colour_manual(values = c("2019" = grey(0.75), 
                                 "2020" = "darkorange"), 
                      aesthetics = c("colour", "fill"), 
                      name = NULL) + 
  scale_y_continuous(labels = label_comma(), 
                     limits = c(0, 25000), 
                     breaks = seq(0, 25000, 5000))

output_chart(chart = chart_last_14_days_arrivals, 
             path = "outputs", 
             xlab = "", 
             ylab = "", 
             ggtitle = "Daily international arrivals to New Zealand", 
             orientation = "wide", 
             legend_position = "top", 
             plot.margin = margin(16, 20, 0, 0, unit = "pt"))

# Calculate total arrivals in last 14 days
last_14_days_total_arrivals <- daily_arrivals_totals %>%
  filter(date >= first_date, date <= latest_date) %>%
  pull(arrivals) %>%
  sum()

paste0("Total arrivals in 14 days to ", latest_date, ": ", last_14_days_total_arrivals)

# ***************************************************************************** 


# *****************************************************************************
# Timeseries since 1 Jan ----

daily_arrivals_comparison <- daily_arrivals_totals %>%
  filter(year == 2020) %>%
  rename(arrivals.2020 = arrivals) %>%
  select(-date) %>%
  left_join(y = daily_arrivals_totals %>%
              filter(year == 2019) %>%
              select(month, day, arrivals.2019 = arrivals), 
            by = c("month", "day")) %>%
  pivot_longer(cols = c(arrivals.2020, arrivals.2019), 
               names_to = "arrivals.year", 
               values_to = "arrivals") %>%
  separate(col = arrivals.year, into = c("junk", "year"), convert = TRUE) %>%
  select(-junk) %>%
  arrange(year, month, day) %>%
  group_by(year) %>%
  mutate(dayorder = row_number()) %>%
  mutate(weekorder = floor((dayorder - 1) / 7)) %>%
  mutate(dayofweek = dayorder - weekorder * 7) %>%
  mutate(xlabel = ifelse(dayofweek == 1, 
                         str_wrap(paste0(day, " ", month.abb[month]), 3), 
                         NA_character_))

xlabs <- daily_arrivals_comparison %>%
  filter(!is.na(xlabel))

chart_daily_arrivals_comparison <- daily_arrivals_comparison %>%
  ggplot(mapping = aes(x = dayorder, 
                       y = arrivals, 
                       group = as.factor(year), 
                       colour = as.factor(year))) + 
  geom_line() +
  scale_colour_manual(values = c("2019" = grey(0.75), 
                                 "2020" = "darkorange"), 
                      aesthetics = c("colour", "fill"), 
                      name = NULL) + 
  scale_x_continuous(breaks = xlabs$dayorder, 
                     labels = xlabs$xlabel, 
                     expand = expansion(0, 0)) + 
  scale_y_continuous(labels = label_comma(), 
                     limits = c(0, 30000), 
                     breaks = seq(0, 30000, 5000))

output_chart(chart = chart_daily_arrivals_comparison, 
             path = "outputs", 
             xlab = "", 
             ylab = "", 
             ggtitle = "Daily international arrivals to New Zealand", 
             orientation = "wide", 
             legend_position = "top", 
             plot.margin = margin(16, 20, 0, 0, unit = "pt"), 
             panel.grid.major.x = element_line(size = 0.2, colour = grey(0.9)), 
             axis.ticks.x = element_blank())

# *****************************************************************************