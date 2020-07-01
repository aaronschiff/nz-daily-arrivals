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
library(RcppRoll)

conflict_prefer("here", "here")
conflict_prefer("filter", "dplyr")

# *****************************************************************************


# *****************************************************************************
# Load data ---- 

data_file <- "daily-movements-across-nz-border-2020-07-01.xlsx"

# Daily movements data
movements_dat <- read_excel(path = here(paste0("data/", data_file)), 
                            sheet = "Data") %>%
  clean_names() %>%
  mutate(date = as_date(date))

# Daily arrivals totals
daily_arrivals_totals <- movements_dat %>%
  filter(direction_code == "A") %>%
  group_by(date) %>%
  summarise(arrivals = sum(total_movements)) %>%
  ungroup() %>%
  complete(date = seq(from = min(movements_dat$date), 
                      to = max(movements_dat$date), 
                      by = "1 day"), 
           fill = list(arrivals = 0)) %>%
  mutate(year = year(date), 
         month = month(date), 
         day = day(date))

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
             xlab = "Arrival date", 
             ylab = "Number of arrivals", 
             ggtitle = "Daily international arrivals to New Zealand", 
             orientation = "wide", 
             legend_position = "top", 
             plot.margin = margin(8, 4, 4, 4, unit = "pt"))

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
             xlab = "Arrival date", 
             ylab = "Number of arrivals", 
             ggtitle = "Daily international arrivals to New Zealand", 
             orientation = "wide", 
             legend_position = "top", 
             plot.margin = margin(8, 4, 4, 4, unit = "pt"),
             panel.grid.major.x = element_line(size = 0.2, colour = grey(0.9)), 
             axis.ticks.x = element_blank())

# *****************************************************************************


# *****************************************************************************
# Daily arrivals since lockdown ----

daily_arrivals_since_lockdown <- daily_arrivals_totals %>%
  mutate(arrivals_14days = roll_sum(x = arrivals, 
                                    n = 14, 
                                    align = "right", 
                                    fill = NA)) %>%
  filter(date > ymd("2020-03-25"))

chart_daily_arrivals_since_lockdown <- 
  daily_arrivals_since_lockdown %>%
  ggplot(mapping = aes(x = date, 
                       y = arrivals)) + 
  geom_col() + 
  geom_vline(xintercept = ymd("2020-03-26") - 0.5, colour = "red", size = 0.5) + 
  geom_vline(xintercept = ymd("2020-04-28") - 0.5, colour = "darkorange", size = 0.5) + 
  geom_vline(xintercept = ymd("2020-05-14") - 0.5, colour = "darkgoldenrod1", size = 0.5) + 
  geom_vline(xintercept = ymd("2020-06-09") - 0.5, colour = "cornflowerblue", size = 0.5) + 
  annotate(geom = "text", 
           x = ymd("2020-03-26") + 0.5, 
           y = 1650, 
           label = "Level 4", 
           colour = "red", 
           hjust = 0, 
           family = "Fira Sans", 
           fontface = "bold", 
           size = 3) +
  annotate(geom = "text", 
           x = ymd("2020-04-28") + 0.5, 
           y = 1650, 
           label = "Level 3", 
           colour = "darkorange", 
           hjust = 0, 
           family = "Fira Sans", 
           fontface = "bold", 
           size = 3) +
  annotate(geom = "text", 
           x = ymd("2020-05-14") + 0.5, 
           y = 1650, 
           label = "Level 2", 
           colour = "darkgoldenrod1", 
           hjust = 0, 
           family = "Fira Sans", 
           fontface = "bold", 
           size = 3) + 
  annotate(geom = "text", 
           x = ymd("2020-06-09") + 0.5, 
           y = 1650, 
           label = "Level 1", 
           colour = "cornflowerblue", 
           hjust = 0, 
           family = "Fira Sans", 
           fontface = "bold", 
           size = 3) + 
  scale_y_continuous(limits = c(0, 1700), 
                     breaks = seq(0, 1700, 100), 
                     labels = comma, 
                     expand = expansion(0, 0)) + 
  scale_x_date(breaks = c(ymd("2020-04-01", "2020-04-15", 
                              "2020-05-01", "2020-05-15", 
                              "2020-06-01", "2020-06-15")), 
               labels = date_format(format = "%d %b"), 
               limits = c(ymd("2020-03-24"), 
                          max(daily_arrivals_since_lockdown$date) + 2), 
               expand = expansion(0, 0))

output_chart(chart = chart_daily_arrivals_since_lockdown, 
             path = here("outputs"), 
             orientation = "wide", 
             xlab = "Arrival date", 
             ylab = "Number of arrivals", 
             ggtitle = "Daily international arrivals to New Zealand", 
             plot.margin = margin(8, 4, 4, 4, unit = "pt"))

# *****************************************************************************


# *****************************************************************************
# Cumulative arrivals over previous 14 days since level 2 ----

chart_cumulative_14day_arrivals_since_level2 <- 
  daily_arrivals_since_lockdown %>%
  filter(date > ymd("2020-05-13")) %>%
  ggplot(mapping = aes(x = date, 
                       y = arrivals_14days)) + 
  geom_col() + 
  geom_vline(xintercept = ymd("2020-05-14") - 0.5, colour = "darkgoldenrod1", size = 0.5) + 
  geom_vline(xintercept = ymd("2020-06-09") - 0.5, colour = "cornflowerblue", size = 0.5) + 
  annotate(geom = "text", 
           x = ymd("2020-05-14"), 
           y = 4250, 
           label = "Level 2", 
           colour = "darkgoldenrod1", 
           hjust = 0, 
           family = "Fira Sans", 
           fontface = "bold", 
           size = 3) + 
  annotate(geom = "text", 
           x = ymd("2020-06-09"), 
           y = 4250, 
           label = "Level 1", 
           colour = "cornflowerblue", 
           hjust = 0, 
           family = "Fira Sans", 
           fontface = "bold", 
           size = 3) +
  scale_y_continuous(limits = c(0, 5100),
                     breaks = seq(0, 5000, 500),
                     labels = comma,
                     expand = expansion(0, 0)) + 
  scale_x_date(breaks = seq(from = ymd("2020-05-14"), 
                            by = "1 week", 
                            length.out = 7),
               labels = date_format(format = "%d %b"),
               limits = c(ymd("2020-05-13"),
                          max(daily_arrivals_since_lockdown$date) + 1),
               expand = expansion(0, 0))

output_chart(chart = chart_cumulative_14day_arrivals_since_level2, 
             path = here("outputs"), 
             orientation = "wide", 
             xlab = "14 days ending on", 
             ylab = "Total number of arrivals", 
             ggtitle = "International arrivals to New Zealand in the past 14 days", 
             plot.margin = margin(8, 4, 4, 4, unit = "pt"))

# *****************************************************************************