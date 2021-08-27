# 1) load packages dplyr, ggplot2, readr

library(dplyr)
library(ggplot2)
library(readr)

# 2) Read in datasets/confirmed_cases_worldwide.csv using read_csv() and assign it to the variable confirmed_cases_worldwide.

confirmed_cases_worldwide <- read_csv("Datasets/confirmed_cases_worldwide.csv")
print(confirmed_cases_worldwide)

# 3) Draw a line plot of cumulative cases vs. date. Label the y-axis as "cumulative confirmed cases"

ggplot(confirmed_cases_worldwide, aes(date, cum_cases)) +
  geom_line() +
  ylab("Cumulative confirmed cases")

# 4) Read in datasets/confirmed_cases_china_vs_world.csv. Show result

confirmed_cases_china_vs_world <- read_csv("Datasets/confirmed_cases_china_vs_world.csv")
print(confirmed_cases_china_vs_world)

# 5) Draw a line plot of cumulative cases vs. date, colored by is_china. Define aesthetics within the line geom

plt_cum_confirmed_cases_china_vs_world <- ggplot(confirmed_cases_china_vs_world) +
  geom_line(aes(date, cum_cases, colour = is_china)) +
  ylab("Cumulative confirmed cases")
plt_cum_confirmed_cases_china_vs_world

# who_events data filled for us

who_events <- tribble(
  ~ date, ~ event,
  "2020-01-30", "Global health\nemergency declared",
  "2020-03-11", "Pandemic\ndeclared",
  "2020-02-13", "China reporting\nchange"
) %>%
  mutate(date = as.Date(date))

# 6) Using who_events, add vertical dashed lines with an xintercept at date 
#and text at date, labeled by event, and at 100000 on the y-axis

plt_cum_confirmed_cases_china_vs_world +
  geom_vline(aes(xintercept = date), data = who_events, linetype = 'dashed') +
  geom_text(aes(date, label = event), data = who_events, y = 100000)

# Filter for China, from Feb 15. We are looking at post feb15. Using china_after_feb15, draw a line plot cum_cases vs. date
# Add a smooth trend line using linear regression, no error bars

china_after_feb15 <- confirmed_cases_china_vs_world %>%
  filter(is_china =='China', date >= '2020-02-15')


ggplot(china_after_feb15, aes(date, cum_cases)) +
  geom_line() +
  geom_smooth(method = 'lm', se = FALSE) +
  ylab("Cumulative confirmed cases")

# Filter confirmed_cases_china_vs_world for not China
not_china <- confirmed_cases_china_vs_world %>%
  filter(is_china == 'Not China')

# 7) Using not_china, draw a line plot cum_cases vs. date. Add a smooth trend line using linear regression, no error bars

plt_not_china_trend_lin <- ggplot(not_china, aes(date, cum_cases)) +
  geom_line() +
  geom_smooth(method = 'lm', se = FALSE) +
  ylab("Cumulative confirmed cases")

plt_not_china_trend_lin 

# 8) Modify the plot to use a logarithmic scale on the y-axis

plt_not_china_trend_lin + 
  scale_y_log10()

# 9) Get the data for each country. Group by country, summarize to calculate total cases, find the top 7
confirmed_cases_by_country <- read_csv("Datasets/confirmed_cases_by_country.csv")
glimpse(confirmed_cases_by_country)

top_countries_by_total_cases <- confirmed_cases_by_country %>%
  group_by(country) %>%
  summarize(total_cases = max(cum_cases)) %>%
  top_n(7, total_cases)
print(top_countries_by_total_cases)

# 10) Load confirmed_cases_top7_outside_china.csv.  Glimpse at the content. 
# Using confirmed_cases_top7_outside_china, draw a line plot of cum_cases vs. date, colored by country

confirmed_cases_top7_outside_china <- read_csv("Datasets/confirmed_cases_top7_outside_china.csv")

glimpse(confirmed_cases_top7_outside_china)

ggplot(confirmed_cases_top7_outside_china, aes(date, cum_cases, color = country)) +
  geom_line() +
  ylab("Cumulative confirmed cases")

