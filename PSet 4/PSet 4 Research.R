#+ Sierra Manship
#+ Research for PSet 4

install.packages('janitor')

library(tidyverse)
library(readxl)
library(janitor)
library(knitr)
library(kableExtra)
install.packages("curl", type = "source")
library(curl)

usethis::use_git()
usethis::use_github()

# import the data sets that i am using
conflict <- read_csv("conflictdata.csv")
inflation <- read_excel("inflationdata.xlsx")

# clean column names
conflict <- conflict %>% clean_names()
inflation <- inflation %>% clean_names()

names(conflict)
names(inflation)

conflict <- conflict %>%
  rename(country = location)

inflation <- inflation %>%
  rename(country = country,
         year = year,
         inflation = inflation)

conflict <- conflict %>%
  filter(!str_detect(country, ","))

conflict <- conflict %>%
  mutate(country = recode(country,
                          "Myanmar (Burma)" = "Myanmar",
                          "Russia (Soviet Union)" = "Russia",
                          "Yemen (North Yemen)" = "Yemen",
                          "Yemen (South Yemen)" = "Yemen",
                          "Iran (Persia)" = "Iran"
  ))


# keep only valid country names
## removes historical/non-matching names
conflict <- conflict %>%
  filter(country %in% inflation$country)


# create 'War' variable
conflict2 <- conflict %>%
  group_by(country, year) %>%
  summarize(war = max(intensity_level > 0, 1, 0), .groups = "drop")


# join the data together
conflict_inflation <- left_join(inflation, conflict2,
                                by = c("country", "year"))


# replace 'NA' values with 0
conflict_inflation <- conflict_inflation %>%
  mutate(war = ifelse(is.na(war), 0, war))


# box plot
ggplot(conflict_inflation, aes(x = factor(war), y = inflation, fill = factor(war))) +
  geom_boxplot(alpha = 0.7) +
  scale_x_discrete(labels = c("No War", "War")) +
  scale_fill_manual(values = c("#4CAF50", "#E63946"),
                    labels = c("No War", "War")) +
  labs(x = "Conflict Status",
       y = "Inflation Rate",
       fill = "Status",
       title = "Inflation During War",
       subtitle = "Comparison of Inflation Rates in Conflict vs Non-Conflict") +
  theme_minimal(base_size = 14) +
  theme(
    legend.position = "none",
    plot.title = element_text(face = "bold"),
    axis.title = element_text(face = "bold")
  ) +
  scale_y_continuous(
    expand = expansion(mult = c(0, .1))
  )

# bar chart
conflict_inflation %>%
  group_by(war) %>%
  summarise(avg_inflation = mean(inflation, na.rm = TRUE)) %>%
  ggplot(aes(x = factor(war), y = avg_inflation, fill = factor(war))) +
  geom_col(width = 0.6, alpha = 0.9) +
  scale_fill_manual(values = c("lightgreen", "red")) +
  labs(x = "Conflict Status",
       y = "Average Inflation",
       title = "Average Inflation During War Periods",
       subtitle = "Mean Inflation Rate by Conflict Status") +
  theme(
    legend.position = "none",
    plot.title = element_text(face = "bold"),
    axis.title = element_text(face = "bold")
  ) +
  scale_y_continuous(
    expand = expansion(mult = c(0, .1))
  )


# over time
time_conflict <- conflict_inflation %>%
  group_by(year) %>%
  summarise(
    avg_inflation = mean(inflation, na.rm = TRUE),
    mean_war = mean(war, na.rm = TRUE)
  )

ggplot(time_conflict, aes(x = year, y = avg_inflation)) +
  geom_line(color = 'red', linewidth = 1.2) +
  geom_point(color = 'red', size = 1.5, alpha = 0.7) +
  labs(title = "Global Inflation Over Time",
       subtitle = "Average Inflation Rate Across Time by Year",
       x = "Year",
       y = "Average Inflation") +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(face = "bold"),
    axis.title = element_text(face = "bold")
  ) +
  scale_y_continuous(
    expand = expansion(mult = c(0, .05))
  ) +
  scale_x_continuous(
    expand = expansion(mult = c(0.01, 0.01))
  )


# summary stats table
sum_conflict <- conflict_inflation %>%
  group_by(war) %>%
  summarise(
    mean_inflation = mean(inflation, na.rm = TRUE),
    sd_inflation = sd(inflation, na.rm = TRUE),
    observations = n()
  )

conflict_tbl <- sum_conflict %>%
  mutate(
    war = ifelse(war == 1, "War", "No War")
  ) %>%
  rename(
    'Conflict Status' = war,
    'Average Inflation' = mean_inflation,
    'Standard Deviation' = sd_inflation,
    'Observations' = observations
  )

kable(conflict_tbl,
      caption = "Inflation During War vs Peace",
      digits = 2,
      align = "c")








