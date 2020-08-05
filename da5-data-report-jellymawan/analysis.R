library(dplyr)
library(leaflet)
library(ggplot2)
library(plotly)

data_df <- read.csv("data/shootings-2018.csv", stringsAsFactors = FALSE)

shootings_num <- nrow(data_df)
lives_lost <- sum(data_df$num_killed)


# impact is defined as combined number of deaths and injuries
impact <- data_df %>%
  group_by(city) %>%
  summarize(total = sum(num_injured, na.rm = TRUE) +
    sum(num_killed, na.rm = TRUE)) %>%
  filter(total == max(total, na.rm = FALSE)) %>%
  pull(city)



most <- function(data_df, col){
  most_x <- data_df %>% 
    filter(col == max(col))
  
  most_x
}

# which event had the most injuries?
most_injured <- most(data_df, select(data_df, num_killed))


# which event had the most deaths?
most_death <- most(data_df, select(data_df, num_killed))


# SUMMARY TABLE
summary_table <- data_df %>%
  mutate(month = format(as.Date(date, format = "%B%d,%Y"), "%B")) %>%
  select(-c(address, lat, long, date)) %>%
  group_by(month) %>%
  summarize(average_death = mean(num_killed),
            average_injuries = mean(num_injured)) %>%
  rename(Month = month, AvgDeath = average_death,
         AvgInjuries = average_injuries) %>%
  arrange(-AvgDeath)


# Description of a particular incident: shooting in Seattle
one_incident <- data_df %>%
  filter(state == "Washington" & city == "Seattle (Skyway)")

# Interactive Map

map <- leaflet(data_df) %>%
  addProviderTiles("CartoDB.Positron") %>%
  setView(lat = 37, lng = -97, zoom = 3.5) %>%
  addCircles(
    lat = ~lat,
    lng = ~long,
    radius = (data_df$num_killed + data_df$num_injured) * 3000,
    stroke = F,
    popup = ~ paste(
      "Deaths:", num_killed, "<br>",
      "Injuries:", num_injured, "<br>",
      "Location:", address
    )
  )

#2 plots: most shootings by month and day most number of people died

# Most shootings by month
most_shootings_month <- data_df %>%
  mutate(month = format(as.Date(date, format = "%B%d,%Y"), "%b")) %>%
  group_by(month) %>%
  summarise(total = length(month))

months <- data_df %>%
  mutate(month = format(as.Date(date, format = "%B%d,%Y"), "%B")) %>%
  group_by(month) %>%
  summarise(total = length(month)) %>%
  filter(total == max(total)) %>%
  pull(month)

most_shootings_month_g <- ggplot(most_shootings_month) +
  geom_col(aes(x = month, y = total)) +
  ggtitle("Most Dangerous Month in 2018 by Total Casualties") +
  xlab("Months") +
  ylab("Total Casualties (killed and injuries)")

most_shootings_month_g <- ggplotly(most_shootings_month_g)

# What days did the most number of people die in the month of June(months)?
most_killed_day <- data_df %>%
  mutate(day = format(as.Date(date, format = "%B%d,%Y"), "%A"),
         month = format(as.Date(date, format = "%B%d,%Y"), "%B")) %>%
  filter(month == months) %>%
  group_by(day) %>%
  summarize(total_killed = sum(num_killed))


most_killed_day_g <- ggplot(most_killed_day) +
  geom_col(aes(x = day, y = total_killed)) +
  ggtitle(paste("Number of Deaths in the month of", months, "by day")) +
  xlab("Days of the Week") +
  ylab("Number of people killed")

most_killed_day_g <- ggplotly(most_killed_day_g)

