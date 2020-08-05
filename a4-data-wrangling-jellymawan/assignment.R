# A4 Data Wrangling

# We provide this line to delete all variables in your workspace.
# This will make it easier to test your script.
rm(list = ls())

# Loading and Exploring Data -------------------------------- (**29 points**)

# First, search online for a dplyr cheatsheet and put the link to one you
# like in the comments here (it's ok if the link makes the line too long):
# - https://dplyr.tidyverse.org/



# To begin, you'll need to download the Kickstarter Projects data from the
# Kaggle website: https://www.kaggle.com/kemical/kickstarter-projects
# Download the `ks-projects-201801.csv` file into a new folder called `data/`

# Load the `dplyr` package

library(dplyr)

# If your computer isn't in English, you made need to use this line of code
# to get the csv to load correctly (if the data gets messed up a few rows in):
# Sys.setlocale("LC_ALL", "English")

# Load your data, making sure to not interpret strings as factors.

data_df <- read.csv("data/ks-projects-201801.csv", stringsAsFactors = FALSE)

# To start, write the code to get some basic information about the dataframe:
# - What are the column names?
# - How many rows is the data frame?
# - How many columns are in the data frame?

colnames(data_df)
nrow(data_df)
ncol(data_df)

# Use the `summary` function to get some summary information

summary(data_df)

# Unfortunately, this doesn't give us a great set of insights. Let's write a
# few functions to try and do this better.
# First, let's write a function `get_col_info()` that takes as parameters a
# column name and a dataframe. If the values in the column are *numeric*,
# the function should return a list with the keys:
# - `min`: the minimum value of the column
# - `max`: the maximum value of the column
# - `mean`: the mean value of the column
# If the column is *not* numeric and there are fewer than 10 unique values in
# the column, you should return a list with the keys:
# - `n_values`: the number of unique values in the column
# - `unique_values`: a vector of each unique value in the column
# If the column is *not* numeric and there are 10 or *more* unique values in
# the column, you should return a list with the keys:
# - `n_values`: the number of unique values in the column
# - `sample_values`: a vector containing a random sample of 10 column values
# Hint: use `typeof()` to determine the column type

get_col_info <- function(col_name, data_df) {
  if (typeof(data_df[, col_name]) == "double") {
    return(list(min = min(data_df[, col_name], na.rm = TRUE),
                 max = max(data_df[, col_name], na.rm = TRUE),
                 mean = mean(data_df[, col_name], na.rm = TRUE)
                 )
            )
  }else if (length(unique(data_df[, col_name])) < 10) {
    return(list(n_values = length(unique(data_df[, col_name])),
                 unique_values = c(unique(data_df[, col_name]))))
  }else if (length(unique(data_df[, col_name])) >= 10) {
    return(list(n_values = length(unique(data_df[, col_name])),
                 sample_values = c(sample(unique(data_df[, col_name]), 10))))
  }
}

# Demonstrate that your function works by passing a column name of your choice
# and the kickstarter data to your function. Store the result in a variable
# with a meaningful name

random_main_category <- get_col_info("ID", data_df)
print(random_main_category)

# To take this one step further, write a function `get_summary_info()`,
# that takes in a data frame  and returns a *list* of information for each
# column (where the *keys* of the returned list are the column names, and the
# _values_ are the summary information returned by the `get_col_info()` function
# The suggested approach is to use the appropriate `*apply` method to
# do this, though you can write a loop
get_summary_info <- function(data_df) {
   summary_info <- sapply(colnames(data_df), get_col_info, data_df)
   summary_info
}

# Demonstrate that your function works by passing the kickstarter data
# into it and saving the result in a variable

kickstarter_data <- get_summary_info(data_df)

# Take note of 3 observations that you find interesting from this summary
# information (and/or questions that arise that want to investigate further)
# YOUR COMMENTS HERE
# LIKELY ON MULTIPLE LINES

# Asking questions of the data ----------------------------- (**29 points**)

# Write the appropriate dplyr code to answer each one of the following questions
# Make sure to return (only) the desired value of interest (e.g., use `pull()`)
# Store the result of each question in a variable with a clear + expressive name
# If there are multiple observations that meet each condition, the results
# can be in a vector. Make sure to *handle NA values* throughout!
# You should answer each question using a single statement with multiple pipe
# operations!
# Note: For questions about goals and pledged, use the usd_pledged_real
# and the usd_goal_real columns, since they standardize the currancy.


# What was the name of the project(s) with the highest goal?

highest_goal <- data_df %>%
  filter(usd_goal_real == max(usd_goal_real, na.rm = TRUE)) %>%
  select(name) %>%
  pull()

# What was the category of the project(s) with the lowest goal?

lowest_goal <- data_df %>%
  filter(usd_goal_real == min(usd_goal_real, na.rm = TRUE)) %>%
  select(category) %>%
  pull()

# How many projects had a deadline in 2018?
# Hint: start by googling "r get year from date" and then look up more about
# different functions you find

deadline_2018 <- data_df %>%
  filter(substring(deadline, 1, 4) == 2018) %>%
  nrow()
  
  

# What proportion of projects weren't marked successful (e.g., failed or live)?
# Your result can be a decimal

not_successful_prop <- data_df %>%
  filter(state != "successful") %>%
  summarise(prop = round(length(state) / nrow(data_df), 2)) %>%
  pull()

# What was the amount pledged for the project with the most backers?
pledged <- data_df %>%
  filter(backers == max(backers, na.rm = TRUE)) %>%
  select(usd_pledged_real) %>%
  pull()
# Of all of the projects that *failed*, what was the name of the project with
# the highest amount of money pledged?

highest_failed_amount <- data_df %>%
  filter(state == "failed") %>%
  filter(usd_pledged_real == max(usd_pledged_real, na.rm = TRUE)) %>%
  select(name) %>%
  pull()
# How much total money was pledged to projects that weren't marked successful?
not_successful <- data_df %>%
  filter(state != "successful") %>%
  summarise(money = sum(usd_pledged_real, na.rm = TRUE)) %>%
  pull()

# Performing analysis by *grouped* observations ----------------- (31 Points)

# Which category had the most money pledged (total)?

category_most_money <- data_df %>%
  group_by(category) %>%
  summarize(most_money = sum(usd_pledged_real, na.rm = TRUE)) %>%
  filter(most_money == max(most_money, na.rm = TRUE)) %>%
  select(category) %>%
  pull()

# Which country had the most backers?

most_backers_country <- data_df %>%
  group_by(country) %>%
  summarize(most_backers = sum(backers, na.rm = TRUE)) %>%
  filter(most_backers == max(most_backers, na.rm = TRUE)) %>%
  select(country) %>%
  pull()

# Which year had the most money pledged (hint: you may have to create a new
# column)?
# Note: To answer this question you can choose to get the year from either
# deadline or launched dates.

most_launched <- data_df %>%
  mutate(year = substring(deadline, 1, 4)) %>%
  group_by(year) %>%
  summarize(most_money = sum(usd_pledged_real)) %>%
  filter(most_money == max(most_money)) %>%
  select(year) %>%
  pull()

# Write one sentence below on why you chose deadline or launched dates to
# get the year from:
# I chose deadline because I thought that it was possible to gain more
# pledges before the deadline

# What were the top 3 main categories in 2018 (as ranked by number of backers)?

top_3 <- data_df %>%
  filter(substring(deadline, 1, 4) == 2018) %>%
  group_by(main_category) %>%
  summarize(back = sum(backers)) %>%
  arrange(-back) %>%
  top_n(3) %>%
  select(main_category) %>%
  pull()

# What was the most common day of the week on which to launch a project?
# (return the name of the day, e.g. "Sunday", "Monday"....)

most_common_dow <- data_df %>%
  mutate(day = weekdays(as.Date(launched))) %>%
  group_by(day) %>%
  summarise(count = length(day)) %>%
  filter(count == max(count)) %>%
  select(day) %>%
  pull()
  

# What was the least successful day on which to launch a project? In other
# words, which day had the lowest success rate (lowest proportion of projects
# that were marked successful )? This might require creative problem solving...
# Hint: Try googling "r summarize with condition in dplyr"

least_successful <- data_df %>%
  mutate(day = weekdays(as.Date(launched))) %>%
  group_by(day) %>%
  summarise(total = n(), success = sum(state == "successful")) %>%
  mutate(prop = success / total) %>%
  filter(prop == min(prop)) %>%
  pull(day)

