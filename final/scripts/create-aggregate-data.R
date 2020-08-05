library("dplyr")
library("knitr")

data <- read.csv("data/insitution-data-cleaned.csv", stringsAsFactors = FALSE)


#Variuos aggregations based on faculty salary ranges

get_salary_range_data <- function(data) {
  #get quartiles of faculty salaries
  salary_levels <- data %>%
    select(AVGFACSAL) %>%
    filter(AVGFACSAL != "NULL") %>%
    summarize(
      q1 = as.numeric(quantile(as.numeric(AVGFACSAL), na.rm = TRUE)[2]),
      q2 = as.numeric(quantile(as.numeric(AVGFACSAL), na.rm = TRUE)[3]),
      q3 = as.numeric(quantile(as.numeric(AVGFACSAL), na.rm = TRUE)[4])
    )
  #returns quartile given a salary
  get_salary_level <- function(salary) {
    if (salary < salary_levels$q1) {
      return(1)
    } else if (salary < salary_levels$q2) {
      return(2)
    } else if (salary < salary_levels$q3) {
      return(3)
    } else {
      return(4)
    }
  }
  #create table (grouping by salary)
  salary_range_agg <- data %>%
    filter(AVGFACSAL != "NULL", COST != "NULL", ADM_RATE != "NULL",
           GRAD_DEBT_MDN != "NULL", GRAD_DEBT_MDN != "PrivacySuppressed") %>%
    mutate(
      salary_quartile = sapply(AVGFACSAL, get_salary_level)
    ) %>%
    group_by(salary_quartile) %>%
    summarize (
      avg_cost = mean(as.numeric(COST)),
      avg_adm_rate = mean(as.numeric(ADM_RATE)),
      avg_med_debt = mean(as.numeric(GRAD_DEBT_MDN))
    ) %>%
    arrange(salary_quartile)
  return(salary_range_agg)
}


salary_range_data <- get_salary_range_data(data)






#Average tuition cost by state
avg_tuition_state <- data %>%
  filter(COST != "NULL") %>%
  group_by(STATE) %>%
  summarize(
    avg_cost = mean(as.numeric(COST), na.rm = TRUE)
  ) %>%
  select(STATE, avg_cost)

#Average tuition cost by city
avg_tuition_city <- data %>%
  filter(COST != "NULL") %>%
  group_by(CITY) %>%
  summarize(
    avg_cost = mean(as.numeric(COST), na.rm = TRUE)
  ) %>%
  select(CITY, avg_cost)

#Average cost with online option vs no online option
online <- data %>%
  filter(COST != "NULL", ONLINE != "NULL") %>%
  group_by(ONLINE) %>%
  summarize(
    avg_cost = mean(as.numeric(COST), na.rm = TRUE)
  ) %>%
  select(ONLINE, avg_cost)

#Average cost based on type (public, private, non-profit, private for-profit)
type <- data %>%
  filter(COST != "NULL", TYPE != "NULL") %>%
  group_by(TYPE) %>%
  summarize(
    avg_cost = mean(as.numeric(COST), na.rm = TRUE)
  ) %>%
  select(TYPE, avg_cost)





  




  