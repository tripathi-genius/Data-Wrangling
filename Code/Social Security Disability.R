library(tidyverse)
library(lubridate)

data <- read_csv("~/Desktop/Ex_Files_Data_Wrangling_in_R/Exercise Files/ssadisability.csv")
glimpse(data)

ssa <- pivot_longer(data = data, !Fiscal_Year, names_to = 'month', values_to = 'application')
view(ssa)
unique(ssa$month)
ssa <- ssa %>%
  separate(month, c('month', 'applicationMethod'), sep='_')
unique(ssa$month)
ssa <- ssa %>%
  mutate(month=substr(month, 1, 3))
unique(ssa$month)
unique(ssa$Fiscal_Year)
ssa <- ssa %>%
  mutate(Fiscal_Year = str_replace(Fiscal_Year, 'FY', '20'))

ssa <- ssa %>%
  mutate(month = str_to_lower(month))

ssa$date <- paste('01', ssa$month, ssa$Fiscal_Year)
unique(ssa$date)  

# ssa <- ssa %>%
#   mutate(Fiscal_Year = as.numeric(Fiscal_Year)) %>%
#   mutate(Fiscal_Year = ifelse(month(month) >= 10, Fiscal_Year - 1, Fiscal_Year)) %>%
#   mutate(date = date(paste('01', month, Fiscal_Year)))






