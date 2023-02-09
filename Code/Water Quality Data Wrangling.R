library(tidyverse)
library(stringr)
library(lubridate)

water_ = read_csv("~/Desktop/Ex_Files_Data_Wrangling_in_R/Exercise Files/austinwater.csv")
glimpse(water_)

water <- water_ %>%
  select(SITE_NAME, SITE_TYPE, SAMPLE_DATE, PARAM_TYPE, PARAMETER, RESULT, UNIT)

water <- water %>%
  rename(site = SITE_NAME, type = SITE_TYPE, date = SAMPLE_DATE, param_type = PARAM_TYPE,
         parameter = PARAMETER, result = RESULT, unit = UNIT)
glimpse(water)


unique(water$parameter)
water %>%
  filter(str_detect(parameter, 'PH')) %>%
  select(parameter) %>%
  unique()

water %>%
  filter(str_detect(param_type, "Alkalinity/Hardness/pH") | 
           str_detect(param_type, "Conventionals")) %>% 
  select(param_type, parameter)
  
#You may also follow

water %>%
  filter(param_type == 'Alkalinity/Hardness/pH' | param_type == 'Conventionals') %>%
  select(param_type, parameter)
  
filtered_water_data <- water %>%
  filter(param_type == 'Alkalinity/Hardness/pH' | param_type == 'Conventionals')
#Taking a look at data for further filtration
glimpse(filtered_water_data)
unique(filtered_water_data$parameter)
filtered_water_data <- filtered_water_data %>%
  filter(parameter == 'PH' | parameter=='WATER TEMPERATURE') 

summary(filtered_water_data)

filtered_water_data <- filtered_water_data %>%
  mutate(type = as.factor(type),
         param_type = as.factor(param_type), 
         parameter = as.factor(parameter),
         unit = as.factor(unit))
summary(filtered_water_data)  
filtered_water_data <- filtered_water_data %>%
  mutate(date = mdy_hms(date))
summary(filtered_water_data)
glimpse(filtered_water_data)

filtered_water_data %>%
  filter(unit == 'Feet')
filtered_water_data <- filtered_water_data %>%
  mutate(unit = recode(unit, 'Feet' = 'Deg. Fahrenheit'))
summary(filtered_water_data)

filtered_water_data <- filtered_water_data %>%
  filter(!unit=='MG/L')

summary(filtered_water_data)

filtered_water_data <- filtered_water_data %>%
  mutate(unit = droplevels(unit))

summary(filtered_water_data)
#Checking the data for the outliers
ggplot(filtered_water_data, mapping = aes(x=date, y=result)) + 
  geom_point()
filtered_water_data %>%
  filter(result > 1000000)
filtered_water_data <- filtered_water_data %>%
  filter(result<1000000)
summary(filtered_water_data)
filtered_water_data <- filtered_water_data %>%
  filter(result < 1000)
ggplot(data = filtered_water_data, mapping = aes(x=unit, y=result)) +
  geom_boxplot()
filtered_water_data <- filtered_water_data %>%
  mutate(unit = as.character(unit))
summary(filtered_water_data)
filtered_water_data <- filtered_water_data %>%
  mutate(unit = ifelse((unit == 'Deg. Celsius' & result > 60), 'Deg. Fahrenheit', unit))
ggplot(data = filtered_water_data, mapping = aes(x=unit, y=result)) +
  geom_boxplot()
filtered_water_data <- filtered_water_data %>%
  mutate(unit = as.factor(unit))
summary(filtered_water_data)
ggplot(data = filtered_water_data, mapping = aes(x=unit, y=result)) +
  geom_boxplot()
deg_fahrenheit <- which(filtered_water_data$unit == 'Deg. Fahrenheit')
filtered_water_data$result[deg_fahrenheit] <- (filtered_water_data$result[deg_fahrenheit] - 32) * (5/9)
ggplot(filtered_water_data, mapping = aes(x=unit, y=result))+
  geom_boxplot()
filtered_water_data$unit[deg_fahrenheit] <- 'Deg. Celsius'
ggplot(filtered_water_data, mapping = aes(x=unit, y=result))+
  geom_boxplot()
summary(filtered_water_data)
filtered_water_data <- filtered_water_data %>%
  mutate(unit = droplevels(unit))
summary(filtered_water_data)

#Removing the duplicated
filtered_water_data <- filtered_water_data %>%
  select(-param_type, -unit)
summary(filtered_water_data)
dupe_check <- filtered_water_data[,-5] 
duplicates <- which(duplicated(dupe_check))
filtered_water_data <- filtered_water_data[-duplicates,]
#widening the data set to include values from both temperature and pH
water_filtered_wide <- pivot_wider(filtered_water_data, names_from = parameter, values_from = result)
water_filtered_wide <- water_filtered_wide %>%
  rename('temperature' = `WATER TEMPERATURE`, 'pH'=PH)
glimpse(water_filtered_wide)
