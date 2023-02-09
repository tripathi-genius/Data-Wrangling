library(tidyverse)

coal = read_csv("~/Desktop/Ex_Files_Data_Wrangling_in_R/Exercise Files/coal.csv") 

glimpse(coal)

coal1 <- read_csv("~/Desktop/Ex_Files_Data_Wrangling_in_R/Exercise Files/coal.csv", skip = 2)
colnames(coal1)[1] <- "Region"
glimpse(coal1)
summary(coal1)

############ Using pivot_longer() function in the data to convert it into 
############ long data set 

coal_long <- pivot_longer(coal1, !Region, names_to = "Year", values_to = "Consumption")

coal_long <- coal_long %>%
  mutate(Year = as.integer(Year))

summary(coal_long)
glimpse(coal_long)

coal_long <- coal_long %>%
  mutate(Consumption = as.numeric(Consumption))

glimpse(coal_long)

unique(coal_long$Region)

continentsOrRegions <- c('Antactica','Asia & Oceania', 'Africa', 'Middle East', 'North America', 'Eurasia', 
                'Europe', 'Central & South America', 'World')

coal_region <- coal_long %>%
  filter(Region %in% continentsOrRegions)

coal_countries <- coal_long %>%
  filter(!(Region %in% continentsOrRegions))
unique(coal_region)
unique(coal_countries)

coal_world <- coal_region %>%
  filter(Region == "World" | Region == "world")
coal_regions <- coal_region %>%
  filter(!(Region == "World" | Region == "world"))

################## Visualizing data ###################

ggplot(data=coal_region, mapping = aes(x=Year, y=Consumption)) +
  geom_point()

ggplot(data=coal_regions, mapping = aes(x=Year, y=Consumption)) + 
  geom_line(mapping = aes(color = Region))


ggplot(data = coal_countries[Region=India], mapping = aes(x=Year, y=Consumption)) +
  geom_line(mapping = aes(color=Year))














