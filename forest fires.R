library(tidyverse)

#summary of the dataset
summary(forestfires)
glimpse(forestfires)


#columns
colnames(forestfires)


# processing the data ie converting converting relevant columns 
#to appropriate data types
forestfires %>% pull(month) %>% unique

forestfires %>% pull(day) %>% unique


monthOrder <- c("jan", "feb", "mar",
                 "apr", "may", "jun",
                 "jul", "aug", "sep",
                 "oct", "nov", "dec")

dayOrder <- c("sun", "mon", "tue", "wed", "thu", "fri", "sat")

forestfires <- forestfires %>% 
  mutate(
    month = factor(month, levels = monthOrder),
    day = factor(day, levels = dayOrder)
  )


#Create bar charts to analyze fire occurrence patterns by month and 
#day of the week

fires_by_month <- forestfires %>%
  group_by(month) %>%
  summarize(totalfires = n())

fires_by_month %>% 
  ggplot(aes(x = month, y = totalfires)) +
  geom_col() +
  labs(
    title = "Number of forest fires in data by month",
    y = "Fire count",
    x = "Month"
  )


fires_by_day <- forestfires %>%
  group_by(day) %>%
  summarize(totalfires = n())

fires_by_day %>% 
  ggplot(aes(x = day, y = totalfires)) +
  geom_col() +
  labs(
    title = "Number of forest fires in data by day of the week",
    y = "Fire count",
    x = "Day of the week"
  )

#plotting bar charts to analyze fire occurrence patterns 
#by month and day of the week
forestfires_plot <- forestfires %>% 
  plot(
    cols = c("FFMC", "DMC", "DC", 
             "ISI", "temp", "RH", 
             "wind", "rain"),
    names_to = "data_col",
    values_to = "value"
  )

forestfires_plot %>% 
  ggplot(aes(x = month, y = value)) +
  geom_boxplot() +
  facet_wrap(vars(data_col), scale = "free_y") +
  labs(
    title = "Variable changes over month",
    x = "Month",
    y = "Variable value"
  )


#Use box plots to explore relationships between environmental factors 
#and fire severity

forestfires_plot %>% 
ggplot(aes(x = value, y = area)) +
  geom_point() +
  facet_wrap(vars(data_col), scales = "free_x") +
  labs(
    title = "Relationships between other variables and area burned",
    x = "Value of column",
    y = "Area burned (hectare)"
  )

#Implement scatter plots to investigate potential outliers and their impact on the analysis

forestfires_plot %>% 
  filter(area < 300) %>% 
  ggplot(aes(x = value, y = area)) +
  geom_point() +
  facet_wrap(vars(data_col), scales = "free_x") +
  labs(
    title = "Relationships between other variables and area burned (area < 300)",
    x = "Value of column",
    y = "Area burned (hectare)"
  )
