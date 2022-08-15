library(dplyr)
library(reshape2)
install.packages("ggplot2")

#Loading the Data
incarceration_data <- read.csv("D:/incarceration_trends.csv")
Alabama_data <- filter(incarceration_data, state == "AL")
Alabama_filter_data <- filter(Alabama_data, county_name == "Autauga County" |
                              county_name == "Baldwin County" |
                              county_name == "Barbour County" |
                              county_name == "Bibb County" |
                              county_name == "Blount County" |
                              county_name == "Bullock County") 
Alabama_Baldwin_Data <- filter(Alabama_data, county_name == "Baldwin County") 
Baldwin_pop_2018_2000 <- filter(Alabama_Baldwin_Data, year == 2018 | year == 2000)
Baldwin_tot_pop_2000 <- filter(Alabama_Baldwin_Data, year == 2000)
Baldwin_tot_pop_2018 <- filter(Alabama_Baldwin_Data, year == 2018)

#Summary Information

summary_info <- list()
summary_info$max_population_year_Baldwin <- max_population_year_Baldwin
summary_info$min_population_year_Baldwin <- min_population_year_Baldwin
summary_info$mean_male <- mean_male
summary_info$mean_female <- mean_female
summary_info$max_population_county_2000 <- max_population_county_2000
summary_info$max_population_county_2018 <- max_population_county_2018
summary_info$max_jail_pop_county_2000 <- max_jail_pop_county_2000
summary_info$max_jail_pop_county_2018 <- max_jail_pop_county_2018

print(summary_info)

#1) The maximum population of Baldwin County from 1970 to 2018 and the year that this maximum total population took place.

Alabama_Baldwin_Data <- filter(Alabama_data, county_name == "Baldwin County")Alabama_data, county_name == "Baldwin County")
max_population <- max(Alabama_Baldwin_Data$total_pop)
max_population_data_year <- filter(Alabama_Baldwin_Data, Alabama_Baldwin_Data$total_pop == max_population)
max_population_year_Baldwin <- select(max_population_data_year, year, total_pop)

#2) The minimum population of Baldwin County from 1970 to 2018 and the year that this minimum total population took place.

Alabama_Baldwin_Data <= filter(Alabama_data, county_name == "Baldwin County") 
min_population <- min(Alabama_Baldwin_Data$total_pop)
min_population_data_year <- filter(Alabama_Baldwin_Data, Alabama_Baldwin_Data$total_pop == min_population)
min_population_year_Baldwin <- select(min_population_data_year, year, total_pop)

#3) The average/mean of the male and female population in Baldwin County, Alabama.

Alabama_Baldwin_Data <- filter(Alabama_data, county_name == "Baldwin County")
mean_male <- mean(Alabama_Baldwin_Data$male_pop_15to64)
mean_female <- mean(Alabama_Baldwin_Data$female_pop_15to64)

#4)The county in Alabama that has the most total population in 2000 and 2018.

Alabama_2000_data <- filter(Alabama_data, year == 2000)
max_population_2000 <- max(Alabama_2000_data$total_pop)
max_population_data_2000 <- filter(Alabama_2000_data, Alabama_2000_data$total_pop == max_population_2000)
max_population_county_2000 <- select(max_population_data_2000, total_pop, county_name, state, year)
Alabama_2018_data <- filter(Alabama_data, year == 2018)
max_population_2018 <- max(Alabama_2018_data$total_pop)
max_population_data_2018 <- filter(Alabama_2018_data, Alabama_2018_data$total_pop == max_population_2018)
max_population_county_2018 <- select(max_population_data_2018, total_pop, county_name, state, year)

#5)The highest total jail population of the counties in Alabama in 2000 and 2018.

Alabama_2000_jail_data <- filter(Alabama_data, year == 2000)
max_jail_pop <- max(Alabama_2000_jail_data$total_jail_pop)
max_jail_pop_2000 <- filter(Alabama_2000_jail_data, Alabama_2000_jail_data$total_jail_pop == max_jail_pop)
max_jail_pop_county_2000 <- select(max_jail_pop_2000, total_jail_pop, county_name, year)
Alabama_2018_jail_data <- filter(Alabama_data, year == 2018)
max_jail_pop <- max(Alabama_2018_jail_data$total_jail_pop)
max_jail_pop_2018 <- filter(Alabama_2018_jail_data, Alabama_2018_jail_data$total_jail_pop == max_jail_pop)
max_jail_pop_county_2018 <- select(max_jail_pop_2018, total_jail_pop, county_name, year)


#Plot 1

plot1v <- ggplot(Alabama_filter_data,
                 mapping = aes(x = year, y = total_jail_pop, color = county_name)) +
  ggtitle("Incarceration rates of 5 different Counties in Alabama within 1970 to 2018")
plot1 <- plot1v +
  geom_line()
print(plot1)

#Plot 2

plot2_data <- select(Alabama_Baldwin_Data, year, total_pop, total_jail_pop)
plot2 <- ggplot(plot2_data, aes(x=year)) +
        geom_bar(aes(y="total_pop"),stat = "identity") +
        geom_bar(aes(y="total_jail_pop"))
  scale_y_continuous(
    "Total Population" , 
    sec.axis = sec_axis(~ , * 1.2, name = "Total Jailed Population")) +
  ggtitle("Relationship between the total population of Baldwin County + total incarcerated population of Baldwin County over 1970-2018")
print(plot2)

#Plot 3 (Map)
install.packages('usmap')
library(usmap)
incarceration_data <- read.csv("D:/incarceration_trends.csv")
jailed_2000_data <- filter(incarceration_data, year == 2000)
plot3 <- plot_usmap(data = jailed_2000_data, values = "total_jail_pop",
color = "red", labels = FALSE) +
  scale_fill_continuous(low = "black",high = "red",
                        name = "Total Population", label = scales::comma)+
  theme(panel.background = element_rect(color = "white")) +
  theme(legend.position = "left") +
  labs(title = "Incarceration Trends across America within the year 2000")
print(plot3)

