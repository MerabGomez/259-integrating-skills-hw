# 259 Homework - integrating skills
# For full credit, answer at least 8/10 questions
# List students working with below:

#Laura Ashlock#

install.packages("tidyverse")
install.packages("lubridate")
install.packages("DataExplorer")

library(tidyverse)
library(lubridate)
library(DataExplorer)

#> These data are drawn from the fivethirtyeight article:
#> http://fivethirtyeight.com/features/what-12-months-of-record-setting-temperatures-looks-like-across-the-u-s/
#> The directory us-weather-history contains a data file for each of 10 cities, labelled by their station name
#> Each data file contains:
#> `date` | The date of the weather record, formatted YYYY-M-D
#> `actual_mean_temp` | The measured average temperature for that day
#> `actual_min_temp` | The measured minimum temperature for that day
#> `actual_max_temp` | The measured maximum temperature for that day
#> `average_min_temp` | The average minimum temperature on that day since 1880
#> `average_max_temp` | The average maximum temperature on that day since 1880
#> `record_min_temp` | The lowest ever temperature on that day since 1880
#> `record_max_temp` | The highest ever temperature on that day since 1880
#> `record_min_temp_year` | The year that the lowest ever temperature occurred
#> `record_max_temp_year` | The year that the highest ever temperature occurred
#> `actual_precipitation` | The measured amount of rain or snow for that day
#> `average_precipitation` | The average amount of rain or snow on that day since 1880
#> `record_precipitation` | The highest amount of rain or snow on that day since 1880

stations <- c("KCLT", "KCQT", "KHOU", "KIND", "KJAX", "KMDW", "KNYC", "KPHL", "KPHX", "KSEA")
cities <- c("Charlotte", "Los Angeles", "Houston", "Indianapolis", "Jacksonville", 
            "Chicago", "New York City", "Philadelphia", "Phoenix", "Seattle")


# QUESTION 1
#> The data files are in the directory 'us-weather-history'
#> Write a function that takes each station abbreviation and reads
#> the data file and adds the station name in a column
#> Make sure the date column is a date
#> The function should return a tibble
#> Call the function "read_weather" 
#> Check by reading/glimpsing a single station's file

#Frankly, we had a lot of trouble with this homework#
#To the point of spending days trying to figure out#
#we asked Mohammed for help and this is the code he gave us but we don't even have a clue if it worked#

library(stringr)
list_of_files <- list.files(path = "us-weather-history",
                            pattern = "*.csv",
                            full.names = TRUE)
final_output <- purrr::map_df('file_names', function(x) {
  
  data <- read_csv(x)
  cbind(file_id = x, data)
  
})

read_weather <- list_of_files %>%
  set_names() %>% 
  map_dfr(
    ~ read_csv(.x, col_types = c(), col_names = T),
    .id = "Station"
  )
read_weather$date <- lubridate::ymd(read_weather$date)
read_weather$Station <- read_weather$Station %>% stringr::str_remove(pattern = "us-weather-history/") %>% stringr::str_remove(pattern = ".csv")
glimpse(read_weather)

# QUESTION 2
#> Use map_dfr() and your new function to read in all 10 stations
#> map_dfr() will take each dataframe and automatically bind them.
#> Save the resulting dataset to "ds"

ds <- map_dfr(stations,read_weather)

# QUESTION 3
#> Make a factor called "city" based on the station variable
#> (station should be the level and city should be the label)
#> Use fct_count to check that there are 365 days of data for each city 

city <- factor(x, levels = stations, labels = cities)
fct_count(ds$city)

# QUESTION 4
#> Since we're scientists, let's convert all the temperatures to C
#> Write a function to convert F to C, and then use mutate across to 
#> convert all of the temperatures, rounded to a tenth of a degree

f2c <-function(x) {
  x <- (5/9) * (x-32)
  return(x)
}

ds <- ds %>% select(contains("temp"))
ds <- ds %>% mutate(across(cols_only(contains("temp")), f2c round(x, digits = 1))

### CHECK YOUR WORK
#> At this point, your data should look like the "compiled_data.csv" file
#> in data-clean. If it isn't, read in that file to use for the remaining
#> questions so that you have the right data to work with.

compiled_data <- read.csv('data-clean/compiled_data.csv')

# QUESTION 5
#> Write a function that counts the number of extreme temperature days,
#> where the actual min or max was equal to the (i.e., set the) record min/max
#> A piped function starting with '.' is a good strategy here.
#> Group the dataset by city to see how many extreme days each city experienced,
#> and sort in descending order to show which city had the most:
#> (Seattle, 20, Charlotte 12, Phoenix 12, etc...)
#> Don't save this summary over the original dataset!

#Once again, completely lost#
#we basically made whatever code we thought would work for the remainder of the assignment but we honestly don't know if it works#
receordtemps <- .%>%
  count(actual_min_temp == record_min_temp, record_max_temp==record_max_temp)
recordtemps(compiled_data)

compiled_data %>%
  mututate(recordtemp = case_when(actual_min_temp == record_min_temp ~ 'extreme low', 
                                  actual_max_temp == record_max_temp ~ 'exteme high')
           ) %>% group_by(city, recordtemp) %>%

# QUESTION 6
#> Pull out the month from the date and make "month" a factor
#> Split the tibble by month into a list of tibbles 

compiled_data <- compiled_data %>% mutate(month=month(date))

month <- as.factor(month, levels(1:12, labels(C("January", "February", "March", "April", "May", "June", "July", "August", "September", "October", "Novemeber", "December"))))


# QUESTION 7
#> For each month, determine the correlation between the actual_precipitation
#> and the average_precipitation (across all cities), and between the actual and average mins/maxes
#> Use a for loop, and print the month along with the resulting correlation
#> Look at the documentation for the ?cor function if you've never used it before

month <- c(1:12)
compiled_data %>% 
  for i in month %>%
  group_by(i) %>%
cor(actual_precipitation, average_precipitation)

compiled_data %>%
  group_by(month) %>%
  cor(actual_precipitation, average_precipitation)

compiled_data %>%
  group_by(month) %>%
  cor(actual_min_temp, actual_max_temp)

# QUESTION 8
#> Use the Data Explorer package to plot boxplots of all of the numeric variables in the dataset
#> grouped by city, then do the same thing grouped by month. 
#> Finally, use plot_correlation to investigate correlations between the continuous variables only
#> Check the documentation for plot_correlation for an easy way to do this

num_variables <- select_if(compiled_data, is.numeric)
compiled_data %>% selec(city, num_variables) %>% plot_boxplot(by ='city')

num_variables <- select_if(compiled_data, is.numeric)
compiled_data %>% selec(month, num_variables) %>% plot_boxplot(by ='month')



# QUESTION 9
#> Create a scatterplot of actual_mean_temp (y axis) by date (x axis)
#> Use facet_wrap to make a separate plot for each city (3 columns)
#> Make the points different colors according to month

library(ggplot2)
ggplot(compiled_data, aes(x = date, y = actual_mean_temp)) +
  geom_point() +
  xlab("Date") +
  ylab("Actual Mean Temperature") +
  geom_vline(xintercept = 0) +
  geom_hline(yintercept = 0)

ggplot(compiled_data, aes(x = date, y = actual_mean_temp, color = month)) +
  geom_point() +
  facet_wrap("city", ncol = 3)+
  xlab("Date") +
  ylab("Actual Mean Temperature") +
  geom_vline(xintercept = 0) +
  geom_hline(yintercept = 0)

# QUESTION 10
#> Write a function that takes the dataset and the abbreviate month as arguments
#> and creates a scatter and line plot of actual temperature (y axis) by date (x axis)
#> Note, just add geom_line() to your ggplot call to get the lines
#> use the ggtitle() function to add the month as a title
#> The function should save the plot as "eda/month_name.png"
#> The eda folder has an example of what each plot should look like
#> Call the function in a map or loop to generate graphs for each month

plots_temp_date <- function(compiled_data, month){
 newplot <- ggplot(compiled_data, aes(x = date, y = actual_mean_temp)) +
            geom_line()  +
            geom_point() +
            xlab("Date") +
            ylab("Actual Mean Temperature") +
            geom_vline(xintercept = 0) +
            geom_hline(yintercept = 0)
 ggsave("eda/month_name.png", plot = newplot
 return(newplot)}

plots_temp_date()
ggtitle("month")

newplot <- ggplot(compiled_data, aes(x = date, y = actual_mean_temp)) + geom_point()
ggsave("eda/month_name.png", plot = newplot)

map(compiled_data, plots_temp_date)

#sorry again, this pretty bad#