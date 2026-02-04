
# Load the necessary libraries
library(dplyr)

# Task 1: Data Aggregation Using `aggregate` and `dplyr`

# Creating the dataframe
df1 <- data.frame(Name = c('James','Paul','Richards','Marico','Samantha','Ravi','Raghu', 'Richards','George','Ema','Samantha','Catherine'),
                  State = c('Alaska','California','Texas','North Carolina','California','Texas','Alaska','Texas','North Carolina','Alaska','California','Texas'),
                  Sales = c(14,24,31,12,13,7,9,31,18,16,18,14))

# Aggregating using aggregate
agg_result <- aggregate(df1$Sales, by = list(df1$State), FUN = sum)
print(agg_result)

# Aggregating using dplyr
dplyr_result <- df1 %>%
  group_by(State) %>%
  summarise(sum_sales = sum(Sales))
print(dplyr_result)

# Task 2: World Cup Matches Data Analysis

# Read the WorldCupMatches dataset
wc_data <- read.csv("C:\\Users\\Lenovo\\Downloads\\WorldCupMatches.csv")

# (a) Find the size of the data frame (rows and columns)
data_size <- dim(wc_data)
print(data_size)

# (b) Use summary function to report the statistical summary of your data
summary(wc_data)

# (c) Find how many unique locations World Cup matches were held at
unique_locations <- length(unique(wc_data$Location))
print(unique_locations)

# (d) Find the average attendance
avg_attendance <- mean(wc_data$Attendance, na.rm = TRUE)
print(avg_attendance)

# (e) For each Home Team, calculate the total number of goals scored
home_goals <- wc_data %>%
  group_by(`Home.Team.Name`) %>%
  summarise(total_goals = sum(`Home.Team.Goals`, na.rm = TRUE))
print(home_goals)

# (f) Average attendance for each year
avg_attendance_year <- wc_data %>%
  group_by(Year) %>%
  summarise(avg_attendance = mean(Attendance, na.rm = TRUE))
print(avg_attendance_year)

# Task 3: Metabolites Data Analysis

# Read the metabolites dataset
metabolites_data <- read.csv("C:\\Users\\Lenovo\\Downloads\\metabolite.csv")

# (a) Find how many Alzheimer's patients there are in the data set
alzheimers_patients <- sum(metabolites_data$Diagnosis == 'Alzheimer', na.rm = TRUE)
print(alzheimers_patients)

# (b) Determine the number of missing values for each column
missing_values <- colSums(is.na(metabolites_data))
print(missing_values)

# (c) Remove the rows which have missing values for the Dopamine column
metabolites_no_na_dopamine <- metabolites_data[!is.na(metabolites_data$Dopamine), ]
print(head(metabolites_no_na_dopamine))

# (d) Replace missing values in the c4-OH-Pro column with the median value
median_c4OHPro <- median(metabolites_no_na_dopamine$c4.OH.Pro, na.rm = TRUE)
metabolites_no_na_dopamine$c4.OH.Pro[is.na(metabolites_no_na_dopamine$c4.OH.Pro)] <- median_c4OHPro
print(head(metabolites_no_na_dopamine))

# (e) (Optional) Drop columns with more than 25% missing values
threshold <- 0.25 * nrow(metabolites_data)
metabolites_clean <- metabolites_data[, colSums(is.na(metabolites_data)) <= threshold]
print(head(metabolites_clean))
