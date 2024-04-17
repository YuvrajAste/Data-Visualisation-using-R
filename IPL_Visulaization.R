# Load necessary libraries
#library(readr)
#library(dplyr)
#library(forecast)

# Load the dataset
sales_data <- read.csv("C:/Users/astey/Downloads/Cricket_data.csv")
# Display the first few rows of the dataset
head(sales_data)     #gives the schema of the data set
nrow(sales_data)     #gives the total no of rows present in the data set
ncol(sales_data)     #gives the total no of column present in the data set
colnames(sales_data) #gives the names of all the columns
str(sales_data) #show the data is in which data type



# Code to plot the Bar chart of the number of matches played by the Teams:
library(ggplot2)
library(dplyr)
# Count matches played by each team
matches_played <- sales_data %>%
  filter(!is.na(home_team) & !is.na(away_team)) %>%
  group_by(team = home_team) %>%
  summarise(matches = n()) %>%
  bind_rows(sales_data %>%
              filter(!is.na(home_team) & !is.na(away_team)) %>%
              group_by(team = away_team) %>%
              summarise(matches = n()))
# Plot
ggplot(matches_played, aes(x = reorder(team, -matches), y = matches, fill = team)) +
  geom_bar(stat = "identity") +
  labs(title = "Matches Played by Teams", x = "Team", y = "Matches Played") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))






# Code to plot the Histogram for the total runs scored by the Teams:

# Convert runs columns to numeric
sales_data$home_runs <- as.numeric(sales_data$home_runs)
sales_data$away_runs <- as.numeric(sales_data$away_runs)

# Combine runs data
total_runs <- data.frame(team = c(sales_data$home_team, sales_data$away_team),
                         runs = c(sales_data$home_runs, sales_data$away_runs))

# Plot
ggplot(total_runs, aes(x = runs, fill = team)) +
  geom_histogram(binwidth = 50, position = "identity", alpha = 0.7) +
  labs(title = "Total Runs Scored by Teams", x = "Runs", y = "Frequency") +
  theme_minimal()




#code to plot the teams having the highest toss win percentage
# Load necessary libraries
library(ggplot2)
library(forcats)

# Load the data from the file
sales_data <- read.csv("C:/Users/astey/Downloads/Cricket_data.csv")

# Remove NA values from toss_won column
toss_winner <- na.omit(sales_data$toss_won)

# Calculate frequency of toss winners
toss_winner_freq <- table(toss_winner)

# Create data frame for plotting
plot_data <- data.frame(team = names(toss_winner_freq), frequency = as.numeric(toss_winner_freq))

# Sort data by frequency
plot_data <- plot_data[order(plot_data$frequency, decreasing = TRUE), ]

# Calculate percentages
plot_data$percentage <- round(100 * plot_data$frequency / sum(plot_data$frequency), 1)

# Plot horizontal bar chart
ggplot(plot_data, aes(x = frequency, y = fct_reorder(team, frequency), label = paste0(team, " (", percentage, "%)"))) +
  geom_col() +
  geom_text(aes(label = paste0(team, " (", percentage, "%)")), hjust = -0.1) +
  labs(title = "Teams that Won the Toss", x = "Frequency", y = "Team") +
  theme_minimal()




#Code to plot the Box Plot to plot Total Runs Scored by Innings
# Load the necessary packages
library(dplyr)
library(ggplot2)

# Assuming sales_data is your dataset containing the IPL data

# Create a long format dataframe for runs data
runs_data <- sales_data %>%
  select(home_team, away_team, home_runs, away_runs) %>%
  tidyr::pivot_longer(cols = c(home_runs, away_runs), names_to = "inning", values_to = "runs")

# Plot box plot
ggplot(runs_data, aes(x = inning, y = runs, fill = inning)) +
  geom_boxplot() +
  facet_wrap(~inning, scales = "free") +  # Facet by innings
  labs(title = "Total Runs Scored by Innings", x = "Inning", y = "Runs") +
  theme_minimal()



#Code to plot the scatter plot of Runs V/S wickets
# Filter the data for matches involving only MI, CSK, and RCB
filtered_data <- sales_data[(sales_data$home_team %in% c("MI", "CSK", "RCB") & sales_data$away_team %in% c("MI", "CSK", "RCB")), ]

# Calculate total runs and wickets for each match
filtered_data$total_runs <- filtered_data$home_runs + filtered_data$away_runs
filtered_data$total_wickets <- filtered_data$home_wickets + filtered_data$away_wickets

# Plot scatter plot of runs vs. wickets
library(ggplot2)
ggplot(filtered_data, aes(x = total_runs, y = total_wickets, color = factor(home_team))) +
  geom_point() +
  labs(title = "Runs vs. Wickets (MI, CSK, RCB Matches)", x = "Total Runs", y = "Total Wickets", color = "Home Team") +
  theme_minimal()




#Code to Plot the line chart for Run-Rate over the years 
# Load necessary libraries
library(dplyr)
library(ggplot2)

# Assuming sales_data is your dataset containing the IPL data

# Convert start_date to POSIXct format
sales_data$start_date <- as.POSIXct(sales_data$start_date)

# Calculate total runs scored in each match
sales_data$total_runs <- sales_data$home_runs + sales_data$away_runs

# Calculate total overs played in each match
sales_data$total_overs <- sales_data$home_overs + sales_data$away_overs

# Calculate run rate (runs per over) in each match
sales_data$run_rate <- sales_data$total_runs / sales_data$total_overs

# Plot line plot of run rate over time
ggplot(sales_data, aes(x = start_date, y = run_rate)) +
  geom_line() +
  labs(title = "Run Rate Over Time", x = "Date", y = "Run Rate (Runs per Over)") +
  theme_minimal()




#Code for comparison of player of the match awards 
# Load necessary libraries
library(dplyr)
library(ggplot2)

# Assuming sales_data is your dataset containing the IPL data

# List of players
players <- c("Shubman Gill", "Virat Kohli", "Rohit Sharma", "David Warner", "Jos Buttler", "MS Dhoni")

# Initialize an empty list to store POM counts
pom_counts <- list()

# Iterate over each player to count POM awards
for (player in players) {
  # Filter the dataset for matches where the player was POM
  player_pom <- sales_data %>%
    filter(pom == player)
  
  # Count the number of times the player was POM
  pom_count <- nrow(player_pom)
  
  # Store the count in the list
  pom_counts[[player]] <- pom_count
}

# Convert the list to a dataframe
pom_counts_df <- data.frame(player = names(pom_counts),
                            pom_count = unlist(pom_counts),
                            stringsAsFactors = FALSE)

# Plot the comparison
ggplot(pom_counts_df, aes(x = player, y = pom_count, fill = player)) +
  geom_bar(stat = "identity") +
  labs(title = "Comparison of Player of the Match Awards",
       x = "Player",
       y = "Number of Player of the Match Awards") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Rotate x-axis labels for better readability
