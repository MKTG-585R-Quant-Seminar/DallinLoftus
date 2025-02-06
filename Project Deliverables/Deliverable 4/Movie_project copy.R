
#credits.csv<- read.csv("/Users/dal/Desktop/phd_class/movie_project/Movie Project/Kaggle Movie Set/credits.csv")
#keywords.csv<- read.csv("/Users/dal/Desktop/phd_class/movie_project/Movie Project/Kaggle Movie Set/keywords.csv")
#links.csv<- read.csv("/Users/dal/Desktop/phd_class/movie_project/Movie Project/Kaggle Movie Set/links.csv")
#links_small.csv<- read.csv("/Users/dal/Desktop/phd_class/movie_project/Movie Project/Kaggle Movie Set/links_small.csv")
movies_metadata.csv<- read.csv("/Users/dal/Desktop/phd_class/movie_project/Movie Project/Kaggle Movie Set/movies_metadata.csv")
#ratings.csv<- read.csv("/Users/dal/Desktop/phd_class/movie_project/Movie Project/Kaggle Movie Set/ratings.csv")
#ratings_small.csv<- read.csv("/Users/dal/Desktop/phd_class/movie_project/Movie Project/Kaggle Movie Set/ratings_small.csv")

# Load necessary libraries
library(dplyr)
library(ggplot2)
library(tidyr)

# Step 1: Prepare the dataset and calculate summary values for each movie
movies_grouped <- movies_metadata.csv |> 
  group_by(id) |> 
  summarise(runtime = mean(runtime, na.rm = TRUE),
            revenue = sum(revenue, na.rm = TRUE),
            release_date = min(as.Date(release_date, format="%Y-%m-%d")))  # Convert release_date to Date type

# -------------------------
# Runtime Graph: Distribution of Movie Runtimes
# -------------------------
ggplot(movies_grouped, aes(x = runtime)) +
  geom_histogram(binwidth = 10, fill = "steelblue", color = "black") +
  theme_minimal() +
  labs(title = "Distribution of Movie Runtimes", 
       x = "Runtime (minutes)", 
       y = "Count")

# -------------------------
# Release Date Graph: Movies Released Over Time
# -------------------------
ggplot(movies_grouped, aes(x = release_date)) +
  geom_histogram(binwidth = 365, fill = "darkgreen", color = "black") +  # Bin by year
  theme_minimal() +
  labs(title = "Movies Released Over Time", 
       x = "Release Date", 
       y = "Number of Movies")

# -------------------------
# Revenue Graph: Distribution of Movie Revenues
# -------------------------
ggplot(movies_grouped, aes(x = revenue)) +
  geom_histogram(binwidth = 50000000, fill = "red", color = "black") +  # Smaller bins
  theme_minimal() +
  labs(title = "Distribution of Movie Revenues", 
       x = "Revenue ($)", 
       y = "Count") +
  scale_x_continuous(labels = scales::dollar_format())  # Format x-axis as currency


# Step 1: Separate the release_date into year, month, and day
movies_grouped_year <- movies_metadata.csv |> 
  filter(!is.na(release_date)) |>  # Remove rows with NA release_date
  separate(release_date, into = c("release_year", "release_month", "release_day"), 
           sep = "-", convert = TRUE)

# Step 2: View the movies_grouped_year dataframe to see the separated columns
head(movies_grouped_year)

# Step 3: Summarize the earliest release year for each movie (keeping release_year separate)
movies_grouped_year_summary <- movies_grouped_year |> 
  group_by(id) |> 
  summarise(earliest_release_year = min(release_year, na.rm = TRUE))  # Get the earliest release year per movie

# View the summary with the earliest release year for each movie
head(movies_grouped_year_summary)


# Remove rows with NA or non-finite earliest_release_year values
movies_grouped_year_summary_clean <- movies_grouped_year_summary |> 
  filter(is.finite(earliest_release_year))  # Keep only finite values

# -------------------------
# Earliest Release Year Graph: Number of Movies Released Per Year
# -------------------------
ggplot(movies_grouped_year_summary_clean, aes(x = earliest_release_year)) +
  geom_histogram(binwidth = 1, fill = "darkblue", color = "black") +  # 1-year bins
  theme_minimal() +
  labs(title = "Number of Movies Released Per Year", 
       x = "Year Released", 
       y = "Number of Movies") +
  scale_x_continuous(breaks = seq(min(movies_grouped_year_summary_clean$earliest_release_year, na.rm = TRUE), 
                                  max(movies_grouped_year_summary_clean$earliest_release_year, na.rm = TRUE), 
                                  by = 5))  # Show every 5 years on x-axis

#I will need to adjust my code. There are less overlapping movies across data sets than expected. 
#My plan is to find some more data sets especially those rich with reviews. I have one different data set 
#with a lot of review data but I need to sommehow insert a movie_id to that data set. 

#I think for now my next step is to keep finding more data and see how it can all join together.
