# IMPORTING REQUIRED LIBRARIES
library(tidyverse)
library(readxl)
library(dplyr)
library(ggplot2)
library(scales)

# IMPORTING THE DATASET
netflix_df <- read_excel("Netflix Dataset Latest 2021.xlsx")
head(netflix_df)


# =================EXPLORATORY DATA ANALYSIS====================
# DATA EXPLORE
dim(netflix_df)        # Data consists of 9425 records and 30 columns
names(netflix_df)      # Displays the name of all columns in the data
length(netflix_df)     # Returns number of columns in the dataset 
glimpse(netflix_df)
view(netflix_df)


# DATA CLEANING
# Dropping unnecessary columns
columns_to_drop <- c("IMDb Link", "Image", "Netflix Link","Trailer Site" ,"TMDb Trailer","Poster","Summary")
netflix_df <- netflix_df %>% select(-one_of(columns_to_drop))

names(netflix_df)
length(netflix_df)


# Handling Missing Values
# Finding total number of null values in all columns
null_counts <- colSums(is.na(netflix_df))
null_counts

# Dropped records with null value in Release Data Column
netflix_df<- netflix_df %>%
  drop_na("Release Date")

view(netflix_df)
dim(netflix_df)

# Extracting the numerical data
numerical_columns <- netflix_df %>% select_if(is.numeric)

# Extracting the categorical data
categorical_columns <- netflix_df %>% select_if(is.character)

# Replace missing values in character columns with "Not Available"
categorical_columns[is.na(categorical_columns)] <- "Not Available"
dim(categorical_columns)

# Replace missing values in numerical columns with 0
numerical_columns[is.na(numerical_columns)] <- 0
dim(numerical_columns)

# Merging categorical and numerical columns
merged_df <- data.frame(categorical_columns,numerical_columns,netflix_df$`Release Date`,netflix_df$"Netflix Release Date")
view(merged_df)  
length(merged_df)
names(merged_df)


# DATA MANIPULATION 
# Renaming Netflix_data.Release.Date. and Netflix_data.Netflix.Release.Date
merged_df<- merged_df %>%
  rename("Release.date" = "netflix_df..Release.Date.", "Netflix.Release.Date" = "netflix_df..Netflix.Release.Date.")
names(merged_df)

# Creating "Release Year" column 
merged_df$Release.date<- as.Date(merged_df$Release.date)

merged_df$Release.year <- as.numeric(format(merged_df$Release.date, "%Y"))
view(merged_df)

# Creating "Netflix Release Month" column 
merged_df$Netflix.Release.Month<- as.Date(merged_df$Netflix.Release.Date)

merged_df$Netflix.Release.Month<- as.numeric(format(merged_df$Netflix.Release.Date, "%m"))
view(merged_df)

# Creating "Netflix Release Month Name" 
merged_df$Netflix.Release.Month_name <- substr(month.name[merged_df$Netflix.Release.Month], 1, 3)
view(merged_df)

# Keeping necessary columns to use for Data Summarization and Data Visualization
# Dropping "Netflix.Release.Date", "Release.date" and Netflix.Release.Month
columns_to_drop <- c("Release.date","Netflix.Release.Date","Netflix.Release.Month")
merged_df <- merged_df%>% select(-one_of(columns_to_drop))
view(merged_df)

# DATA SUMMARIZATION
# Specify the columns you want to summarize
columns_to_summarize <- c("IMDb.Score", "Rotten.Tomatoes.Score", "Hidden.Gem.Score","Metacritic.Score")
names(merged_df)
summary(merged_df[, columns_to_summarize])


# DATA VISUALIZATION
# QUESTION 1
# COLUMN CHART BY GENRE
genre_counts <- merged_df %>%
  group_by(Genre) %>%
  summarise(Count = n()) %>%
  arrange(desc(Count)) %>%
  head(5)

ggplot(genre_counts, aes(x = reorder(Genre, -Count), y = Count, fill = Genre)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = Count), vjust = -0.5, size = 5, color = "black") +  
  labs(title = "DISTRIBUTION BY GENRE",
       x = "Genre",
       y = "Count") +
  guides(fill = FALSE) +  
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 20),  
    axis.text.x = element_text(size = 15),  
    axis.title.x = element_text(size = 15)  
  )

# QUESTION 2
# COLUMN CHART BY RUNTIME
filtered_data <- merged_df %>%
  filter(Runtime != "Not Available")

ggplot(filtered_data, aes(x = reorder(factor(Runtime), -table(Runtime)[Runtime]))) +
  geom_bar(fill = "#3e6fc1") +  
  labs(title = "Distribution of Runtimes",
       x = "Runtime",
       y = "Count") +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 20), 
    axis.title.x = element_text(size = 15),  
    axis.text.x = element_text(size = 15)  
  )

# QUESTION 3
# COLUMN CHART BY LANGUAGES
lang_counts <- merged_df %>%
  group_by(Languages) %>%
  summarise(Count = n()) %>%
  arrange(desc(Count)) %>%
  head(5)

ggplot(lang_counts, aes(x = reorder(Languages, -Count), y = Count, fill = Languages)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = Count), position = position_dodge(width = 0.8), vjust = -0.5, size = 5, color = "black") +  
  labs(title = "Content Distribution by Languages",
       x = "Languages",
       y = "Count") +
  guides(fill = FALSE) +  
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 20),  
    axis.text.x = element_text(size = 15),  
    axis.title.x = element_text(size = 15)  
  )

# QUESTION 4
# PIE CHART FOR CONTENT DISTRIBUTION
merged_df %>%
  ggplot(aes(x = "", fill = Series.or.Movie)) +
  geom_bar(width = 1, color = "black") +
  geom_text(stat='count', aes(label = paste0(round((..count..)/sum(..count..) * 100, 1), "%")),
            position = position_stack(vjust = 0.5),
            color = "black",
            size = 5) +  
  scale_fill_manual(values = c("Series" = "#FFD700", "Movie" = "#3673e3")) +
  coord_polar(theta = "y") +
  theme_void() +
  labs(title = 'Distribution of Series and Movies on Netflix',
       fill = 'Type') +
  theme(legend.position = 'top', 
        plot.title.position = "plot",  
        plot.title = element_text(hjust = 0.5, size = 20))

# CLUSTERED COLUMN CHART FOR MONTHLY PATTERN
# Converting month names to a factor with a custom order
merged_df$Netflix.Release.Month_name <- factor(
  merged_df$Netflix.Release.Month_name,
  levels = c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"),
  ordered = TRUE
)

merged_df %>%
  ggplot(aes(x = Netflix.Release.Month_name, fill = Series.or.Movie)) +
  geom_bar(position = "dodge", stat = "count") +
  theme_bw() +
  labs(x = 'Month',
       y = 'Count',
       title = 'Number of Series and Movies Released on Netflix Monthwise',
       fill = 'Type') +
  scale_fill_manual(values = c('#3673e3', '#FFD700')) +
  theme(axis.text.x = element_text(size = 15),
        plot.title = element_text(size = 20))  


# QUESTION 5
# BAR CHART FOR TOP 5 MOVIES
top5_movies <- merged_df%>%
  arrange(desc(Boxoffice)) %>%
  head(5)

ggplot(top5_movies, aes(x = reorder(Title, Boxoffice), y = Boxoffice)) +
  geom_bar(stat = "identity", fill = "#3673e3") +
  labs(title = "Top 5 Movies by Boxoffice",
       x = "Movie Title", y = NULL) +
  theme_minimal() +
  coord_flip() +  
  geom_text(aes(label = scales::dollar(Boxoffice, scale = 1e-6, suffix = "M")),
            hjust = 1.5, color = "black", size = 5) +
  theme(legend.position = "none", axis.text.y = element_text(size = 15),
        plot.title = element_text(size = 20))  


# BAR CHART FOR BOTTOM 5 MOVIES
# Filtering out movies with zero Box Office values
non_zero_movies <- merged_df[merged_df$Boxoffice > 0, ]

bottom5_movies <- non_zero_movies %>%
  arrange(Boxoffice) %>%
  head(5)

ggplot(bottom5_movies, aes(x = reorder(Title, Boxoffice), y = Boxoffice)) +
  geom_bar(stat = "identity", fill = "#e34a33") +
  labs(title = "Bottom 5 Movies by Boxoffice",
       x = "Movie Title", y = NULL) +
  theme_minimal() +
  coord_flip() +
  geom_text(aes(label = scales::dollar(Boxoffice)),
            hjust = 1.5, color = "black", size = 5) +
  theme(legend.position = "none", axis.text.y = element_text(size = 15),
        plot.title = element_text(size = 20))  


# QUESTION 6
# Clustered Column Chart of Awards.Received and Awards.Nominated 
top_movies <- merged_df %>%
  arrange(desc(Awards.Received + Awards.Nominated.For)) %>%
  head(5)

# Reshaping the data for plotting
top_movies_long <- top_movies %>%
  pivot_longer(cols = c(Awards.Received, Awards.Nominated.For),
               names_to = "AwardType", values_to = "Count")

ggplot(top_movies_long, aes(x = Title, y = Count, fill = AwardType)) +
  geom_col(position = position_dodge(width = 0.8), width = 0.7) +
  geom_text(aes(label = Count), position = position_dodge(width = 0.8), vjust = -0.5, size = 6, color = "black") +  
  labs(title = "Top 5 Movies: Awards Received and Nominated",
       x = "Movie",
       y = "Count") +
  scale_fill_manual(values = c("Awards.Received" = "#f2c235", "Awards.Nominated.For" = "#60188f"), 
                    name = "Award Type") +  
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 12),  
        plot.title = element_text(hjust = 0.5, size = 16),  
        legend.text = element_text(size = 10),  
        legend.title = element_text(size = 12))

# QUESTION 7
# COLUMN CHART BY COUNTRY
filtered_data <- merged_df[merged_df$Country.Availability != 'Not Available', ]

selected_countries <- c("India", "United States", "United Kingdom", "Japan")
filtered_data <- merged_df %>%
  filter(Country.Availability %in% selected_countries)

custom_colors <- c("#3665e1", "#fc8d62")


ggplot(filtered_data, aes(x = Country.Availability, fill = Series.or.Movie)) +
  geom_bar(position = "dodge", stat = "count") +
  geom_text(stat='count', aes(label = ..count..),
            position = position_dodge(width = 0.9),
            vjust = -0.5, size = 5, color = "black") +  
  labs(title = "Content Distribution by Country",
       x = "Country",
       y = "Count",
       fill = "Type") +
  scale_fill_manual(values = custom_colors) +  
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 20),  
    axis.text.x = element_text(size = 15),  
    axis.text.y = element_text(size = 12),  
    axis.title = element_text(size = 15)  
  )

