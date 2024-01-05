#Setting the path for the excel file
setwd("C:\\Users\\Owner\\Desktop\\Term4\\Big data2")

#Storing the spotify excel sheet data inside the spotify_data variable
spotify_data <- read.csv("spotify_songs.csv")

#Displaying the data.
spotify_data


#Printing the first 10 values from the data frame
head(spotify_data,10)

#Setting the printing option to maximum 150 rows
options(max.print = 150)


#Selecting first 150 rows for analysis
spotify_first_150 <- head(spotify_data, 150)

#Displaying the data with 150 rows
nrow(spotify_first_150)

#Checking for null values
is.na(spotify_first_150)

#Describing the dataset
summary(spotify_data)


#Vectors:
#1. Popularity [Number]
#2. Artist     [String]
#3. Release Year [Number]
#4. Track name  [String]
#5. Acousticness [Number]
#6. Duration  [Number]
#7. Valence [Number]

#Creating vector for popularity from column track_popularity
popularity_vector <- spotify_first_150$track_popularity
popularity_vector

#Creating vector for artist from column track_artists
artist_vector <- spotify_first_150$track_artist
artist_vector

#Creating vector for release year from column track_album_release_date
#Storing only years from the actual date inside the vector
spotify_first_150$track_album_release_date <- as.Date(spotify_first_150$track_album_release_date, format="%Y-%m-%d")
releaseyear_vector <- format(spotify_first_150$track_album_release_date, "%Y")

#Printing the release year vector which only year inside it
cat("Extracted Years:\n")
cat(releaseyear_vector, sep = "\n")

#Creating vector for artist from column playlist_genre
name_vector <- spotify_first_150$track_name
name_vector

#Creating vector for artist from column acousticness
acousticness_vector <- spotify_first_150$acousticness
acousticness_vector

#Creating vector for artist from column duration_ms
duration_vector <- spotify_first_150$duration_ms
duration_vector

#Creating vector for artist from column valence
valence_vector <- spotify_first_150$valence
valence_vector

#Matrix:

#Creating the matrix with elements having the same data type which is number
number_matrix <- cbind(popularity_vector, releaseyear_vector, acousticness_vector, duration_vector, valence_vector)
print(number_matrix)

#Dataframe

# Creating a data frame from vectors
spotify_data <- data.frame(
  Popularity = popularity_vector,
  Artist = artist_vector,
  Release_Year = as.numeric(releaseyear_vector),
  Track_Name = name_vector,
  Acousticness = acousticness_vector,
  Duration = duration_vector,
  Valence = valence_vector
)

# Print the first few rows of the data frame
head(spotify_data)

#Factor
spotify_data$Artist <- factor(spotify_data$Artist)
spotify_data$Track_Name <- factor(spotify_data$Track_Name)

head(spotify_data)

#List
factor_list <- list(
  Artist = spotify_data$Artist,
  Track_Name = spotify_data$Track_Name
)

#Printing the list of Artist and Track name
print(factor_list)

# Loading the dplyr package
library(dplyr)

#Sub setting
subset_data <- spotify_data %>% 
  select(Popularity, Artist, Track_Name, Release_Year) %>% 
  filter(Popularity > 50)

head(subset_data)

#Arrange
arrange_data <- spotify_data %>% 
  arrange(Release_Year)

head(arrange_data)

#Filtering
filtered_data <- spotify_data %>% 
  filter(Popularity > 50 & Release_Year>2015)

head(filtered_data)

#Pipe
filter_and_arrange_data <- spotify_data %>%
  select(Popularity, Artist, Track_Name, Release_Year) %>%
  filter(Popularity > 70) %>%
  arrange(Release_Year)

head(filter_and_arrange_data)

#Mutate
mutate_data <- spotify_data %>% 
  mutate(Duration_in_Minutes = Duration / 60000)

head(mutate_data)

#Rank
rank_data <- spotify_data %>% 
  mutate(Popularity_Rank = rank(Popularity))

head(rank_data)

#Installing and loading the ggplot library
install.packages("ggplot2")
library(ggplot2)

#Bar chart
#Aggregated popularity data
#Eliminating the duplicate values
#Calculating the average popularity score

agg_data <- data.frame(Artist = unique(artist_vector))
agg_data$MeanPopularity <- sapply(agg_data$Artist, function(artist) mean(popularity_vector[artist_vector == artist]))

#Plotting the histogram for Average popularity score against the Artist
ggplot(agg_data, aes(x = reorder(Artist, MeanPopularity), y = MeanPopularity)) +
  geom_bar(stat = "identity", fill = "lightgreen") +
  labs(title = "Average Popularity by Artist", x = "Artist", y = "Average Popularity Score") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


#Plot
plot_release_year <- plot(spotify_data$Release_Year, type = "p", pch = 19, col = "green",
                          main = "Release Year Plot", xlab = "Frequency", ylab = "Released Year")


#Pie Chart
spotify_data$Emotional_State <- cut(spotify_data$Valence, breaks = c(0, 0.4, 0.6, 1),
                                    labels = c("Sad", "Neutral", "Happy"))
state_counts <- table(spotify_data$Emotional_State)
pie_data <- data.frame(Emotional_State = names(state_counts), Count = as.vector(state_counts))

pie_chart <- ggplot(pie_data, aes(x = "", y = Count, fill = Emotional_State)) +
  geom_bar(stat = "identity") +
  coord_polar(theta = "y") +
  labs(title = "Distribution of Tracks by Emotional State")

print(pie_chart)

#Histogram
multi_histogram <- ggplot(spotify_data, aes(x = Popularity, y = Acousticness)) +
  geom_point(aes(color = Release_Year), alpha = 1.5) +
  labs(title = "Comparison of Popularity and Acousticness by Release Year", x = "Popularity", y = "Acousticness") +
  facet_wrap(~Release_Year, scales = "free")

multi_histogram


#Line graph
line_graph <- ggplot(spotify_data, aes(x = Release_Year, y = Popularity, label = Artist)) +
  geom_line(size = 2, color = "lightgreen") +
  geom_text(data = subset(spotify_data, Popularity > 80), aes(label = Artist), nudge_x = 0.5, nudge_y = 5, size = 3, vjust = 2, hjust = 2) +
  labs(title = "Popularity Over Release Year with Artist Labels", x = "Release Year", y = "Popularity") +
  theme_minimal() +
  theme(legend.position = "none") +
  theme(axis.text = element_text(size = 12), plot.title = element_text(size = 16), axis.title = element_text(size = 14))

line_graph


#Correlation and Heat Map
library(reshape2)
library(corrplot)

numeric_columns <- spotify_data[, c("Popularity", "Release_Year", "Acousticness", "Duration", "Valence")]
correlation_matrix <- cor(numeric_columns)
correlation_heatmap <- ggplot(melt(correlation_matrix), aes(Var1, Var2, fill = value)) +
  geom_tile() +
  scale_fill_gradient(low = "white", high = "blue") +
  labs(title = "Correlation Heatmap") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  geom_text(aes(label = round(value, 2)), vjust = 1) 

corrplot(correlation_matrix, method = "color", type = "upper", order = "hclust", tl.col = "black")

print(correlation_matrix)
correlation_heatmap





