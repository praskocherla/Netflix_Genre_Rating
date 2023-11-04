# Load the required packages
library(dummies)
library(ggplot2)

# Load the dataset
data <- read.csv("https://query.data.world/s/x5f5k5pmtbw5wj5jwjxmfz6xkh65no")

# Create dummy variables for the categorical variable 'rating'
dummy_rating <- dummy.data.frame(data$rating, sep = "_")

# Combine the original dataset with the dummy variables
data_dummy <- cbind(data, dummy_rating)

# Remove the original categorical variable
data_dummy$rating <- NULL

# Run the regression line for the dependent variable 'release_year'
model <- lm(release_year ~., data = data_dummy)
summary(model)
# Create a scatterplot with multiple regression lines
ggplot(data_dummy, aes(x = rating_PG_13, y = release_year, color = rating)) + 
  geom_point() + 
  geom_smooth(method = "lm", se = FALSE)
# Create separate regression lines for each subset
models <- lapply(unique(data$rating), function(x) {
  subset_data <- subset(data_dummy, rating == x)
  lm(release_year ~., data = subset_data)
})
ggplot(netflix_data, aes(x = release_year, y = imdb_rating, color = rating)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  labs(title = "Relationship between Release Year, IMDB Rating, and Rating",
       x = "Release Year",
       y = "IMDB Rating",
       color = "Rating")
ggplot(netflix_data, aes(x = rating, y = user_rating_score)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  labs(x = "Rating", y = "User Rating", 
       title = "Scatter plot with regression line") +
  theme_bw()
