library(ggplot2)
library(dplyr)

movies <- read.csv("D:\\Data Sets\\23AIML052 HRIDAY MITULKUMAR PATEL - Data for repository.csv")

movies$Release_Period <- as.factor(movies$Release_Period)

summary(movies$Revenue.INR)
summary(movies$Budget.INR)

movies$Profit <- movies$Revenue.INR - movies$Budget.INR
movies$Profit_Margin <- (movies$Profit / movies$Budget.INR) * 100

revenue_trends <- movies %>%
  group_by(Release_Period) %>%
  summarise(Average_Revenue = mean(Revenue.INR., na.rm = TRUE))

ggplot(revenue_trends, aes(x = Release_Period, y = Average_Revenue)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  theme_minimal() +
  labs(title = "Average Movie Revenue by Release Period",
       x = "Release Period", y = "Average Revenue (INR)")

genre_trends <- movies %>%
  group_by(Genre) %>%
  summarise(Average_Revenue = mean(Revenue.INR., na.rm = TRUE)) %>%
  arrange(desc(Average_Revenue))

ggplot(genre_trends, aes(x = reorder(Genre, Average_Revenue), y = Average_Revenue)) +
  geom_bar(stat = "identity", fill = "coral") +
  coord_flip() +
  theme_minimal() +
  labs(title = "Average Movie Revenue by Genre",
       x = "Genre", y = "Average Revenue (INR)")

print("Conclusion: The analysis suggests that movies released in certain periods tend to generate higher revenue.
    Similarly, some genres outperform others in revenue generation. 
    This insight can help producers optimize release strategies and genre selection.")

