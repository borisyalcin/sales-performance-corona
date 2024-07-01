# Clear the environment
rm(list = ls())

# Load necessary libraries
library(tidyverse)
library(ggplot2)
library(keras)
library(lubridate)
library(forecast)
library(tseries)
library(dplyr)
library(corrplot)
library(plotly)


df_250 <- read.csv('C:/Users/brsle/Desktop/food_services/data/Top250.csv')
df_future <- read.csv('C:/Users/brsle/Desktop/food_services/data/Future50.csv')
df_ind <- read.csv('C:/Users/brsle/Desktop/food_services/data/Independence100.csv')

glimpse(df_250)

#Engineering Part

df_250 <- subset(df_250, select = -c(Content, Headquarters))

#NULL Checking
cat("Rows     :", nrow(df_250), "\n")
cat("Columns  :", ncol(df_250), "\n")
cat("\nFeatures :\n", names(df_250), "\n")
cat("\nMissing values : ", sum(is.na(df_250)), "\n")
cat("\nUnique values :\n")
print(sapply(df_250, function(x) length(unique(x))))

#Column Renaming
df_250 <- df_250 %>%
  rename(Branches = Units, sub_category = Segment_Category)

#Indepence Restaurants in TOP 250
rest_ind <- df_ind$Restaurant
df_250$ind_100 <- ifelse(df_250$Restaurant %in% rest_ind, 1, 0)
table(df_250$ind_100)

#Floats to %
df_250$YOY_Sales <- gsub("%", "", df_250$YOY_Sales)
df_250$YOY_Units <- gsub("%", "", df_250$YOY_Units)

#Convert the cleaned columns to numeric type
df_250$YOY_Sales <- as.numeric(df_250$YOY_Sales)
df_250$YOY_Units <- as.numeric(df_250$YOY_Units)

summary(df_250)

#Making YOY more understandable
df_250$Sales_year <- ifelse(df_250$YOY_Sales > 0, "positive", "negative")
df_250$unit_stat <- ifelse(df_250$YOY_Units > 0, "positive", "negative")

# Replace values in sub_category and create category column
df_250$category <- df_250$sub_category
df_250$category <- gsub('Quick Service & Burger', 'Burger', df_250$category)
df_250$category <- gsub('Italian/Pizza', 'Pizza', df_250$category)
df_250$category <- gsub('Quick Service & Pizza', 'Pizza', df_250$category)
df_250$category <- gsub('Bakery Cafe ', 'Cafe', df_250$category)
df_250$category <- gsub('Coffee Cafe', 'Cafe', df_250$category)
df_250$category <- gsub('Quick Service & Chicken', 'Chicken', df_250$category)
df_250$category <- gsub('Casual Dining & Pizza', 'Pizza', df_250$category)
df_250$category <- gsub('Quick Service & Cafe', 'Cafe', df_250$category)
df_250$category <- gsub('Fast Casual & Pizza', 'Pizza', df_250$category)
df_250$category <- gsub('Fast Casual & Bakery Cafe', 'Cafe', df_250$category)
df_250$category <- gsub('Quick Service & Frozen Desserts', 'Dessert', df_250$category)
df_250$category <- gsub('Quick Service & Family Casual', 'Family', df_250$category)
df_250$category <- gsub('Casual Dining & Asian', 'Asian', df_250$category)
df_250$category <- gsub('Asian/Noodle', 'Asian', df_250$category)
df_250$category <- gsub('Casual Dining & Seafood', 'Seafood', df_250$category)
df_250$category <- gsub('Bakery Cafe', 'Cafe', df_250$category)
df_250$category <- gsub('Frozen Desserts', 'Dessert', df_250$category)
df_250$category <- gsub('Family Dining & Family Style', 'Family', df_250$category)
df_250$category <- gsub('Fast Casual & Sandwich', 'Sandwich', df_250$category)
df_250$category <- gsub('Family Casual', 'Family', df_250$category)
df_250$category <- gsub('Fast Casual & Chicken', 'Chicken', df_250$category)
df_250$category <- gsub('Fast Casual & Burger', 'Burger', df_250$category)
df_250$category <- gsub('Casual Dining & Steak', 'Steak', df_250$category)
df_250$category <- gsub('Casual Dining & Sports Bar', 'Sports Bar', df_250$category)
df_250$category <- gsub('Quick Service & Mexican', 'Mexican', df_250$category)
df_250$category <- gsub('Quick Service & Seafood', 'Seafood', df_250$category)
df_250$category <- gsub('Quick Service & Sandwich', 'Sandwich', df_250$category)
df_250$category <- gsub('Seafodd', 'Seafood', df_250$category)
df_250$category <- gsub('Fine Dining & Steak', 'Steak', df_250$category)
df_250$category <- gsub('Quick Service & Bakery Cafe', 'Cafe', df_250$category)
df_250$category <- gsub('Fast Casual & Asian/Noodle', 'Asia', df_250$category)
df_250$category <- gsub('Quick Service & Snack', 'Snack', df_250$category)
df_250$category <- gsub('Fast Casual & Mexican', 'Mexican', df_250$category)
df_250$category <- gsub('Quick Service & Beverage', 'Drinks', df_250$category)
df_250$category <- gsub('Asian', 'Asia', df_250$category)
df_250$category <- gsub('Sports Bar', 'Sports Bar', df_250$category)
df_250$category <- gsub('Casual Dining & Varied Menu', 'Varied Menu', df_250$category)
df_250$category <- gsub('Steak', 'Meat', df_250$category)
df_250$category <- gsub('BBQ', 'Meat', df_250$category)
df_250$category <- gsub('Family Style', 'Family', df_250$category)

head(df_250)

#Data Visulizations

# YOY Distribution
# Define the data for the KDE plots
data <- data.frame(
  value = c(df_250$YOY_Sales, df_250$YOY_Units),
  variable = factor(rep(c('Sales', 'Units'), each = nrow(df_250)))
)

# Create the KDE plot using ggplot
ggplot(data, aes(x = value, fill = variable, color = variable)) +
  geom_density(alpha = 0.5) +
  scale_fill_manual(values = c("blue", "green")) +
  scale_color_manual(values = c("blue", "green")) +
  labs(x = "Percentage", y = "Density") +
  theme_minimal() +
  theme(legend.position = "top") +
  guides(fill = guide_legend(title = "Category")) +
  ggtitle("Kernel Density Plot of YOY Sales and Units")

#Outcome of YOY Distribution -> These distributions illustrate that the year-over-year sales percentage does not exceed 40%.

#YOY Sales Indicator
# Compute counts
count_sales_year <- table(df_250$Sales_year)
# Create pie chart
pie_chart <- plot_ly(labels = names(count_sales_year),
                     values = count_sales_year,
                     type = "pie",
                     textposition = "inside",
                     textinfo = "percent+label",
                     marker = list(colors = c("blue", "green"))) %>%
  layout(title = "Distribution of Sales Year",
         showlegend = TRUE)

# Print the pie chart
pie_chart

#Outcome of YOY Sales Indicator -> Despite being listed as top performers throughout the year, approximately 35% of restaurants exhibited negative performance indicators.

#Restaurant Category and Sub Category
# Create sunburst chart
sunburst_chart <- plot_ly(df_250, type = "sunburst", ids = ~category, labels = ~sub_category, 
                          parents = "", values = ~Sales) %>%
  layout(title = "Sunburst Chart of Sales by Category and Sub-Category")

# Print the sunburst chart
sunburst_chart

#Category and Sales Indicator
# Define custom color palette
pal <- c("#58D3F7", "#FA5858")

# Create bar plot
bar_plot <- ggplot(df_250, aes(x = category, y = Sales, fill = Sales_year)) +
  geom_bar(stat = "identity", position = "dodge") +
  scale_fill_manual(values = pal) +
  theme_minimal() +
  labs(title = "Sales Distribution by Category and Sales Year",
       x = "Category",
       y = "Sales",
       fill = "Sales Year") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 12),
        axis.title = element_text(size = 14),
        plot.title = element_text(hjust = 0.5, size = 16))

# Print the bar plot
print(bar_plot)

#Top Restaurants with Categories

# Filter for Pizza category and take top 10 by Sales
pizza <- df_250[df_250$category == "Pizza",]
pizza <- pizza[order(-pizza$Sales),][1:10,]

# Create bar plot using ggplot2
p <- ggplot(pizza, aes(x = reorder(Restaurant, -Sales), y = Sales, fill = Sales)) +
  geom_bar(stat = "identity") +
  scale_fill_viridis_c(option = "D") +
  theme_minimal() +
  labs(title = "Pizza Sales Top Restaurants",
       x = "Restaurant",
       y = "Sales") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 12),
        axis.title = element_text(size = 14),
        plot.title = element_text(hjust = 0.5, size = 16))

# Make the plot interactive using plotly
fig <- ggplotly(p)

# Display the plot
fig

# Filter for Burger category and take top 10 by Sales
Burger <- df_250[df_250$category == "Burger",]
Burger <- Burger[order(-Burger$Sales),][1:10,]

# Create bar plot using ggplot2
p <- ggplot(Burger, aes(x = reorder(Restaurant, -Sales), y = Sales, fill = Sales)) +
  geom_bar(stat = "identity") +
  scale_fill_gradientn(colors = scales::seq_gradient_pal("black", "yellow", "Lab")(seq(0,1,length.out = 100))) +
  theme_minimal() +
  labs(title = "Burger Top Restaurants",
       x = "Restaurant",
       y = "Sales") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 12),
        axis.title = element_text(size = 14),
        plot.title = element_text(hjust = 0.5, size = 16))

# Make the plot interactive using plotly
fig <- ggplotly(p)

# Display the plot
fig

#Top 50 Future Restaurants

# Display the first 10 rows of df_future
head(df_future, 10)

# FRANCHISING Y-N
# Calculate the counts of each unique value in the Franchising column
franchising_counts <- table(df_future$Franchising)
franchising_df <- as.data.frame(franchising_counts)
names(franchising_df) <- c("Franchising", "Count")

# Create pie chart using plotly
fig <- plot_ly(
  data = franchising_df,
  labels = ~Franchising,
  values = ~Count,
  type = 'pie',
  textposition = 'inside',
  textinfo = 'percent+label',
  insidetextorientation = 'radial'
) %>%
  layout(
    title = 'Franchising Or Not',
    showlegend = TRUE
  )

# Display the plot
fig

# FRANCHISING/SALES
# Remove '%' from YOY_Sales and convert to numeric
df_future$YOY_Sales <- as.numeric(gsub('%', '', df_future$YOY_Sales))

# Group by Franchising and sum YOY_Sales
franch <- df_future %>%
  group_by(Franchising) %>%
  summarise(YOY_Sales = sum(YOY_Sales, na.rm = TRUE))

# Create bar plot using plotly
fig <- plot_ly(
  data = franch,
  x = ~Franchising,
  y = ~YOY_Sales,
  type = 'bar',
  marker = list(color = c('#1f77b4', '#ff7f0e')),
  text = ~paste('Franchising:', Franchising, '<br>YOY Sales:', YOY_Sales),
  hoverinfo = 'text+y',
  color = I('yellow')
) %>%
  layout(
    title = 'Franchising Or Not',
    xaxis = list(title = 'Franchising'),
    yaxis = list(title = 'YOY Sales')
  )

# Display the plot
fig

#Correlation with YOY
# Assuming df_future is already loaded
# Assuming df_future is already loaded
numeric_cols <- names(df_future)[sapply(df_future, is.numeric)]
corr_matrix <- cor(df_future[, numeric_cols])

## Create heatmap using corrplot
corrplot(corr_matrix, method = "color", type = "upper",
         col = colorRampPalette(c("white", "blue"))(100),
         tl.col = "black", tl.srt = 45,
         title = "Correlation Matrix of Numeric Variables in df_future", mar = c(1,1,3,1))
