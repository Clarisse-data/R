library(ggplot2)
library(dplyr)
library(summarytools)
library(reshape2)

#---

# Format Date column into Date data type
Walmart...Original$Date <- as.Date(Walmart...Original$Date, format="%d-%m-%Y") 

#---

# Statistical values and checking presence of null values

dfSummary(Walmart...Original)
summary(is.na(Walmart...Original))

#---

# Correlations of key columns
cor_matrix <- Walmart...Original %>%
  select(Weekly_Sales, Temperature, Fuel_Price, CPI, Unemployment, Holiday_Flag) %>%
  cor()
print(cor_matrix)

# Melt the correlation matrix
melted_cor <- melt(cor_matrix)

# Heatmap of the correlation matrix
ggplot(melted_cor, aes(x = Var1, y = Var2, fill = value)) +
  geom_tile() +
  scale_fill_gradient2(low = "blue", high = "red", mid = "white", midpoint = 0) +
  labs(title = "Heatmap of Correlation Matrix", x = "Variable", y = "Variable") +
  theme_minimal()

#---

# Group by Store and calculate average weekly sales
average_sales_by_store <- Walmart...Original %>%
  group_by(Store) %>%
  summarise(Average_Weekly_Sales = mean(Weekly_Sales, na.rm = TRUE))

# Order from lowest to highest
average_sales_by_store <- average_sales_by_store %>%
  arrange(Average_Weekly_Sales)

# Create a bar chart of average weekly sales by store
ggplot(data = average_sales_by_store, aes(x = Average_Weekly_Sales, y = factor(Store, levels = Store), fill = Average_Weekly_Sales)) +
  geom_bar(stat = "identity") +
  scale_fill_gradient2(low = "red", mid = "yellow", high = "green", midpoint = median(average_sales_by_store$Average_Weekly_Sales)) +
  labs(title = "Average Weekly Sales by Store", x = "Average Weekly sales", y = "Store Number") +
  theme(axis.text.y = element_text(angle = 0, hjust = 1))

#---

# Extract the month name from the Date column
Walmart...Original$Month <- format(Walmart...Original$Date, "%B")

# Convert Month to a factor to ensure the correct order (Jan to Dec)
Walmart...Original$Month <- factor(Walmart...Original$Month, 
                                   levels = c("January", "February", "March", "April", "May", 
                                              "June", "July", "August", "September", "October", 
                                              "November", "December"))
#---

# Group by Month and calculate total sales per month (across all years)
monthly_sales <- Walmart...Original %>%
  group_by(Month) %>%
  summarise(Total_Sales = sum(Weekly_Sales))

# Bar chart of monthly sales
ggplot(data = monthly_sales, aes(x = Month, y = Total_Sales, group = 1)) +
  geom_line() +
  geom_point() +  # Optional: Adds points to the line
  labs(title = "Total Sales by Month (Accumulated across Years)", x = "Month", y = "Total Sales") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_y_continuous(labels = scales::comma)

#---

# Year column
Walmart...Original$Year <- format(Walmart...Original$Date, "%Y")

# Group by Year and Month
monthly_sales_by_year <- Walmart...Original %>%
  group_by(Year, Month) %>%
  summarise(Total_Sales = sum(Weekly_Sales))

# Line graph for each year
ggplot(data = monthly_sales_by_year, aes(x = Month, y = Total_Sales, color = Year, group = Year)) +
  geom_line() +
  geom_point() +
  labs(title = "Monthly Sales Over Years", x = "Month", y = "Total Sales") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_y_continuous(labels = scales::comma)

#---

# Aggregating monthly sales by store
monthly_sales_by_store <- Walmart...Original %>%
  group_by(Store, Month) %>%
  summarise(Monthly_Sales = sum(Weekly_Sales, na.rm = TRUE))

# Heatmap of Monthly Sales by Store
ggplot(monthly_sales_by_store, aes(x = Month, y = factor(Store), fill = Monthly_Sales)) +
  geom_tile() +
  scale_fill_gradient(low = "white", high = "blue") +
  labs(title = "Monthly Sales by Store", x = "Month", y = "Store Number") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

#---

# Scatter graph of Weekly Sales vs. Unemployment
ggplot(Walmart...Original, aes(x = Unemployment, y = Weekly_Sales, color = Weekly_Sales)) +
  geom_point() + 
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm", color = "orangered") +
  labs(title = "Weekly Sales vs. Unemployment with Regression Line", x = "Unemployment", y = "Weekly Sales") +
  theme_minimal() +
  scale_y_continuous(labels = scales::comma)


#---

# Scatter graph of Weekly Sales and Temperature
ggplot(data=Walmart...Original, aes(x = Temperature, y = Weekly_Sales, color = Weekly_Sales)) + 
  geom_point() + 
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm", color = "orangered") +
  labs(title = "Weekly Sales vs Temperature with Regression Line") +
  theme_minimal() +
  scale_y_continuous(labels = scales::comma)

#--

# Histogram of Fuel Prices
ggplot(Walmart...Original, aes(x = Fuel_Price)) +
  geom_histogram(binwidth = 0.1, fill = "purple", color = "black", alpha = 0.7) +
  labs(title = "Distribution of Fuel Prices", x = "Fuel Price ($)", y = "Frequency") +
  theme_minimal() +
  scale_x_continuous(labels = scales::comma)

# Histogram of Weekly Sales
ggplot(Walmart...Original, aes(x = Weekly_Sales)) +
  geom_histogram(binwidth = 50000, fill = "blue", color = "black", alpha = 0.7) +
  labs(title = "Distribution of Weekly Sales", x = "Weekly Sales", y = "Frequency") +
  theme_minimal() +
  scale_x_continuous(labels = scales::comma)

#---

# CPI minimum, median and maximum
cpi_stats <- Walmart...Original %>%
  summarise(
    Min = min(CPI, na.rm = TRUE),
    Median = median(CPI, na.rm = TRUE),
    Max = max(CPI, na.rm = TRUE))

# Box plot of CPI with labels for min, median, and max
ggplot(Walmart...Original, aes(x = "", y = CPI)) +
  geom_boxplot(fill = "orchid", color = "black") +
  geom_text(data = cpi_stats, aes(x = 1, y = Min, label = paste("Min:", round(Min, 2))), vjust = -0.5, color = "navyblue") +
  geom_text(data = cpi_stats, aes(x = 1, y = Median, label = paste("Median:", round(Median, 2))), vjust = -0.5, color = "black") +
  geom_text(data = cpi_stats, aes(x = 1, y = Max, label = paste("Max:", round(Max, 2))), vjust = -0.5, color = "navyblue") +
  labs(title = "Box Plot of Consumer Price Index (CPI)", x = "", y = "CPI") +
  theme_minimal()
