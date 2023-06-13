library(dplyr)
library(dplyr)
library(dplyr)
library(readxl)

print("NA value Check")

sum(is.na(transcation_data))

sum(is.na(purchase_data))

summary_stats <- purchase_data %>%
  group_by(LIFESTAGE, PREMIUM_CUSTOMER) %>%
  summarise(count = n())

# Print the summary statistics
print(summary_stats)

#transaction Datat
print("Transaction Data")

str(transcation_data)
summary(transcation_data)

summary_stats2 <- transcation_data %>%
  summarise(
    total_sales = sum(TOT_SALES),
    total_quantity = sum(PROD_QTY),
    num_transactions = n(),
    avg_sales_per_transaction = mean(TOT_SALES),
    top_selling_products = paste(unique(PROD_NAME), collapse = ", ")
  )

# Print the summary statistics
print(summary_stats2)

print("outlier detection")
# Identify outliers using the IQR method
outliers <- transcation_data %>%
  filter(TOT_SALES > quantile(TOT_SALES, 0.75) + 1.5 * IQR(TOT_SALES) |
           TOT_SALES < quantile(TOT_SALES, 0.25) - 1.5 * IQR(TOT_SALES))

# Print the outliers
print(outliers)

print("removing")

q1 <- quantile(transcation_data$TOT_SALES, 0.25)
q3 <- quantile(transcation_data$TOT_SALES, 0.75)
iqr <- q3 - q1
lower_threshold <- q1 - 1.5 * iqr
upper_threshold <- q3 + 1.5 * iqr

# Remove outliers
transaction_data <- transcation_data %>%
  filter(TOT_SALES >= lower_threshold, TOT_SALES <= upper_threshold)

View(transaction_data)

#MUTATE 

transaction_data <- transaction_data %>%
  mutate(
    PACK_SIZE = as.numeric(gsub("[^0-9]", "", PROD_NAME)),
    BRAND_NAME = gsub("[0-9]", "", PROD_NAME)
  )

# Print the updated dataset
View(transaction_data)

#Merge datasets

transaction_data <- merge(transaction_data, purchase_data, by = "LYLTY_CARD_NBR")

#required matrices
metrics <- transaction_data %>%
  group_by(LIFESTAGE, PREMIUM_CUSTOMER) %>%
  summarise(
    total_spending = sum(TOT_SALES),
    average_spending = mean(TOT_SALES),
    total_quantity = sum(PROD_QTY),
    average_price_per_chip = sum(TOT_SALES) / sum(PROD_QTY),
    purchase_frequency = n(),
    top_brand = names(which.max(table(BRAND_NAME))),
    top_pack_size = names(which.max(table(PACK_SIZE)))
  )
install.packages(tinytex)# Print the metrics
print(metrics)


#TASK 2

library(ggplot2)
total_revenue <- sum(transaction_data$TOT_SALES)

# Print the total sales revenue
print(total_revenue)

plot <- ggplot() +
  geom_bar(data = data.frame(total_revenue), aes(x = "", y = total_revenue), stat = "identity", fill = "red") +
  labs(x = "", y = "Total Sales Revenue") +
  ggtitle("Total Sales Revenue") +
  theme_minimal()

# Print the bar plot
print(plot)

# Calculate total number of customers
total_customers <- transaction_data %>%
  group_by(STORE_NBR) %>%
  summarise(total_customers = n_distinct(LYLTY_CARD_NBR))

# Calculate average number of transactions per customer
avg_transactions_per_customer <- transaction_data %>%
  group_by(STORE_NBR) %>%
  summarise(avg_transactions_per_customer = n()/n_distinct(LYLTY_CARD_NBR))

# Merge the metrics into a single dataset
metrics <- merge(total_customers, avg_transactions_per_customer, by = "STORE_NBR")

# Create a grouped bar plot
plot <- ggplot(metrics, aes(x = factor(STORE_NBR))) +
  geom_bar(aes(y = total_customers, fill = "Total Customers"), stat = "identity", position = "dodge") +
  geom_bar(aes(y = avg_transactions_per_customer, fill = "Avg Transactions per Customer"), stat = "identity", position = "dodge") +
  labs(x = "Store Number", y = "Count") +
  ggtitle("Total Customers and Avg Transactions per Customer") +
  scale_fill_manual(values = c("Total Customers" = "blue", "Avg Transactions per Customer" = "green")) +
  theme_minimal() +
  theme(legend.position = "top")

# Print the bar plot
print(plot)

print(total_customers)
print(avg_transactions_per_customer)

calculate_magnitude_distance <- function(trial_store, control_stores, data) {
  # Subset the data for the trial store and control stores
  trial_data <- subset(data, STORE_NBR == trial_store)
  control_data <- subset(data, STORE_NBR %in% control_stores)
  
  # Calculate the total sales for the trial store and control stores
  trial_sales <- sum(trial_data$TOT_SALES)
  control_sales <- aggregate(data$TOT_SALES, by = list(data$STORE_NBR), sum)$x
  
  # Calculate the observed distance for the trial store
  observed_distance <- abs(trial_sales - control_sales)
  
  # Calculate the minimum and maximum distances among the control stores
  min_distance <- min(abs(control_sales - trial_sales))
  max_distance <- max(abs(control_sales - trial_sales))
  
  # Calculate the magnitude distance measure
  magnitude_distance <- 1 - (observed_distance - min_distance) / (max_distance - min_distance)
  
  # Return the magnitude distance measure
  return(magnitude_distance)
}

# Example usage of the function
trial_store <- 77
control_stores <- c(86, 77, 88)
data <- transaction_data 

magnitude_distance <- calculate_magnitude_distance(trial_store, control_stores, data)
print(magnitude_distance)

analyze_trial_and_control <- function(trial_store, control_store, data) {
  # Subset the data for the trial store and control store during the trial period
  trial_data <- subset(data, STORE_NBR == trial_store)
  control_data <- subset(data, STORE_NBR == control_store)
  
  # Calculate the total sales for the trial store and control store during the trial period
  trial_total_sales <- sum(trial_data$TOT_SALES)
  control_total_sales <- sum(control_data$TOT_SALES)
  
  # Perform a t-test to compare total sales between trial store and control store
  t_test_result <- t.test(trial_data$TOT_SALES, control_data$TOT_SALES)
  
  # Calculate the metrics for trial and control stores during the trial period
  trial_num_customers <- n_distinct(trial_data$LYLTY_CARD_NBR)
  control_num_customers <- n_distinct(control_data$LYLTY_CARD_NBR)
  
  trial_avg_transactions_per_customer <- nrow(trial_data) / trial_num_customers
  control_avg_transactions_per_customer <- nrow(control_data) / control_num_customers
  
  # Create a summary dataframe
  summary_df <- data.frame(
    Store = c("Trial", "Control"),
    Total_Sales = c(trial_total_sales, control_total_sales),
    Num_Customers = c(trial_num_customers, control_num_customers),
    Avg_Transactions_per_Customer = c(trial_avg_transactions_per_customer, control_avg_transactions_per_customer)
  )
  
  # Print the t-test result and the summary dataframe
  print(paste("T-Test Result for Trial Store", trial_store, "and Control Store", control_store))
  print(t_test_result)
  plot_sales <- ggplot(data = summary_df, aes(x = Store, y = Total_Sales, fill = Store)) +
    geom_bar(stat = "identity", position = "dodge") +
    labs(x = "Store", y = "Total Sales", title = "Total Sales Comparison") +
    theme_minimal()
  
  # Plotting number of customers comparison
  plot_customers <- ggplot(data = summary_df, aes(x = Store, y = Num_Customers, fill = Store)) +
    geom_bar(stat = "identity", position = "dodge") +
    labs(x = "Store", y = "Number of Customers", title = "Number of Customers Comparison") +
    theme_minimal()
  
  # Plotting average transactions per customer comparison
  plot_avg_transactions <- ggplot(data = summary_df, aes(x = Store, y = Avg_Transactions_per_Customer, fill = Store)) +
    geom_bar(stat = "identity", position = "dodge") +
    labs(x = "Store", y = "Average Transactions per Customer", title = "Average Transactions per Customer Comparison") +
    theme_minimal()
  
  
  print("Summary:")
  print(summary_df)
}



# Example usage of the function
trial_store <- 77
control_store <- 86
data <- transaction_data  # Replace with your actual data file

analyze_trial_and_control(trial_store, control_store, data)
analyze_trial_and_control <- function(trial_store, control_store, data) {
  # Subset the data for the trial store and control store during the trial period
  trial_data <- subset(data, STORE_NBR == trial_store)
  control_data <- subset(data, STORE_NBR == control_store)
  
  # Calculate the total sales for the trial store and control store during the trial period
  trial_total_sales <- sum(trial_data$TOT_SALES)
  control_total_sales <- sum(control_data$TOT_SALES)
  
  # Perform a t-test to compare total sales between trial store and control store
  t_test_result <- t.test(trial_data$TOT_SALES, control_data$TOT_SALES)
  
  # Calculate the metrics for trial and control stores during the trial period
  trial_num_customers <- n_distinct(trial_data$LYLTY_CARD_NBR)
  control_num_customers <- n_distinct(control_data$LYLTY_CARD_NBR)
  
  trial_avg_transactions_per_customer <- nrow(trial_data) / trial_num_customers
  control_avg_transactions_per_customer <- nrow(control_data) / control_num_customers
  
  # Create a summary dataframe
  summary_df <- data.frame(
    Store = c("Trial", "Control"),
    Total_Sales = c(trial_total_sales, control_total_sales),
    Num_Customers = c(trial_num_customers, control_num_customers),
    Avg_Transactions_per_Customer = c(trial_avg_transactions_per_customer, control_avg_transactions_per_customer)
  )
  
  # Print the t-test result and the summary dataframe
  print(paste("T-Test Result for Trial Store", trial_store, "and Control Store", control_store))
  print(t_test_result)
  
  print("Summary:")
  print(summary_df)
  
  # Plotting total sales comparison
  plot_sales <- ggplot(data = summary_df, aes(x = Store, y = Total_Sales, fill = Store)) +
    geom_bar(stat = "identity", position = "dodge") +
    labs(x = "Store", y = "Total Sales", title = "Total Sales Comparison") +
    theme_minimal()
  
  # Plotting number of customers comparison
  plot_customers <- ggplot(data = summary_df, aes(x = Store, y = Num_Customers, fill = Store)) +
    geom_bar(stat = "identity", position = "dodge") +
    labs(x = "Store", y = "Number of Customers", title = "Number of Customers Comparison") +
    theme_minimal()
  
  # Plotting average transactions per customer comparison
  plot_avg_transactions <- ggplot(data = summary_df, aes(x = Store, y = Avg_Transactions_per_Customer, fill = Store)) +
    geom_bar(stat = "identity", position = "dodge") +
    labs(x = "Store", y = "Average Transactions per Customer", title = "Average Transactions per Customer Comparison") +
    theme_minimal()
  
  # Print the plots
  print(plot_sales)
  print(plot_customers)
  print(plot_avg_transactions)
}

# Example usage of the function
trial_store <- 77
control_store <- 86
data <- transaction_data  # Replace with your actual data file

analyze_trial_and_control(trial_store, control_store, data)


