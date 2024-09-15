# Install and load packages
if (!require("pacman")) install.packages("pacman")

# Load required packages using pacman
pacman::p_load(
  readr,       # For reading data
  dplyr,       # For data manipulation
  tidyr,       # For data tidying
  stringr,     # For string manipulation
  ggplot2,     # For plotting
  gridExtra,   # For grid layouts in plots
  rsample,     # For data splitting
  recipes,     # For preprocessing data
  parsnip,     # For machine learning models
  workflows,   # For workflow management
  dials,       # For tuning parameters
  tune,        # For hyperparameter tuning
  yardstick,   # For model metrics
  rstatix      # For statistical tests and summaries
)

# Preview the data
df <- read.csv("C:\\Users\\Rohan\\Documents\\week11_gstore.csv")
str(df) # Check the structure of the data

# Ensure df exists and check for multiple columns
required_columns <- c("channelGrouping", "transactionRevenue", "pageviews", "hits", "newVisits",
                      "operatingSystem", "deviceCategory", "campaign", "timeOnSite", "visit_wday", "visit_hour")

if (all(required_columns %in% colnames(df))) {
  
  # Ensure transactionRevenue is numeric
  df$transactionRevenue <- as.numeric(as.character(df$transactionRevenue))
  
  # Ensure pageviews is numeric
  df$pageviews <- as.numeric(as.character(df$pageviews))
  
  # Ensure hits is numeric
  df$hits <- as.numeric(as.character(df$hits))
  
  # Ensure visit_wday is numeric
  df$visit_wday <- as.numeric(as.character(df$hits))
  
  # Ensure visit_hour is numeric
  df$visit_hour <- as.numeric(as.character(df$hits))
  
  # Ensure timeOnSite is numeric
  df$hits <- as.numeric(as.character(df$hits)) }  


# Plot mean transaction revenue by channelGrouping
df %>%
  group_by(channelGrouping) %>%
  summarise(Revenue_mean = mean(transactionRevenue, na.rm = TRUE)) %>%
  ggplot(aes(x = Revenue_mean, y = reorder(channelGrouping, Revenue_mean))) +
  geom_point(colour = "red", size = 3) +
  geom_text(aes(label = round(Revenue_mean, 2)),
            hjust = -0.2,                         
            color = "black")                      
labs(title = "Transaction Revenue (USD)",
     x = "Mean Transaction Revenue",
     y = "Channel Grouping") +
  theme_classic() +
  coord_cartesian(xlim = c(0, NA))




# Define custom colors
custom_colors <- c("desktop" = "skyblue", "mobile" = "salmon", "tablet" = "lightgreen")

# Plot mean transaction revenue by device category with custom colors
df %>%
  group_by(deviceCategory) %>%
  summarise(Revenue_mean = mean(transactionRevenue, na.rm = TRUE)) %>%
  ggplot(aes(x = reorder(deviceCategory, Revenue_mean), y = Revenue_mean, fill = deviceCategory)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = scales::dollar(Revenue_mean)),  # Label with actual revenue numbers formatted as dollars
            vjust = -0.5,                               # Adjust vertical position of labels
            color = "black")                            # Set label color
labs(title = "Transaction Revenue (USD) by Device Category",
     x = "Device Category",
     y = "Mean Transaction Revenue") +
  theme_classic() +
  coord_flip() +  # Optional: Flip coordinates to make category names readable
  scale_fill_manual(values = custom_colors, 
                    labels = c("Desktop", "Mobile", "Tablet"))  # Apply custom colors and labels 



# Plot mean transaction revenue by operating system
df %>%
  group_by(operatingSystem) %>%
  summarise(Revenue_mean = mean(transactionRevenue, na.rm = TRUE)) %>%
  ggplot(aes(x = Revenue_mean, y = reorder(operatingSystem, Revenue_mean))) +
  geom_point(colour = "blue", size = 3) +  # Blue points with custom size
  geom_text(aes(label = round(Revenue_mean, 2)),  # Add labels with rounded mean values
            hjust = -0.2,                        # Adjust horizontal position of the labels
            color = "black")                     # Set label color
labs(title = "Transaction Revenue (USD) by Operating System",
     x = "Mean Transaction Revenue",
     y = "Operating System") +
  theme_classic() +
  coord_cartesian(xlim = c(0, NA))  # Set x-axis limits



# Relationship between compensation and other variables (Examples)

# Density plot for transactionRevenue by channelGrouping
df_filtered <- df %>%
  group_by(channelGrouping) %>%
  filter(n() > 1)

df_filtered %>%
  ggplot(aes(x = transactionRevenue, colour = channelGrouping)) +
  geom_histogram() +
  labs(x = "Transaction Revenue", y = "Density") +
  theme_minimal() +
  theme(legend.position = "bottom")


# Density plot for transactionRevenue by deviceCategory
df_filtered <- df %>%
  group_by(deviceCategory) %>%
  filter(n() > 1)

df_filtered %>%
  ggplot(aes(x = transactionRevenue, colour = deviceCategory)) +
  geom_boxplot() +
  labs(x = "Transaction Revenue", y = "Density") +
  theme_minimal() +
  theme(legend.position = "bottom")

# Density plot for transactionRevenue by operatingSystem
df_filtered <- df %>%
  group_by(operatingSystem) %>%
  filter(n() > 1)

df_filtered %>%
  ggplot(aes(x = transactionRevenue, colour = operatingSystem)) +
  geom_boxplot() +
  labs(x = "Transaction Revenue", y = "Density") +
  theme_minimal() +
  theme(legend.position = "bottom")




# Install the corrr package
install.packages("corrr")

# Load the corrr package
library(corrr)


# Model 1: Linear regression on log(pageviews)
m1 <- lm(transactionRevenue ~ log(pageviews + 1), data = df)
summary(m1)
plot(m1)


# Ensure factors are properly set
df$channelGrouping <- as.factor(df$channelGrouping)
df$operatingSystem <- as.factor(df$operatingSystem)
df$deviceCategory <- as.factor(df$deviceCategory)

# Model 2: Linear regression on log_transactionRevenue with factors
df$log_transactionRevenue <- log(df$transactionRevenue + 1)
m2 <- lm(log_transactionRevenue ~ channelGrouping + operatingSystem + deviceCategory, data = df)
summary(m2)
plot(m2)


# Model 3: Stepwise regression
m3 <- stats::step(lm(log_transactionRevenue ~ ., data = df %>% select(-transactionRevenue)), direction = "forward")
summary(m3)
plot(m3)


# Identify high-leverage points
leverage_points <- which(hatvalues(m3) == 1)
df[leverage_points, ]  # View the rows with high leverage


# Optionally, remove rows with high-leverage points
df_no_leverage <- df[-leverage_points, ]

# Refit the model without high-leverage points
m3_no_leverage <- stats::step(lm(log_transactionRevenue ~ ., data = df_no_leverage %>% select(-transactionRevenue)), direction = "forward")
summary(m3_no_leverage)
plot(m3_no_leverage)

# Install the jtools package
install.packages("jtools")

# Load the jtools package
library(jtools)


# Correlation matrix for selected columns
df_filtered <- df %>%
  select(transactionRevenue, pageviews, hits, newVisits, timeOnSite, visit_wday, visit_hour)

# Ensure columns are numeric
if (all(sapply(df_filtered, is.numeric))) {
  df_filtered %>%
    correlate(method = "pearson") %>%
    stretch() %>%
    cor_mark_significant()
} else {
  print("Non-numeric columns detected, unable to compute correlation.")
}

# Revert plot window settings
par(mfrow = c(1, 1))