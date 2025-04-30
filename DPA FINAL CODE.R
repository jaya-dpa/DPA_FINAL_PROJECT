# Load the CSV file (adjust path if needed)
df <- read.csv("~/Downloads/overall.csv")
head(df)

dim(df)

#data preprocessing
summary(df)


#data cleaning
sum(is.na(df))

#checking the missing values in target variable
sum(is.na(df$Next_Tmax))

#In remaining columns
df_without_target <- df[, names(df) != "Next_Tmax"]
colSums(is.na(df_without_target))

#Dropping nulls in target variable
df <- df[!is.na(df$Next_Tmax), ]
sum(is.na(df$Next_Tmax))  

#replacing with median
numeric_cols <- sapply(df, is.numeric)

df[numeric_cols] <- lapply(df[numeric_cols], function(x) {
  ifelse(is.na(x), median(x, na.rm = TRUE), x)
})

sum(is.na(df))  

#checking if dataset is all good
colSums(is.na(df))

#checking the size of dataset after cleaning
dim(df)

#data exploration
library(ggplot2)

# Histogram for Next_Tmax
ggplot(df, aes(x = Next_Tmax)) +
  geom_histogram(bins = 30, fill = "darkorange", color = "white", alpha = 0.8) +
  theme_minimal() +
  labs(
    title = "Distribution of Next_Tmax",
    x = "Next_Tmax (°C)",
    y = "Frequency"
  )
#histogram for Present_Tmax
hist(df$Present_Tmax,
     main = "Histogram of Present_Tmax",
     xlab = "Present_Tmax",
     col = "lightblue",
     border = "white")

#histogram for LDAPS_Tmax_lapse
hist(df$LDAPS_Tmax_lapse,
     main = "Histogram of LDAPS_Tmax_lapse",
     xlab = "LDAPS_Tmax_lapse",
     col = "lightpink",
     border = "white")

#histogram for solar radiation
# Convert Solar.radiation to numeric (in case it's not)
df$Solar.radiation <- as.numeric(as.character(df$Solar.radiation))

# Plot histogram for Solar.radiation
hist(df$Solar.radiation,
     main = "Histogram of Solar Radiation",
     xlab = "Solar Radiation",
     col = "orange",
     border = "white")


# Make sure all relevant columns are numeric
df$Solar.radiation <- as.numeric(as.character(df$Solar.radiation))

# Filter numeric columns only
numeric_df <- df[sapply(df, is.numeric)]

# Correlation matrix (only numeric features)
cor_matrix <- cor(numeric_df, use = "complete.obs")

# Correlation of predictors with Next_Tmax
cor_matrix["Next_Tmax", ]

install.packages("corrplot")
library(corrplot)
# Plot Correlation Heatmap
corrplot(cor_matrix,
         method = "color",
         type = "upper",
         tl.col = "black",
         tl.cex = 0.8,
         col = colorRampPalette(c("blue", "white", "red"))(200),
         title = "Correlation Heatmap",
         mar = c(0,0,1,0))

library(ggplot2)
# Scatterplot for Next_Tmax vs Present_Tmax
ggplot(df, aes(x = Present_Tmax, y = Next_Tmax)) +
  geom_point(color = "darkblue", alpha = 0.6) +
  theme_minimal() +
  labs(
    title = "Scatterplot of Next_Tmax vs Present_Tmax",
    x = "Present_Tmax (°C)",
    y = "Next_Tmax (°C)"
  )


# Scatterplot for Next_Tmax vs LDAPS_Tmax_lapse
ggplot(df, aes(x = LDAPS_Tmax_lapse, y = Next_Tmax)) +
  geom_point(color = "darkgreen", alpha = 0.6) +
  theme_minimal() +
  labs(
    title = "Scatterplot of Next_Tmax vs LDAPS_Tmax_lapse",
    x = "LDAPS_Tmax_lapse (°C)",
    y = "Next_Tmax (°C)"
  )

# Scatterplot for Next_Tmax vs Solar radiation
ggplot(df, aes(x = Solar.radiation, y = Next_Tmax)) +
  geom_point(color = "orange", alpha = 0.6) +
  theme_minimal() +
  labs(
    title = "Scatterplot of Next_Tmax vs Solar Radiation",
    x = "Solar Radiation",
    y = "Next_Tmax (°C)"
  )

# Scatterplot for Next_Tmax vs lat / lon (to see geographic influence)
ggplot(df, aes(x = lat, y = Next_Tmax)) +
  geom_point(color = "blue", alpha = 0.6) +
  theme_minimal() +
  labs(
    title = "Scatterplot of Next_Tmax vs Latitude",
    x = "Latitude",
    y = "Next_Tmax (°C)"
  )

ggplot(df, aes(x = lon, y = Next_Tmax)) +
  geom_point(color = "red", alpha = 0.6) +
  theme_minimal() +
  labs(
    title = "Scatterplot of Next_Tmax vs Longitude",
    x = "Longitude",
    y = "Next_Tmax (°C)"
  )

# Density plot: Next_Tmax
ggplot(df, aes(x = Next_Tmax)) +
  geom_density(fill = "orange", alpha = 0.6) +
  theme_minimal() +
  labs(title = "Density of Next_Tmax", x = "Next_Tmax (°C)", y = "Density")


# Violin plot: Next_Tmax by Latitude
ggplot(df, aes(x = as.factor(lat), y = Next_Tmax)) +
  geom_violin(fill = "lightblue") +
  theme_minimal() +
  labs(title = "Next_Tmax by Latitude", x = "Latitude", y = "Next_Tmax (°C)")

# Violin plot: Next_Tmax by Longitude
ggplot(df, aes(x = as.factor(lon), y = Next_Tmax)) +
  geom_violin(fill = "lightgreen") +
  theme_minimal() +
  labs(title = "Next_Tmax by Longitude", x = "Longitude", y = "Next_Tmax (°C)")

# Boxplot: Next_Tmax by Quantiles of Solar Radiation
# Create Solar radiation quantiles
df$Solar_rad_quantile <- cut(df$Solar.radiation, breaks = quantile(df$Solar.radiation, probs = seq(0, 1, 0.25), na.rm = TRUE), include.lowest = TRUE)
ggplot(df, aes(x = Solar_rad_quantile, y = Next_Tmax)) +
  geom_boxplot(fill = "orange") +
  theme_minimal() +
  labs(
    title = "Next_Tmax by Solar Radiation Quantiles",
    x = "Solar Radiation Quantiles",
    y = "Next_Tmax (°C)"
  )

#feature engineering
# 1. Drop unnecessary columns
df <- df[, !(names(df) %in% c("X", "LDAPS_CC2", "LDAPS_CC3", "LDAPS_CC4", 
                              "LDAPS_PPT2", "LDAPS_PPT3", "LDAPS_PPT4"))]

# Only drop columns that actually exist
drop_cols_existing <- intersect(names(df), drop_cols)
if (length(drop_cols_existing) > 0) {
  df <- df[, !(names(df) %in% drop_cols_existing)]
}

# 2. Create new features 

# (a) Temperature Range
if (all(c("Present_Tmax", "Present_Tmin") %in% names(df))) {
  df$Temp_Range <- df$Present_Tmax - df$Present_Tmin
} else {
  cat("Present_Tmax or Present_Tmin not found, Temp_Range not created.\n")
}

# (b) Humidity Range
if (all(c("LDAPS_RHmax", "LDAPS_RHmin") %in% names(df))) {
  df$Humidity_Range <- df$LDAPS_RHmax - df$LDAPS_RHmin
} else {
  cat("LDAPS_RHmax or LDAPS_RHmin not found, Humidity_Range not created.\n")
}

# (c)Normalized Solar Radiation
if ("Solar.radiation" %in% names(df)) {
  max_solar <- max(df$Solar.radiation, na.rm = TRUE)
  if (max_solar > 0) {
    df$Norm_Solar <- df$Solar.radiation / max_solar
  } else {
    cat("Max solar.radiation is 0, Norm_Solar not created.\n")
  }
} else {
  cat("Solar.radiation column not found, Norm_Solar not created.\n")
}

# (d) Humidity to Temperature Ratio
if (all(c("LDAPS_RHmax", "Present_Tmax") %in% names(df))) {
  df$Humidity_Temp_Ratio <- df$LDAPS_RHmax / (df$Present_Tmax + 1)
} else {
  cat("LDAPS_RHmax or Present_Tmax not found, Humidity_Temp_Ratio not created.\n")
}

# (e) Latitude-based Solar Adjustment
if ("lat" %in% names(df) & "Solar.radiation" %in% names(df)) {
  df$Latitude_Solar <- df$Solar.radiation * cos(df$lat * pi / 180)
} else {
  cat("lat or Solar.radiation not found, Latitude_Solar not created.\n")
}

# Check the size again
dim(df)

# Check updated structure
str(df)

#standardization
# Standardize all numeric features
df_scaled <- scale(df[sapply(df, is.numeric)])

# Check the first few rows of the standardized data
head(df_scaled)

#model building
#XG boost regression


install.packages("caret")
library(caret)
predictor_cols <- setdiff(names(df), "Next_Tmax")
X <- df[, predictor_cols]
y <- df$Next_Tmax


# Split into Training and Testing sets (80% train, 20% test)
# For reproducibility
set.seed(123)  
train_index = createDataPartition(y, p = 0.8, list = FALSE)
X_train = X[train_index, ]
X_test  = X[-train_index, ]
y_train = y[train_index]
y_test  = y[-train_index]

X_train = X_train[, sapply(X_train, is.numeric)]
X_test  = X_test[, sapply(X_test, is.numeric)]

# Convert to DMatrix (special format for XGBoost)
install.packages("xgboost")
library(xgboost)
dtrain = xgb.DMatrix(data = as.matrix(X_train), label = y_train)
dtest  = xgb.DMatrix(data = as.matrix(X_test), label = y_test)

# Train XGBoost Regressor
xgb_model = xgboost(
  data = dtrain,
  objective = "reg:squarederror",  # for regression tasks
  nrounds = 100,                   # number of boosting iterations
  max_depth = 6,                   # maximum depth of trees
  eta = 0.3,                        # learning rate
  verbose = 1
)

# Predict on test set
y_pred = predict(xgb_model, dtest)


library(Metrics)
# Evaluate model performance
rmse_value = rmse(y_test, y_pred)
mae_value  = mae(y_test, y_pred)

cat("RMSE on Test Set:", rmse_value, "\n")
cat("MAE on Test Set:", mae_value, "\n")


# Calculate R-squared manually
rss = sum((y_test - y_pred)^2)   # residual sum of squares
tss = sum((y_test - mean(y_test))^2)  # total sum of squares
r_squared = 1 - rss/tss
cat("R-squared on Test Set:", r_squared, "\n")

plot(y_test, y_pred,
     xlab = "Actual Next_Tmax",
     ylab = "Predicted Next_Tmax",
     main = "Actual vs Predicted Next_Tmax",
     col = "darkblue", pch = 16)
abline(0, 1, col = "red", lwd = 2)  # Ideal line
grid()

# 5-fold cross-validation
library(xgboost)
cv_model = xgb.cv(
  data = dtrain,
  nfold = 5,
  nrounds = 100,
  objective = "reg:squarederror",
  early_stopping_rounds = 10,
  verbose = 1
)

# Best RMSE from cross-validation
print(cv_model$evaluation_log)


#random forest
# Install necessary packages if not already installed
install.packages("randomForest")
library(randomForest)
install.packages("Metrics")
library(Metrics)
install.packages("caret")
library(caret)

# Split the dataset into training and testing sets (80% train, 20% test)
set.seed(123)  # For reproducibility
train_index = createDataPartition(y, p = 0.8, list = FALSE)
X_train = X[train_index, ]
X_test  = X[-train_index, ]
y_train = y[train_index]
y_test  = y[-train_index]

# Train Random Forest Regression model
rf_model <- randomForest(x = X_train, y = y_train, ntree = 100, mtry = 4, importance = TRUE)

# Make predictions on the test set
y_pred <- predict(rf_model, X_test)

# Evaluate model performance
rmse_value <- rmse(y_test, y_pred)
mae_value  <- mae(y_test, y_pred)

cat("RMSE on Test Set:", rmse_value, "\n")
cat("MAE on Test Set:", mae_value, "\n")

# Calculate R-squared manually
rss = sum((y_test - y_pred)^2)   # Residual Sum of Squares
tss = sum((y_test - mean(y_test))^2)  # Total Sum of Squares
r_squared = 1 - rss / tss
cat("R-squared on Test Set:", r_squared, "\n")

# Plot Actual vs Predicted values
plot(y_test, y_pred,
     xlab = "Actual Next_Tmax",
     ylab = "Predicted Next_Tmax",
     main = "Actual vs Predicted Next_Tmax",
     col = "blue", pch = 16)
abline(0, 1, col = "red", lwd = 2)  # Ideal line
grid()



