#Installing necessary libraries
#install.packages("zoo")
#install.packages("depmixS4")
#install.packages("dplyr")
#install.packages("lubridate")

library(zoo)
library(depmixS4)
library(dplyr)
library(lubridate)


#Loading Data
data <- read.table("/Path/", header = TRUE, sep = ",")

####Part 1: Feature Engineering###

# Creating a variable for Date and Time
data$DateTime <- as.POSIXct(paste(data$Date, data$Time), 
                            format="%d/%m/%Y %H:%M:%S", tz = "UTC")

# Creating a new column in the data from for Date and Time
interpolated_data <- data.frame(
  DateTime = seq(min(data$DateTime),max(data$DateTime), by = "min")
)

# Use approx function to interpolate the NA values for Global active power
interpolated_values_GAP <- approx(
  x = data$DateTime,
  y = data$Global_active_power,
  xout = interpolated_data$DateTime,
  method = "linear",
  rule = 2
)

# Use approx function to interpolate the NA values for Global reactive power
interpolated_values_GRP <- approx(
  x = data$DateTime,
  y = data$Global_reactive_power,
  xout = interpolated_data$DateTime,
  method = "linear",
  rule = 2
)

# Use approx function to interpolate the NA values for Voltage
interpolated_values_Voltage <- approx(
  x = data$DateTime,
  y = data$Voltage,
  xout = interpolated_data$DateTime,
  method = "linear",
  rule = 2
)

# Global Intensity
interpolated_values_GI <- approx(
  x = data$DateTime,
  y = data$Global_intensity,
  xout = interpolated_data$DateTime,
  method = "linear",
  rule = 2
)

# Sub metering 1
interpolated_values_SM1 <- approx(
  x = data$DateTime,
  y = data$Sub_metering_1,
  xout = interpolated_data$DateTime,
  method = "linear",
  rule = 2
)

# Sub metering 2
interpolated_values_SM2 <- approx(
  x = data$DateTime,
  y = data$Sub_metering_2,
  xout = interpolated_data$DateTime,
  method = "linear",
  rule = 2
)

# Sub metering 3
interpolated_values_SM3 <- approx(
  x = data$DateTime,
  y = data$Sub_metering_3,
  xout = interpolated_data$DateTime,
  method = "linear",
  rule = 2
)


# Add the interpolated data
interpolated_data$Global_active_power <- interpolated_values_GAP$y
interpolated_data$Global_reactive_power <- interpolated_values_GRP$y
interpolated_data$Voltage <- interpolated_values_Voltage$y
interpolated_data$Global_intensity <- interpolated_values_GI$y
interpolated_data$Sub_metering_1 <- interpolated_values_SM1$y
interpolated_data$Sub_metering_2 <- interpolated_values_SM2$y
interpolated_data$Sub_metering_3 <- interpolated_values_SM3$y

# Add interpolated data back into the original data set
data$Global_active_power[is.na(data$Global_active_power)] <- interpolated_data$Global_active_power
data$Global_reactive_power[is.na(data$Global_reactive_power)] <- interpolated_data$Global_reactive_power
data$Voltage[is.na(data$Voltage)] <- interpolated_data$Voltage
data$Global_intensity[is.na(data$Global_intensity)] <- interpolated_data$Global_intensity
data$Sub_metering_1[is.na(data$Sub_metering_1)] <- interpolated_data$Sub_metering_1
data$Sub_metering_2[is.na(data$Sub_metering_2)] <- interpolated_data$Sub_metering_2
data$Sub_metering_3[is.na(data$Sub_metering_3)] <- interpolated_data$Sub_metering_3

# Separate non-numeric columns
non_numeric_cols <- c("Date", "Time")
numeric_cols <- names(data)[sapply(data, is.numeric)]

# Ensure only numeric columns are scaled
scaled_numeric_cols <- scale(data[, numeric_cols])
data[, numeric_cols] <- as.data.frame(scaled_numeric_cols)

# Divide Data into Train and Test Sets (70% training and 30% testing)
set.seed(123)
sample_size <- floor(0.7 * nrow(data))
test_size <- floor(0.3 * nrow(data))

# Create indices for train and test sets
train_indices <- sample(seq_len(nrow(data)), size = sample_size)
test_indices <- sample(seq_len(nrow(data)), size = test_size)

# Split the data into train and test
train_data <- data[train_indices, ]
test_data <- data[test_indices, ]

#checking contents
sapply(train_data, class)
scaled_train_data <- as.data.frame(train_data)

# Exclude non-numeric columns from scaled_train_data
numeric_scaled_train_data <- train_data[, sapply(train_data, is.numeric)]

# Compute the PCA on the scaled train data
pca_result <- prcomp(numeric_scaled_train_data)

# Print summary to see the proportion of variance explained
summary(pca_result)

# Graphing the PCA components
# Extract the proportion of variance
variance <- summary(pca_result)$importance[2,] * 100

# Number of principal components
num_pcs <- length(variance)

# Create a custom plot
plot(x = 1:num_pcs, y = variance, type = 'b', xlab = "Principal Component", ylab = "Percentage of Variance", main = "PCA Variance Plot")

####Part 1: HMM Training and Testing###
# Filtering of Train Data
train_data$Date <- as.POSIXlt(train_data$Date, format = "%d/%m/%Y")
train_data$Time <- as.POSIXlt(train_data$Time, format = "%H:%M:%S")
filtered_train_data <- train_data %>%
  filter(
    wday(Date) == 2,
    between(
      Time,
      as.POSIXlt("08:00:00", format = "%H:%M:%S"),
      as.POSIXlt("11:00:00", format = "%H:%M:%S") 
    )
  )

# Filtering of Test Data
test_data$Date <- as.POSIXlt(test_data$Date, format = "%d/%m/%Y")
test_data$Time <- as.POSIXlt(test_data$Time, format = "%H:%M:%S")
filtered_test_data <- test_data %>%
  filter(
    wday(Date) == 2,
    between(
      Time,
      as.POSIXlt("08:00:00", format = "%H:%M:%S"),
      as.POSIXlt("11:00:00", format = "%H:%M:%S") 
    )
  )

# Selection of specific Columns
selected_columns <- c(3, 4, 5)
train_data <- filtered_train_data[, selected_columns]
test_set <- filtered_test_data[, selected_columns]

# Range of states to test (from 4 to 24)
state_range <- 4:24

# Initialize vectors to store AIC, BIC, and log likelihood
aic_values <- numeric(length(state_range))
bic_values <- numeric(length(state_range))
logLik_values <- numeric(length(state_range))

# Initialize variables to store the best model's details
best_bic <- Inf
best_logLik <- -Inf
best_model <- NULL
best_num_states <- NA

n <- nrow(train_data)

# Loop over state_range
for (i in 1:length(state_range)) {
  num_states <- state_range[i]
  
  model <- depmix(list(
    Global_active_power ~ 1,
    Global_reactive_power ~ 1,
    Voltage ~ 1
  ), data = train_data, nstates = num_states, family = list(gaussian(), gaussian(), gaussian()))
  
  fitted_model <- fit(model)
  
  # Compute AIC, BIC, and log likelihood
  aic_values[i] <- AIC(fitted_model)
  bic_values[i] <- BIC(fitted_model)
  logLik_values[i] <- logLik(fitted_model) / n #normalizing
  
  if (bic_values[i] < best_bic && logLik_values[i] > best_logLik) {
    best_bic <- bic_values[i]
    best_logLik <- logLik_values[i]
    best_model <- fitted_model
    best_num_states <- num_states
  }
}

# Plotting the results
plot(state_range, aic_values, type="b", col="blue", xlab="Number of States", ylab="AIC")
points(state_range, bic_values, type="b", col="red")
points(state_range, logLik_values, type="b", col="green")
legend("topright", legend=c("AIC", "BIC", "Log Likelihood"), col=c("blue", "red", "green"), lty=1)

# Optional: Print the details of the best model
cat("Number of States (Best Model):", best_num_states, "\n")
cat("BIC (Best Model):", best_bic, "\n")
cat("Log Likelihood (Best Model):", best_logLik, "\n")

numState <- 12 #We found out that 12 states yealds the best results

#train model on Test data
modelTestData <- depmix(list(
  Global_active_power ~ 1,
  Global_reactive_power ~ 1,
  Voltage ~ 1
), data = test_set, nstates = numState, family = list(gaussian(), gaussian(), gaussian()))

testModel <- fit(modelTestData)

#Calculate the log-likelihoods for testing set
testLogLikelihood <- logLik(testModel)

#Normalize the log Likelihood
testLogLikelihood_normalized <- testLogLikelihood / nrow(test_set)


###Part 2: Anomaly Detection###

#loading of the anomalous data
data <- read.table("/Users/danielmac/Desktop/CMPT 318 Term Project/Datasets/Anomalous Datasets/DataWithAnomalies1.txt", header = TRUE, sep = ",")

# Creating a variable for Date and Time
data$DateTime <- as.POSIXct(paste(data$Date, data$Time), 
                            format="%d/%m/%Y %H:%M:%S", tz = "UTC")

# Creating a new column in the data from for Date and Time
interpolated_data <- data.frame(
  DateTime = seq(min(data$DateTime),max(data$DateTime), by = "min")
)

# Use approx function to interpolate the NA values for Global active power
interpolated_values_GAP <- approx(
  x = data$DateTime,
  y = data$Global_active_power,
  xout = interpolated_data$DateTime,
  method = "linear",
  rule = 2
)

# Use approx function to interpolate the NA values for Global reactive power
interpolated_values_GRP <- approx(
  x = data$DateTime,
  y = data$Global_reactive_power,
  xout = interpolated_data$DateTime,
  method = "linear",
  rule = 2
)

# Use approx function to interpolate the NA values for Voltage
interpolated_values_Voltage <- approx(
  x = data$DateTime,
  y = data$Voltage,
  xout = interpolated_data$DateTime,
  method = "linear",
  rule = 2
)

# Add the interpolated data
interpolated_data$Global_active_power <- interpolated_values_GAP$y
interpolated_data$Global_reactive_power <- interpolated_values_GRP$y
interpolated_data$Voltage <- interpolated_values_Voltage$y

# Add interpolated data back into the original data set
data$Global_active_power[is.na(data$Global_active_power)] <- interpolated_data$Global_active_power
data$Global_reactive_power[is.na(data$Global_reactive_power)] <- interpolated_data$Global_reactive_power
data$Voltage[is.na(data$Voltage)] <- interpolated_data$Voltage

# Separate non-numeric columns
non_numeric_cols <- c("Date", "Time")
numeric_cols <- names(data)[sapply(data, is.numeric)]

# Ensure only numeric columns are scaled
scaled_numeric_cols <- scale(data[, numeric_cols])
data[, numeric_cols] <- as.data.frame(scaled_numeric_cols)

set.seed(123)
sample_size <- floor(nrow(data))

# Create indices for train and test sets
train_indices <- sample(seq_len(nrow(data)), size = sample_size)
train_data <- data[train_indices, ]

# Exclude non-numeric columns from scaled_train_data
numeric_scaled_train_data <- train_data[, sapply(train_data, is.numeric)]

# Filtering of Data
train_data$Date <- as.POSIXlt(train_data$Date, format = "%d/%m/%Y")
train_data$Time <- as.POSIXlt(train_data$Time, format = "%H:%M:%S")
filtered_train_data <- train_data %>%
  filter(
    wday(Date) == 2,
    between(
      Time,
      as.POSIXlt("08:00:00", format = "%H:%M:%S"),
      as.POSIXlt("11:00:00", format = "%H:%M:%S") 
    )
  )

# Selection of specific Columns
selected_columns <- c(3, 4, 5)
train_data <- filtered_train_data[, selected_columns]

numState <- 12 

#train model on Test data
model1 <- depmix(list(
  Global_active_power ~ 1,
  Global_reactive_power ~ 1,
  Voltage ~ 1
), data = train_data, nstates = numState, family = list(gaussian(), gaussian(), gaussian()))

testModel <- fit(model1)

#Scaling and Evaluating test data
#Calculate the log-likelihoods for testing set
testLogLikelihood <- logLik(testModel)

#Normalize the log Likelihood
testLogLikelihood_normalized_dataset1 <- testLogLikelihood / nrow(train_data)

if (testLogLikelihood_normalized_dataset1 > (best_logLik + 0.5) || testLogLikelihood_normalized_dataset1 < (best_logLik - 0.5)) { #Set the difference of log likelyhood more than 1 then treat it as anomalyous data
  cat("This data contains anomalies")
} else {
  cat("This data doesn't contain anomalies")
}



###########
data <- read.table("/Users/danielmac/Desktop/CMPT 318 Term Project/Datasets/Anomalous Datasets/DataWithAnomalies2.txt", header = TRUE, sep = ",")

# Creating a variable for Date and Time
data$DateTime <- as.POSIXct(paste(data$Date, data$Time), 
                            format="%d/%m/%Y %H:%M:%S", tz = "UTC")

# Creating a new column in the data from for Date and Time
interpolated_data <- data.frame(
  DateTime = seq(min(data$DateTime),max(data$DateTime), by = "min")
)

# Use approx function to interpolate the NA values for Global active power
interpolated_values_GAP <- approx(
  x = data$DateTime,
  y = data$Global_active_power,
  xout = interpolated_data$DateTime,
  method = "linear",
  rule = 2
)

# Use approx function to interpolate the NA values for Global reactive power
interpolated_values_GRP <- approx(
  x = data$DateTime,
  y = data$Global_reactive_power,
  xout = interpolated_data$DateTime,
  method = "linear",
  rule = 2
)

# Use approx function to interpolate the NA values for Voltage
interpolated_values_Voltage <- approx(
  x = data$DateTime,
  y = data$Voltage,
  xout = interpolated_data$DateTime,
  method = "linear",
  rule = 2
)

# Add the interpolated data
interpolated_data$Global_active_power <- interpolated_values_GAP$y
interpolated_data$Global_reactive_power <- interpolated_values_GRP$y
interpolated_data$Voltage <- interpolated_values_Voltage$y

# Add interpolated data back into the original data set
data$Global_active_power[is.na(data$Global_active_power)] <- interpolated_data$Global_active_power
data$Global_reactive_power[is.na(data$Global_reactive_power)] <- interpolated_data$Global_reactive_power
data$Voltage[is.na(data$Voltage)] <- interpolated_data$Voltage

# Separate non-numeric columns
non_numeric_cols <- c("Date", "Time")
numeric_cols <- names(data)[sapply(data, is.numeric)]

# Ensure only numeric columns are scaled
scaled_numeric_cols <- scale(data[, numeric_cols])
data[, numeric_cols] <- as.data.frame(scaled_numeric_cols)

set.seed(123)
sample_size <- floor(nrow(data))

# Create indices for train and test sets
train_indices <- sample(seq_len(nrow(data)), size = sample_size)
train_data <- data[train_indices, ]

# Exclude non-numeric columns from scaled_train_data
numeric_scaled_train_data <- train_data[, sapply(train_data, is.numeric)]

# Filtering of Data
train_data$Date <- as.POSIXlt(train_data$Date, format = "%d/%m/%Y")
train_data$Time <- as.POSIXlt(train_data$Time, format = "%H:%M:%S")

filtered_train_data <- train_data %>%
  filter(
    wday(Date) == 2,
    between(
      Time,
      as.POSIXlt("08:00:00", format = "%H:%M:%S"),
      as.POSIXlt("11:00:00", format = "%H:%M:%S") 
    )
  )

# Selection of specific Columns
selected_columns <- c(3, 4, 5)
train_data <- filtered_train_data[, selected_columns]

numState <- 12 

#train model on Test data
model2 <- depmix(list(
  Global_active_power ~ 1,
  Global_reactive_power ~ 1,
  Voltage ~ 1
), data = train_data, nstates = numState, family = list(gaussian(), gaussian(), gaussian()))

testModel <- fit(model2)

#Scaling and Evaluating test data
#Calculate the log-likelihoods for testing set
testLogLikelihood <- logLik(testModel)

#Normalize the log Likelihood
testLogLikelihood_normalized_dataset2 <- testLogLikelihood / nrow(train_data)

if (testLogLikelihood_normalized_dataset2 > (best_logLik + 0.5) || testLogLikelihood_normalized_dataset2 < (best_logLik - 0.5)) { #Set the difference of log likelyhood more than 1 then treat it as anomalyous data
  cat("This data contains anomalies")
} else {
  cat("This data doesn't contain anomalies")
}


#######
data <- read.table("/Users/danielmac/Desktop/CMPT 318 Term Project/Datasets/Anomalous Datasets/DataWithAnomalies3.txt", header = TRUE, sep = ",")

# Creating a variable for Date and Time
data$DateTime <- as.POSIXct(paste(data$Date, data$Time), 
                            format="%d/%m/%Y %H:%M:%S", tz = "UTC")

# Creating a new column in the data from for Date and Time
interpolated_data <- data.frame(
  DateTime = seq(min(data$DateTime),max(data$DateTime), by = "min")
)

# Use approx function to interpolate the NA values for Global active power
interpolated_values_GAP <- approx(
  x = data$DateTime,
  y = data$Global_active_power,
  xout = interpolated_data$DateTime,
  method = "linear",
  rule = 2
)

# Use approx function to interpolate the NA values for Global reactive power
interpolated_values_GRP <- approx(
  x = data$DateTime,
  y = data$Global_reactive_power,
  xout = interpolated_data$DateTime,
  method = "linear",
  rule = 2
)

# Use approx function to interpolate the NA values for Voltage
interpolated_values_Voltage <- approx(
  x = data$DateTime,
  y = data$Voltage,
  xout = interpolated_data$DateTime,
  method = "linear",
  rule = 2
)

# Add the interpolated data
interpolated_data$Global_active_power <- interpolated_values_GAP$y
interpolated_data$Global_reactive_power <- interpolated_values_GRP$y
interpolated_data$Voltage <- interpolated_values_Voltage$y

# Add interpolated data back into the original data set
data$Global_active_power[is.na(data$Global_active_power)] <- interpolated_data$Global_active_power
data$Global_reactive_power[is.na(data$Global_reactive_power)] <- interpolated_data$Global_reactive_power
data$Voltage[is.na(data$Voltage)] <- interpolated_data$Voltage

# Separate non-numeric columns
non_numeric_cols <- c("Date", "Time")
numeric_cols <- names(data)[sapply(data, is.numeric)]

# Ensure only numeric columns are scaled
scaled_numeric_cols <- scale(data[, numeric_cols])
data[, numeric_cols] <- as.data.frame(scaled_numeric_cols)

set.seed(123)
sample_size <- floor(nrow(data))

# Create indices for train and test sets
train_indices <- sample(seq_len(nrow(data)), size = sample_size)
train_data <- data[train_indices, ]

# Exclude non-numeric columns from scaled_train_data
numeric_scaled_train_data <- train_data[, sapply(train_data, is.numeric)]

# Filtering of Data
train_data$Date <- as.POSIXlt(train_data$Date, format = "%d/%m/%Y")
train_data$Time <- as.POSIXlt(train_data$Time, format = "%H:%M:%S")

filtered_train_data <- train_data %>%
  filter(
    wday(Date) == 2,
    between(
      Time,
      as.POSIXlt("08:00:00", format = "%H:%M:%S"),
      as.POSIXlt("11:00:00", format = "%H:%M:%S") 
    )
  )

# Selection of specific Columns
selected_columns <- c(3, 4, 5)
train_data <- filtered_train_data[, selected_columns]

numState <- 12 

#train model on Test data
model3 <- depmix(list(
  Global_active_power ~ 1,
  Global_reactive_power ~ 1,
  Voltage ~ 1
), data = train_data, nstates = numState, family = list(gaussian(), gaussian(), gaussian()))

testModel <- fit(model3)

#Scaling and Evaluating test data
#Calculate the log-likelihoods for testing set
testLogLikelihood <- logLik(testModel)

#Normalize the log Likelihood
testLogLikelihood_normalized_dataset3 <- testLogLikelihood / nrow(train_data)

if (testLogLikelihood_normalized_dataset3 > (best_logLik + 0.5) || testLogLikelihood_normalized_dataset3 < (best_logLik - 0.5)) { #Set the difference of log likelyhood more than 1 then treat it as anomalyous data
  cat("This data contains anomalies")
} else {
  cat("This data doesn't contain anomalies")
}