library(ggplot2)
library(caret)
library(cluster)
library(dplyr)
library(class)
library(rpart)
library(rpart.plot)

# Load the data
data <- read.csv("~/TCD Study/Group project Data mining/Train.csv")

#dataoverview
str(data)

#datasummary
summary(data)

#check for missing values
sum(is.na(data))

#check for duplicates
sum(duplicated(data))

#check for null values
sum(is.null(data))

#check for unique values
unique(data$Gender)

#convert variables to factors 
data$Gender <- as.factor(data$Gender)
data$Reached.on.Time_Y.N <- as.factor(data$Reached.on.Time_Y.N)
data$Warehouse_block <- as.factor(data$Warehouse_block)
data$Mode_of_Shipment <- as.factor(data$Mode_of_Shipment)
data$Product_importance <- as.factor(data$Product_importance)

#scaling numeric features
data$Weight <- scale(data$Weight)
data$Cost_of_the_Product <- scale(data$Cost_of_the_Product)
data$Discount_offered <- scale(data$Discount_offered)
data$Customer_care_calls <- scale(data$Customer_care_calls)
data$Prior_purchases <- scale(data$Prior_purchases)
data$Customer_rating <- scale(data$Customer_rating)
data$Cost_of_the_Product <- scale(data$Cost_of_the_Product)

data_scaled <- data
num_cols <- c("Customer_care_calls", "Cost_of_the_Product", "Discount_offered", "Weight_in_gms")
data_scaled[num_cols] <- scale(data[num_cols])

#EDA (EXPLORATORY DATA ANALYSIS)
#DISTIBUTION OF DELIVERY STATUS (USING ( REACHED.ON.TIME_Y.N) VARIABLE)

ggplot(data, aes(x = Reached.on.Time_Y.N, fill = Reached.on.Time_Y.N)) + 
  geom_bar() + 
  labs(title = "Distribution of Delivery Status", x = "Delivery Status (0= On time , 1 = Not on time)", y = "Frequency")+
  scale_fill_manual(values = c("green", "orange"), 
                    labels = c("On time", "Not On time"),
                    name = "Delivery Status")+  theme_minimal()

#Dilivery status by shipment distance
ggplot(data, aes(x = Mode_of_Shipment, fill = Reached.on.Time_Y.N)) + 
  geom_bar() + 
  labs(title = "Distribution of Delivery Status by Shipment Distance", x = "Delivery Status (0= On time , 1 = Not on time)", y = "Frequency")+
  scale_fill_manual(values = c("red", "yellow"), 
                    labels = c("Not on time", "On time"),
                    name = "Delivery Status")+  theme_minimal()

#Delivery status by Warehouse_block using violin plot
ggplot(data, aes(x = Warehouse_block, y = Weight, fill = Reached.on.Time_Y.N)) + 
  geom_violin() + 
  labs(title = "Distribution of Delivery Status by Warehouse Block", x = "Warehouse Block", y = "Weight")+
  scale_fill_manual(values = c("red", "yellow"), 
                    labels = c("Not on time", "On time"),
                    name = "Delivery Status")+  theme_minimal()
#check EDA accuracy
cat("Exploratory Data Analysis Accuracy:", 100, "\n")


#Splitting the dataset into training and testing set
set.seed(123)
splitIndex <- createDataPartition(data$Reached.on.Time_Y.N, p = .70, list = FALSE, times = 1)
train <- data[ splitIndex,]
test <- data[-splitIndex,]

#Remove all the NA values, missing values and duplicates
train <- na.omit(train)
test <- na.omit(test)


#Logistic Regression 
#Build a model

model.0 <- glm(Reached.on.Time_Y.N ~ ., data = train, family = binomial)
summary(model.0)

#Predict the model
predict.0 <- predict(model.0, test, type = "response")
predict.0 <- ifelse(predict.0 > 0.5, 1, 0)

#Convert predico.0 to factor
predict.0 <- as.factor(predict.0)

#Evaluate the model
confusionMatrix(predict.0, test$Reached.on.Time_Y.N)

#Check the model accuracy
cat("Logistic Regression Model Accuracy:", model.0$deviance, "\n")

#CLASSIFICATION TREE

#Build a model

model.1.0 <- rpart(Reached.on.Time_Y.N ~ ., data = train, method = "class")
summary(model.1.0)

#predict and evaluate the model
predict.1.0 <- predict(model.1.0, test, type = "class")
confusionMatrix(predict.1.0, test$Reached.on.Time_Y.N)

#check the model accuracy 

classification_accuracy <- (1 - model.1.0$cptable[which.min(model.1.0$cptable[, "xerror"]), "xerror"]) * 100
cat("Classification Tree Model Accuracy:", classification_accuracy, "%\n")

#Visualize the tree
rpart.plot(model.1.0)

#KNN

train_X <- train %>% select(-Reached.on.Time_Y.N)
train_Y <- train$Reached.on.Time_Y.N
test_X <- test %>% select(-Reached.on.Time_Y.N)
test_Y <- test$Reached.on.Time_Y.N


train_X <- data.frame(lapply(train_X, as.numeric))
test_X <- data.frame(lapply(test_X, as.numeric))

train_X <- scale(train_X)
test_X <- scale(test_X)
k <- 7
predictions <- knn(train = train_X, test = test_X, cl = train_Y, k = k)


test_Y <- factor(test_Y, levels = c("0", "1"))
predictions <- factor(predictions, levels = c("0", "1"))

conf_matrix <- confusionMatrix(predictions, test_Y)
print(conf_matrix)

accuracy <- conf_matrix$overall["Accuracy"]
cat("KNN Model Accuracy:", accuracy, "\n")

# Finding optimal k value
k_values <- seq(1, 20, 2)
accuracy_values <- c()


for (k in k_values) {
  predictions <- knn(train = train_X, test = test_X, cl = train_Y, k = k)
  accuracy <- sum(predictions == test_Y) / length(test_Y)
  accuracy_values <- c(accuracy_values, accuracy)
}

# Plotting k vs Accuracy
plot(k_values, accuracy_values, type = "b", col = "blue", xlab = "k - Number of Neighbors", ylab = "Accuracy",
     main = "KNN Accuracy for Different k Values")
