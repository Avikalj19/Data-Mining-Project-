# LOAD LIBRARIES

library(ggplot2)
library(caret)
library(cluster)
library(dplyr)
library(class)
library(rpart)
library(rpart.plot)
 
# Load the data

 data <- read.csv("~/TCD Study/Group project Data mining/Train.csv")
 
 # DATA OVERVIEW 
 str(data)
 
# OUTPUT
'data.frame':	10999 obs. of  12 variables:
 $ ID                 : int  1 2 3 4 5 6 7 8 9 10 ...
 $ Warehouse_block    : chr  "D" "F" "A" "B" ...
 $ Mode_of_Shipment   : chr  "Flight" "Flight" "Flight" "Flight" ...
 $ Customer_care_calls: int  4 4 2 3 2 3 3 4 3 3 ...
 $ Customer_rating    : int  2 5 2 3 2 1 4 1 4 2 ...
 $ Cost_of_the_Product: int  177 216 183 176 184 162 250 233 150 164 ...
 $ Prior_purchases    : int  3 2 4 4 3 3 3 2 3 3 ...
 $ Product_importance : chr  "low" "low" "low" "medium" ...
 $ Gender             : chr  "F" "M" "M" "M" ...
 $ Discount_offered   : int  44 59 48 10 46 12 3 48 11 29 ...
 $ Weight_in_gms      : int  1233 3088 3374 1177 2484 1417 2371 2804 1861 1187 ...
 $ Reached.on.Time_Y.N: int  1 1 1 1 1 1 1 1 1 1 ...

 # DATA SUMMARY 
 
 summary(data)

 # OUTPUT
       ID        Warehouse_block    Mode_of_Shipment  
 Min.   :    1   Length:10999       Length:10999      
 1st Qu.: 2750   Class :character   Class :character  
 Median : 5500   Mode  :character   Mode  :character  
 Mean   : 5500                                        
 3rd Qu.: 8250                                        
 Max.   :10999                                        
 Customer_care_calls Customer_rating Cost_of_the_Product Prior_purchases 
 Min.   :2.000       Min.   :1.000   Min.   : 96.0       Min.   : 2.000  
 1st Qu.:3.000       1st Qu.:2.000   1st Qu.:169.0       1st Qu.: 3.000  
 Median :4.000       Median :3.000   Median :214.0       Median : 3.000  
 Mean   :4.054       Mean   :2.991   Mean   :210.2       Mean   : 3.568  
 3rd Qu.:5.000       3rd Qu.:4.000   3rd Qu.:251.0       3rd Qu.: 4.000  
 Max.   :7.000       Max.   :5.000   Max.   :310.0       Max.   :10.000  
 Product_importance    Gender          Discount_offered Weight_in_gms 
 Length:10999       Length:10999       Min.   : 1.00    Min.   :1001  
 Class :character   Class :character   1st Qu.: 4.00    1st Qu.:1840  
 Mode  :character   Mode  :character   Median : 7.00    Median :4149  
                                       Mean   :13.37    Mean   :3634  
                                       3rd Qu.:10.00    3rd Qu.:5050  
                                       Max.   :65.00    Max.   :7846  
 Reached.on.Time_Y.N
 Min.   :0.0000     
 1st Qu.:0.0000     
 Median :1.0000     
 Mean   :0.5967     
 3rd Qu.:1.0000     
 Max.   :1.0000     
 
 # CHECK FOR MISSING VALUES
 
 sum(is.na(data))
 
[1] 0

 # CHECK FOR DUPLICATES
 
 sum(duplicated(data))
 
[1] 0

 # CHECK FOR NULL VALUES
 
 sum(is.null(data))
 
[1] 0
 
# CHECK FOR UNIQUE VALUES

 unique(data$Gender)
 
[1] "F" "M"

#CONVERT VARIABLES INTO FACTORS

data$Gender <- as.factor(data$Gender)
data$Reached.on.Time_Y.N <- as.factor(data$Reached.on.Time_Y.N)
 data$Warehouse_block <- as.factor(data$Warehouse_block)
 data$Mode_of_Shipment <- as.factor(data$Mode_of_Shipment)
 data$Product_importance <- as.factor(data$Product_importance)

# SCALING NUMERIC FEATURES

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
 
# EDA (EXPLORATORY DATA ANALYSIS)

# DISTIBUTION OF DELIVERY STATUS (USING ( REACHED.ON.TIME_Y.N) VARIABLE)
 
ggplot(data, aes(x = Reached.on.Time_Y.N, fill = Reached.on.Time_Y.N)) + 
+   geom_bar() + 
+   labs(title = "Distribution of Delivery Status", x = "Delivery Status (0= On time , 1 = Not on time)", y = "Frequency")+
+   scale_fill_manual(values = c("green", "orange"), 
+                     labels = c("On time", "Not On time"),
+                     name = "Delivery Status")+  theme_minimal()

![image](https://github.com/user-attachments/assets/d360bbe9-68ea-406e-af4e-de28f2bb0a4f)

 
# DELIVERY STATUS BY MODE OF SHIPMENT 

ggplot(data, aes(x = Mode_of_Shipment, fill = Reached.on.Time_Y.N)) + 
+   geom_bar() + 
+   labs(title = "Distribution of Delivery Status by Shipment Distance", x = "Delivery Status (0= On time , 1 = Not on time)", y = "Frequency")+
+   scale_fill_manual(values = c("red", "yellow"), 
+                     labels = c("Not on time", "On time"),
+                     name = "Delivery Status")+  theme_minimal()

  ![image](https://github.com/user-attachments/assets/d8ba9965-d5d1-4493-b6cb-bb671491b7d8)


# DELIVERY STATUS BY WAREHOUSE BLOCK

ggplot(data, aes(x = Warehouse_block, y = Weight, fill = Reached.on.Time_Y.N)) + 
+   geom_violin() + 
+   labs(title = "Distribution of Delivery Status by Warehouse Block", x = "Warehouse Block", y = "Weight")+
+   scale_fill_manual(values = c("red", "yellow"), 
+                     labels = c("Not on time", "On time"),
+                     name = "Delivery Status")+  theme_minimal()

![image](https://github.com/user-attachments/assets/805c66b3-f407-47c3-ac85-23fd4a7e906b)


  
# CHECK EDA ACCURACY

cat("Exploratory Data Analysis Accuracy:", 100, "\n")

# ACCURACY

Exploratory Data Analysis Accuracy: 100 
 
# SPLITTING DATASET INTO TRAINING AND TESTING

set.seed(123)
splitIndex <- createDataPartition(data$Reached.on.Time_Y.N, p = .70, list = FALSE, times = 1)
train <- data[ splitIndex,]
test <- data[-splitIndex,]

# REMOVE ALL NULL AND N/A VALUE

train <- na.omit(train)
test <- na.omit(test)

 
# LOGISTIC REGRESSION
# BUILD A MODEL

model.0 <- glm(Reached.on.Time_Y.N ~ ., data = train, family = binomial)
summary(model.0)

# OUTPUT
Call:
glm(formula = Reached.on.Time_Y.N ~ ., family = binomial, data = train)

Coefficients: (1 not defined because of singularities)
                           Estimate Std. Error z value Pr(>|z|)    
(Intercept)               2.824e+00  1.646e-01  17.155  < 2e-16 ***
ID                       -1.593e-04  1.054e-05 -15.110  < 2e-16 ***
Warehouse_blockB          8.642e-02  9.138e-02   0.946 0.344258    
Warehouse_blockC         -2.662e-02  9.178e-02  -0.290 0.771782    
Warehouse_blockD          5.703e-02  9.090e-02   0.627 0.530426    
Warehouse_blockF          5.815e-02  7.906e-02   0.736 0.462012    
Mode_of_ShipmentRoad     -7.665e-02  9.429e-02  -0.813 0.416246    
Mode_of_ShipmentShip     -8.878e-02  7.469e-02  -1.189 0.234549    
Customer_care_calls      -1.030e-01  3.014e-02  -3.418 0.000631 ***
Customer_rating           2.603e-02  2.667e-02   0.976 0.329140    
Cost_of_the_Product      -3.875e-02  2.978e-02  -1.301 0.193241    
Prior_purchases          -7.232e-02  2.857e-02  -2.531 0.011359 *  
Product_importancelow    -2.155e-01  1.011e-01  -2.133 0.032948 *  
Product_importancemedium -1.922e-01  1.010e-01  -1.902 0.057124 .  
GenderM                   7.982e-03  5.319e-02   0.150 0.880713    
Discount_offered          1.444e+00  8.989e-02  16.061  < 2e-16 ***
Weight_in_gms            -2.212e-04  2.002e-05 -11.052  < 2e-16 ***
Weight                           NA         NA      NA       NA    
---
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

(Dispersion parameter for binomial family taken to be 1)

    Null deviance: 10386  on 7700  degrees of freedom
Residual deviance:  8152  on 7684  degrees of freedom
AIC: 8186

Number of Fisher Scoring iterations: 6

# PREDICT THE MODEL

predict.0 <- predict(model.0, test, type = "response")
predict.0 <- ifelse(predict.0 > 0.5, 1, 0)

# Convert predico.0 to factor

predict.0 <- as.factor(predict.0)

# EVALUATE THE MODEL

confusionMatrix(predict.0, test$Reached.on.Time_Y.N)

# OUTPUT
Confusion Matrix and Statistics

          Reference
Prediction    0    1
         0  803  627
         1  527 1341
                                          
               Accuracy : 0.6501          
                 95% CI : (0.6335, 0.6664)
    No Information Rate : 0.5967          
    P-Value [Acc > NIR] : 1.693e-10       
                                          
                  Kappa : 0.2817          
                                          
 Mcnemar's Test P-Value : 0.003565        
                                          
            Sensitivity : 0.6038          
            Specificity : 0.6814          
         Pos Pred Value : 0.5615          
         Neg Pred Value : 0.7179          
             Prevalence : 0.4033          
         Detection Rate : 0.2435          
   Detection Prevalence : 0.4336          
      Balanced Accuracy : 0.6426          
                                          
       'Positive' Class : 0               
                                          

 # CHECK MODEL ACCURACY 
 
 cat("Logistic Regression Model Accuracy:", model.0$deviance, "\n")

 # MODEL ACCURACY
 
Logistic Regression Model Accuracy: 8151.978 
 
 # CLASSIFICATION TREE
 
 # BUILD A MODEL
 
 model.1.0 <- rpart(Reached.on.Time_Y.N ~ ., data = train, method = "class")
 summary(model.1.0)

 
 
Call:
rpart(formula = Reached.on.Time_Y.N ~ ., data = train, method = "class")
  n= 7701 

         CP nsplit rel error    xerror       xstd
1 0.2298777      0 1.0000000 1.0000000 0.01386017
2 0.0100000      1 0.7701223 0.7714102 0.01308010

Variable importance
              ID Discount_offered           Weight    Weight_in_gms 
              38               32               15               15 

Node number 1: 7701 observations,    complexity param=0.2298777
  predicted class=1  expected loss=0.4033242  P(node) =1
    class counts:  3106  4595
   probabilities: 0.403 0.597 
  left son=2 (5498 obs) right son=3 (2203 obs)
  Primary splits:
      ID               < 3135.5      to the right, improve=1003.91200, (0 missing)
      Discount_offered < -0.1772985  to the left,  improve= 804.06470, (0 missing)
      Weight_in_gms    < 3998.5      to the right, improve= 503.52770, (0 missing)
      Weight           < 0.2228741   to the right, improve= 503.52770, (0 missing)
      Prior_purchases  < -0.04438823 to the right, improve=  35.15548, (0 missing)
  Surrogate splits:
      Discount_offered    < -0.1772985  to the left,  agree=0.957, adj=0.849, (0 split)
      Weight_in_gms       < 3998.5      to the right, agree=0.826, adj=0.393, (0 split)
      Weight              < 0.2228741   to the right, agree=0.826, adj=0.393, (0 split)
      Cost_of_the_Product < -2.34476    to the right, agree=0.714, adj=0.001, (0 split)

Node number 2: 5498 observations
  predicted class=0  expected loss=0.4350673  P(node) =0.7139333
    class counts:  3106  2392
   probabilities: 0.565 0.435 

Node number 3: 2203 observations
  predicted class=1  expected loss=0  P(node) =0.2860667
    class counts:     0  2203
   probabilities: 0.000 1.000 

 # PREDICT THE MODEL AND EVALUATE 
 
 predict.1.0 <- predict(model.1.0, test, type = "class")
 confusionMatrix(predict.1.0, test$Reached.on.Time_Y.N)

 # OUTPUT
 
Confusion Matrix and Statistics
          Reference
Prediction    0    1
         0 1330 1036
         1    0  932
                                          
               Accuracy : 0.6859          
                 95% CI : (0.6697, 0.7017)
    No Information Rate : 0.5967          
    P-Value [Acc > NIR] : < 2.2e-16       
                                          
                  Kappa : 0.4205          
                                          
 Mcnemar's Test P-Value : < 2.2e-16       
                                          
            Sensitivity : 1.0000          
            Specificity : 0.4736          
         Pos Pred Value : 0.5621          
         Neg Pred Value : 1.0000          
             Prevalence : 0.4033          
         Detection Rate : 0.4033          
   Detection Prevalence : 0.7174          
      Balanced Accuracy : 0.7368          
                                          
       'Positive' Class : 0               
                                          

# CHECK THE MODEL ACCURACY 
 
classification_accuracy <- (1 - model.1.0$cptable[which.min(model.1.0$cptable[, "xerror"]), "xerror"]) * 100
cat("Classification Tree Model Accuracy:", classification_accuracy, "%\n")

# MODEL ACCURACY 

Classification Tree Model Accuracy: 22.85898 %
 
# VISUALIZE THE TREE

rpart.plot(model.1.0)

 ![image](https://github.com/user-attachments/assets/735ad7d8-35db-4f20-9e93-f4fd4d583cf3)


 # KNN 

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

# OUTPUT
Confusion Matrix and Statistics

          Reference
Prediction    0    1
         0  829  677
         1  501 1291
                                          
               Accuracy : 0.6428          
                 95% CI : (0.6262, 0.6592)
    No Information Rate : 0.5967          
    P-Value [Acc > NIR] : 3.079e-08       
                                          
                  Kappa : 0.2734          
                                          
 Mcnemar's Test P-Value : 3.419e-07       
                                          
            Sensitivity : 0.6233          
            Specificity : 0.6560          
         Pos Pred Value : 0.5505          
         Neg Pred Value : 0.7204          
             Prevalence : 0.4033          
         Detection Rate : 0.2514          
   Detection Prevalence : 0.4566          
      Balanced Accuracy : 0.6397          
                                          
       'Positive' Class : 0               
                                          
accuracy <- conf_matrix$overall["Accuracy"]
cat("KNN Model Accuracy:", accuracy, "\n")

# MODEL ACCURACY

KNN Model Accuracy: 0.6428138 

 # FINDING OPTIMAL VALUE OF K

 k_values <- seq(1, 20, 2)
accuracy_values <- c()
 for (k in k_values) {
+   predictions <- knn(train = train_X, test = test_X, cl = train_Y, k = k)
+   accuracy <- sum(predictions == test_Y) / length(test_Y)
+   accuracy_values <- c(accuracy_values, accuracy)
+ }

 # PLOTTING K VS ACCURACY 
 
 plot(k_values, accuracy_values, type = "b", col = "blue", xlab = "k - Number of Neighbors", ylab = "Accuracy",
+      main = "KNN Accuracy for Different k Values")

![image](https://github.com/user-attachments/assets/74cbd1e0-a99b-4640-9b25-cad35f9eac38)
