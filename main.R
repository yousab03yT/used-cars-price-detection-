#---------------------------------------#Dependencies#---------------------------------------------
suppressPackageStartupMessages({
  library(dplyr)
  library(ggplot2)
  library(tidyverse)
  library(lubridate)
  library(car)
  library(stringr)
  library(psych)
  library(rpart)
  library(rpart.plot)
  library(ipred)
  library(caret)
  library(GGally)
  library(scales)
  library(reshape)
  library(corrgram)
  library(knitr)
  library(gridExtra)
  library(caTools)
  library(Metrics)
  library(dplyr)
  library(tidyr)
  })

#---------------------------------------#Reading CSV#---------------------------------------------
a <- read.csv("./used_car_dataset.csv")
CarsData <- data.frame(a)
rm(a)

#---------------------------------------#Data Cleaning#---------------------------------------------
CleanedData <-CarsData

CleanedData$PostedDate <- NULL
CleanedData$AdditionInfo <- NULL
# CleanedData$Age <- NULL
#CleanedData$model <- NULL

CleanedData$Age[CleanedData$Age<0]<-CleanedData$Age[CleanedData$Age<0]*(-1)
CleanedData$Year[CleanedData$Year<0]<-CleanedData$Year[CleanedData$Year<0]*(-1)

CleanedData$kmDriven  <- as.integer(as.numeric(gsub("[^0-9.]", "", CleanedData$kmDriven)))
CleanedData$AskPrice  <- as.integer(as.numeric(gsub("[^0-9.]", "", CleanedData$AskPrice)))

CleanedData$Age <- ifelse(CleanedData$Age != 2024 - CleanedData$Year, 
                          2024 - CleanedData$Year, 
                          CleanedData$Age)

CleanedData <- CleanedData[CleanedData$kmDriven > 0 & CleanedData$AskPrice > 0, ]

CleanedData <- na.omit(CleanedData)
dim(CleanedData)
print(paste("NA values:",sum(is.na(CleanedData))))


#---------------------------------------#Data Exploring and Distribution#---------------------------------------------

#==========Car Count/Year==========#
YearCount <- CleanedData %>%
  group_by(Year) %>%
  summarise(n = n()) %>%
  mutate(Freq = n/sum(n)*100) %>% as.data.frame(usedcars_tibble) %>% 
  arrange(desc(Freq))

ggplot(data=YearCount,aes(x=Year,y=Freq)) +
  geom_line(color="lightblue3",size=1) + 
  geom_point(size=3,color="blue4") +
  labs(x="Year",y="Number of cars(Freq)",
       title="Plot of Frequency of Car count per Year")+
  theme(plot.title =element_text(color="black",size=12,
                                 face="bold",
                                 lineheight = 0.8),
        axis.text.x = element_text())

#==========Car Count/Brand==========#
BrandCount <- CleanedData %>%
  group_by(Brand) %>%
  summarise(n = n()) %>%
  mutate(Freq = n/sum(n)*100) %>% as.data.frame(usedcars_tibble) %>% 
  arrange(desc(Freq))

ggplot(data=BrandCount, aes(x=Brand, y=n,fill=Brand))+
  geom_bar(stat="identity")+
  labs(x="Car Make",y="Number of Cars",
       title=" Bar Plot of Car Make Count.")+
  theme(plot.title =element_text(color="black",
                                 size=12,face="bold",lineheight = 0.8))+
  theme(axis.text.x = element_text(angle = 90, hjust = 0.5))

#==========Car Count/Fuel Type==========#
FuelCount <- CleanedData %>%
  group_by(FuelType) %>%
  summarise(n = n()) %>%
  mutate(Freq = n/sum(n)*100) %>% as.data.frame(usedcars_tibble) %>% 
  arrange(desc(Freq))

ggplot(data=FuelCount, aes(x=FuelType, y=n))+
  geom_bar(stat="identity",fill="red")+
  labs(x="Fuel Type",y="Number of Cars",
       title="Bar Plot of The Type of Fuels utilized by Used Cars")+
  theme(plot.title =element_text(color="black",
                                 size=12,face="bold",               
                                 lineheight = 0.8))

#==========Car Prices/Brand==========#
ggplot(CleanedData, aes(x = Brand, y = AskPrice)) +
  geom_boxplot(fill = "cyan3")+
  labs(
    title = "Box plot of Car Make prices",
    x = "Car Make",
    y = "Price")+
  coord_flip()

#==========Mean Brand Prices==========#
options(scippen=999)
MeanBrandPrice <- CleanedData %>%
  group_by(Brand) %>%
  summarize(MeanBrandPrice =mean(AskPrice)) %>%
  mutate(Brand=fct_reorder(Brand, MeanBrandPrice))%>% 
  arrange(desc(MeanBrandPrice))

ggplot(data=MeanBrandPrice, aes(x=Brand, y=MeanBrandPrice)) +
  geom_col(fill="skyblue2") +
  labs(x="Brand",y="Mean Car prices",title=" Bar Plot of Car Mean prices")+
  theme(plot.title =element_text(color="black",size=12,face="bold",
                                 lineheight = 0.8),axis.text.x = element_text())+
  theme(axis.text.x = element_text(angle = 90, hjust = 0.5))

#==========Car Count/Km Driven==========#
options(scippen=999)
ggplot(CleanedData, aes(x=kmDriven)) + 
  geom_histogram(color="red", fill="blue4", bins = 160) +
  labs(x='kmDriven',y='Number of Cars(Freq)',title = "Histogram Plot of kmDriven Distribution")  +
  scale_x_continuous(trans='log10')

#==========Car Count/Owner==========#
OwnerNum <- CleanedData %>%
  group_by(Owner) %>%
  summarise(n = n()) %>%
  mutate(Freq = n/sum(n)*100) %>% as.data.frame(usedcars_tibble) %>% 
  arrange(desc(Freq))

#==========Prices of Used Cars==========#
ggplot(CleanedData, aes(x=AskPrice)) + 
  labs(x='Price of Used cars') +
  labs(title = "Histogram Graph of the Prices of used Cars") +
  geom_histogram(aes(y=after_stat(density)),
                 colour="violet",
                 fill="maroon" ,
                 bins = 60 ,
                 ) +
  geom_density() +
  scale_x_continuous(trans='log2')

#==========Reshaping==========#
Reshaped_Data <- melt(CleanedData)

boxplots <- ggplot(Reshaped_Data, aes(factor(variable), value))
boxplots + 
  geom_boxplot() + 
  facet_wrap(~variable, scale="free")

#==========rm()==========#
rm(YearCount)
rm(Reshaped_Data)
rm(OwnerNum)
rm(MeanBrandPrice)
rm(FuelCount)
rm(BrandCount)

#---------------------------------------#Data Encoding#---------------------------------------------
# Brands start at 3 and ends at 37
# sum(Age == 0) = 0?

EncodedData <- CleanedData

EncodedData$Brand <- as.numeric(factor(EncodedData$Brand))
EncodedData$model <- as.numeric(factor(EncodedData$model))
EncodedData$Transmission <- as.numeric(factor(EncodedData$Transmission))
EncodedData$Owner <- as.numeric(factor(EncodedData$Owner))
EncodedData$FuelType <- as.numeric(factor(EncodedData$FuelType))

EncodedData <- na.omit(EncodedData)
print(paste("NA values:", sum(is.na(EncodedData))))

#---------------------------------------#Summary#---------------------------------------------
summary(EncodedData)
describe(EncodedData)
cor(EncodedData)
#---------------------------------------#Correlation Preview#---------------------------------------------
corrgram(EncodedData, order=TRUE)
ggcorr(EncodedData, label = T)
#---------------------------------------#Handling Outliers#---------------------------------------------
#==========Km-Driven==========#
#kmDriven outliers
# Q1 <- quantile(EncodedData$kmDriven, 0.25)
# Q3 <- quantile(EncodedData$AskPrice, 0.75)
# IQR <- Q3 - Q1
# 
# LowerBound <- max(100, Q1 - 1.5 * IQR)
# UpperBound <- Q3 + 1.5 * IQR
# mean_kmDriven <- mean(EncodedData$kmDriven[EncodedData$kmDriven >= LowerBound & EncodedData$kmDriven <= UpperBound])
# 
# #AskPrice outliers
# Q1 <- quantile(EncodedData$AskPrice, 0.25)
# Q3 <- quantile(EncodedData$AskPrice, 0.75)
# IQR <- Q3 - Q1
# 
# LowerBound <- max(100, Q1 - 1.5 * IQR)
# UpperBound <- Q3 + 1.5 * IQR
# mean_AskPrice <- mean(EncodedData$AskPrice[EncodedData$AskPrice >= LowerBound & EncodedData$AskPrice <= UpperBound])
# 
# EncodedData$AskPrice[EncodedData$AskPrice < LowerBound | EncodedData$AskPrice > UpperBound] <- mean_AskPrice 
# #==========Ask-Price==========#
# Q1 <- quantile(EncodedData$AskPrice, 0.25)
# Q3 <- quantile(EncodedData$AskPrice, 0.75)
# IQR <- Q3 - Q1
# 
# LowerBound <- max(15, Q1 - 1.5 * IQR)
# UpperBound <- Q3 + 1.5 * IQR
# EncodedData <- EncodedData[EncodedData$AskPrice >= LowerBound & EncodedData$AskPrice <= UpperBound, ]
# 
# ggplot(EncodedData, aes(x = kmDriven, y = AskPrice)) +
#   geom_point(alpha = 0.6) +
#   labs(title = "Relationship between kmDriven and AskPrice (After Cleaning)", 
#        x = "Kilometers Driven", y = "Asking Price") +
#   theme_minimal()

#---------------------------------------#Before and after removing the Outliers#---------------------------------------------
#==========Before Cleaning==========#
p1 <- ggplot(CleanedData, aes(x = kmDriven)) +
  geom_histogram(bins = 30, fill = "blue3", alpha = 0.7) +
  labs(title = "Distribution of kmDriven (Original)", x = "kmDriven", y = "Count") +
  theme_minimal()

p2 <- ggplot(CleanedData, aes(x = AskPrice)) +
  geom_histogram(bins = 30, fill = "blue4", alpha = 0.7) +
  labs(title = "Distribution of AskPrice (Original)", x = "AskPrice", y = "Count") +
  theme_minimal()

#==========After Cleaning==========#
p3 <- ggplot(EncodedData, aes(x = kmDriven)) +
  geom_histogram(bins = 30, fill = "green3", alpha = 0.7) +
  labs(title = "Distribution of kmDriven (Cleaned)", x = "kmDriven", y = "Count") +
  theme_minimal()

p4 <- ggplot(EncodedData, aes(x = AskPrice)) +
  geom_histogram(bins = 30, fill = "green4", alpha = 0.7) +
  labs(title = "Distribution of AskPrice (Cleaned)", x = "AskPrice", y = "Count") +
  theme_minimal()

grid.arrange(p1, p3, p2, p4, ncol = 2)

#---------------------------------------#Log Transforming#---------------------------------------------
#==========Ask Price==========#
EncodedData$LogAskPrice <- log(EncodedData$AskPrice)

EncodedData <- na.omit(EncodedData)
dim(EncodedData)
print(paste("NA values:",sum(is.na(EncodedData))))

p5 <- ggplot(EncodedData, aes(x=LogAskPrice)) + 
  geom_histogram(bins=50, fill="skyblue") +
  theme_minimal() +
  labs(title="Distribution of Log-Transformed Ask Price")

print(p5)

describe(EncodedData)
#---------------------------------------#Feature Engineering#---------------------------------------------
#brand price
EncodedData <- EncodedData %>%
  group_by(Brand) %>%
  mutate(BrandPrice = mean(AskPrice, na.rm = TRUE)) %>%
  ungroup()
#model price
EncodedData <- EncodedData %>%
  group_by(model) %>%
  mutate(ModelPrice = mean(AskPrice, na.rm = TRUE)) %>%
  ungroup()
#KmDivenPerYear 
EncodedData$KmDivenPerYear <- ifelse(EncodedData$Age == 0, EncodedData$kmDriven, EncodedData$kmDriven / EncodedData$Age)

#---------------------------------------#Data Splitting#---------------------------------------------
set.seed(123)

split <- sample.split(EncodedData$LogAskPrice, SplitRatio = 0.8)
train <- subset(EncodedData, split == TRUE)
test <- subset(EncodedData, split == FALSE)


evaluate_model <- function(actual, predicted) {
  mae <- mean(abs(actual - predicted))
  rmse <- sqrt(mean((actual - predicted)^2))
  mape <- mean(abs((actual - predicted) / actual)) * 100
  r_squared <- 1 - sum((actual - predicted)^2) / sum((actual - mean(actual))^2)
  list(MAE = mae, RMSE = rmse, MAPE = mape, R2 = r_squared)
}

#---------------------------------------#Linear Regression Model#---------------------------------------------
LRmodel <- lm(LogAskPrice ~ Age + kmDriven + ModelPrice + BrandPrice + KmDivenPerYear, data = train)

test$PredictedLogPrice <- predict(LRmodel, newdata = test)
test$PredictedPrice <- exp(test$PredictedLogPrice)
test$ActualPrice <- exp(test$LogAskPrice)

lr_metrics <- evaluate_model(test$ActualPrice, test$PredictedPrice)

cat("Linear Regression:\n")
cat("MAE: ", lr_metrics$MAE, "\n")
cat("RMSE: ", lr_metrics$RMSE, "\n")
cat("MAPE: ", lr_metrics$MAPE, "\n")
cat("R2: ", lr_metrics$R2, "\n")

#---------------------------------------#XGBoost Model#---------------------------------------------
# XGBoost Model
library(xgboost)

train_x <- train
train_x$AskPrice <- NULL
train_y <- train$LogAskPrice
train_x$Brand <- NULL
train_x$model <- NULL
train_x$Transmission <- NULL
train_x$Owner <- NULL
train_x$FuelType <- NULL

test_x <- test
test_x$AskPrice <- NULL
test_y <- test$LogAskPrice

test_x$Brand <- NULL
test_x$model <- NULL
test_x$Transmission <- NULL
test_x$Owner <- NULL
test_x$FuelType <- NULL

# Ensure that the column names match between train and test data
test_x <- test_x[, colnames(train_x)]

dtrain <- xgb.DMatrix(data = as.matrix(train_x), label = train_y, missing = NA)
params <- list(objective = "reg:squarederror", eta = 0.3, max_depth = 6)

xgb_model <- xgb.train(params, dtrain, nrounds = 10)
preds <- predict(xgb_model, as.matrix(test_x))

xgb_metrics <- evaluate_model(test_y, preds)

cat("XGBoost:\n")
cat("MAE: ", xgb_metrics$MAE, "\n")
cat("RMSE: ", xgb_metrics$RMSE, "\n")
cat("MAPE: ", xgb_metrics$MAPE, "\n")
cat("R2: ", xgb_metrics$R2, "\n")
#---------------------------------------#Decision Tree Model#---------------------------------------------
fit <- rpart(LogAskPrice ~ Age + kmDriven + ModelPrice + BrandPrice + KmDivenPerYear , data = train, method = "anova")
dtpred <- predict(fit, test)

dt_metrics <- evaluate_model(test$LogAskPrice, dtpred)

cat("Decision Tree:\n")
cat("MAE: ", dt_metrics$MAE, "\n")
cat("RMSE: ", dt_metrics$RMSE, "\n")
cat("MAPE: ", dt_metrics$MAPE, "\n")
cat("R2: ", dt_metrics$R2, "\n")

#---------------------------------------#SVR Model#---------------------------------------------
# SVR Model

library(e1071)
train_x <- train
train_x$AskPrice <- NULL
train_y <- train$LogAskPrice

test_x <- test
test_x$AskPrice <- NULL
test_y <- test$LogAskPrice

# Ensure that the column names match between train and test data
test_x <- test_x[, colnames(train_x)]

svr_model <- svm(train_x, train_y, type = "eps-regression", kernel = "radial")
predictions <- predict(svr_model, test_x)

svr_metrics <- evaluate_model(test_y, predictions)

cat("SVR:\n")
cat("MAE: ", svr_metrics$MAE, "\n")
cat("RMSE: ", svr_metrics$RMSE, "\n")
cat("MAPE: ", svr_metrics$MAPE, "\n")
cat("R2: ", svr_metrics$R2, "\n")
#---------------------------------------#Random Forest#---------------------------------------------
library(randomForest)
set.seed(222)
rf_model <- randomForest(LogAskPrice ~ ., data = train, ntree = 500, mtry = 3, importance = TRUE)
rf_predictions <- predict(rf_model, newdata = test)

rf_metrics <- evaluate_model(test$LogAskPrice, rf_predictions)

cat("Random Forest:\n")
cat("MAE: ", rf_metrics$MAE, "\n")
cat("RMSE: ", rf_metrics$RMSE, "\n")
cat("MAPE: ", rf_metrics$MAPE, "\n")
cat("R2: ", rf_metrics$R2, "\n")