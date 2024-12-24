library(randomForest)
library(datasets)
library(caret)
library(dplyr)
setwd("/Users/tq/Downloads/car_price")
data <- read_csv("analysisData.csv")

set.seed(617)
split = sample(1:nrow(data),size = nrow(data)*0.8)
train = data[split,]
test = data[-split,]

remove_cols <- nearZeroVar(train, names = TRUE)

model = randomForest(price ~ fuel_tank_volume_gallons+highway_fuel_economy+city_fuel_economy+power+torque+transmission+wheel_system+wheelbase_inches+back_legroom_inches+front_legroom_inches+length_inches+width_inches+height_inches+engine_type+horsepower, 
                     data=train,
                     na.action = na.omit,
                     ntree = 500)
pred_train = predict(model, newdata = train)
rmse_pred_train = sqrt(mean((pred_train - train$price)^2))
rmse_pred_train

scoringData = read.csv('/Users/edeveve/Desktop/scoringData.csv')
pred = predict(model, newdata=scoringData)
rmse_pred = sqrt(mean((pred - scoringData$price)^2)); rmse_pred
summary(pred)

submissionFile = data.frame(id = scoringData$id, price = pred)
write.csv(submissionFile, 'sample_submission.csv',row.names = F)