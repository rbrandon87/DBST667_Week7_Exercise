#Week 7 Exercise - Linear Regression
#Brandon Russell - DBST667
#Install and Load necessary packages
#Verify working directory
getwd()
#Part 2b - Load dataset
imports85 <- read.csv("imports-85.csv", header = TRUE, sep = ",")
#View data to determine if needs any preprocessing
summary(imports85)
str(imports85)
plot(imports85)
#Part 2bi - Remove attributes
imports85$engine_type <- NULL
imports85$make <- NULL
imports85$num_of_cylinders <- NULL
imports85$fuel_system <- NULL
str(imports85)
#Verify no empty fields
apply(imports85, 2, function (imports85) sum(is.na(imports85)))
#Part 2bii - Replace empty fields with attribute mean 
imports85$normalized_losses[is.na(imports85$normalized_losses)]<-0
imports85$bore[is.na(imports85$bore)]<-mean(imports85$bore, na.rm=TRUE)
imports85$stroke[is.na(imports85$stroke)]<-mean(imports85$stroke, na.rm=TRUE)
imports85$horsepower[is.na(imports85$horsepower)]<-mean(imports85$horsepower, na.rm=TRUE)
imports85$peak_rpm[is.na(imports85$peak_rpm)]<-mean(imports85$peak_rpm, na.rm=TRUE)
#Verify no empty fields
apply(imports85, 2, function (imports85) sum(is.na(imports85)))


#Part 2ci - Split data between training and test sets
set.seed(12345)
ind <- sample(2, nrow(imports85), replace = TRUE, prob = c(0.7, 0.3))
train.data <- imports85[ind == 1, ]
test.data <- imports85[ind == 2, ]

#Part 2cii - Build the Linear Model
mlr_model <- lm(Price~., train.data)

#Part 2ciii - Summary
summary(mlr_model)

#Part 2di - Predict
pred <- predict(mlr_model, test.data)

#Part 2dii - Scatter plot predictions
plot(test.data$Price, pred, xlab = "Observed", ylab = "Prediction")
abline(a = 0, b = 1)

#Part 2ei - Plot model
plot (mlr_model)

#Part 2eii - Minimal adequate model
mlr_model_min <- step(mlr_model, direction="backward")
#Part 2eiii - Summary
summary(mlr_model_min)


#Part 2gi - New Instance
NewCarTest <- imports85[1,] #Move row 1 into new data frame
str(NewCarTest) #Get some of the values
NewCarTest$Price <- NULL #Remove price
NewCarTest$symboling <- 2 #Change some of the values around
NewCarTest$wheel_base <- 99.8
NewCarTest$width <- 70
NewCarTest$height <- 50
newpred <- predict(mlr_model_min, NewCarTest) #Run prediction
newpred #Get prediction - 26,368.86

#Part 2hi - Simple Linear Regression
slr_model = lm(Price~normalized_losses, train.data)
summary(slr_model)
