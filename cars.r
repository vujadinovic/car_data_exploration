install.packages("dplyr")
install.packages("car")
library(dplyr)
library(ggplot2)
library(stringr)
library(caret) 
library(car)

carsData <- read.csv("cars.csv")
View(carsData)
# Let's tackle the problem of brand names.
# As we can see, we have same brand names in different cases, for example "Nissan" and "NISSAN"
unique(carsData$Company.Name)
# Obviously, we don't want to have them as different values
carsData$Company.Name <- tolower(carsData$Company.Name)
carsData$Company.Name <- trimws(carsData$Company.Name)
unique(carsData$Company.Name)
# We can take engine capacity like this: if the value is more than or equal to 800, then it is cylinder capacity,
# if it is less than that, then it is battery capacity:
# Cylinder / battery capacity (ensure only first number is taken)


# Now, before exploring average prices per brand, let's get price information in more favourable state
carsData$carsPricesNum <- as.numeric(str_replace_all(str_extract(carsData$Cars.Prices, "\\$[0-9,]+"), "[$,]", ""))
carsData$carsPricesNum <- as.numeric(gsub("[$,]", "", carsData$carsPricesNum))

# Do it for HorsePower and other columns also
carsData$horsePowerNum <- as.numeric(str_extract(carsData$HorsePower, "\\d+"))
carsData$totalSpeedNum <- as.numeric(str_extract(carsData$Total.Speed, "\\d+"))
carsData$performance0to100Num <- as.numeric(str_extract(carsData$Performance.0...100..KM.H, "\\d+\\.?\\d*"))
carsData$seatsNum <- as.numeric(str_extract(carsData$Seats, "\\d+"))
carsData$torqueNum <- as.numeric(str_extract(carsData$Torque, "\\d+"))

# As we have Engines column, which is very messy, and from which, given that we have cylinder/battery capacity 
# only thing that we can possibly extract is number of cylinders and engine configuration. That is linked to cylinder capacity
# anyway, so we will drop it for now.
# The Cars.Names column was dropped because it contains model names with many unique values, for which most of information is 
# already captured.


carsData$ccBatteryNum <- as.numeric(str_extract(carsData$CC.Battery.Capacity, "\\d+\\.?\\d*"))

carsData$cylinderCapacityNum <- ifelse(!is.na(carsData$ccBatteryNum) & carsData$ccBatteryNum > 800,
                                       carsData$ccBatteryNum, NA)

carsData$batteryCapacityNum <- ifelse(!is.na(carsData$ccBatteryNum) & carsData$ccBatteryNum <= 800,
                                      carsData$ccBatteryNum, NA)
# The only problem noticed with this is that there are some hybrid vehicles that regex badly extracts data out of
# in the sense that it reads battery power (that is a small percent of hybrid engine's capacity) as whole capacity.
# It remains to be solved.



# Calculate average price per brand
brand_prices <- carsData %>%
  group_by(Company.Name) %>%
  summarise(avg_price = mean(carsPricesNum, na.rm = TRUE)) %>%
  arrange(desc(avg_price))

# Plot bar chart
ggplot(brand_prices, aes(x = reorder(Company.Name, avg_price), y = avg_price)) +
  geom_col(fill = "steelblue") +
  coord_flip() +  # so as not to clash with other names
  labs(title = "Price per Brand",
       x = "Brand",
       y = "Price $") +
  theme_minimal()


# Let's split it into training, test and validation sets
carsDataNoNA <- carsData[!is.na(carsData$carsPricesNum), ]
set.seed(11231)

trainIndex <- createDataPartition(carsDataNoNA$carsPricesNum, p = 0.7, list = FALSE)
trainData <- carsDataNoNA[trainIndex, ]
tempData  <- carsDataNoNA[-trainIndex, ]

valIndex <- createDataPartition(tempData$carsPricesNum, p = 0.5, list = FALSE)
validationData <- tempData[valIndex, ]
testData       <- tempData[-valIndex, ]



# Now let's target encode based on training dataset:
brandMeans <- trainData %>%
  group_by(Company.Name) %>%
  summarise(meanPrice = mean(carsPricesNum, na.rm = TRUE))


trainData$brandTargetEnc <- trainData$Company.Name %>%
  sapply(function(x) brandMeans$meanPrice[brandMeans$Company.Name == x])

valData$brandTargetEnc <- valData$Company.Name %>%
  sapply(function(x) brandMeans$meanPrice[brandMeans$Company.Name == x])

testData$brandTargetEnc <- testData$Company.Name %>%
  sapply(function(x) brandMeans$meanPrice[brandMeans$Company.Name == x])

# And handle unseen brands:
globalMean <- mean(trainData$carsPricesNum, na.rm = TRUE)

valData$brandTargetEnc[is.na(valData$brandTargetEnc)] <- globalMean
testData$brandTargetEnc[is.na(testData$brandTargetEnc)] <- globalMean


onlyBrandNameData <- trainData[, c("brandTargetEnc", "carsPricesNum")]


firstModel <- lm(onlyBrandNameData$carsPricesNum ~ ., data = onlyBrandNameData)
summary(firstModel)
# As we can see, we have a strong reason to consider car brand as a predictor for this data


# Let's see other columns now:

secondModelData <- trainData[, c("brandTargetEnc", "carsPricesNum", "horsePowerNum", "totalSpeedNum",
                                 "performance0to100Num", "seatsNum", "torqueNum")]
secondModelData <- na.omit(secondModelData)
secondModel <- lm(secondModelData$carsPricesNum ~ ., data = secondModelData)
summary(secondModel)
# Interesting, other than brand name, none of the predictors have low p values
# How about we remove brand name:
thirdModel <- lm(secondModelData$carsPricesNum ~ .-brandTargetEnc, data = secondModelData)
summary(thirdModel)


# Now we see that horsepower, total speed and performance 0 to 100 have small p values
# And that our R^2 dropped from 0.6 to 0.3, so we are explaining less variability without brand name data

# Let's try some interactions 
fourthModel <- lm(secondModelData$carsPricesNum ~ . + brandTargetEnc*seatsNum, data = secondModelData)
# This particular interaction was tried because some car manufacturers, have normal passenger vehicles with usually 4/5 doors,
# and at the same time other sporty vehicles, pricier, that are characterized by 2/3 door (as in 2 door coupe).
summary(fourthModel)
# Significantly low p value for this is seen.




residuals <- resid(thirdModel)
fittedValues <- fitted(thirdModel)

# Plot residuals vs fitted values
plot(
  fittedValues, residuals,
  xlab = "Fitted Values",
  ylab = "Residuals",
  main = "Residuals vs Fitted",
  pch = 19, col = "blue"
)
abline(h = 0, col = "red", lwd = 2)


studentizedResiduals <- rstudent(thirdModel)

# Plot studentized residuals vs fitted values
plot(
  fitted(thirdModel), studentizedResiduals,
  xlab = "Fitted Values",
  ylab = "Studentized Residuals",
  main = "Studentized Residuals vs Fitted",
  pch = 19, col = "blue"
)
abline(h = 0, col = "red", lwd = 2)
abline(h = c(-2, 2), col = "darkgreen", lty = 2)


# Let's check what are outliers:
studentizedResiduals <- rstudent(thirdModel)
secondModelData$studentizedResiduals <- studentizedResiduals
outliers3 <- secondModelData[abs(studentizedResiduals) > 3, ]   

studentizedResiduals <- rstudent(secondModel)
secondModelData$studentizedResiduals <- studentizedResiduals
outliers2 <- secondModelData[abs(studentizedResiduals) > 3, ]   
# As expected, 4 out of 5 outliers are very expensive, multi-million-dollar cars 
# However, we can see that, one remaining outlier is Nissan's van, that actually costs 28000$, for which
# studentized residual is around -8, which means that we predicted much bigger value for that vehicle than 
# it actually is.
# How could that be? Upon more detailed look, we see that in horsepower column, that vehicle has value "2488cc"
# which is a measure for cylinder capacity, not horsepower. Also that car has no Cylinder capacity column.
# So, our regression probably looked at the car that has about 2500 horsepower and thought, oh it probably 
# is very expensive, considering that that is the biggest value for horsepower in the dataset.



# Model that uses all of the available predictors:
wholeModelData <- trainData[, c("brandTargetEnc", "carsPricesNum", "horsePowerNum", "totalSpeedNum",
                                "performance0to100Num", "seatsNum", "torqueNum", "cylinderCapacityNum", "batteryCapacityNum")]
wholeModel <-lm(wholeModelData$carsPricesNum ~ ., data = wholeModelData)
