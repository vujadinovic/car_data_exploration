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
carsData$fuelsType <- as.factor(carsData$Fuel.Types)
levels(carsData$fuelsType)
# 23 levels are seen, mostly redundant
# This column is used to classify cars as Fossil/Electric/Hybrid


carsData <- carsData %>%
  mutate(fuelGroup = case_when(
    grepl("Hybrid|hyrbrid", Fuel.Types, ignore.case = TRUE) ~ "Hybrid",
    grepl("Electric|EV|Battery", Fuel.Types, ignore.case = TRUE) ~ "Electric",
    grepl("Petrol|Diesel|CNG", Fuel.Types, ignore.case = TRUE) ~ "Fossil",
    TRUE ~ "Other" 
  ))

carsData$fuelGroup <- factor(carsData$fuelGroup, levels = c("Fossil", "Hybrid", "Electric", "Other"))

table(carsData$fuelGroup)
carsData[carsData$fuelGroup == "Other", c("Fuel.Types", "fuelGroup")]
# As we have three hydrogen cars in dataset, we can drop them:
carsData <- carsData[carsData$fuelGroup != "Other", ]


# As we have Engines column, which is very messy, and from which, given that we have cylinder/battery capacity 
# only thing that we can possibly extract is number of cylinders and engine configuration. That is linked to cylinder capacity
# anyway, so we will drop it for now.
# The Cars.Names column was dropped because it contains model names with many unique values, for which most of information is 
# already captured.


carsData$ccBatteryNum <- as.numeric(str_extract(carsData$CC.Battery.Capacity, "\\d+\\.?\\d*"))


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


brandNameModel <- lm(onlyBrandNameData$carsPricesNum ~ ., data = onlyBrandNameData)
summary(brandNameModel)
# As we can see, we have a strong reason to consider car brand as a predictor for this data


# Let's see other columns now:

fullModelData <- trainData[, c("brandTargetEnc", "carsPricesNum", "horsePowerNum", "totalSpeedNum",
                                 "performance0to100Num", "seatsNum", "torqueNum", "ccBatteryNum", "fuelGroup")]
fullModelData <- na.omit(fullModelData)
fullModel <- lm(fullModelData$carsPricesNum ~ ., data = fullModelData)
summary(fullModel)
# Interesting, other than brand name, none of the predictors have low p values
# How about we remove brand name:
thirdModel <- lm(fullModelData$carsPricesNum ~ .-brandTargetEnc, data = fullModelData)
summary(thirdModel)
# Now we see that horsepower, total speed, performance 0 to 100, torque number have small p values
# And that our R^2 dropped from 0.7 to 0.52, so we are explaining less variability without brand name data

# Let's try some interactions 
brandSeatsIntModel <- lm(fullModelData$carsPricesNum ~ . + brandTargetEnc*seatsNum, data = fullModelData)
# This particular interaction was tried because some car manufacturers, have normal passenger vehicles with usually 4/5 doors,
# and at the same time other sporty vehicles, pricier, that are characterized by 2/3 door (as in 2 door coupe).
summary(brandSeatsIntModel)
# Significantly low p value for this interaction is seen.

# Now we can "filter" whether ccBatteryNum is for electric, hybrid or standard car by considering the following interaction:
powerFuelIntModel <- lm(fullModelData$carsPricesNum ~ . + fuelGroup*ccBatteryNum, data = fullModelData)
summary(powerFuelIntModel)
# Interaction columns show no statistical significance.
# It remains to be checked









#-----------OUTLIERS-------------------------
#-------------TO BE DONE---------------------

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
fullModelData$studentizedResiduals <- studentizedResiduals
outliers3 <- fullModelData[abs(studentizedResiduals) > 3, ]   

studentizedResiduals <- rstudent(fullModel)
fullModelData$studentizedResiduals <- studentizedResiduals
outliers2 <- fullModelData[abs(studentizedResiduals) > 3, ]   
# As expected, 4 out of 5 outliers are very expensive, multi-million-dollar cars 
# However, we can see that, one remaining outlier is Nissan's van, that actually costs 28000$, for which
# studentized residual is around -8, which means that we predicted much bigger value for that vehicle than 
# it actually is.
# How could that be? Upon more detailed look, we see that in horsepower column, that vehicle has value "2488cc"
# which is a measure for cylinder capacity, not horsepower. Also that car has no Cylinder capacity column.
# So, our regression probably looked at the car that has about 2500 horsepower and thought, oh it probably 
# is very expensive, considering that that is the biggest value for horsepower in the dataset.




