install.packages('dichromat')
install.packages('leaflet')
install.packages('GGally')
install.packages('gridExtra')
install.packages('DT')
installed.packages('ggcor')

library(lubridate)
library(dplyr)
library(ggplot2)
library(dichromat)
library(leaflet)
library(GGally)
library(gridExtra)
library(DT)

data <- read.csv("~/Downloads/kc_house_dataset.csv")

data$is_renovated <- with(data, ifelse(yr_renovated==0, 0, 1))
data$is_basement <- with(data,ifelse(sqft_basement==0, 0, 1))
data$is_renovated <- as.factor(data$is_renovated)
data$is_basement <- as.factor(data$is_basement)
data$waterfront <- as.factor(data$waterfront)

# Research Question 1

data0 <- data %>% select(-id, -date)
lm0 <- lm(price ~ ., data = data0)
summary(lm0)
par(mfrow = c(2,2))
plot(lm0, 1:4)

data1 <- data %>% select(-id, -date, -zipcode, -lat, -long)
lm1 <- lm(price ~ ., data = data1)
summary(lm1)
par(mfrow = c(2,2))
plot(lm1, 1:4)

red_model <- lm(price ~ 1 , data = data1)
full_model <- lm(price ~ ., data=data1)
n <- nrow(data1)

#FOWARD AIC
(forward_aic <- step(red_model, scope = list(lower = red_model, upper = full_model), direction = "forward", trace = 0))
#FOWARD BIC
(forward_bic <- step(red_model, scope = list(lower = red_model, upper = full_model), direction = "forward", k = log(n), trace = 0))
#Backward AIC
(back_aic <- step(full_model, direction = "backward", trace = 0))
#Backward BIC
(back_bic <- step(full_model, direction = "backward", k = log(n), trace = 0))
#Stepwise AIC
(step_aic <- step(red_model, scope = list(lower = red_model, upper = full_model), trace = 0))
#Stepwise BIC
(step_bic <- step(red_model, scope = list(lower = red_model, upper = full_model), k = log(n), trace = 0))

vs_fit <- lm(price ~ bedrooms + bathrooms + sqft_living + floors + 
    waterfront + view + condition + grade + yr_built + yr_renovated + 
    sqft_living15 + sqft_lot15 + is_renovated + is_basement,
    data = data1)
summary(vs_fit)
par(mfrow=c(2,2))
plot(vs_fit,1:4)

library(car)
#Transformation
#checking whether transformation is required on predictors
pt <- powerTransform(cbind(sqft_living, sqft_living15 , sqft_lot15 ) ~ 1, data1)
summary(pt)
#taking log transformation on predictors and refitting the model
pvs_fit <- lm(price ~ bedrooms + bathrooms + log(sqft_living)  +
    floors + waterfront + view + condition + grade +
    yr_built + yr_renovated + log(sqft_living15) + log(sqft_lot15) +
    is_renovated + is_basement , data = data1)
#checking whether transformation is required on response
summary(powerTransform(pvs_fit))
#taking log transformation on response and refitting the model
rvs_fit <- lm(log(price) ~ bedrooms + bathrooms + log(sqft_living) +
    floors + waterfront + view + condition + grade +
    yr_built + yr_renovated +  log(sqft_living15) + log(sqft_lot15)     + is_renovated + is_basement , data = data1)
summary(rvs_fit)
par(mfrow=c(2,2))
plot(rvs_fit,1:4)

#outlier
house_std <- rstandard(rvs_fit)
which(abs(house_std) > 6)
p <- 14
n <- nrow(data1)
house_hats <- hatvalues(rvs_fit)
sum(house_hats)
plot(hatvalues(rvs_fit), rstandard(rvs_fit),
xlab='Leverage', ylab='Standardized Residuals')
abline(v = 3*(p+1)/n , lty = 2, lwd = 2, col = "red")
abline(h = c(-4, 4), lty = 2, lwd = 2, col = "blue")
#Influential points: Cook's Distance
cooks <- cooks.distance(rvs_fit)
influenceIndexPlot(rvs_fit)

# Research Question 2

data <- read.csv("~/Downloads/kc_house_dataset.csv")
data$avg_price_per_zip <- ave(data$price, data$zipcode)
data$is_renovated <- with(data, ifelse(yr_renovated==0, 0, 1))
data$is_basement <- with(data,ifelse(sqft_basement==0, 0, 1))
data$above_avg_zip_price <- ifelse(data$price > data$avg_price_per_zip, 1, 0)
data$is_renovated <- as.factor(data$is_renovated)
data$is_basement <- as.factor(data$is_basement)
data$above_avg_zip_price <- as.factor(data$above_avg_zip_price)
data$waterfront <- as.factor(data$waterfront)

data_1 <- data %>% select(-id, -date, -price)
mylogit <- glm(above_avg_zip_price ~ ., data = data_1, family = "binomial")
summary(mylogit)

#After removing insignificant variables
data_1 <- data %>% select(-id, -date, -price, -sqft_basement, -sqft_lot15, -zipcode)
mylogit <- glm(above_avg_zip_price ~ ., data = data_1, family = "binomial")
summary(mylogit)

glm.probs <- predict(mylogit,type = "response")
glm.pred <- ifelse(glm.probs > 0.5, 1, 0)
attach(data)
table(glm.pred, above_avg_zip_price)
mean(glm.pred == above_avg_zip_price) # Accuracy

# Split Train & Test

data_1 <- data %>% select(-id, -date, -price, -sqft_basement, -sqft_lot15, -zipcode)
n <- nrow(data_1)
train <- sample(1:n, size = 15129)
glm_train <- glm(above_avg_zip_price ~ ., data = data_1, family = "binomial", subset = train)
summary(glm_train)
test <- data_1[-train, ]
probs_test <- predict(glm_train, newdata = test, type = 'response')
preds_test <- rep(0,6484)
preds_test[probs_test > 0.5] <- 1
tb <- table(prediction = preds_test, actual = test$above_avg_zip_price)
addmargins(tb)

#Prediction

user_details <- data.frame(bedrooms=3, bathrooms=2, sqft_living=1500,sqft_lot=200,floors=2,waterfront=1,
                           view=2,condition=4,grade=7,sqft_above=1500,yr_built=1994,yr_renovated=0,lat=47.6740,long=-122.1215,
                           sqft_living15=1450, avg_price_per_zip=350000,is_renovated=0,
                           is_basement=1)
user_details$is_renovated <- as.factor(user_details$is_renovated)
user_details$is_basement <- as.factor(user_details$is_basement)
user_details$waterfront <- as.factor(user_details$waterfront)

prediction_val = predict(mylogit, newdata = user_details, type="response")

prediction_val[findInterval(prediction_val, c(0,0.25)) == 1L] <- "Definitely not worth bidding over the asking price for this location!"
prediction_val[findInterval(prediction_val, c(0.25,0.5)) == 1L] <- "Better to wait or avoid bidding over asking price for this location."
prediction_val[findInterval(prediction_val, c(0.5,0.75)) == 1L] <- "Could be a good idea to bid a little over the asking price for this location!"
prediction_val[findInterval(prediction_val, c(0.75,1)) == 1L] <- "It is a steal of a deal! Great idea to bid over the asking price for this location!"

cat("Recommendation for this property:\n \"", prediction_val, "\"\nThanks for using our service!\n\n- Vaishnavi Real Estate Intelligence Services")

#EDA

library(leaflet)
#Map representing king county houses based on price.
KCHouseData <- read.csv("~/Downloads/kc_house_dataset.csv")
KCHouseData$PriceBin<-cut(KCHouseData$price, c(0,250e3,500e3,750e3,1e6,2e6,999e6))

center_lon = median(KCHouseData$long,na.rm = TRUE)
center_lat = median(KCHouseData$lat,na.rm = TRUE)

factpal <- colorFactor(c("black","blue","yellow","orange","#0B5345","red"), 
                       KCHouseData$PriceBin)

library(ggplot2)
par(mfrow=c(1,2))
#Histogram for price
ggplot(kc_house_data, aes(price)) + 
  geom_histogram(col="pink", aes(fill=..count..)) +
  scale_fill_gradient("Count", low="white", high="coral") + 
  labs(title = "Price histogram", x = "Price", y = "Count")

#Histogram for price after log
ggplot(kc_house_data, aes(log(price))) + 
  geom_histogram(col="pink", aes(fill=..count..)) +
  scale_fill_gradient("Count", low="white", high="coral") + 
  labs(title = "Price histogram", x = "Log Price", y = "Count")



leaflet(KCHouseData) %>% addProviderTiles("Esri.NatGeoWorldMap") %>%
  addCircles(lng = ~long, lat = ~lat, 
             color = ~factpal(PriceBin))  %>%
  # controls
  setView(lng=center_lon, lat=center_lat,zoom = 12) %>%
  
  addLegend("bottomright", pal = factpal, values = ~PriceBin,
            title = "House Price Distribution",
            opacity = 1)

#correlation plot
library("corrplot")
M<-cor(select_if(kc_house_data, is.numeric))
corrplot(M, method="circle")

#scatter plot for price

library(ggplot2)
library(gridExtra)

house_good_1 <- kc_house_data %>% select(bathrooms, sqft_living, grade, sqft_above, lat, long, sqft_living15, price)

p1 <- ggplot(house_good_1, aes(x = bathrooms, y = price)) +
geom_point(color = 'blue', size = 0.5) +
geom_smooth(method="lm", color = 'red', size = 0.5) +
theme_linedraw() + 
labs(x = 'Total Bathrooms', y = 'Price (USD)') + 
scale_y_continuous(labels = scales::comma)

p2 <- ggplot(house_good_1, aes(x = sqft_living, y = price)) +
geom_point(color = 'red', size = 0.5) +
geom_smooth(method="lm", color = 'blue', size = 0.5) +
theme_linedraw() + 
labs(x = 'Living Area (sq.ft)', y = 'Price (USD)') + 
scale_y_continuous(labels = scales::comma)

p3 <- ggplot(house_good_1, aes(x = grade, y = price)) +
geom_point(color = 'green', size = 0.5) +
geom_smooth(method="lm", color = 'purple', size = 0.5) +
theme_linedraw() + 
labs(x = 'Grade', y = 'Price (USD)') + 
scale_y_continuous(labels = scales::comma)

p4 <- ggplot(house_good_1, aes(x = sqft_above, y = price)) +
geom_point(color = 'purple', size = 0.5) +
geom_smooth(method="lm", color = 'yellow', size = 0.5) +
theme_linedraw() + 
labs(x = 'House Area (sq.ft)', y = 'Price (USD)') + 
scale_y_continuous(labels = scales::comma)

p5 <- ggplot(house_good_1, aes(x = sqft_living15, y = price)) +
geom_point(color = 'orange', size = 0.5) +
geom_smooth(method="lm", color = 'green', size = 0.5) +
theme_linedraw() + 
labs(x = 'Living Area 15 (sq.ft)', y = 'Price (USD)') + 
scale_y_continuous(labels = scales::comma)
grid.arrange(p1,p2,p3,p4,p5, nrow = 3,
              top = "House Sales in King County, USA")

#scatter plot for log_price(house_good)

#library(ggplot2)
#library(gridExtra)

house_good <- kc_house_data %>% select(price,bathrooms, sqft_living, grade, sqft_above, lat, long, sqft_living15)

p1 <- ggplot(house_good, aes(x = bathrooms, y = log(price))) +
geom_point(color = 'blue', size = 0.5) +
geom_smooth(method="lm", color = 'red', size = 0.5) +
theme_linedraw() + 
labs(x = 'Total Bathrooms', y = 'Log Price (USD)') + 
scale_y_continuous(labels = scales::comma)

p2 <- ggplot(house_good, aes(x = sqft_living, y = log(price))) +
geom_point(color = 'red', size = 0.5) +
geom_smooth(method="lm", color = 'blue', size = 0.5) +
theme_linedraw() + 
labs(x = 'Living Area (sq.ft)', y = 'Log Price (USD)') + 
scale_y_continuous(labels = scales::comma)

p3 <- ggplot(house_good, aes(x = grade, y = log(price))) +
geom_point(color = 'green', size = 0.5) +
geom_smooth(method="lm", color = 'purple', size = 0.5) +
theme_linedraw() + 
labs(x = 'Grade', y = 'Log Price (USD)') + 
scale_y_continuous(labels = scales::comma)

p4 <- ggplot(house_good, aes(x = sqft_above, y = log(price))) +
geom_point(color = 'purple', size = 0.5) +
geom_smooth(method="lm", color = 'yellow', size = 0.5) +
theme_linedraw() + 
labs(x = 'House Area (sq.ft)', y = 'Log Price (USD)') + 
scale_y_continuous(labels = scales::comma)

p5 <- ggplot(house_good, aes(x = sqft_living15, y = log(price))) +
geom_point(color = 'orange', size = 0.5) +
geom_smooth(method="lm", color = 'green', size = 0.5) +
theme_linedraw() + 
labs(x = 'Living Area 15 (sq.ft)', y = 'Log Price (USD)') + 
scale_y_continuous(labels = scales::comma)
grid.arrange(p1,p2,p3,p4,p5, nrow = 3,
              top = "House Sales in King County, USA")

# Homes by year
kc_house_data %>%
group_by(yr_built) %>%
summarise(n = n()) %>%
ggplot(aes(x = yr_built, y = n)) +
geom_line(color = 'black') +
geom_point(color = 'blue', size = 0.5) +
geom_smooth(method="lm", color = 'red', size = 0.5) +
theme_linedraw() +
theme(plot.title = element_text(hjust = 0, face = 'bold',color = 'black'),
      plot.subtitle = element_text(face = "italic")) +
labs(x = 'Year', y = 'Total', title = "House Sales in King County, USA",
     subtitle = "Number of house built in 1900 - 2015") +
scale_x_continuous(breaks=seq(1900, 2015, 10))

# Boxplot

par(mfrow=c(1,2))
boxplot(log(price)~view,data=kc_house_data,main="View Vs. Log Price", xlab="view",ylab="log_price",col="orange",border="brown")

boxplot(log(price)~grade,data=kc_house_data,main="Grade Vs. Log Price", xlab="grade",ylab="log_price",col="orange",border="brown")

par(mfrow=c(1,3))
#categorical
boxplot(log(price)~waterfront,data=kc_house_data,main="Waterfront Vs. Log Price", xlab="Waterfront",ylab="log_price",col="orange",border="brown")

boxplot(log(price)~is_renovated,data=kc_house_data,main="is_renovated Vs. Log Price", xlab="is_renovated",ylab="log_price",col="orange",border="brown")

boxplot(log(price)~is_basement,data=kc_house_data,main="is_basement Vs. Log Price", xlab="is_basement",ylab="log_price",col="orange",border="brown")

