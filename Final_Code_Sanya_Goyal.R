
# Libraries required in this project
library(caTools)
library(dplyr)
library(plotly)
library(ggplot2)
library(rpart)
library(rattle)
library(rpart.plot)
library(RColorBrewer)

# Importing the wine dataset
wine_data = read.csv(file = "SAPio_DataScience_Challenge.csv", header = T) 

# creating a copy to keep the master set untouched
wine_data_dummy = wine_data

# Getting a sense of the dataset
str(wine_data) #for class of each variable
summary(wine_data) #for summary statistics of each variable

# converting quality to factor variables 
wine_data$quality = as.factor(wine_data$quality)

# Variables with missing values
colMeans(is.na(wine_data)) # volatile acidity = 4.6%
# astringency rating = 5.1%
# residual sugar = 36.3%
# pH = 0.9%
# vintage = 1%

# Treating missing values in wine_data dataset - using similar case imputation, 
# where I calculate median values of continous variables (volatile acidity, astringency rating, pH) 
# and mode for vintage for each quality rating and replace the missing values by the median for its group

# Creating a dataframe like wine_data grouped on the quality rating variable
quality_rating = group_by(wine_data, quality)

# Creating the mode function.
calculated_mode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}

# Creating a table with median and mode values of variables grouped by quality rating 
median_values = summarise(quality_rating, med.volatile = median(volatile.acidity, na.rm = T),
                          med.astringency = median(astringency.rating, na.rm = T),
                          med.residual = median(residual.sugar, na.rm = T),
                          med.pH = median(pH, na.rm = T),
                          mode.vintage = mode(vintage))
# calculating mode without the function 
rat3 = wine_data[wine_data$quality == "5",]
rat8 = wine_data[wine_data$quality == "8",]
rat6 = wine_data[wine_data$quality == "6",]
rat9 = wine_data[wine_data$quality == "9",]

mode(rat9$vintage)

# Replacing missing values in volatile acidity with median in each quality group
wine_data_dummy$volatile.acidity[is.na(wine_data_dummy$volatile.acidity) & wine_data_dummy$quality == "3"] = 0.48 
wine_data_dummy$volatile.acidity[is.na(wine_data_dummy$volatile.acidity) & wine_data_dummy$quality == "4"] = 0.38 
wine_data_dummy$volatile.acidity[is.na(wine_data_dummy$volatile.acidity) & wine_data_dummy$quality == "5"] = 0.33 
wine_data_dummy$volatile.acidity[is.na(wine_data_dummy$volatile.acidity) & wine_data_dummy$quality == "6"] = 0.27 
wine_data_dummy$volatile.acidity[is.na(wine_data_dummy$volatile.acidity) & wine_data_dummy$quality == "7"] = 0.27 
wine_data_dummy$volatile.acidity[is.na(wine_data_dummy$volatile.acidity) & wine_data_dummy$quality == "8"] = 0.28 
wine_data_dummy$volatile.acidity[is.na(wine_data_dummy$volatile.acidity) & wine_data_dummy$quality == "9"] = 0.27 

# Replacing missing values in astringency rating with median in each quality group
wine_data_dummy$astringency.rating[is.na(wine_data_dummy$astringency.rating) & wine_data_dummy$quality == "3"] = 0.81 
wine_data_dummy$astringency.rating[is.na(wine_data_dummy$astringency.rating) & wine_data_dummy$quality == "4"] = 0.75 
wine_data_dummy$astringency.rating[is.na(wine_data_dummy$astringency.rating) & wine_data_dummy$quality == "5"] = 0.74 
wine_data_dummy$astringency.rating[is.na(wine_data_dummy$astringency.rating) & wine_data_dummy$quality == "6"] = 0.72 
wine_data_dummy$astringency.rating[is.na(wine_data_dummy$astringency.rating) & wine_data_dummy$quality == "7"] = 0.71 
wine_data_dummy$astringency.rating[is.na(wine_data_dummy$astringency.rating) & wine_data_dummy$quality == "8"] = 0.70 
wine_data_dummy$astringency.rating[is.na(wine_data_dummy$astringency.rating) & wine_data_dummy$quality == "9"] = 0.73 

# Replacing missing values in residual sugar with median in each quality group
wine_data_dummy$residual.sugar[is.na(wine_data_dummy$residual.sugar) & wine_data_dummy$quality == "3"] = 2.50 
wine_data_dummy$residual.sugar[is.na(wine_data_dummy$residual.sugar) & wine_data_dummy$quality == "4"] = 2.20 
wine_data_dummy$residual.sugar[is.na(wine_data_dummy$residual.sugar) & wine_data_dummy$quality == "5"] = 3.05 
wine_data_dummy$residual.sugar[is.na(wine_data_dummy$residual.sugar) & wine_data_dummy$quality == "6"] = 3.20 
wine_data_dummy$residual.sugar[is.na(wine_data_dummy$residual.sugar) & wine_data_dummy$quality == "7"] = 2.80 
wine_data_dummy$residual.sugar[is.na(wine_data_dummy$residual.sugar) & wine_data_dummy$quality == "8"] = 4.20 
wine_data_dummy$residual.sugar[is.na(wine_data_dummy$residual.sugar) & wine_data_dummy$quality == "9"] = 3.20 

# Replacing missing values in ph with median in each quality group
wine_data_dummy$pH[is.na(wine_data_dummy$pH) & wine_data_dummy$quality == "3"] = 3.245
wine_data_dummy$pH[is.na(wine_data_dummy$pH) & wine_data_dummy$quality == "4"] = 3.220
wine_data_dummy$pH[is.na(wine_data_dummy$pH) & wine_data_dummy$quality == "5"] = 3.190
wine_data_dummy$pH[is.na(wine_data_dummy$pH) & wine_data_dummy$quality == "6"] = 3.210
wine_data_dummy$pH[is.na(wine_data_dummy$pH) & wine_data_dummy$quality == "7"] = 3.220
wine_data_dummy$pH[is.na(wine_data_dummy$pH) & wine_data_dummy$quality == "8"] = 3.230
wine_data_dummy$pH[is.na(wine_data_dummy$pH) & wine_data_dummy$quality == "9"] = 3.280

# Replacing missing values in vintage with mode in each quality group
wine_data_dummy$vintage[is.na(wine_data_dummy$vintage) & wine_data_dummy$quality == "3"] = 2004
wine_data_dummy$vintage[is.na(wine_data_dummy$vintage) & wine_data_dummy$quality == "4"] = 2005
wine_data_dummy$vintage[is.na(wine_data_dummy$vintage) & wine_data_dummy$quality == "5"] = 2003
wine_data_dummy$vintage[is.na(wine_data_dummy$vintage) & wine_data_dummy$quality == "6"] = 2008
wine_data_dummy$vintage[is.na(wine_data_dummy$vintage) & wine_data_dummy$quality == "7"] = 2007
wine_data_dummy$vintage[is.na(wine_data_dummy$vintage) & wine_data_dummy$quality == "8"] = 2003
wine_data_dummy$vintage[is.na(wine_data_dummy$vintage) & wine_data_dummy$quality == "9"] = 2008

#checking for NA values after missinh values treatment
colMeans(is.na(wine_data_dummy)) 

# outlier testing and treatment

summary(wine_data_dummy)

# percentile cut-offs (everything below 1 percentile and above 95th percentile is an outlier)
quantile(wine_data_dummy$fixed.acidity, c(0.01, 0.05, 0.95, 0.99))
quantile(wine_data_dummy$residual.sugar, c(0.01, 0.05, 0.95, 0.99))
quantile(wine_data_dummy$free.sulfur.dioxide, c(0.01, 0.05, 0.95, 0.99))
quantile(wine_data_dummy$total.sulfur.dioxide, c(0.01, 0.05, 0.95, 0.99))
quantile(wine_data_dummy$alcohol, c(0.01, 0.05, 0.95, 0.99))
quantile(wine_data_dummy$sulphates, c(0.01, 0.05, 0.95, 0.99))


# extracting outliers (above 99 percentlile and below 1 percentile from the wine_data_dummy dataset)
FA0.99 = subset(wine_data_dummy, quantile(fixed.acidity, 0.99) < fixed.acidity) 
FA0.01 = subset(wine_data_dummy, quantile(fixed.acidity, 0.01) > fixed.acidity)

RS0.99 = subset(wine_data_dummy, quantile(residual.sugar, 0.99) < residual.sugar) 
RS0.01 = subset(wine_data_dummy, quantile(residual.sugar, 0.01) > residual.sugar) 

FSD0.99 = subset(wine_data_dummy, quantile(free.sulfur.dioxide, 0.99) < free.sulfur.dioxide) 
FSD0.01 = subset(wine_data_dummy, quantile(free.sulfur.dioxide, 0.01) > free.sulfur.dioxide) 

TSD0.99 = subset(wine_data_dummy, quantile(total.sulfur.dioxide, 0.99) < total.sulfur.dioxide) 
TSD0.01 = subset(wine_data_dummy, quantile(total.sulfur.dioxide, 0.01) > total.sulfur.dioxide) 

ALC0.99 = subset(wine_data_dummy, quantile(alcohol, 0.99) < alcohol) 
ALC0.01 = subset(wine_data_dummy, quantile(alcohol, 0.01) > alcohol) 

SUL0.99 = subset(wine_data_dummy, quantile(sulphates, 0.99) < sulphates) 
SUL0.01 = subset(wine_data_dummy, quantile(sulphates, 0.01) > sulphates) 

# creating a copy of wine_data_dummy dataset as wine_data_outlier 
wine_data_outlier = wine_data_dummy
summary(wine_data_outlier)

#removing outliers
wine_data_outlier = wine_data_outlier[!(wine_data_outlier$fixed.acidity %in% FA0.99$fixed.acidity),]
wine_data_outlier = wine_data_outlier[!(wine_data_outlier$fixed.acidity %in% FA0.01$fixed.acidity),]
wine_data_outlier = wine_data_outlier[!(wine_data_outlier$residual.sugar %in% RS0.99$residual.sugar),]
wine_data_outlier = wine_data_outlier[!(wine_data_outlier$residual.sugar %in% RS0.01$residual.sugar),]
wine_data_outlier = wine_data_outlier[!(wine_data_outlier$free.sulfur.dioxide %in% FSD0.99$free.sulfur.dioxide),]
wine_data_outlier = wine_data_outlier[!(wine_data_outlier$free.sulfur.dioxide %in% FSD0.01$free.sulfur.dioxide),]
wine_data_outlier = wine_data_outlier[!(wine_data_outlier$total.sulfur.dioxide %in% TSD0.99$total.sulfur.dioxide),]
wine_data_outlier = wine_data_outlier[!(wine_data_outlier$total.sulfur.dioxide %in% TSD0.01$total.sulfur.dioxide),]
wine_data_outlier = wine_data_outlier[!(wine_data_outlier$alcohol %in% ALC0.99$alcohol),]
wine_data_outlier = wine_data_outlier[!(wine_data_outlier$alcohol %in% ALC0.01$alcohol),]
wine_data_outlier = wine_data_outlier[!(wine_data_outlier$sulphates %in% SUL0.99$sulphates),]
wine_data_outlier = wine_data_outlier[!(wine_data_outlier$sulphates %in% SUL0.01$sulphates),]


# creating a final dataset
final_dataset = wine_data_outlier

# converting the quality variable to a factor 
final_dataset$quality = as.factor(final_dataset$quality)
final_dataset$quality_num = as.integer(final_dataset$quality)

# descriptive analysis - looking at data distribution through box plots
# Distribution of the quality variable
QR = ggplot(final_dataset, aes(x = quality)) + geom_bar(color="black", fill="green", alpha=0.2)
QR # quality variable is very close to being normally distributed with 93% of wines rated between 5 to 7

# boxplots using ggplot2 
# Fixed acidity - good wines tend to have low  fixed acidity
p1 = ggplot(final_dataset, aes(x=quality, y=fixed.acidity)) + 
  geom_boxplot(color="black", fill="green", alpha=0.2) +
  geom_smooth(method = "lm", se=TRUE, aes(group=1))

p1

# Volatile acidity - good wines tend to have low volatile acidity
p2 = ggplot(final_dataset, aes(x=quality, y=volatile.acidity)) + 
  geom_boxplot(color="black", fill="green", alpha=0.2) +
  geom_smooth(method = "lm", se=TRUE, aes(group=1))

p2

# citric acid - there is a slight increase in citric acidity for good wines 
p3 = ggplot(final_dataset, aes(x=quality, y=citric.acid)) + 
  geom_boxplot(color="black", fill="green", alpha=0.2) +
  geom_smooth(method = "lm", se=TRUE, aes(group=1))

p3

# astringency rating - good wines tend to have low  astringency rating
p4 = ggplot(final_dataset, aes(x=quality, y=astringency.rating)) + 
  geom_boxplot(color="black", fill="green", alpha=0.2) +
  geom_smooth(method = "lm", se=TRUE, aes(group=1))

p4

# residual sugar - residual sugar in itself does not seem to determine wine quality, however it may affect in combination with other variables 
p5 = ggplot(final_dataset, aes(x=quality, y=residual.sugar)) + 
  geom_boxplot(color="black", fill="green", alpha=0.2) +
  geom_smooth(method = "lm", se=TRUE, aes(group=1))

p5

# chlorides - good wines tend to have low chlorides
p6 = ggplot(final_dataset, aes(x=quality, y=chlorides)) + 
  geom_boxplot(color="black", fill="green", alpha=0.2) +
  geom_smooth(method = "lm", se=TRUE, aes(group=1))

p6

# free sulphur dioxide - high rated wines usually have high free sulphur dioxide content
p7 = ggplot(final_dataset, aes(x=quality, y=free.sulfur.dioxide)) + 
  geom_boxplot(color="black", fill="green", alpha=0.2) +
  geom_smooth(method = "lm", se=TRUE, aes(group=1))

p7

# total sulphur dioxide - good wines have high total sulphur dioxide content but it may not monotonically increase
p8 = ggplot(final_dataset, aes(x=quality, y=total.sulfur.dioxide)) + 
  geom_boxplot(color="black", fill="green", alpha=0.2) +
  geom_smooth(method = "lm", se=TRUE, aes(group=1))

p8

# density - there seems to be a very high negative correlation between density and quality, high rated wines are less denser 
p9 = ggplot(final_dataset, aes(x=quality, y=density)) + 
  geom_boxplot(color="black", fill="green", alpha=0.2) +
  geom_smooth(method = "lm", se=TRUE, aes(group=1))

p9

# pH - pH for all wines in the dataset lies in the range of 3 to 3.5
p10 = ggplot(final_dataset, aes(x=quality, y=pH)) + 
  geom_boxplot(color="black", fill="green", alpha=0.2) +
  geom_smooth(method = "lm", se=TRUE, aes(group=1))

p10

# sulphates - good wines tend to have low sulphates  
p11 = ggplot(final_dataset, aes(x=quality, y=sulphates)) + 
  geom_boxplot(color="black", fill="green", alpha=0.2) +
  geom_smooth(method = "lm", se=TRUE, aes(group=1))

p11

# alcohol - there is a very high positive correlation between alcohol and wine quality. High rated wines have higher alcohol content 
p12 = ggplot(final_dataset, aes(x=quality, y=alcohol)) + 
  geom_boxplot(color="black", fill="green", alpha=0.2) +
  geom_smooth(method = "lm", se=TRUE, aes(group=1))

p12


# Calculating pairwise correlation for ordinal vs continous variables (Spearman's rank correlation)
cor(x= as.numeric(final_dataset$quality), y=final_dataset$fixed.acidity, method = 'spearman')
cor(x=as.numeric(final_dataset$quality), y=final_dataset$volatile.acidity, method = 'spearman')
cor(x=as.numeric(final_dataset$quality), y=final_dataset$citric.acid, method = 'spearman')
cor(x=as.numeric(final_dataset$quality), y=final_dataset$astringency.rating, method = 'spearman')
cor(x=as.numeric(final_dataset$quality), y=final_dataset$residual.sugar, method = 'spearman')
cor(x=as.numeric(final_dataset$quality), y=final_dataset$chlorides, method = 'spearman')
cor(x=as.numeric(final_dataset$quality), y=final_dataset$free.sulfur.dioxide, method = 'spearman')
cor(x=as.numeric(final_dataset$quality), y=final_dataset$total.sulfur.dioxide, method = 'spearman')
cor(x=as.numeric(final_dataset$quality), y=final_dataset$density, method = 'spearman')
cor(x=as.numeric(final_dataset$quality), y=final_dataset$pH, method = 'spearman')
cor(x=as.numeric(final_dataset$quality), y=final_dataset$sulphates, method = 'spearman')
cor(x=as.numeric(final_dataset$quality), y=final_dataset$alcohol, method = 'spearman')
cor(x=as.numeric(final_dataset$quality), y=final_dataset$vintage, method = 'spearman')


# standardizing the data (might be required later)
vars.to.scale = final_dataset[,c(2:13)]
categorical.vars = final_dataset[, c(1,14,15)]
scaled_dataset = as.data.frame(scale(vars.to.scale))
scaled_final_dataset = cbind(scaled_dataset, categorical.vars ) 


# creating a binary variable to represent high rated(1) and low rated wines(0)
final_dataset$rating = if_else(final_dataset$quality_num >= 4, 1, 0 )
final_dataset$rating = as.factor(final_dataset$rating)

# checking the number of 1s and 0s
table(final_dataset$rating)

# Splitting the data into training and validation dataset
set.seed(121)
split = sample.split(final_dataset$rating, SplitRatio = 0.75)
wine_train = subset(final_dataset, split == TRUE)
wine_test = subset(final_dataset, split == FALSE)


# CART model 
x.vars.in.cart.train = wine_train[,c(1:14)]
x.vars.in.cart.test = wine_test[,c(1:14)]
y.var.in.cart.train = wine_train$rating
y.var.in.cart.test = wine_test$rating

# fitting the cart model
fit = rpart(y.var.in.cart.train ~ ., data = x.vars.in.cart.train, method="class")

# plotting the results of the cart model
fancyRpartPlot(fit)

# starting at the root node, 64% of wines are good(high rating) and 36% of them are not so good(low rating)
# 80% of the wines with higher than 10 alcohol content tends to be good wines
# wines having greater than 10 alcohol content are further split by residual sugar content, 
# if the residual sugar content is greater than 3.2, they have a 90% chance of being good.
# a wine with less than 10 alcohol content, greater than 3.2 residual sugar and less than 0.3 volatile acidity has 65% chance of being a good wine

#Predict Output 
predicted = predict(fit, x.vars.in.cart.test, type = "class")

# confusion matrix
table(y.var.in.cart.test, predicted)

#                   predicted
#y.var.in.cart.test   0   1
#                  0 343 190
#                  1  71 866

# recall - 92.4%
r.cart = 866/(866+71)

#overall accuracy - 82.2%
acc.cart = (343+866)/(343+866+190+71)

#precision
p.cart = 866/(866+190)

# F-score - 86.9%
f.cart = 2*((p.cart * r.cart)/(p.cart + r.cart)) 

# Logistics regression
model <- glm (y.var.in.cart.train ~ ., data = x.vars.in.cart.train, family = binomial(link="logit"))
summary(model)

# odds ratio calculations- odds of being a good wine given a unit change in dependent variables 
exp(model$coefficients)



