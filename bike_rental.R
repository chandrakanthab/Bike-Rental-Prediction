#get Working directory
getwd()


#Install required packages 

#Load Libraries
x = c("ggplot2", "corrgram", "DMwR", "caret", "randomForest", "unbalanced", "C50", "dummies", "e1071", "Information",
      "MASS", "rpart", "gbm", "ROSE", 'sampling', 'DataCombine', 'inTrees')

#install.packages(x)
lapply(x, require, character.only = TRUE)

for ( i in x ) {
  print(i)
  library("ggplot2")
  
}

install.packages(c("dplyr","plyr","reshape","ggplot2","data.table"))
install.packages("GGally")

# Install  Require libraries
library("dplyr")
library("plyr")
library("ggplot2")
library("data.table")
library("GGally")

#load Bike rental data in R

df_day = read.csv("D:/Edwisor assignments/Edwisor Project/day.csv",header=T)

# Summarizing  data 

#Verify first five rows of data
head(df_day)
#target variable is 'cnt' and other variables are independent  variable(or predictors)


#Verify  summary of data
summary(df_day)

#It  shows  variables like 'mnth',holiday','weekday','weathersit' are 
#catogical variabless  and already encoded

#Nummeric  vaiables like 'temp','atem','hum','windspeed' are 
#standardized form

# data  contains  no  missing  values 
# Outliers might be present in variables 'actual','registered','cnt'

#structure of  data
str(df_day)

# for  four variables data type 'mnth',holiday','weekday','weathersit' have to convert to factor


# Analyze variables  by visualize

# function to create univariate distribution of numeric  variables
univariate_numeric <- function(num_x) {
  
  
  ggplot(df_day)+
    geom_histogram(aes(x=num_x,y=..density..),
                   fill= "grey")+
    geom_density(aes(x=num_x,y=..density..))
  
}


# analyze the distribution of  target variable 'cnt'
univariate_numeric(df_day$cnt)

# analyse the distrubution of  independence variable 'temp'
univariate_numeric(df_day$temp)

# analyse the distrubution of  independence variable 'atemp'
univariate_numeric(df_day$atemp)

# analyse the distrubution of  independence variable 'hum'
univariate_numeric(df_day$hum)

# analyse the distrubution of  independence variable 'windspeed'
univariate_numeric(df_day$windspeed)

# analyse the distrubution of  independence variable 'casual'
univariate_numeric(df_day$casual)

# analyse the distrubution of  independence variable 'casual'
univariate_numeric(df_day$regestered)






# the above graph is showing   'cnt' data is normally   distributed


# Visualize categorical Variable 'mnth' with target variable 'cnt'

ggplot(df_day, aes(x=as.factor(mnth), y=cnt),fill="grey") + 
  stat_summary(fun.y="mean", geom="bar")

ggplot(df_day)+
  geom_histogram(aes(x=cnt,y=..density..),
                 fill= "grey")+
  geom_density(aes(x=cnt,y=..density..))

  # Visualize categorical Variable 'holiday' 
  
  
  ggplot(df_day) +
    geom_bar(aes(x=holiday),fill="grey")

 # it is showing that almost all the  cycle rentals are happening  on holidays
  
  # Visualize categorical Variable 'weekday' 
  
ggplot(df_day) +
    geom_bar(aes(x=weekday),fill="grey") 

# it is showing  counts are same on all weekdays

# Visualize categorical Variable 'weathersit' 

ggplot(df_day) +
  geom_bar(aes(x=weathersit),fill="grey") 
  
# count  is more when  whether is " Clear, Few clouds, Partly cloudy, Partly cloudy"


# *****************bivariate  relationship between numeric variables****************************

#check the relationship between 'temp' and 'atemp' variable

ggplot(df_day, aes(x= temp,y=atemp)) +
  geom_point()+
  geom_smooth()

#This  graph is saying that very strong relationship  between 'temp' and 'atemp'

#check the relationship between 'temp' and 'hum' variable

ggplot(df_day, aes(x= temp,y=hum)) +
  geom_point()+
  geom_smooth()

# here  it is showing  Humidity is increses  till temparature is 0.7 and it is decreasing  gradually

#check the relationship between 'temp' and 'windspeed' variable

ggplot(df_day, aes(x= temp,y=windspeed)) +
  geom_point()+
  geom_smooth()

# it is showing that very less nagative   correlation between  temp and windspeed

#check the relationship between all numeric variable using pair plot

ggpairs(df_day[,c('atemp','temp','hum','windspeed','cnt')])

# that above plot stating that less  nagative relationship between
# 'cnt'-'hum'  and cnt-windspeed

# and there is strong positive relationship between 
# temp- cnt and  atemp-cnt

# *************visualize the relationship between categorical variable***************

#check relationship between  season and holiday
rel_mnth_holi= table(df_day$season,df_day$holiday)

rel_mnth_holi

barplot(rel_mnth_holi)
# here contgency table showing  holiday=0  is same for almost all the seasons

#check relationship between  season and weekday

rels_cats_2 <- table(df_day$season,df_day$weekday)

barplot(rels_cats_2)

#check relationship between  season and weathersit

rels_cats_3 <- table(df_day$weathersit,df_day$season)
rels_cats_3

prop.table(rels_cats_3,2)

barplot(rels_cats_3)

#It isstating that in all the season  whether 1 type is large numbers

##check relationship between  holiday and weathersit



rels_cats_4 <- table(df_day$weathersit,df_day$holiday)
rels_cats_4

barplot(df_day$weathersit,df_day$holiday)
#to check in proportion

prop.table(rels_cats_4,2)

barplot(rels_cats_4)

# it it staing that holiday type '0' and  weathersit type '1' almost covered 0.63%


##################################Missing Values Analysis###############################################

missing_val = data.frame(apply(df_day,2,function(x){sum(is.na(x))}))

missing_val$columns=row.names(missing_val)

missing_val
names(missing_val)[1] =  "Missing_percentage"

missing_val$Missing_percentage = (missing_val$Missing_percentage/nrow(df_day)) * 100
missing_val = missing_val[order(-missing_val$Missing_percentage),]
row.names(missing_val) = NULL
missing_val = missing_val[,c(2,1)]

#So , no missing  values are presnt in the data set

########################### Outlier Analysis ###################################

# detect outliers in  'actual' , 'registered' and 'cnt' variables

ggplot(data = df_day, aes(x = "", y = casual)) + 
  geom_boxplot() 
  #coord_cartesian(ylim = c(0, 10000))

# it is showing there are few outliers in  casual variables
# boxplot for  Registered  variable

ggplot(data = df_day, aes(x = "", y = registered)) + 
  geom_boxplot() 
#coord_cartesian(ylim = c(0, 10000))

# there  is no outliers  in registered variables

# boxplot for  cnt variable

ggplot(data = df_day, aes(x = "", y = cnt)) + 
  geom_boxplot() 
#coord_cartesian(ylim = c(0, 10000))

# it is  showing that there is no outliers in  cnt variable


# #################  Treat Outliers ##############

# analyse relationship between causal and cnt variables before  outlier treatment
ggplot(df_day, aes(x= casual,y=cnt)) +
  geom_point()+
  geom_smooth()
df_day_out <-  df_day

# #Remove outliers using boxplot method
 
 
val = df_day_out$casual[df_day_out$casual %in% boxplot.stats(df_day_out$casual)$out]

 df_day_out = df_day_out[which(!df_day_out$casual %in% val),]

 # Boxplot after removing  outliers
 
 # boxplot for  casual variable
 
 ggplot(data = df_day_out, aes(x = "", y = casual)) + 
   geom_boxplot() 
 
 
# verify the relationship after  outliers
 ggplot(df_day_out, aes(x= casual,y=cnt)) +
   geom_point()+
   geom_smooth()

cor(df_day$casual,df_day$cnt)
cor(df_day_out$casual,df_day_out$cnt) 

# there is difference  in correleation between  casual and  cnt before and after outlier detection
# and also loosing  number of observations 
 ######################################### Feature Selection or dimension reduction ################
library(corrgram)

# verify correleation between   Numeric variable

corrgram(df_day[,c('temp','atemp','hum','windspeed','cnt')], order = F,
         upper.panel=panel.pie, text.panel=panel.txt, main = "Correlation Plot")


# correlation matrix  stating  'temp' and 'atemp' having strong relationship
# and there is no  relationship between 'hum' and 'cnt'

#  dimensional  reduction

df_day_features = subset(df_day,select=-c(atemp,hum))

########################  Normality  check #################################


#Normalisation
cnames = c("casual","registered")

for(i in cnames){
  print(i)
  df_day_features[,i] = (df_day_features[,i] - min(df_day_features[,i]))/
    (max(df_day_features[,i] - min(df_day_features[,i])))
}

df_day$casual
#df_day_features

###################################Model Development#######################################

colnames(train)

feture_train_columns=c("season" ,"yr" ,"mnth","holiday","weekday","workingday","weathersit","temp","windspeed","casual","registered","cnt")

#Divide data into train and test using stratified sampling method
set.seed(1234)
library(caret)
train.index = createDataPartition(df_day_features$cnt, p = .80, list = FALSE)
train = df_day_features[ train.index,]
test  = df_day_features[-train.index,]

train_feature = train[,c("season" ,"yr" ,"mnth","holiday","weekday","workingday","weathersit","temp","windspeed","casual","registered","cnt")]

train_feature

test_features = test[,c("season" ,"yr" ,"mnth","holiday","weekday","workingday","weathersit","temp","windspeed","casual","registered","cnt")]

##############  develop Decision tree model ######################


# ##rpart for regression
fit = rpart(cnt ~ ., data = train_feature, method = "anova")

#Predict for new test cases
predictions_DT = predict(fit, test_features[,-12])

print(fit)


#  plotting decision tree

par(cex= 0.8)
plot(fit)
text(fit)


############# Evaluate  Decision tree ###################


#MAPE
#calculate MAPE
MAPE = function(y, yhat){
  mean(abs((y - yhat)/y))
}

MAPE(test_features[,12], predictions_DT)

#Error Rate: 0.1479
#Accuracy: 85.21

#Evaluate  Model using RMSE

RMSE <- function(y_test,y_predict) {
  
  difference = y_test - y_predict
  root_mean_square = sqrt(mean(difference^2))
  return(root_mean_square)
  
}


RMSE(test_features[,12], predictions_DT)

#RMSE = 588.0896


########################## Random Forest ##########################

Rental_rf=randomForest(cnt ~ . , data = train_feature)

Rental_rf

plot(Rental_rf)
################# Evaluate Random Forest ##############

#Predict for new test cases
predictions_DT_two = predict(Rental_rf, test_features[,-12])


MAPE(test_features[,12], predictions_DT_two)

#Error Rate: 0.078
#Accuracy: 92.2



RMSE(test_features[,12], predictions_DT_two)

#RMSE = 270


########  Parameter Tuning for random forest

Rental_rf_2=randomForest(cnt ~ . , data = train_feature,mtry =7,ntree=500 ,nodesize =10 ,importance =TRUE)

Rental_rf_2

  
  
#Predict for new test cases
predictions_RF_two = predict(Rental_rf_2, test_features[,-12])


MAPE(test_features[,12], predictions_RF_two)

#Error Rate: 0.025
#Accuracy: 97.5



RMSE(test_features[,12], predictions_RF_two)

#RMSE = 131


# check Variable  Importance 

varimp <- importance(Rental_rf_2)

varimp
# sort variable  

sort_var <- names(sort(varimp[,1],decreasing =T))

sort_var
# draw varimp plot 

varImpPlot(Rental_rf_2,type = 2)


###################  Tuning Random Forest Dimensional reduction ############################

#   remove four variables  which is  contributing  less

#"season"     "weathersit" "windspeed"  "holiday"   are removing and  developing the  new model

train_feature_two = train[,c("yr" ,"mnth","weekday","workingday","temp","casual","registered","cnt")]
test_features_two = test[,c("yr" ,"mnth","weekday","workingday","temp","casual","registered","cnt")]

# Develop Random Forest  Model


Rental_rf_3=randomForest(cnt ~ . , data = train_feature_two,mtry =7,ntree=500 ,nodesize =10 ,importance =TRUE)

Rental_rf_3



#Predict for new test cases
predictions_RF_three = predict(Rental_rf_3, test_features_two[,-8])


MAPE(test_features_two[,8], predictions_RF_three)

#Error Rate: 0.022
#Accuracy: 97.8



RMSE(test_features_two[,8], predictions_RF_three)

#RMSE = 119


#################### Develop  Linear Regression Model ##########################

#check multicollearity
install.packages('usdm')
library(usdm)
vif(train_feature[,-12])

vifcor(train_feature[,-12], th = 0.9)
# Correleation between two variables is 'season' and 'mnth' is 0.82 so, removing one variable from the model


train_feature_three = train[,c("yr" ,"mnth","holiday","weekday","workingday","weathersit","temp","windspeed","casual","registered","cnt")]
test_features_three = test[,c("yr" ,"mnth","holiday","weekday","workingday","weathersit","temp","windspeed","casual","registered","cnt")]

# develop Linear Regression  model


#run regression model
lm_model = lm(cnt ~., data = train_feature_three)


#Summary of the model
summary(lm_model)


# observe the  residuals and   coefficients  of the linear regression model


# Predict  the Test data 


#Predict
predictions_LR = predict(lm_model, test_features_three[,-11])

# Evaluate Linear Regression Model



MAPE(test_features_three[,11], predictions_LR)

#Error Rate: 6.416578e-16
#Accuracy: 99.9 + accuracy

RMSE(test_features_three[,11], predictions_LR)

#RMSE = 2.327632e-12


# COnclusion  For this Dataset  Linear Regression is  Accuracy  is '99.9'
# and RMSE = 2.327632e-12 

