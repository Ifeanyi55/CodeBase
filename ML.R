library(caTools)
library(cluster)
library(caret)
library(carData)
library(archdata)
library(arulesViz)
library(aod)
library(DiagrammeR)
library(FeatureTerminatoR)
library(DMwR2)
library(e1071)
library(xgboost)
library(kernlab)
library(klaR)
library(ISLR)
library(factoextra)
library(parameters)
library(tidymodels)
library(tm)
library(see)
library(neuralnet)
library(performance)
library(gmodels)
library(class)
library(mlflow)
library(rpart)
library(rpart.plot)
library(rattle)
library(randomForest)
library(Boruta)
library(RColorBrewer)
library(SnowballC)
library(ggplot2)
library(ggfortify)
library(GGally)
library(ggshadow)
library(gapminder)
library(plotly)
library(psych)
library(ggvis)
library(gridExtra)
library(dplyr)
library(DataExplorer)
library(dendRoAnalyst)
library(plyr)
library(readxl)
library(palmerpenguins)
library(highcharter)
library(modelsummary)
library(modeldata)
library(modelStudio)
library(gtsummary)
library(DALEX)
library(ranger)
library(h2o)

## Linear regression

demo <- read.csv("demo.csv")
View(demo)

## Splitting the data

demo_split <- sample.split(demo,SplitRatio = 0.7)

## Create training and test data

train <- subset(demo,demo_split=TRUE)
test <- subset(demo,demo_split=FALSE)

## Create trained model

model <- lm(demo$INCOME ~.,data = train)

## Visualize model parameters in table
model_parameters(model = model)

summary(model)

## Run predictor with test data

pred <- predict(model,test)
pred

plot(test$INCOME,type = "l",lty=1.8,col="red")
plot(pred,type = "l",lty=1.8,col="blue")

## Find accuracy

accuracy <- sqrt(mean(pred-demo$INCOME)^2)
accuracy

linear_regression <- lm(mpg ~ hp+disp,data = mtcars)
summary(linear_regression)


## Mtcars linear regression
datacars <- mtcars
datacars <- datacars %>% 
  select(mpg,disp,hp,wt)
View(datacars)

## Split data into train and test
ind <- sample(2,nrow(datacars),replace = TRUE,prob = c(0.7,0.3))

traincar <- datacars[ind == 1,]
testcar <- datacars[ind == 2,]

dim(traincar)
dim(testcar)

## Fit linear model
set.seed(123)
lm <- lm(mpg ~ disp + hp + wt,data = datacars)

## View model summary in a table
summary(lm)

tbl_regression(lm)

model_parameters(lm)

## Visualize model
ggplotly(modelplot(lm)) 

## Predict with the model
carpred <- predict(lm,testcar)

carpred


plot(carpred,type = "l")


## Test with new data
newcar <- data.frame(
  disp = c(125.7),
  hp = c(60),
  wt = c(4.710)
)

predict(lm,newcar)

## Find model correlation accuracy
actuals_pred <- data.frame(cbind(
  actuals = testcar,
  predicted = carpred
))

correlation_accuracy <- cor(actuals_pred$actuals.mpg,actuals_pred$predicted)

correlation_accuracy


## Carseats linear regression

csdata <- Carseats 

## Select variables of interest and remoc=ve any missing values

csdata <- csdata %>% 
  select(Sales,Income,Advertising) %>% 
  na.omit()
View(csdata)

## Do some preliminary analysis of the data

# Scatter plot
csdata %>% 
  ggplot()+aes(x = Income,y = Sales)+geom_point(size = 2,color = "blue")+
  geom_smooth()+labs(title = "Income ~ Sales")

csdata %>% 
  ggplot()+aes(x = Advertising,y = Sales)+geom_point(size = 2,color = "green")+
  geom_smooth()+labs(title = "Advertising ~ Sales")

csdata %>% 
  ggplot()+aes(x = Price,y = Sales)+geom_point(size = 2,color = "red")+
  geom_smooth()+labs(title = "Price ~ Sales")

# Density plot
csdata %>% 
  ggplot()+aes(x = Income)+geom_density(stat = "density",fill = "blue")+
  labs(title = paste("Skewness:",round(skewness(csdata$Income),2)))

csdata %>% 
  ggplot()+aes(x = Advertising)+geom_density(stat = "density",fill = "green")+
  labs(title = paste("Skewness:",round(skewness(csdata$Advertising),2)))

csdata %>% 
  ggplot()+aes(x = Price)+geom_density(stat = "density",fill = "red")+
  labs(title = paste("Skewness:",round(skewness(csdata$Price),2)))

csdata %>% 
  ggplot()+aes(x = Sales)+geom_density(stat = "density",fill = "purple")+
  labs(title = paste("Skewness:",round(skewness(csdata$Sales),2)))


## Correlation analysis
ccor <- cor(csdata)

gcor <- ggcorrplot::ggcorrplot(ccor)

ggplotly(gcor)

## With Data explorer
DataExplorer::create_report(csdata)

## Split data into train and test
csind <- sample(2,nrow(csdata),replace = TRUE,prob = c(0.8,0.2))

cstrain <- csdata[csind == 1,]
cstest <- csdata[csind == 2,]

dim(cstrain)
dim(cstest)

## Fit model
set.seed(1234)
csmodel <- lm(Sales ~.,data = cstrain)

summary(csmodel)

model_parameters(csmodel)

csmodel$rank
csmodel$coefficients

AIC(csmodel)
BIC(csmodel)

## Predicting with the model
cspred <- predict(csmodel,cstest)

cspred

## Make actuals vs predicted data frame
actuals.pred <- data.frame(cbind(actuals = cstest$Sales,
                                 predicted = cspred))

## Min/Max accuracy calculation
min_max_accuracy <- mean(apply(actuals.pred,1,min)/apply(actuals.pred,1,max))

min_max_accuracy

## Predicting on new data
csnew <- data.frame(
  Income = c(127),
  Advertising = c(17)
)

predict(csmodel,csnew)

## Mtcars linear regression
datacars <- mtcars

# Check for linearity
datacars %>% 
  hchart(type = "scatter",hcaes(x = cyl,y = mpg),color = "blue")

datacars %>% 
  hchart(type = "scatter",hcaes(x = disp, y = mpg),color = "green")

datacars %>% 
  hchart(type = "scatter",hcaes(x = hp,y = mpg),color = "red")

## Check normalcy of predictors distribution
datacars %>% 
  ggplot()+aes(x = cyl)+geom_density(fill = "blue")+
  labs(title = paste("Skweness:",skewness(datacars$cyl)))

datacars %>% 
  ggplot()+aes(x = disp)+geom_density(fill = "green")+
  labs(title = paste("Skweness:",skewness(datacars$disp)))

datacars %>% 
  ggplot()+aes(x = hp)+geom_density(fill = "red")+
  labs(title = paste("Skweness:",skewness(datacars$hp)))

## Run correlation analysis
datacars <- datacars %>% 
  select(mpg,cyl,disp,hp)
  
dcor <- cor(datacars)  

ggplotly(ggcorrplot::ggcorrplot(dcor))

## Build model
lmcars <- lm(mpg ~ cyl+disp+hp,data = datacars)

summary(lmcars)

model_parameters(lmcars)

## Check model
check_model(lmcars)


## With tidy model
lmcars_tidy <- linear_reg() %>% 
  set_engine("lm") %>% 
  fit(mpg ~ cyl+disp+hp,data = datacars)

check_model(lmcars_tidy)


## Academic results binary logistic
mydata <- read.csv("https://stats.idre.ucla.edu/stat/data/binary.csv")

View(mydata)
str(mydata)
is.data.frame(mydata)


write.csv(mydata,file = "GRE.csv")
mydata <- read.csv("GRE.csv")

summary(mydata)

## Compute mean and standard deviation
apply(mydata,2,mean)
apply(mydata,2,sd)

## Make sure there are no 0 cells with xtab
xtabs(~admit + rank,data = mydata)

## Convert rank to a factor variable'
mydata$rank <- factor(mydata$rank)
str(mydata)

## Build logit model
mylogit <- glm(admit ~.,data = mydata,family = "binomial")

summary(mylogit)
model_parameters(mylogit)

## Get confidence intervals
confint(mylogit)

confint.default(mylogit)

## You can exponentiate the coefficients into odd ratios
oddratios <- exp(coef(mylogit))

oddratios

## Exponentiate odd ratios and confidence intervals
exp_tab <- exp(cbind(OddsR = coef(mylogit),confint(mylogit)))
exp_tab

## Decision tree on mydata

## Clean data by removing last column
mydata <- mydata[,-4]

View(mydata)
str(mydata)

## Convert target variable to factor
mydata$admit <- factor(mydata$admit,levels = c(1,0),labels = c("Yes","No"))

## Visualize data distribution

mydata %>% 
  hchart(type = "scatter",hcaes(x = gpa,y = gre,color = admit))

## Split data into training and testing
mysplit <- sample.split(mydata$admit, SplitRatio = 0.7)
mydata_train <- subset(mydata,mysplit == TRUE)
mydata_test <- subset(mydata,mysplit == FALSE)

glimpse(mydata_train)
glimpse(mydata_test)

## Fit decision tree model
set.seed(123)
mymodel <- rpart(admit ~.,data = mydata_train)
print(mymodel)
summary(mymodel)

## Visualize tree
rpart.plot(mymodel)
fancyRpartPlot(mymodel)

## Predict with the model
mypred <- predict(mymodel,mydata_test,method = "classification")
plot(mypred)

mydata_test$pred <- mypred
View(mydata_test)

plot(mydata_test$pred,type = "l",color = mydata$admit)

## Build confusion matrix
confusionMatrix(table(mypred,mydata_test$admit))


## Mtcars linear regression

mtcars_split <- sample.split(mtcars,SplitRatio = 0.8)
training <- subset(mtcars,mtcars_split = "TRUE")
testing <- subset(mtcars,mtcars_split = "FALSE")

training

linear_model <- lm(mpg ~ disp+wt+hp,data = training)
summary(linear_model)
model_parameters(linear_model)

pred <- predict(linear_model,testing,family = "response")
pred
plot(pred,type = "l",col = "blue")
summary(pred)

## Iris regression

iris_split <- sample.split(iris,SplitRatio = 0.7,)

iris_train <- subset(iris,iris_split == TRUE)
iris_test <- subset(iris,iris_split == FALSE)

iris_train
iris_test

## K-Nearest Neighbor

head(iris)

## Sepal length and width
iris %>%
ggvis(~Sepal.Length,~Sepal.Width,fill = ~Species) %>% layer_points() %>% layer_lines()

## Petal length and width

iris %>%
  ggvis(~Petal.Length,~Petal.Width,fill = ~Species) %>% layer_points() %>% layer_lines()


iris %>%
  ggplot()+aes(x=Sepal.Length,y=Sepal.Width,color=Species)+geom_point(size = 2)+
  theme_light()

## Correlation scores

setosa <- iris %>%
  filter(Species == "setosa")
cor(setosa$Petal.Length,setosa$Petal.Width)

versicolor <- iris %>%
  filter(Species == "versicolor")
cor(versicolor$Petal.Length,versicolor$Petal.Width)
str(iris)

table(iris$Species)
prop.table(table(iris$Species))

## Summary of iris dataset

summary(iris)

## Normalizing the min and max of the iris dataset

iris_preprocess <- preProcess(iris[,c(1:4)],method = c("range"))
iris_norm <- predict(iris_preprocess,iris[,c(1:4)])

summary(iris_norm)
iris_norm
iris

## Take a random sample of 90% of the dataset in order to increase the probability of all species being included


iris_ran <- sample(1:nrow(iris),0.9*nrow(iris))
iris_ran

## Create training and test dataset

training_iris <- iris_norm[iris_ran,] 
testing_iris <-  iris_norm[-iris_ran,]

training_iris
testing_iris

## Create training and testing labels

training_iris_labels <- iris[iris_ran,5]
testing_iris_labels <- iris[-iris_ran,5]

training_iris_labels
testing_iris_labels

## Build the KNN model

iris_knn <- knn(train = training_iris,test = testing_iris,cl = training_iris_labels,k = 13)

## Create a confusion matrix and accuracy score

iris_tab <- table(iris_knn,testing_iris_labels)

iris_tab

iris_confusion_matrix <- confusionMatrix(table(iris_knn,testing_iris_labels))
iris_confusion_matrix

## Test for accuracy

accuracy <- function(x){
  sum(diag(x)/(sum(rowSums(x))))*100
}

accuracy(iris_tab)

## Selecting the optimal value for k

i = 1
opt.k = 1
for(i in 1:28){
  
  iris_k <- knn(training_iris,testing_iris,training_iris_labels,k = i)
  opt.k[i] <- 100 * sum(testing_iris_labels == iris_k)/NROW(testing_iris_labels)
  k = i
  cat(k, "=",opt.k[i],"\n")
  
}

plot(opt.k,type = "b",xlab = "K-value",ylab = "Accuracy level")



## Visualizing the knn classification
plot.df <- data.frame(
  testing_iris,predicted = iris_knn
)

plot.df1 <- data.frame(
  x = plot.df$Sepal.Length,
  y = plot.df$Sepal.Width,
  predicted = plot.df$predicted
)

find_hull <- function(df) df[chull(df$x,df$y),]
boundary <- ddply(plot.df1,.variables = "predicted",.fun = find_hull)

plot.df %>%
  ggplot()+aes(x=Sepal.Length,y=Sepal.Width,color=predicted)+
  geom_point(size = 5)+geom_polygon(data = boundary,aes(x,y),alpha = 0.5)


## CART

titanic_data <- "https://goo.gl/At238b" %>%  #DataFlair
  read.csv %>% # read in the data
  select(survived, embarked, sex, 
         sibsp, parch, fare) %>%
  mutate(embarked = factor(embarked),
         sex = factor(sex))

titanic_data
write.csv(titanic_data,file = "titanic.csv")

titanic.data <- read.csv("titanic.csv")

titanic.data

## Classification tree with rpart

titanic_rpart <- rpart(sex ~.,data = titanic.data,method = "class")
nrow(titanic.data)

rpart.plot::rpart.plot(titanic_rpart)

## Assign values to codes in the survived variable

titanic.data[titanic.data$survived ==1,]$survived <- "yes"
titanic.data[titanic.data$survived ==0,]$survived <- "no"

titanic.data[["survived"]]

titanic_class <- rpart(survived ~.,data = titanic.data,method = "class")
rpart.plot::rpart.plot(titanic_class)
titanic_rpart_plot <- fancyRpartPlot(titanic_class,main = "Titanic Data",palettes = "RdPu")

titanic_class

titanic.data

## Car classification
car_test <- car90 %>% na.omit()
car_test

## Load the dataset
car.test.frame

## Clean the dataset, removing missing values

cars_data <- car.test.frame %>%
  select(Reliability,Mileage,Weight,Disp.,HP) %>%
  mutate(Reliability = factor(Reliability,levels = c(1,2,3,4,5),labels = c("Not Reliable","Quite Reliable","Reliable","Very Reliable","Highly Reliable"))) %>%
  na.omit()

cars_data

## Shuffle data

shuffle <- sample(1:nrow(cars_data))

## Apply shuffle on data

cars_data <- cars_data[shuffle,]

cars_data

## Split data

cars_split <- sample.split(cars_data$Reliability, SplitRatio = 0.7)

## Create training and test data

cars_train <- subset(cars_data,cars_split == TRUE)
cars_test <- subset(cars_data,cars_split == FALSE)

nrow(cars_train)
nrow(cars_test)
nrow(cars_data)

## Build classification tree with training data using rpart

set.seed(1234)
cars_tree <- rpart(Reliability ~.,data = cars_train,method = "class")

## Visualise tree

fancyRpartPlot(cars_tree,main = "Cars Data")
visNetwork::visTree(cars_tree)

## Predict

cars_predict <- predict(cars_tree,cars_test,type = "class")

cars_predict

## Confusion matrix

cars_confusion <- confusionMatrix(table(cars_test$Reliability,cars_predict))
cars_confusion

## Classification tree test data

exams <- data.frame(
  results = rep(c(1,0),times = 200),
  study_hours = round(runif(n = 400,min = 1,max = 10),0),
  recreation_hours = round(runif(n = 400,min = 5,max = 20),0)
)

exams
str(exams)

## code data 

exams <- exams %>%
  mutate(results = factor(results,levels = c(1,0),labels = c("Pass","Fail")))

exams

## Build tree model
set.seed(1234)

exams_model <- rpart(results ~ study_hours+recreation_hours,data = exams,method = "class")

## Visualize tree

exams_tree <- fancyRpartPlot(exams_model,main = "Exam Performance Model",sub = " ",type = 2,
                             palettes = "Dark2")

## Predicting with the model

student <- data.frame(
  study_hours = c(10),
  recreation_hours = c(10)
)

exams_predict <- predict(exams_model,student)

print(exams_predict)

rpart.plot::rpart.plot(exams_model)


carData::AMSsurvey
carData::Chile
carData::Cowles
carData::Davis
carData::Depredations
carData::Mroz
carData::Prestige
carData::TitanicSurvival
carData::UN98
carData::UN
carData::Womenlf
carData::WeightLoss

## Regular exercise data

exercise <- Davis
str(exercise)

## Clean data

exercise <- exercise %>%
  select(c(-repwt,-repht)) %>%
  na.omit()

exercise  

## Create training and test data

exercise_split <- sample.split(exercise$sex,SplitRatio = 0.7)

train <- subset(exercise,exercise_split == T)
test <- subset(exercise,exercise_split == F)

nrow(train)
nrow(test)

## Build decision tree model 

set.seed(1234)
exercise_fit <- rpart(sex ~.,data = train,method = "class")

## Visualize model

rpart.plot(exercise_fit,extra = 106)
fancyRpartPlot(exercise_fit,caption = "Source: Davis Exercise Data")

## Test model accuracy

exercise_pred <- predict(exercise_fit,test,type = "class")

exercise_pred

table(test$sex,exercise_pred)

confusionMatrix(table(test$sex,exercise_pred))

## Making a prediction

workout_buff <- data.frame(
  height = c(180),
  weight = c(85)
)

pred <- predict(exercise_fit,workout_buff)
pred
print(pred)

datasets::HairEyeColor
datasets::npk
datasets::precip
datasets::women

ggwomen <- women %>%
  ggplot()+aes(x=height,y=weight,fill = "#5dade2")+geom_point(size = 4,color = "#5dade2",show.legend = FALSE)+
   theme(axis.text = element_text(face = "bold",size = 10),
        axis.title = element_text(face = "bold",size = 13),
        plot.title = element_text(face = "bold",size = 14),
        legend.position = "none")

ggwomen

rayplot <- rayshader::plot_gg(ggwomen,width = 5,height = 5,zoom = 0.85,phi = 35,theta = 30,sunangle = 200,soliddepth = -100,raytrace = TRUE,
                              height_aes = "color",windowsize = c(1200,750))
rayshader::render_snapshot(filename = "American women",title_text = "Average Heights and Weights of American Women Aged 30 - 39",
                           title_position = "northwest")

## Regression tree

npk_split <- sample.split(npk$yield,SplitRatio = 0.7)

## Create training and test data

npk_train <- subset(npk,npk_split == TRUE)
npk_test <- subset(npk,npk_split == FALSE)
nrow(npk_train)
nrow(npk_test)

## Building regression tree

npk_part <- rpart(yield ~.,data = npk_train,method = "anova")

## Visualize tree

npk_tree <- rpart.plot(npk_part)


## K-means clustering

arrests <- USArrests

## Remove any missing values
arrests <- arrests %>% na.omit()

arrests

## Standardize the dataset

arrests <- scale(arrests)

arrests
summary(arrests)

## Compute distance

distance <- get_dist(arrests)

## Visualize matrix

dm <- fviz_dist(distance,gradient = list(low = "#00AFBB",mid = "white",high = "#FC4E07"),show_labels = TRUE)
dm

## Computing the K-means

k_means <- kmeans(arrests,centers = 2,nstart = 25)
str(k_means)

## Print result
print(k_means)

## Visualize cluster

k_means_viz <- fviz_cluster(k_means, data = arrests,geom = "point")+ggtitle("k = 2")

k_means_viz

## Repeat process for 3,4, and 5 clusters

k3 <- kmeans(arrests,centers = 3,nstart = 25)
k4 <- kmeans(arrests,centers = 4,nstart = 25)
k5 <- kmeans(arrests,centers = 5,nstart = 25)

v3 <- fviz_cluster(k3, data = arrests,geom = "point")+ggtitle("K = 3")
v4 <- fviz_cluster(k4, data = arrests,geom = "point")+ggtitle("k = 4")
v5 <- fviz_cluster(k5,data = arrests,geom = "point")+ggtitle("k = 5")

## Visualize cluster grid

grid.arrange(k_means_viz,v3,v4,v5,nrow = 2)

## Determine optimal cluster

set.seed(123)
elbow_method <- fviz_nbclust(arrests,kmeans,method = "wss")
elbow_method

average_sihouette_method <- fviz_nbclust(arrests,kmeans,method = "silhouette")
average_sihouette_method

gap_statistic_method <- clusGap(arrests,FUNcluster = kmeans,nstart = 25,K.max = 10,B = 50)
print(gap_statistic_method)

fviz_gap_stat(gap_statistic_method)

## Final result

final <- kmeans(arrests,4,nstart = 25)
print(final)

## Visualize

fviz_cluster(final,data = arrests)

## Extract and add to original data

clust_data <- USArrests %>%
  mutate(Cluster = final$cluster) %>%
  group_by(Cluster) %>%
  summarise_all("mean")

clust_data

view(clust_data)


## HR dataset

hrdata <- read_excel("HRDataset.xlsx")

## Clean data

hr <- hrdata %>%
  select(Employee_Name,EmpSatisfaction,Absences)
hr  

## Rename columns

names(hr)[c(1:2)] <- c("Employee","JobSatisfaction")

hr


## Remove missing values

hr <- hr %>% na.omit()

hr$Employee <- NULL

## Scale data

hr <- scale(hr)

## Assess data

nrow(hr)

str(hr)

summary(hr)

## Compute and visualize distance matrix

hr_distance <- get_dist(hr)

fviz_dist(hr_distance,gradient = list(low="red",mid="white",high="blue"),show_labels = FALSE)
## Search for optimal cluster

gap_hr <- clusGap(hr,FUNcluster = kmeans,nstart = 25,K.max = 10,B = 50)
fviz_gap_stat(gap_hr)

elbow_hr <- fviz_nbclust(hr,kmeans,method = "wss")
elbow_hr

silhouette_hr <- fviz_nbclust(hr,kmeans,method = "silhouette")
silhouette_hr

## Kmeans
set.seed(123)
hrk <- kmeans(hr,centers = 4,nstart = 25)

hrk

## Visualize

hrv <- fviz_cluster(hrk,data = hr,geom = "point")+ggtitle("HR Data Clusters")

hrv

## Classification tree

bestsellers <- read.csv("bestsellers.csv")

nrow(bestsellers)
ncol(bestsellers)

## Clean dataset

bestsellers <- bestsellers[,-c(1:3,6)]
bestsellers

## Split data into training and test

bestsellers_split <- sample.split(bestsellers$Genre,SplitRatio = 0.7)

bstrain <- subset(bestsellers,bestsellers_split ==TRUE)
bstest <- subset(bestsellers,bestsellers_split ==FALSE)

nrow(bstrain)
nrow(bstest)

## Build classification tree model

best_fit <- rpart(Genre ~ .,data = bstrain,method = "class")

## Visualize tree

best_tree <- rpart.plot(best_fit)
fancyRpartPlot(best_fit)

## Test model prediction

best_pred <- predict(best_fit,bstest,type = "class")

## Build confusion matrix

bestcm <- confusionMatrix(table(best_pred,bstest$Genre))

bestcm

## Example

book <- data.frame(Price = c(37),Reviews = c(17785))

book_pred <- predict(best_fit,book,type = "class")

book_pred

## Automobiles cluster analysis

automobiles <- read_excel("Automobile.xlsx")

## Clean data, removing missing values

automob <- automobiles %>%
  select(Make,`Wheel-base`,Length,Width,Height) %>%
  filter(complete.cases(.))

automob

## Remove label in dataset

automob$Make <- NULL

automob

nrow(automob)

## Standardize the dataset

auto <- scale(automob)
str(auto)
summary(auto)

## Create distance matrix

auto_distance <- get_dist(auto)

## Visualize matrix

fviz_dist(auto_distance,gradient = list(low ="#e74c3c",mid = "white",high = "#3498db"),show_labels = FALSE)

## Find optimal number of clusters

auto_wss <- fviz_nbclust(auto,kmeans,method = "wss")
auto_wss

auto_sil <- fviz_nbclust(auto,kmeans,method = "silhouette")
auto_sil

auto_gap <- clusGap(auto,FUNcluster = kmeans,nstart = 25,K.max = 10,B = 50)
print(auto_gap)

fviz_gap_stat(auto_gap)

## Compute K-means

autoK2 <- kmeans(auto,centers = 2,nstart = 25)
autoK3 <- kmeans(auto,centers = 3,nstart = 25)
autoK4 <- kmeans(auto,centers = 4,nstart = 25)
autoK5 <- kmeans(auto,centers = 5,nstart = 25)


f2 <- fviz_cluster(autoK2,data = auto,geom = "point")+ggtitle("Clusters = 2")
f3 <- fviz_cluster(autoK3,data = auto,geom = "point")+ggtitle("Clusters = 3")
f4 <- fviz_cluster(autoK4,data = auto,geom = "point")+ggtitle("Clusters = 4")
f5 <- fviz_cluster(autoK5,data = auto,geom = "point")+ggtitle("Clusters = 5")

grid.arrange(f2,f3,f4,f5)

autoK3
autoK4
autoK5

## Continent K-Nearest Neighbors 

condata <- head(gapminder,200)

## Clean data

condata <- condata[,-c(1,3)]

condata
summary(condata)

## Normalize data

condata_preprocess <- preProcess(condata[,c(2:4)],method = c("range"))
condata_norm <- predict(condata_preprocess,condata[,c(2:4)])

summary(condata_norm)

## Split data into training and test

condata_ran <- sample(1:nrow(condata),0.7*nrow(condata))

condata_train <- condata_norm[condata_ran,]
condata_test <- condata_norm[-condata_ran,]

nrow(condata_train)
nrow(condata_test)
view(condata_train)

## Create training and test labels

condata_train_labels <- condata[condata_ran,1]
condata_test_labels <- condata[-condata_ran,1]

condata_train_labels
condata_test_labels
nrow(condata_train_labels)
view(cbind.data.frame(condata_train_labels,condata_train))

## Build KNN

condata_knn <- knn(condata_train,condata_test,cl = condata_train_labels$continent,k = 15)

condata_knn

## Build confusion matrix

condata_matrix <- confusionMatrix(table(condata_knn,condata_test_labels$continent))

condata_matrix

## Predicting

con <- data.frame(lifeExp = c(40),
                  pop = c(8588721),
                  gdpPercap = c(350))

con_knn <- knn(condata_train,con,condata_train_labels$continent,k = 17,prob = TRUE)

con_knn

table(con_knn,con)

## Naive Bayes 

data <- iris
data
nrow(data)

## Split data into train and test data set

split <- sample.split(data,SplitRatio = 0.7)

train <- subset(data,split == TRUE)
test <- subset(data,split == FALSE)

## Feature scaling

train_scale <- scale(train[,1:4])
test_scale <- scale(test[,1:4])

## Fit Naive Bayes model

set.seed(123)

naive_model <- naiveBayes(Species ~.,data = train)
naive_model

## Predicting

naive_pred <- predict(naive_model,test)

## Build confusion matrix

naive_con <- confusionMatrix(table(naive_pred,test$Species))
naive_con

## Predicting with new data

new <- data.frame(
  Petal.Length = c(3.3),
  Petal.Width = c(8.8),
  Sepal.Length = c(4.1),
  Sepal.Width = c(5.9)
)


model_predict <- predict(naive_model,new)
model_predict
glimpse(model_predict)
print(model_predict)

adult <- read.table("adult.data",sep = ",")
adult
view(adult)

car <- read.table("car.data",sep = ",")
car

haberman <- read.table("haberman.data",sep = ",")
haberman

sponge <- read.table("sponge.data",sep = ",")
sponge

wine <- read.table("wine.data",sep = ",")
wine

## View wine data

str(wine)
dim(wine)
summary(wine)

## Rename columns

names(wine)[c(1:13)] <- c("Alcohol","Malic_acid","Ash","Alcalinity","Magnesium",
                          "Total_phenols","Flavanoids","Nonflavanoids","Proanthocynins",
                          "Color_intensity","Hue","Diluted_wines","Prolines")

wine

## Convert Alcohol column to factor variable
wine$Alcohol <- factor(wine$Alcohol,levels = c(1,2),labels = c("High","Low"))

## Eliminate the last column, which is not relevant
wine <- wine[,-14]

## Remove missing values

wine <- wine %>% na.omit()
wine


## Check structure again

str(wine)

## Plot correlation graph

ggwine <- wine %>%
  ggplot()+aes(x=V2,y=V3,color = V1)+geom_point(size = 4)+
  geom_shadowpoint()+theme(panel.background = element_rect(fill = "black"))
ggwine

## Calculate the distance of the continuous variables

wine_dist <- dist(wine[,-1],method = "euclidean")

print(wine_dist)

## Visualize matrix

wine_mat <- as.matrix(wine_dist)
wine_mat

fviz_dist(wine_dist)

## Convert to dendrogram

wine_hc <- hclust(wine_dist,method = "ward.D")

## Visualize dendrogram

plot(wine_hc,hang = -1)

## Add rectangles for distinction

wine_rec <- rect.hclust(wine_hc,k=5,border = "blue")

wine_den <- as.dendrogram(wine_hc)
plot_dendro(wine_den,width = 500)

## Shuffle the wine data for Naive bayes model

wine_shuffle <- sample(1:nrow(wine))
wine_shuffle

## Split data into Training and Testing data

wine_data <- wine[wine_shuffle,]
wine_data

wine_split <- sample.split(wine_data$Alcohol,SplitRatio = 0.7)

wine_train <- subset(wine_data,wine_split == TRUE)
wine_test <- subset(wine_data,wine_split == FALSE)

## Build Naive bayes model

set.seed(123)
wine_fit <- naiveBayes(Alcohol ~., data = wine_train,laplace = 2)

## Run prediction

wine_pred <- predict(wine_fit,wine_test)

## Build confusion matrix

confusionMatrix(table(wine_pred,wine_test$Alcohol))

## Test new data

wine_new <- data.frame(
  Malic_acid = c(17.5),
  Ash = c(2.71),
  Alcalinity = c(3.58),
  Magnesium = c(24.2),
  Total_phenols = c(137),
  Flavanoids = c(1.40),
  Nonflavanoids = c(2.49),
  Proanthocynins = c(0.32),
  Color_intensity = c(1.51),
  Hue = c(7.5),
  Diluted_wines = c(0.965),
  Prolines = c(2.22)
)

wine_pred2 <- predict(wine_fit,wine_new)

wine_pred2

datasets::PlantGrowth


play <- round(runif(n=50,min = 10,max = 80),1)
dance <- round(runif(n=50,min = 5,max = 40),1)

data_play <- data.frame(play,dance)

data_play

ggplay <- data_play %>%
  ggplot()+aes(x=play,y=dance)+geom_line(size=3,color="#5dade2")+geom_glowline()+
  theme_light()
ggplay

## Support vector machine

data_iris <- iris
data_iris

## Split the data into training and testing sets

iris_shuffle <- sample(1:nrow(data_iris),0.7*nrow(data_iris))

train_iris <- data_iris[iris_shuffle,]
test_iris <- data_iris[-iris_shuffle,]

dim(train_iris)
dim(test_iris)

## Build SVM model

iris_fit <- svm(Species ~.,data = train_iris)

## Visualize model fit

plot(iris_fit,train_iris)

## Predict with the model

iris_pred <- predict(iris_fit,test_iris)

iris_pred

## Build confusion matrix

confusionMatrix(table(iris_pred,test_iris$Species))

## Iris data set 2

plot(iris)
str(iris)

## Visualize data points

iris %>%
  ggplot()+aes(x=Petal.Length,y=Petal.Width,color=Species)+
  geom_point(size = 4)

## Split data into train and test

split_iris <- sample(1:nrow(iris),0.7*nrow(iris))

data_train <- iris[split_iris,]
data_test <- iris[-split_iris,]

str(data_train)

data_train

## Fit model
set.seed(123)
svmfit <- svm(Species ~.,data = data_train,kernel = "linear",cost = 10,scale = FALSE)

summary(svmfit)

svmfit$index

## Visualize model

plot(svmfit,data_train,
     Petal.Width ~ Petal.Length,
     slice = list(Sepal.Width = 3,Sepal.Length = 4))

## Predicting with the model

svmpred <- predict(svmfit,data_test,type = "class")

print(svmpred)


## Build confusion matrix

confusionMatrix(table(svmpred,data_test[["Species"]]))


## Predicting with new data

newdata <- data.frame(
  Petal.Length = c(1.9),
  Petal.Width = c(0.5),
  Sepal.Length = c(7.7),
  Sepal.Width = c(3.5)
)


newpred <- predict(svmfit,newdata)

newpred



iris

## Tune the model

tunedmodel <- tune(svm,Species ~.,data = data_train,kernel = "linear",
                   ranges = list(cost = c(0.001,0.01,0.1,1,5,10,100)))

summary(tunedmodel)

bestmodel <- tunedmodel$best.model

summary(bestmodel)

## Predicting with tuned model

tune_pred <- predict(bestmodel,data_test)

confusionMatrix(table(tune_pred,data_test$Species))

## Stock prediction SVM

stock <- Weekly
stock

## Clean data, remove columns that are not needed

stockclean <- stock %>%
  select(-c(Year,Volume,Today))

stockclean

## Visualize data

stockclean %>%
  ggplot()+aes(x=Lag1,y=Lag2,color=Direction)+geom_point(size = 3)

stockclean %>%
  ggplot()+aes(x=Lag3,y=Lag4,color=Direction)+geom_point(size = 3)

## Split data into training and testing set

weeksplit <- sample.split(stock$Direction,SplitRatio = 0.7)

train_stock <- subset(stockclean,weeksplit == TRUE)
test_stock <- subset(stockclean,weeksplit == FALSE)

dim(train_stock)
dim(test_stock)

## Build the model

set.seed(123)
stockfit <- svm(Direction ~.,data = train_stock,kernel = "linear",cost = 10,scale = FALSE)

summary(stockfit)

## Predicting with the model

stockpred <- predict(stockfit,test_stock,type = "class")

## Build confusion matrix

confusionMatrix(stockpred,test_stock$Direction)

##########################################################

ISLR::Default

adult <- read.table("adult.data",sep = ",")

adult

soybean <- read.table("soybean-small.data",sep = ",")

soybean

wine <- read.table("wine.data",sep = ",")

wine

tae <- read.table("tae.data",sep = ",")

tae

haberman <- read.table("haberman.data",sep = ",")

haberman

habermanclean <- haberman[,-3]

habermanclean

names(habermanclean)[c(1,2,3)] <- c("CATest","ExamScore","Result")

habermanclean

## Haberman SVM

str(habermanclean)

## Convert "Results" into factor variable

habermanclean[["Result"]] <- factor(habermanclean[["Result"]],levels = c(1,2),labels = c("Pass","Fail"))

str(habermanclean)

## Visualize data

habermanclean %>%
  ggvis(~CATest,~ExamScore,fill=~Result) %>% layer_points()


## Shuffle and split data

haberman_shuffle <- sample(1:nrow(habermanclean),0.7*nrow(habermanclean))

haberman_shuffle

haberman_train <- habermanclean[haberman_shuffle,]
haberman_test <- habermanclean[-haberman_shuffle,]

dim(haberman_train)
dim(haberman_test)

## Build SVM model

habermanfit <- svm(Result ~.,data = haberman_train,kernel = "linear",cost = 10,scale = TRUE)

summary(habermanfit)

## Fitting the model

habermanpred <- predict(habermanfit,haberman_test,type = "class")

## Build confusion matrix

confusionMatrix(table(habermanpred,haberman_test$Result))

habermanclean

## Test with Naive Bayes

haberfit <- naiveBayes(Result ~.,data = haberman_train)

## Fit the model

haberpred <- predict(haberfit,haberman_test)

## Build confusion matrix

confusionMatrix(table(haberpred,haberman_test$Result))

## Classification tree model

default <- ISLR::Default

default

## Shuffle and split data into training and testing set

defaultRan <- sample(1:nrow(default),0.7*nrow(default))

default_train <- default[defaultRan,]
default_test <- default[-defaultRan,]

## Build tree model

set.seed(1234)
defaultfit <- rpart(default ~.,data = default_train,method = "class")

summary(defaultfit)


## Visualize tree model

rpart.plot(defaultfit)
fancyRpartPlot(defaultfit,caption = "Credit Default Model")

## Predicting with the model

defaultpred <- predict(defaultfit,default_test,type = "class")

## Build confusion matrix

confusionMatrix(table(defaultpred,default_test$default))

## Predicting new data with the model

newdef <- data.frame(
  student = c("Yes"),
  balance = c(7880.1242),
  income = c(9340.375)
)

newpred <- predict(defaultfit,newdef)

print(newpred)

newpred

## Using Naive Bayes algorithm on the default data set

set.seed(1234)
defaultNaive_fit <- naiveBayes(default ~.,data = default_train)

## Predicting with the model

defaultNaive_pred <- predict(defaultNaive_fit,default_test)

## Build confusion matrix

confusionMatrix(table(defaultNaive_pred,default_test$default))

## Testing the model with new data

newdef <- data.frame(
  student = c("Yes"),
  balance = c(17050.1242),
  income = c(200.375)
)

prednew <- predict(defaultNaive_fit,newdef)

prednew

print(prednew)

## Applying the SVM algorithm to the default data set

set.seed(1234)
defaultsvm_fit <- svm(default ~.,data = default_train,kernel = "linear",cost = 300,scale = FALSE)

summary(defaultsvm_fit)

## Predicting with the model

defaultsvm_pred <- predict(defaultsvm_fit,default_test)

## Build confusion matrix

confusionMatrix(table(defaultsvm_pred,default_test$default))


## XGBOOST algorithm

xgiris <- iris

## Convert the classifier factor into an integer class

species <- xgiris$Species
label <- as.integer(xgiris$Species)-1
xgiris$Species <- NULL

species
label

## Split the entire data into training and testing data set

n <- nrow(xgiris)
train.index <- sample(n,floor(0.75*n))
training_data <- as.matrix(xgiris[train.index,])
training_label <- label[train.index]
testing_data <- as.matrix(xgiris[-train.index,])
testing_label <- label[-train.index]

## Transform training and testing data set into Xgb.Dmatrix objects

xgb.train <- xgb.DMatrix(data = training_data,label = training_label)
xgb.test <- xgb.DMatrix(data = testing_data,label = testing_label)

## Define main parameters, like booster, objective, and learning rate

num_class <- length(levels(species))
params <- list(
  booster = "gbtree",
  eta = 0.001,
  max_depth = 5,
  gamma = 3,
  subsample = 0.75,
  colsample_bytree = 1,
  objective = "multi:softprob",
  eval_metric = "mlogloss",
  num_class = num_class
)

## Train the XGBOOST classifier
set.seed(1234)
xgb.fit <- xgb.train(
  params = params,
  data = xgb.train,
  nrounds = 100,
  early_stopping_rounds = 10,
  watchlist = list(val1 = xgb.train,val2 = xgb.test),
  verbose = 0
)

xgb.fit

## Visualize training and testing error

e <- data.frame(xgb.fit$evaluation_log)

plot(e$iter,e$val1_mlogloss,col = "blue")
lines(e$iter,e$val2_mlogloss,col = "red")

## To know which iteration gave the minimum value of the test error
min(e$val2_mlogloss)

e[e$val2_mlogloss == 0.981434,]

## Visualize feature importance

xgb.imp <- xgb.importance(colnames(xgb.train),model = xgb.fit)

print(xgb.imp)

xgb.ggplot.importance(xgb.imp)

## Predict new outcomes

xgb.pred <- predict(xgb.fit,testing_data,reshape = T)
xgb.pred <- as.data.frame(xgb.pred)
colnames(xgb.pred) <- levels(species)

xgb.pred

## Build confusion matrix

confusionMatrix(table(Predicton = xgb.pred$prediction,Actual = xgb.pred$label))

## Identify the class with the highest probability

xgb.pred$prediction <- apply(xgb.pred,1,function(x) colnames(xgb.pred)[which.max(x)])
xgb.pred$label <- levels(species)[testing_label+1]

## Check prediction accuracy

result <- sum(xgb.pred$prediction == xgb.pred$label)/nrow(xgb.pred)
print(paste("Final Accuracy:",sprintf("%1.2f%%",100*result)))


newdata <- data.frame(
  Petal.Length = c(1.8),
  Petal.Width = c(7.5),
  Sepal.Length = c(3.4),
  Sepal.Width = c(6.9)
)



xgb.newdata <- xgb.DMatrix(as.matrix(newdata))
newpred <- predict(xgb.fit,xgb.newdata,family = "gaussian")

newpred


## SVM Bestsellers

best_sellers <- read.csv("bestsellers.csv")

## Clean the dataset

bstClean <- best_sellers[,-c(1:3)]

bstClean

dim(bstClean)
glimpse(bstClean)

## Visualize data in a scatter plot

ggsell <- bstClean %>% 
  ggplot()+aes(x = Price,y = User.Rating,color = Genre)+geom_point(size = 2)

ggsell

## Split the data into training and testing data

bstSplit <- sample.split(bstClean$Genre,SplitRatio = 0.8)

bst_train <- subset(bstClean,bstSplit == TRUE)
bst_test <- subset(bstClean,bstSplit == FALSE)

dim(bst_train)
dim(bst_test)

## Build SVM algorithm

bstM <- svm(Genre ~., data = bst_train,kernel = "linear",cost = 100,scale = TRUE)

## Visualize model

plot(bstM,bst_train,
     User.Rating ~ Price,
     slice = list(Reviews = 3,
                  Year = 4))

summary(bstM)

## Fit model

bstPred <- predict(bstM,bst_test,type = "class")

## Build confusion matrix

confusionMatrix(table(bstPred,bst_test$Genre))

## Using Naive bayes

bstNB <- naiveBayes(Genre ~.,data = bst_train,laplace = 2)

summary(bstNB)

## Predicting with the model

bstNBPred <- predict(bstNB,bst_test,method = "class")

## Build confusion matrix

confusionMatrix(table(bstNBPred,bst_test$Genre))


########### Boruta Feature Selection ############

wine <- read.table("wine.data",sep = ",")
View(wine)

## Rename columns

names(wine)[c(1:13)] <- c("Alcohol","Malic_acid","Ash","Alcalinity","Magnesium",
                          "Total_phenols","Flavanoids","Nonflavanoids","Proanthocynins",
                          "Color_intensity","Hue","Diluted_wines","Prolines")


## Convert Alcohol column to factor variable
wine$Alcohol <- factor(wine$Alcohol,levels = c(1,2),labels = c("High","Low"))

## Remove the last column

wine[["V14"]] <- NULL
view(wine)

## Remove any missing values

wine <- wine %>% 
  filter(complete.cases(.))

## Select the most important features

set.seed(123)

select_features <- Boruta(Alcohol ~.,data = wine,doTrace = 2,maxRuns = 200)  

print(select_features)

## Plot result

plot(select_features,las = 2,cex.axis = 0.7)

plotImpHistory(select_features)

## Take final decision on tentative features with tentative rough fix

features <- TentativeRoughFix(select_features)

## Check attributes stats

features.df <- attStats(select_features)

print(features.df)

## Get the list of confirmed attributes

getSelectedAttributes(select_features,withTentative = FALSE)


################ Random Forest ##################

bat <- data.frame(
  top = c(1,2,3,4,5,6),
  cap = c("Hom","Com","Lom","Hom","Hom","Com")
)

## Assigning numeric values to string in a data frame
bat$cap <- bat$cap %>% 
  recode("Hom" = 4,
         "Com" = 5,
         "Lom" = 6)
glimpse(bat)
str(bat)

## Load data set and remove missing values 

titanicS <- TitanicSurvival %>% na.omit() %>% select(survived,sex,age)
row.names(titanicS) <- NULL

## Convert the sex class into integers

titanicS$sex <- titanicS$sex %>% 
  recode("male" = 1,
         "female" = 0)

## Split data set into training and testing

titanic <- sample(1:nrow(titanicS),0.8*nrow(titanicS))

titanic_train <- titanicS[titanic,] 
titanic_test <- titanicS[-titanic,]

dim(titanic_train)
dim(titanic_test)

## Building the random forest model

RFmodel <- randomForest(survived ~.,data = titanic_train,importance = TRUE,ntree = 500)

RFmodel

## Predicting with the model

RFpred <- predict(RFmodel,titanic_test,type = "class")

## Building a confusion matrix

confusionMatrix(table(RFpred,titanic_test$survived))

## Random forest on wine data set

## Split data into train and test

set.seed(123)

wine_samp <- sample(1:nrow(wine)*0.8,nrow(wine))
View(wine)

train <- wine[wine_samp,]
test <- wine[-wine_samp,]

dim(train)
dim(test)

## Build the model

wineRF <- randomForest(Alcohol~., data = train)

wineRF

## View the attributes of the model

attributes(wineRF)

## Visualize the error rate of model

plot(wineRF)

## Visualize the number of nodes for the trees

hist(treesize(wineRF),
     col = "lightblue",
     main = "Number of Nodes for the Trees")

## Visualize variable importance

wineImp <- varImpPlot(wineRF,
                      sort = TRUE,
                      n.var = 10,
                      main = "Top 10 Most Important Variables in Wine Data Set")

## Quantitative measures of importance

importance(wineRF)

## Find out predictors values actively used in the model

varUsed(wineRF)

## Extract information about a single tree in the forest

getTree(wineRF,10,labelVar = TRUE)

## Predicting with the model

winePred <- predict(wineRF,test)

## Build a confusion matrix

confusionMatrix(table(winePred,test$Alcohol))

view(train)

## Predicting with new values

new_wine <- data.frame(
  Malic_acid = 13.56,
  Ash = 1.24,
  Alcalinity = 1.78,
  Magnesium = 11.5,
  Total_phenol = 100,
  
)

penguins
View(penguins)

## Create fake data with charlatan
fake_data <- data.frame(
  Names = charlatan::ch_name(n = 20,locale = "en_US"),
  Job = charlatan::ch_job(n = 20,locale = "en_US")
)

fake_data

## Naive baiyes and decision tree on palmer penguins
attach(penguins)
penguins

glimpse(penguins)

## Remove missing values
penguins <- penguins %>% 
  filter(complete.cases(.)) 
  
## Visualize distribution of features
penguins %>% 
  hchart(type = "point",hcaes(x = bill_length_mm,y = bill_depth_mm,color = species))

## Split data into training and testing data 
penData <- sample(1:nrow(penguins),0.75*nrow(penguins))
penData
penTrain <- penguins[penData,]
penTest <- penguins[-penData,]

view(penTrain)

## Visualize distribution of training data
penTrain %>% 
  hchart(type = "point",hcaes(x = bill_length_mm,y = bill_depth_mm,color = species))

## Build naive baiyes model
penModel <- naiveBayes(species ~.,data = penTrain,laplace = 2)

## Predict with model
penPred <- predict(penModel,penTest,method = "class")

## Build confusion matrix
penCon <- confusionMatrix(table(penPred,penTest$species))
penCon

## Decision tree
penTree <- rpart(species ~.,data = penTrain) 

## Visualize tree
rpart.plot(penTree)
fancyRpartPlot(penTree)

## Predict with model
penTree_pred <- predict(penTree,penTest)

## Confusion matrix
confusionMatrix(table(penTree_pred,penTest$species))

## New data
birds <- data.frame(
  bill_length_mm = 34.2,
  bill_depth_mm = 15.7,
  flipper_length_mm = 167
)

pred <- predict(penTree,birds)

## Model studio

attach(TitanicSurvival)
TitanicSurvival <- TitanicSurvival %>% filter(!is.na(age))
glimpse(TitanicSurvival)

TitanicSurvival$sex <- TitanicSurvival$sex %>% 
  recode("male" = 1,
         "female" = 2)
TitanicSurvival <- TitanicSurvival %>% 
  select(-passengerClass) %>% 
  na.omit()

model <- ranger::ranger(survived ~.,data = titanic_imputed,probability = TRUE)

## Create explainer
explainer <- explain(model,
                     data = titanic_imputed,
                     y = titanic_imputed$survived,
                     label = "Random Forest")
modelStudio(explainer = explainer)

## Hierarchical cluster 
data <- head(apartments,100)

## Remove the labeled column
datadf <- data[,-6]

dim(datadf)

## Scale data
datadf_scaled <- scale(datadf)

## Compute distance
datadf_dist <- dist(datadf_scaled,method = "euclidean")

## Make into hierarchical cluster object
datadf_hc <- hclust(datadf_dist,method = "ward.D")

## Visualize cluster dendrogram
plot(datadf_hc,hang = -1,label = data$district)

## Naive bayes on HR dataset
hrd <- HR
glimpse(hrd)

## Encode gender column
hrd[["gender"]] <- hrd[["gender"]] %>% 
  recode("male" = 1,
         "female" = 2)
glimpse(hrd)

## Split data set into train and test
hrdsamp <- sample(1:nrow(hrd),0.8*nrow(hrd))

## Visualize distribution of data points
hrTrain <- hrd[hrdsamp,]
hrTest <- hrd[-hrdsamp,]

dim(hrTrain)
dim(hrTest)

## Fit model
modelNB <- naiveBayes(status ~.,data = hrTrain,laplace = 2)

## Save model
saveRDS(modelNB,file = "HRModel.RDS")

## Predict with model
hrPred <- predict(modelNB,hrTest)

## Build confusion matrix
confusionMatrix(table(hrPred,hrTest$status))

## Using Random forest
set.seed(1234)
modelRF <- randomForest(status ~.,data = hrTrain,ntree = 500)

## Visualize variable importance
varImpPlot(modelRF,
           sort = TRUE,
           main = "Variables Ranked by Importance")

## Predict with model
hrRf_pred <- predict(modelRF,hrTest)

## Build confusion matrix
confusionMatrix(table(hrRf_pred,hrTest$status))

## Save model
saveRDS(modelRF,file = "HRForest.RDS")

## Predict new data
newData <- data.frame(
  hours = 68.9,
  age = 30,
  evaluation = 6,
  salary = 3,
  gender = 1
)

predict(modelRF,newData)

## Using XGBoost
status = hrd$status
label = as.integer(hrd$status)-1
hrd$status = NULL

## Split data into train and test
n = nrow(hrd)
index = sample(n,floor(0.75*n))
trainingHR <- as.matrix(hrd[index,])
trainingLabel <- label[index]
testingHR <- as.matrix(hrd[-index,])
testingLabel <- label[-index]

## Transform the training and testing data into XGBoost matrix objects
xgbHR_train <- xgb.DMatrix(data = trainingHR,label = trainingLabel)
xgbHR_test <- xgb.DMatrix(data = testingHR,label = testingLabel)

## Define model parameters
numClass <- length(levels(status))
params <- list(
  booster = "gbtree",
  eta = 0.001,
  max_depth = 5,
  gamma = 3,
  subsample = 0.75,
  colsample_bytree = 1,
  objective = "multi:softprob",
  eval_metric = "mlogloss",
  num_class = numClass
)

## Train the algorithm
xgb.fit <- xgb.train(
  params = params,
  data = xgbHR_train,
  nrounds = 500,
  nthreads = 1,
  watchlist = list(
    val1 = xgbHR_train,val2 = xgbHR_test),
  verbose = 1
)

## Plot train and test error
errorPlot <- data.frame(xgb.fit$evaluation_log)
plot(errorPlot$iter,errorPlot$val1_mlogloss,col = "blue")
lines(errorPlot$iter,errorPlot$val2_mlogloss,col = "red")

## Predict with model
xgb.predict <- predict(xgb.fit,xgbHR_test,reshape = TRUE)
xgb.predict <- as.data.frame(xgb.predict)
colnames(xgb.predict) <- levels(status)

## Identify class with highest probability
xgb.predict$prediction <- apply(xgb.predict,1,function(x)colnames(xgb.predict)[which.max(x)])
xgb.predict$label <- levels(status)[testingLabel+1]

## Check prediction accuracy
result <- sum(xgb.predict$prediction == xgb.predict$label)/nrow(xgb.predict)
print(paste("Final Accuracy:",sprintf("%1.2f%%",100*result)))

## Working with H2O machine learning platform
mydata <- iris

## Start up h2o instance
h2o.init()

## Load data set into h2o instance
iris.hex <- as.h2o(mydata)

## Look at data frame loaded into h2o
h2o.ls()

## Get summary statistics of data set
h2o.describe(iris.hex)

## Plot histogram
h2o.hist(iris.hex$Sepal.Length)

## Splitting data into training and testing
splits <- h2o.splitFrame(data = iris.hex,
                         ratios = c(0.8),
                         seed = 1234)
trainI <- splits[[1]]
testI <- splits[[2]]

## Building a random forest model
rforest <- h2o.randomForest(x = c("Sepal.Length","Sepal.Width","Petal.Length","Petal.Width"),
                            y = c("Species"),
                            training_frame = trainI,
                            model_id = "h2.rf",
                            seed = 1234)
rforest

## Testing model performance
rperf1 <- h2o.performance(model = rforest,
                          newdata = testI)

rperf1

## Predicting with the model
irispred <- h2o.predict(rforest,testI)

irispred

## Build confusion matrix
h2o.confusionMatrix(rperf1)

## Principal component analysis
demo_data <- read.csv(file.choose())
view(demo_data)
## Select key variables
demo_data <- demo_data %>% 
  select(AGE,ADDRESS,INCOME,CAR,EMPLOY)
demo_data

## Check the level of correlation among variables
pairs.panels(demo_data,
             gap = 0,
             bg = c("red","blue","green"),
             pch = 21)

## Performing the PCA
pca <- prcomp(demo_data,center = TRUE,scale. = TRUE)
print(pca)
## Check attributes
attributes(pca)

## Check orthogonality
pairs.panels(pca$x,
             gap = 0,
             bg = c("red","blue","green"),
             pch = 21)

## Visualize PCA clusters
pca_plot <- autoplot(pca)
ggplotly(pca_plot)
fviz_eig(pca)

## PCA of decathlon
decathlon <- decathlon2
view(decathlon)

## Extract only active individuals and variables
decathlon.active <- decathlon[1:23,1:10]
head(decathlon.active[,1:6])

## Visually check correlation among variables
pairs.panels(decathlon.active)

## With prcomp()
set.seed(1234)
res.pca <- prcomp(decathlon.active,scale. = TRUE,center = TRUE)

## View summary data of PCA for most variance
summary(res.pca)

## Visualize percentage of variance
fviz_eig(res.pca)

## Print PCA
print(res.pca)

## Visualize PCA correlation, which will all be 0
pairs.panels(res.pca$x,pch = 21)


## Visualize the group of individuals with a similar profile
fviz_pca_ind(res.pca,
             col.ind = "cos2", #Color by the quality of representation
             gradient.cols = c("#00AFBB","#E7B800","#FC4E07"),
             repel = TRUE, #Avoids text overlapping
             )

## Visualize correlations among variables
fviz_pca_var(res.pca,
             col.var = "contrib", #Color by contribution to pc
             gradient.cols = c("#00AFBB","#E7B800","#FC4E07"),
             repel = TRUE)

## Biplot of individuals and variables
fviz_pca_biplot(res.pca,
                repel = TRUE,
                col.var = "#2E9FDF",
                col.ind = "#696969")


## Accessing the PCA results
eig.val <- get_eigenvalue(res.pca)
eig.val

## Results for variables
res.var <- get_pca_var(res.pca)
print(res.var)

res.var$cor
res.var$coord
res.var$contrib

## PCA on USA Arrests data
USArrests

## Check level of variance
apply(USArrests,2,var)

## Scale data
USArrests_scaled <- scale(USArrests)
print(head(USArrests_scaled))

DALEX::apartments
DALEX::HR

## Naive bayes on HR data
hrm <- DALEX::HR
hrm <- hrm %>% na.omit()

hrm <- hrm[,-c(1,5)]

hrm

## Visualize the data
hd <- head(hrm,100)

hd %>% 
  ggplot()+aes(x = age, y = hours, color = status)+geom_point(size = 3)

hd %>% 
  ggplot()+aes(x = hours)+geom_density(fill = "green")+labs(title = paste("Skewness:",skewness(hd$hours)))

cor(hd$age,hd$hours)

## Split data into train and test
indx <- sample.split(hrm$status,SplitRatio = 0.8)

hrmTr <- subset(hrm,indx == TRUE)
hrmTs <- subset(hrm,indx == FALSE)

dim(hrmTr)
dim(hrmTs)

## Fit model
set.seed(123)
hrmodel <- naiveBayes(status ~.,data = hrmTr,laplace = 2)
summary(hrmodel)

## Predict with model
hrmpred <- predict(hrmodel,hrmTs)

print(hrmpred)

## Build confusion matrix
confusionMatrix(table(hrmpred,hrmTs$status))

## Append prediction to test data
hrmTs$pred <- hrmpred

hrmTs

## Feature termination
hrf <- rfeTerminator(hrm,x_cols = 1:3,y_cols = 4,alter_df = TRUE,eval_funcs = rfFuncs)

## Explore model
print(hrf$rfe_model_fit_results)

print(hrf$rfe_model_fit_results$optVariables)

## Obtaining the data after rfe termination
hrm2 <- hrf$rfe_reduced_data

hrm2

## Split the data to train and test
indx_hrm2 <- sample.split(hrm2$status,SplitRatio = 0.8)

hrmTr2 <- subset(hrm2,indx_hrm2 == TRUE)
hrmTs2 <- subset(hrm2,indx_hrm2 == FALSE)

dim(hrmTr2)
dim(hrmTs2)

## Fit the model
set.seed(123)
hrmodel2 <- naiveBayes(status ~ hours + evaluation, data = hrmTr2)

## Predict with the model
hrmpred2 <- predict(hrmodel2,hrmTs2)

## Build confusion matrix
confusionMatrix(table(hrmpred2,hrmTs2$status))

## SMS classification
sms <- read.csv("spam.csv")
view(sms)
str(sms)

## Rename columns
colnames(sms)[c(1:2)] <- c("Type","Text")

## Delete unwanted columns
sms <- sms[,-c(3:5)]

## Convert type to a factor variable
sms$Type <- as.factor(sms$Type)

str(sms)
table(sms$Type)

## Transform text data
sms_corpus <- Corpus(VectorSource(sms$Text))

## Inspect corpus
inspect(sms_corpus[1:3])
as.character(sms_corpus[[3]])

## Clean corpus
mystopwords <- readLines("stopwords.txt")
mystopwords

sms_corpus <- sms_corpus %>% 
  tm_map(removeNumbers) %>% 
  tm_map(removePunctuation) %>% 
  tm_map(tolower) %>% 
  tm_map(stripWhitespace) %>% 
  tm_map(removeWords,stopwords("english")) %>% 
  tm_map(removeWords,mystopwords) %>% 
  tm_map(stemDocument) # Stem document

sms_dtm <- DocumentTermMatrix(sms_corpus) #Not Term Document Matrix

## Split data into train and test set
smsTrain <- sms_dtm[1:4180,]
smsTest <- sms_dtm[4181:5559,]

## Save vector labeling rows
smsTrain_labels <- sms[1:4180,]$Type
smsTest_labels <- sms[4181:5559,]$Type

prop.table(table(smsTrain_labels))
prop.table(table(smsTest_labels))

## Remove words from the matrix that appear less than 5 times
sms_freq_words <- findFreqTerms(smsTrain,5)
str(sms_freq_words)
print(sms_freq_words)

## Limit matrix to only include words in the frequency vector
smsTrain_freq <- smsTrain[,sms_freq_words]
smsTest_freq <- smsTest[,sms_freq_words]
smsTrain_freq$ncol

## Convert matrix to "yes" and "no" categorical variable
convert <- function(x){
  result <- ifelse(x > 0,"Yes","No")
  return(result)
}

## Apply to data
sms_train <- apply(smsTrain_freq,2,convert)
sms_test <- apply(smsTest_freq,2,convert)

view(sms_train)
view(sms_test)

## Build model
set.seed(1234)
sms_classifier <- naiveBayes(sms_train,smsTrain_labels)

## Predict with model
sms_pred <- predict(sms_classifier,sms_test)

## Evaluate prediction
CrossTable(sms_pred,smsTest_labels,prop.chisq = FALSE,
           prop.t = FALSE,dnn = c("Predicted","Actual"))

confusionMatrix(table(sms_pred,smsTest_labels))

newdf <- data.frame(
  Text = c("WINNER!! As a valued network customer you have been selected to receivea å£900 prize reward! To claim call 09061701461. Claim code KL341. Valid 12 hours only.")
)

newpred <- function(text){
  
  
  mystopwords <- readLines("stopwords.txt")
  
  text_corp <- Corpus(VectorSource(text))
  text_corp <- text_corp %>% 
    tm_map(removeNumbers) %>% 
    tm_map(removePunctuation) %>% 
    tm_map(tolower) %>% 
    tm_map(stripWhitespace) %>% 
    tm_map(removeWords,stopwords("english")) %>% 
    tm_map(removeWords,mystopwords) %>% 
    tm_map(stemDocument)
  
  text_dtm <- DocumentTermMatrix(text_corp)
  freq <- findFreqTerms(text_dtm,0.5)
  text_freq <- text_dtm[,freq]
  texter <- apply(text_freq,2,convert)
  
  return(texter)
}

newText <- data.frame(
  Text = c("WINNER!! As a valued network customer you have been selected to receivea å£900 prize reward! To claim call 09061701461. Claim code KL341. Valid 12 hours only.")
)  

newText <- newpred(text = newText$Text)  

newText 

pred <- predict(sms_classifier,newText)  
pred  

############### MLFLOW #################
library(processx)
library(mlflow)
library(reticulate)

install_mlflow()

Sys.setenv(MLFLOW_BIN = system("which mlflow"))
Sys.setenv(MLFLOW_PYTHON_BIN = system("which python"))


conda_install("r-mlflow", "<local github repo>", pip = TRUE)
install_mlflow(python_version = "3.8")
mlflow_log_param("foo",42)
mlflow_ui()

processmonitR::activity_dashboard()
processmonitR::performance_dashboard()

## Decision tree on student data
students <- read.csv("SP.csv")

## Wrangle data
students_df <- students %>% 
  select(gender,math.score,reading.score,writing.score)

## Visualize data
students_df %>% 
  ggplot()+aes(x = math.score,y = reading.score,col = gender)+
  geom_point(size = 2)+geom_smooth()

students_df %>% 
  ggplot()+aes(x = math.score,y = writing.score,col = gender)+
  geom_point(size = 2)+geom_smooth()

students_df %>% 
  ggplot()+aes(x = writing.score,y = reading.score,col = gender)+
  geom_point(size = 2)+geom_smooth()

student_cor <- cor(students_df[,-1])

ggplotly(ggcorrplot::ggcorrplot(student_cor))

## Split data into train and test
set.seed(1234)
indx <- sample(2,nrow(students_df),replace = TRUE,prob = c(0.8,0.2))
student_train <- students_df[indx == 1,]
student_test <- students_df[indx == 2,]

dim(student_train)
dim(student_test)

## Fit model
set.seed(2222)
Smodel <- rpart(gender ~.,data = student_train,na.action = na.rpart)
print(Smodel)
summary(Smodel)

## Visualize model
rpart.plot(Smodel)
fancyRpartPlot(Smodel)

## Predict with model
Spred <- predict(Smodel,student_test,type = "class")

## Build confusion matrix
confusionMatrix(table(Spred,student_test$gender))

## Classify new data
newd <- data.frame(
  math.score = c(70),
  reading.score = c(86),
  writing.score = c(71)
)

predict(Smodel,newd)

## With Naive Bayes
set.seed(123)
modelNBS <- naiveBayes(gender ~.,data = student_train,laplace = 2)

print(modelNBS)
summary(modelNBS)

## Predict with model
Stpred <- predict(modelNBS,student_test,type = "class")

## Build confusion matrix
confusionMatrix(table(Stpred,student_test$gender))

## With new data
newdat <- data.frame(
  math.score = c(85),
  reading.score = c(67),
  writing.score = c(68)
)

predict(modelNBS,newdat)

## With Support Vector Machines
set.seed(1234)
modelsvm <- svm(as.factor(gender) ~.,data = student_train,kernel = "linear",cost = 10,scale = FALSE)

print(modelsvm)
summary(modelsvm)

## Plot support vectors
plot(modelsvm,student_train,
     math.score ~ reading.score,
     slice = list(writing.score = 50))

## Predict with model
predsvm <- predict(modelsvm,student_test,type = "class")

## Build confusion matrix
confusionMatrix(table(predsvm,student_test$gender))

## With new data
newdt <- data.frame(
  math.score = c(87),
  reading.score = c(66),
  writing.score = c(70)
)

np <- predict(modelsvm,newdt)

np <- unlist(np)

np[[1]]

saveRDS(modelsvm,file = "student_performance.rds")
modelsvm <- readRDS("student_performance.rds")

newdata <- function(math.score,reading.score,writing.score){
  
  modelsvm <- readRDS("student_performance.rds")
  
  newdf <- data.frame(
    math.score = c(math.score),
    reading.score = c(reading.score),
    writing.score = c(writing.score)
  )
  
  prediction <- predict(modelsvm,newdf)
  
  return(paste(prediction[[1]]))
  
}

newdata(math.score = 87,reading.score = 68,writing.score = 71)



## Arules visualization
data("Groceries")
Groceries
class(Groceries)
list(head(Groceries,3))

itemFrequencyPlot(Groceries,support = 0.1,cex.names = 0.8)

## Create rules
rules <- apriori(
  Groceries,
  parameter = list(support = 0.005,confidence = 0.5)
)

## Visualoze rules
plot(rules,method = "graph",control = list(reorder = "similarity",limit = 20))

## Credit card problem
library(drake)

mall <- read.csv("Mall_Customers.csv")

mall <- mall[,-1];mall
names(mall)[c(3,4)] <- c("Annual_Income","Spending_Score");mall
str(mall)

## Split data into train and test
indxm <- sample(2,nrow(mall),replace = T,prob = c(0.8,0.2))
train <- mall[indxm == 1,]
test <- mall[indxm == 2,]
dim(train)
dim(test)

## Fit random forest model
set.seed(1234)
mallmodel <- naiveBayes(Gender ~.,data = train,laplace = 2)
print(mallmodel)

## Validate model
mallpred <- predict(mallmodel,test,response = "classification")
confusionMatrix(table(mallpred,test$Gender))

######## Neural network of iris ########
data("iris")
str(iris)

## Split the data
set.seed(1234)
indexes <- createDataPartition(iris$Species,p = 0.8,list = FALSE)
irisTrain <- iris[indexes,]
irisTest <- iris[-indexes,]

## Extract test labels
ytest <- irisTest[, 5]
xtest <- irisTest[, -5]
str(ytest)
## Define the model
model <- neuralnet(Species ~.,data = irisTrain,hidden = c(4,3),
                   linear.output = FALSE,
                   lifesign = "full",
                   rep = 1)
plot(model,
     col.hidden = "blue",
     col.hidden.synapse = "blue",
     show.weights = TRUE,
     information = TRUE,
     fill = "yellow"
     )
    
## Prediction and accuracy
pred <- compute(model,xtest)
testpred <- pred$net.result

## Extract class with highest prediction
mpred <- data.frame(
  "testpred" = ifelse(max.col(testpred[,1:3])==1,"setosa",
                      ifelse(max.col(testpred[,1:3])==2,"versicolor","virginica")))
str(mpred$testpred)
## Build confusion matrix
confusionMatrix(ytest,as.factor(mpred$testpred))

## XGBoost of penguins
projdata <- penguins
View(projdata)

## Clean data
projdata <- projdata %>%
  select(bill_length_mm,bill_depth_mm,body_mass_g,flipper_length_mm,species) %>% 
  filter(complete.cases(.))

## Check distribution of data points
projdata %>% 
  ggplot()+aes(x = bill_length_mm,y = bill_depth_mm,color = species)+
  geom_point(size = 3)

projdata %>% 
  ggplot()+aes(x = body_mass_g,y = flipper_length_mm,color = species)+
  geom_point(size = 3)

## Pre-processing
species <- projdata$species
labels <- as.integer(projdata$species)-1
projdata$species <- NULL

## Split data into train and test
n <- nrow(projdata)
index <- sample(n,floor(0.75*n))
training <- as.matrix(projdata[index,])
train.label <- labels[index]
testing <- as.matrix(projdata[-index,])
test.label <- labels[-index]

## Transform to D matrix
xg.train <- xgb.DMatrix(training,label = train.label)
xg.test <- xgb.DMatrix(testing,label = test.label)

## Define main parameters
classNum <- length(levels(species))
parameters <- list(
  booster = "gbtree",
  eta = 0.001,
  max_depth = 5,
  gamma = 3,
  subsample = 0.75,
  colsample_bytree = 1,
  objective = "multi:softprob",
  eval_metric = "mlogloss",
  num_class = classNum
)

## Fit model
set.seed(12345)
modelfit <- xgb.train(
  params = parameters,
  data = xg.train,
  nrounds = 10000,
  nthreads = 1,
  early_stopping_rounds = 10,
  watchlist = list(val1 = xg.train,val2 = xg.test),
  verbose = 1
)

## Predict new outcomes
predict.xg <- predict(modelfit,testing,reshape = T)
predict.xg <- as.data.frame(predict.xg)
colnames(predict.xg) <- levels(species)

## Identify class with highest probability
prob <- function(x){
  result <- colnames(predict.xg)[which.max(x)]
  return(result)
}

predict.xg$prediction <- apply(predict.xg,1,prob)
predict.xg$label <- levels(species)[test.label+1]

## Check prediction accuracy
result <- sum(predict.xg$prediction == predict.xg$label)/nrow(predict.xg)
print(paste("Final Accuracy:",sprintf("%1.2f%%",100 * result)))

## Plot train and test error
error_plot <- data.frame(modelfit$evaluation_log)
plot(error_plot$iter,error_plot$val1_mlogloss,col = "blue")
lines(error_plot$iter,error_plot$val2_mlogloss,col = "red")

## With new data
ndata <- data.frame(
  bill_length_mm = c(35.2),
  bill_depth_mm = c(15.3),
  body_mass_g = c(2550),
  flipper_length_mm = c(165)
)

ndmatrix <- as.matrix(ndata)

newpred <- predict(modelfit,ndmatrix,reshape = T)
newpred <- as.data.frame(newpred)
colnames(newpred) <- levels(species)

prob(newpred)
