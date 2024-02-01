
# Ge Gao 260893304
# Import packages and dataset

library(car)
library(ggplot2)
require(methods)
library(viridis)
library(tree)
library(rpart)
library(rpart.plot)
library(MASS)
library(klaR)
library(randomForest)
library(gbm)
library(dplyr)
library(caret)

olympic <- read.csv("/Users/sunnygao/Desktop/mgsc661_final_project/Olympic events data.csv")

colnames(olympic)

olympic_impact <- read.csv("/Users/sunnygao/Desktop/mgsc661_final_project/olympic_impact.csv")



# Data Visualization

# drop columns
olympic <- subset(olympic, select = -c(Name, Team))

# Replace N/A with NA for the Medal column
olympic["Medal"][is.na(olympic["Medal"])] <- "No Medal"

# drop NA
olympic <- na.omit(olympic)

rownames(olympic) <- 1:nrow(olympic) 

unique_city <- c('Albertville', 'Amsterdam', 'Antwerpen', 'Athina', 'Atlanta',
                 'Barcelona', 'Beijing', 'Berlin', 'Calgary', 'Chamonix',
                 "Cortina d'Ampezzo", 'Garmisch-Partenkirchen', 'Grenoble',
                 'Helsinki', 'Innsbruck', 'Lake Placid', 'Lillehammer', 'London',
                 'Los Angeles', 'Melbourne', 'Mexico City', 'Montreal', 'Moskva',
                 'Munich', 'Nagano', 'Oslo', 'Paris', 'Rio de Janeiro', 'Roma',
                 'Salt Lake City', 'Sankt Moritz', 'Sapporo', 'Sarajevo', 'Seoul',
                 'Sochi', 'Squaw Valley', 'St. Louis', 'Stockholm', 'Sydney',
                 'Tokyo', 'Torino', 'Vancouver')

hosted_country <- c("USA", "NED", "BEL", "GRE", "USA", "ESP", "CHN",
                    "GER", "CAN", "FRA", "ITA", "GER", "FRA", "FIN",
                    "AUT", "USA", "NOR", "GBR", "USA", "AUS", "MEX", "CAN",
                    "RUS", "GER", "JPN", "NOR", "FRA", "BRA", "ITA",
                    "USA", "SUI", "JPN", "BIH", "KOR", "RUS", "USA",
                    "USA", "SWE", "AUS", "JPN", "ITA", "CAN")

# Assign the country to the hosted city
Country <- rep(0, nrow(olympic))
for (i in 1:nrow(olympic)){
  city = olympic[,"City"][i]
  ind = which(unique_city == city)
  Country[i] = hosted_country[ind]
}

olympic[,"Country"] = Country

# determine whether the athelete comes from the hosted city
Comes_from_City <- rep(0, nrow(olympic))
for (i in 1:nrow(olympic)){
  noc = olympic[,"NOC"][i]
  country = olympic[,"Country"][i]
  if (noc == country) {
    Comes_from_City[i] = 1
  } else {
    Comes_from_City[i] = 0
  }
}

olympic[,"Comes_from_City"] = Comes_from_City

olympic <- na.omit(olympic)

attach(olympic)

olympic_medal = olympic[olympic$Medal != "No Medal",]



# Data Exploration
## Medal

ggplot(olympic, aes(x=factor(Medal)))+
  geom_bar(stat="count", width=0.7, fill="steelblue")+
  theme_minimal()

## Medal and Sport

ggplot(olympic_medal, aes(fill=Medal, x=factor(Sport))) + 
  geom_bar(position="stack", stat="count")+
  theme_minimal()


## Medal and Year

ggplot(olympic_medal, aes(fill=Medal, x=factor(Year))) + 
  geom_bar(position="stack", stat="count")+
  theme_minimal()


# Medal and City

ggplot(olympic_medal, aes(fill=Medal, x=factor(City))) + 
  geom_bar(position="stack", stat="count")+
  theme_minimal()



## Medal and Comes_from_City

ggplot(olympic, aes(fill=factor(Comes_from_City), x=factor(Medal))) + 
  geom_bar(position="stack", stat="count")+
  theme_minimal()

ggplot(olympic_medal, aes(fill=factor(Comes_from_City), x=factor(Medal))) + 
  geom_bar(position="stack", stat="count")+
  theme_minimal()


## Total_Medal_Score and Comes_from_City

ggplot(olympic_impact, aes(x=Comes_from_City, y=Total_Medal_Score)) + 
  geom_point()+
  geom_smooth(method=lm)


## Pairs plot
pairs(olympic_impact[,-c(2)])




# Model Building
## MinMax Normalization

decimal_scale <- function(x) {
  # Find the maximum absolute value of x
  max_abs <- max(abs(x))
  
  # Find the smallest power of 10 that is equal to or larger than max_abs
  power <- ceiling(log10(max_abs))
  
  # Divide x by 10^power
  x / (10^power)
}


# Apply decimal scaling to the mpg variable of mtcars
olympic_impact_std = olympic_impact[, -c(2)]
for (i in colnames(olympic_impact_std)){
  olympic_impact_std[[i]] = decimal_scale(olympic_impact_std[[i]])
}


olympic_impact_std$NOC = olympic_impact$NOC


## PCA preprocessing

olympic_impact_pca <- olympic_impact_std[, -c(7, 13)]
pca_result <- princomp(olympic_impact_pca, cor = TRUE)
summary(pca_result)

pve=(pca_result$sdev^2)/sum(pca_result$sdev^2) # variance explained by each PC
par(mfrow=c(1,2)) # remember from the 80% rule
plot(pve, ylim=c(0,1))
plot(cumsum(pve), ylim=c(0,1))

pca_transformed_data <- predict(pca_result, newdata = olympic_impact_std)[, 1:11]
combined_data <- cbind(olympic_impact_std, pca_transformed_data)

olympic_impact_pca <- combined_data

pairs(olympic_impact_pca[,-c(1:5, 7:13)])



## Feature Engineering

colnames(olympic_impact)

reg <- lm(Total_Medal_Score ~ Year + Comes_from_City + Avg_Age + factor(NOC)
          + Avg_Height + Avg_Weight + Number_of_Athelete + Sport_type_Both
          + Sport_type_Individual + Sport_type_Team + Sex_F + Sex_M, 
          data = olympic_impact)

summary(reg)

corrMat <- cor(olympic_impact[,-c(2)])
corrMat



## Prediction for the Total_Medal_Score
### Random Forest
set.seed(123)
forest1=randomForest(Total_Medal_Score ~ Year + NOC + Comes_from_City + Avg_Age 
                     + Avg_Height + Avg_Weight + Number_of_Athelete + Sport_type_Both
                     + Sport_type_Individual + Sport_type_Team + Sex_F + Sex_M,
                     ntree=1000, data=olympic_impact, importance=TRUE, 
                     na.action = na.omit, do.trace=50) 

importance(forest1)

set.seed(123)
forest2=randomForest(Total_Medal_Score ~ NOC + Comes_from_City + Avg_Age 
                     + Avg_Height + Avg_Weight + Number_of_Athelete + Sport_type_Both
                     + Sport_type_Individual + Sport_type_Team + Sex_F + Sex_M,
                     ntree=1000, data=olympic_impact, importance=TRUE, 
                     na.action = na.omit, do.trace=50) 
# Year cannot be dropped

set.seed(123)
forest3=randomForest(Total_Medal_Score ~ Year + NOC + Comes_from_City + Avg_Age 
                     + Avg_Height + Avg_Weight + Number_of_Athelete + Sport_type_Both
                     + Sport_type_Individual + Sport_type_Team,
                     ntree=1000, data=olympic_impact, importance=TRUE, 
                     na.action = na.omit, do.trace=50) 
# Sex cannot be dropped

set.seed(123)
forest4=randomForest(Total_Medal_Score ~ Year + NOC + Avg_Age 
                     + Avg_Height + Avg_Weight + Number_of_Athelete + Sport_type_Both
                     + Sport_type_Individual + Sport_type_Team + Sex_F + Sex_M,
                     ntree=1000, data=olympic_impact, importance=TRUE, 
                     na.action = na.omit, do.trace=50)
# Comes_from_City cannot be dropped

set.seed(123)
forest5=randomForest(Total_Medal_Score ~ Year + NOC + Comes_from_City + Avg_Age 
                     + Avg_Height + Avg_Weight + Number_of_Athelete + Sex_F + Sex_M,
                     ntree=1000, data=olympic_impact, importance=TRUE, 
                     na.action = na.omit, do.trace=50) 
# Sport_type cannot be dropped



set.seed(123)
forest6=randomForest(Total_Medal_Score ~ Year + NOC + Comes_from_City + Avg_Age 
                     + Avg_Height + Avg_Weight + Sport_type_Both
                     + Sport_type_Individual + Sport_type_Team + Sex_F + Sex_M,
                     ntree=1000, data=olympic_impact, importance=TRUE, 
                     na.action = na.omit, do.trace=50) 
# Number_of_Athelete cannot be dropped



set.seed(123)
forest7=randomForest(Total_Medal_Score ~ Year + NOC + Comes_from_City + Avg_Age 
                     + Avg_Height + Number_of_Athelete + Sport_type_Both
                     + Sport_type_Individual + Sport_type_Team + Sex_F + Sex_M,
                     ntree=1000, data=olympic_impact, importance=TRUE, 
                     na.action = na.omit, do.trace=50) 
# Avg_Weight cannot be dropped



set.seed(123)
forest8=randomForest(Total_Medal_Score ~ Year + NOC + Comes_from_City + Avg_Age 
                     + Avg_Weight + Number_of_Athelete + Sport_type_Both
                     + Sport_type_Individual + Sport_type_Team + Sex_F + Sex_M,
                     ntree=1000, data=olympic_impact, importance=TRUE, 
                     na.action = na.omit, do.trace=50)
# Avg_Height cannot be dropped




set.seed(123)
forest9=randomForest(Total_Medal_Score ~ Year + NOC + Comes_from_City 
                     + Avg_Height + Avg_Weight + Number_of_Athelete + Sport_type_Both
                     + Sport_type_Individual + Sport_type_Team + Sex_F + Sex_M,
                     ntree=1000, data=olympic_impact, importance=TRUE, 
                     na.action = na.omit, do.trace=50) 
# Avg_Age cannot be dropped




# Best model for random forest
set.seed(123)
forest=randomForest(Total_Medal_Score ~ Year + NOC + Comes_from_City + Avg_Age 
                    + Avg_Height + Avg_Weight + Number_of_Athelete + Sport_type_Both
                    + Sport_type_Individual + Sport_type_Team + Sex_F + Sex_M,
                    ntree=350, data=olympic_impact, importance=TRUE, 
                    na.action = na.omit) 

forest


### Random Forest with MinMax Scaling

set.seed(123)
forest_std=randomForest(Total_Medal_Score ~ .,
                        ntree=350, data=olympic_impact_std, importance=TRUE, 
                        na.action = na.omit) 

forest_std


### Random Forest with PCA preprocessing

set.seed(123)
forest_pca1=randomForest(Total_Medal_Score ~ NOC + Comp.1 + Comp.2 + Comp.3 + Comp.4 +
                           Comp.5 + Comp.6 + Comp.7 + Comp.8 + Comp.9 + Comp.10 + Comp.11,
                         ntree=350, data=olympic_impact_pca, importance=TRUE, 
                         na.action = na.omit) 



forest_pca1


importance(forest_pca1)




set.seed(123)
forest_pca2=randomForest(Total_Medal_Score ~ NOC + Comp.1 + Comp.2 + Comp.3 + Comp.4 +
                           Comp.5 + Comp.6,
                         ntree=350, data=olympic_impact_pca, importance=TRUE, 
                         na.action = na.omit) 



forest_pca2


### Gradient Boosting

set.seed(123)
train_control <- trainControl(
  method = 'cv',
  number = 5
)
gbmGrid <- expand.grid(
  n.trees = seq(50, 400, by = 50),
  interaction.depth = seq(1, 5, by = 1),
  shrinkage = c(0.001, 0.01, 0.05, 0.1), 
  n.minobsinnode = c(5, 10, 15)
)

boosted_model <- train(
  Total_Medal_Score ~ Year + Comes_from_City + Avg_Age 
  + Avg_Height + Avg_Weight + Number_of_Athelete + Sport_type_Both
  + Sport_type_Individual + Sport_type_Team + Sex_F + Sex_M,
  data = olympic_impact_std,
  method = "gbm",
  distribution= "gaussian",
  trControl = train_control,
  tuneGrid = gbmGrid,
  verbose = FALSE
)



boosted_model



boosted_model$results




















































