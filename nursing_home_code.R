#Open the csv file and replace "x" by NA

#Packages prerequired
library(caret)
library(lattice)
library(mice)
library(VIM)
library(caTools)
library(randomForest)
library(ggplot2)
library(e1071)
library(ROCR)
library(RColorBrewer)
library(scales)
library(goldeneye)

#setwd("C:/")
nursing.data.2017 <- read.table(file="Nursing_home_data_2017.csv",
                                header = TRUE, sep=",", 
                                colClasses = NA, stringsAsFactor = FALSE, na.strings = "x")

nursing.data.2016 <- read.table(file="Nursing_home_data_2016.csv",
                                header = TRUE, sep=",", 
                                colClasses = NA, stringsAsFactor = FALSE, na.strings = "x")

nursing.data.2015 <- read.table(file="Nursing_home_data_2015.csv",
                                header = TRUE, sep=",", 
                                colClasses = NA, stringsAsFactor = FALSE, na.strings = "x")

nursing.data.2014 <- read.table(file="Nursing_home_data_2014.csv",
                                header = TRUE, sep=",", 
                                colClasses = NA, stringsAsFactor = FALSE, na.strings = "x")


### PRE-PROCESSING ###

#Sort demens form somatik in 2014

data<-nursing.data.2014
data["type"]<-"Normal"
data[grep("emens",nursing.data.2014[,3]),"type"]<-"Demens"
data[grep("emens",nursing.data.2014[,4]),"type"]<-"Demens"
data[grep("emens",nursing.data.2014[,5]),"type"]<-"Demens"
data[grep("emens",nursing.data.2014[,6]),"type"]<-"Demens"

summary(as.factor(data$type))
demens_df.2014<-subset(data,data$type=="Demens")
normal_df.2014<-subset(data,data$type=="Normal")

#Sort demens form somatik in 2015

data<-nursing.data.2015
data["type"]<-"Normal"
data[grep("emens",nursing.data.2015[,3]),"type"]<-"Demens"
data[grep("emens",nursing.data.2015[,4]),"type"]<-"Demens"
data[grep("emens",nursing.data.2015[,5]),"type"]<-"Demens"
data[grep("emens",nursing.data.2015[,6]),"type"]<-"Demens"

summary(as.factor(data$type))
demens_df.2015<-subset(data,data$type=="Demens")
normal_df.2015<-subset(data,data$type=="Normal")


#Sort demens form somatik in 2016

data<-nursing.data.2016
data["type"]<-"Normal"
data[grep("emens",nursing.data.2016[,3]),"type"]<-"Demens"
data[grep("emens",nursing.data.2016[,4]),"type"]<-"Demens"
data[grep("emens",nursing.data.2016[,5]),"type"]<-"Demens"
data[grep("emens",nursing.data.2016[,6]),"type"]<-"Demens"

summary(as.factor(data$type))

demens_df.2016<-subset(data,data$type=="Demens")
normal_df.2016<-subset(data,data$type=="Normal")

#Sort demens form somatik in 2017

data<-nursing.data.2017
data["type"]<-"Normal"
data[grep("emens",nursing.data.2017[,3]),"type"]<-"Demens"
data[grep("emens",nursing.data.2017[,4]),"type"]<-"Demens"
data[grep("emens",nursing.data.2017[,5]),"type"]<-"Demens"
data[grep("emens",nursing.data.2017[,6]),"type"]<-"Demens"

summary(as.factor(data$type))

demens_df.2017<-subset(data,data$type=="Demens")
normal_df.2017<-subset(data,data$type=="Normal")

# remove all missing data
normal_df.2014 <- na.omit(normal_df.2014[,10:34])
normal_df.2015 <- na.omit(normal_df.2015[,10:34])
normal_df.2016 <- na.omit(normal_df.2016[,10:34])
normal_df.2017 <- na.omit(normal_df.2017[,10:34])

#create categories based on quantiles to transform the regression problem into a classification problem for 2014
normal_df.2014["class"]<- NA

for(k in 1:nrow(normal_df.2014)){
  if (normal_df.2014[k,25] <=90){
    normal_df.2014[k,26] <- "<=90%"
  }
  else if (normal_df.2014[k,25] >90){
    normal_df.2014[k,26] <- ">90%"
  }
  else{
    normal_df.2014[k,26] <- NA
  }
}

#same for 2015
normal_df.2015["class"]<- NA

for(k in 1:nrow(normal_df.2015)){
  if (normal_df.2015[k,25] <=90){
    normal_df.2015[k,26] <- "<=90%"
  }
  else if (normal_df.2015[k,25] >90){
    normal_df.2015[k,26] <- ">90%"
  }
  else{
    normal_df.2015[k,26] <- NA
  }
}

#same for 2016
normal_df.2016["class"]<- NA

for(k in 1:nrow(normal_df.2016)){
  if (normal_df.2016[k,25] <=90){
    normal_df.2016[k,26] <- "<=90%"
  }
  else if (normal_df.2016[k,25] >90){
    normal_df.2016[k,26] <- ">90%"
  }
  else{
    normal_df.2016[k,26] <- NA
  }
}

# same for 2017
normal_df.2017["class"]<- NA

for(k in 1:nrow(normal_df.2017)){
  if (normal_df.2017[k,25] <=90){
    normal_df.2017[k,26] <- "<=90%"
  }
  else if (normal_df.2017[k,25] >90){
    normal_df.2017[k,26] <- ">90%"
  }
  else{
    normal_df.2017[k,26] <- NA
  }
}

# Splitting the dataset into the Training set and Test set for 2014
# install.packages('caTools')
set.seed(123)
split <- sample.split(normal_df.2014$F24, SplitRatio = 0.75)
training_set.2014 <- subset(normal_df.2014, split == TRUE)
test_set.2014 <- subset(normal_df.2014, split == FALSE)

# Splitting the dataset into the Training set and Test set for 2015
# install.packages('caTools')
set.seed(123)
split <- sample.split(normal_df.2015$F24, SplitRatio = 0.75)
training_set.2015 <- subset(normal_df.2015, split == TRUE)
test_set.2015 <- subset(normal_df.2015, split == FALSE)

# Splitting the dataset into the Training set and Test set for 2016
# install.packages('caTools')
set.seed(123)
split <- sample.split(normal_df.2016$F24, SplitRatio = 0.75)
training_set.2016 <- subset(normal_df.2016, split == TRUE)
test_set.2016 <- subset(normal_df.2016, split == FALSE)

# Splitting the dataset into the Training set and Test set for 2017
# install.packages('caTools')
set.seed(123)
split <- sample.split(normal_df.2017$F24, SplitRatio = 0.75)
training_set.2017 <- subset(normal_df.2017, split == TRUE)
test_set.2017 <- subset(normal_df.2017, split == FALSE)

### REGRESSION ###

#random forest regression on data set for 2014: need only 150 trees according to plot(regression)

set.seed(101)

regression1.2014 <- randomForest(x= training_set.2014[,1:24], y= training_set.2014$F24, mtry = 4, ntree= 51, importance = TRUE, do.trace = TRUE)

set.seed(42)
regression2.2014 <- randomForest(x= training_set.2014[,1:24], y= training_set.2014$F24, mtry = 4, ntree= 51, importance = TRUE, do.trace = TRUE)

set.seed(666)
regression3.2014 <- randomForest(x= training_set.2014[,1:24], y= training_set.2014$F24, mtry = 4, ntree= 51, importance = TRUE, do.trace = TRUE)

set.seed(1988)
regression4.2014 <- randomForest(x= training_set.2014[,1:24], y= training_set.2014$F24, mtry = 4, ntree= 51, importance = TRUE, do.trace = TRUE)

set.seed(1984)
regression5.2014 <- randomForest(x= training_set.2014[,1:24], y= training_set.2014$F24, mtry = 4, ntree= 51, importance = TRUE, do.trace = TRUE)


regression.final.2014<-combine(regression1.2014, regression2.2014, regression3.2014, regression4.2014, regression5.2014)
pred.reg.2014 <- predict(regression.final.2014, test_set.2014[,1:24], type = "response")
print(regression.final.2014)

##Root Mean square error
sqrt(mean((test_set.2014$F24-pred.reg.2014)^2))

##variable importance
varImpPlot(regression.final.2014, main="Attribute importance by regression RF on 2014 dataset", scale = FALSE)

##Regression curve 2014
fit2014 <- lm(pred.reg.2014 ~ test_set.2014$F24)

ggplotRegression.2014 <- function (fit2014) {
  
  require(ggplot2)
  
  ggplot(fit2014$model, aes_string(x = names(fit2014$model)[2], y = names(fit2014$model)[1])) + 
    geom_point() +
    stat_smooth(method = "lm", col = "red") +
    labs(title = paste("Adj R2 = ",signif(summary(fit2014)$adj.r.squared, 4),
                       " P =",signif(summary(fit2014)$coef[2,4], 4),
                       " Slope =",signif(fit2014$coef[[2]], 4),
                       " RMSE =", signif(sqrt(mean((test_set.2014$F24-pred.reg.2014)^2)), 4)),
         x="test set satisfaction values",
         y= "predicted satisfaction values")+
    theme_bw(base_size = 13)
}

ggplotRegression.2014(lm( pred.reg.2014 ~ test_set.2014$F24))


#random forest regression on data set for 2015

set.seed(101)

regression1.2015 <- randomForest(x= training_set.2015[,1:24], y= training_set.2015$F24, mtry = 4, ntree= 51, importance = TRUE, do.trace = TRUE)

set.seed(42)
regression2.2015 <- randomForest(x= training_set.2015[,1:24], y= training_set.2015$F24, mtry = 4, ntree= 51, importance = TRUE, do.trace = TRUE)

set.seed(666)
regression3.2015 <- randomForest(x= training_set.2015[,1:24], y= training_set.2015$F24, mtry = 4, ntree= 51, importance = TRUE, do.trace = TRUE)

set.seed(1988)
regression4.2015 <- randomForest(x= training_set.2015[,1:24], y= training_set.2015$F24, mtry = 4, ntree= 51, importance = TRUE, do.trace = TRUE)

set.seed(1984)
regression5.2015 <- randomForest(x= training_set.2015[,1:24], y= training_set.2015$F24, mtry = 4, ntree= 51, importance = TRUE, do.trace = TRUE)


regression.final.2015<-combine(regression1.2015, regression2.2015, regression3.2015, regression4.2015, regression5.2015)
pred.reg.2015 <- predict(regression.final.2015, test_set.2015[,1:24], type = "response")
print(regression.final.2015)

##Root Mean square error
sqrt(mean((test_set.2015$F24-pred.reg.2015)^2))

##variable importance
varImpPlot(regression.final.2015, main="Attribute importance by regression RF on for 2015 dataset", scale = FALSE)

##Regression curve 2015
fit2015 <- lm(pred.reg.2015 ~ test_set.2015$F24)

ggplotRegression.2015 <- function (fit2015) {
  
  require(ggplot2)
  
  ggplot(fit2015$model, aes_string(x = names(fit2015$model)[2], y = names(fit2015$model)[1])) + 
    geom_point() +
    stat_smooth(method = "lm", col = "red") +
    labs(title = paste("Adj R2 = ",signif(summary(fit2015)$adj.r.squared, 4),
                       " P =",signif(summary(fit2015)$coef[2,4], 4),
                       " Slope =",signif(fit2015$coef[[2]], 4),
                       " RMSE =", signif(sqrt(mean((test_set.2015$F24-pred.reg.2015)^2)), 4)),
         x="test set satisfaction values",
         y= "predicted satisfaction values")+
  theme_bw(base_size = 13)
}

ggplotRegression.2015(lm( pred.reg.2015 ~ test_set.2015$F24))



#random forest regression on data set for 2016

set.seed(101)

regression1.2016 <- randomForest(x= training_set.2016[,1:24], y= training_set.2016$F24, mtry = 4, ntree= 51, importance = TRUE, do.trace = TRUE)

set.seed(42)
regression2.2016 <- randomForest(x= training_set.2016[,1:24], y= training_set.2016$F24, mtry = 4, ntree= 51, importance = TRUE, do.trace = TRUE)

set.seed(666)
regression3.2016 <- randomForest(x= training_set.2016[,1:24], y= training_set.2016$F24, mtry = 4, ntree= 51, importance = TRUE, do.trace = TRUE)

set.seed(1988)
regression4.2016 <- randomForest(x= training_set.2016[,1:24], y= training_set.2016$F24, mtry = 4, ntree= 51, importance = TRUE, do.trace = TRUE)

set.seed(1984)
regression5.2016 <- randomForest(x= training_set.2016[,1:24], y= training_set.2016$F24, mtry = 4, ntree= 51, importance = TRUE, do.trace = TRUE)


regression.final.2016<-combine(regression1.2016, regression2.2016, regression3.2016, regression4.2016, regression5.2016)
pred.reg.2016 <- predict(regression.final.2016, test_set.2016[,1:24], type = "response")
print(regression.final.2016)

##Root Mean square error
sqrt(mean((test_set.2016$F24-pred.reg.2016)^2))

##variable importance
varImpPlot(regression.final.2016, main="Attribute importance by regression RF on for 2016 dataset", scale = FALSE)


##Regression curve 2016
fit2016 <- lm(pred.reg.2016 ~ test_set.2016$F24)

ggplotRegression.2016 <- function (fit2016) {
  
  require(ggplot2)
  
  ggplot(fit2016$model, aes_string(x = names(fit2016$model)[2], y = names(fit2016$model)[1])) + 
    geom_point() +
    stat_smooth(method = "lm", col = "red") +
    labs(title = paste("Adj R2 = ",signif(summary(fit2016)$adj.r.squared, 4),
                       " P =",signif(summary(fit2016)$coef[2,4], 4),
                       " Slope =",signif(fit2016$coef[[2]], 4),
                       " RMSE =", signif(sqrt(mean((test_set.2016$F24-pred.reg.2016)^2)), 4)),
         x="test set satisfaction values",
         y= "predicted satisfaction values")+
    theme_bw(base_size = 13)
}

ggplotRegression.2016(lm( pred.reg.2016 ~ test_set.2016$F24))


#random forest regression on data set with omitted NA for 2017

set.seed(101)
regression1.2017 <- randomForest(x= training_set.2017[,1:24], y= training_set.2017$F24, mtry = 4, ntree= 51, importance = TRUE, do.trace = TRUE)

set.seed(42)
regression2.2017 <- randomForest(x= training_set.2017[,1:24], y= training_set.2017$F24, mtry = 4, ntree= 51, importance = TRUE, do.trace = TRUE)

set.seed(666)
regression3.2017 <- randomForest(x= training_set.2017[,1:24], y= training_set.2017$F24, mtry = 4, ntree= 51, importance = TRUE, do.trace = TRUE)

set.seed(1988)
regression4.2017 <- randomForest(x= training_set.2017[,1:24], y= training_set.2017$F24, mtry = 4, ntree= 51, importance = TRUE, do.trace = TRUE)

set.seed(1984)
regression5.2017 <- randomForest(x= training_set.2017[,1:24], y= training_set.2017$F24, mtry = 4, ntree= 51, importance = TRUE, do.trace = TRUE)


regression.final.2017<-combine(regression1.2017, regression2.2017, regression3.2017, regression4.2017, regression5.2017)
pred.reg.2017 <- predict(regression.final.2017, test_set.2017[,1:24], type = "response")
print(regression.final.2017)

##Root Mean square error
sqrt(mean((test_set.2017$F24-pred.reg.2017)^2))

##variable importance
varImpPlot(regression.final.2017, main="Attribute importance by regression RF on 2017 dataset", scale = FALSE)
print(regression.final.2017$importance)

##Regression curve 2017
fit2017 <- lm(pred.reg.2017 ~ test_set.2017$F24)

ggplotRegression.2017 <- function (fit2017) {
  
  require(ggplot2)
  
  ggplot(fit2017$model, aes_string(x = names(fit2017$model)[2], y = names(fit2017$model)[1])) +  
    geom_point() +
    stat_smooth(method = "lm", col = "red") +
    labs(title = paste("Adj R2 = ",signif(summary(fit2017)$adj.r.squared, 4),
                       " P =",signif(summary(fit2017)$coef[2,4], 4),
                       " Slope =",signif(fit2017$coef[[2]], 4),
                       " RMSE =", signif(sqrt(mean((test_set.2017$F24-pred.reg.2017)^2)), 4)),
         x="test set satisfaction values",
         y= "predicted satisfaction values")+
    theme_bw(base_size = 13)
}

ggplotRegression.2017(lm( pred.reg.2017 ~ test_set.2017$F24))

##Top 9 features %IncMSE over the years

top.features.reg <- data.frame(Features<- rep(c("F5", "F6", "F9", "F10", "F16", "F17", "F18", "F23", "F27"), each=4),
                                 Year <- rep(c("2014", "2015", "2016", "2017"), 9),
                                 IncMSE <- c(regression.final.2014$importance[5,1], regression.final.2015$importance[5,1], regression.final.2016$importance[5,1], regression.final.2017$importance[5,1],
                                             regression.final.2014$importance[6,1], regression.final.2015$importance[6,1], regression.final.2016$importance[6,1], regression.final.2017$importance[6,1],
                                             regression.final.2014$importance[9,1], regression.final.2015$importance[9,1], regression.final.2016$importance[9,1], regression.final.2017$importance[9,1],
                                             regression.final.2014$importance[10,1], regression.final.2015$importance[10,1], regression.final.2016$importance[10,1], regression.final.2017$importance[10,1],
                                             regression.final.2014$importance[15,1], regression.final.2015$importance[15,1], regression.final.2016$importance[15,1], regression.final.2017$importance[15,1],
                                             regression.final.2014$importance[16,1], regression.final.2015$importance[16,1], regression.final.2016$importance[16,1], regression.final.2017$importance[16,1],
                                             regression.final.2014$importance[17,1], regression.final.2015$importance[17,1], regression.final.2016$importance[17,1], regression.final.2017$importance[17,1],
                                             regression.final.2014$importance[22,1], regression.final.2015$importance[22,1], regression.final.2016$importance[22,1], regression.final.2017$importance[22,1],
                                             regression.final.2014$importance[24,1], regression.final.2015$importance[24,1], regression.final.2016$importance[24,1], regression.final.2017$importance[24,1]),
                                 
                                 SD <-c(regression.final.2014$importanceSD[5], regression.final.2015$importanceSD[5], regression.final.2016$importanceSD[5], regression.final.2017$importanceSD[5],
                                        regression.final.2014$importanceSD[6], regression.final.2015$importanceSD[6], regression.final.2016$importanceSD[6], regression.final.2017$importanceSD[6],
                                        regression.final.2014$importanceSD[9], regression.final.2015$importanceSD[9], regression.final.2016$importanceSD[9], regression.final.2017$importanceSD[9],
                                        regression.final.2014$importanceSD[10], regression.final.2015$importanceSD[10], regression.final.2016$importanceSD[10], regression.final.2017$importanceSD[10],
                                        regression.final.2014$importanceSD[15], regression.final.2015$importanceSD[15], regression.final.2016$importanceSD[15], regression.final.2017$importanceSD[15],
                                        regression.final.2014$importanceSD[16], regression.final.2015$importanceSD[16], regression.final.2016$importanceSD[16], regression.final.2017$importanceSD[16],
                                        regression.final.2014$importanceSD[17], regression.final.2015$importanceSD[17], regression.final.2016$importanceSD[17], regression.final.2017$importanceSD[17],
                                        regression.final.2014$importanceSD[22], regression.final.2015$importanceSD[22], regression.final.2016$importanceSD[22], regression.final.2017$importanceSD[22],
                                        regression.final.2014$importanceSD[24], regression.final.2015$importanceSD[24], regression.final.2016$importanceSD[24], regression.final.2017$importanceSD[24]))

Features <- ordered(Features, levels = c("F5", "F6", "F9", "F10", "F16", "F17", "F18", "F23", "F27"))# required to give levels for the vector features F27>F23>...>F5 in order for the heatmap y-axis to be in right order
colnames(top.features.reg) <- c("features","year", "IncMSE", "SD") #tidy up the structure of the dataframe and give nice names to columns

#plot heatmap
heatmap.reg <- ggplot(data = top.features.reg, aes(x = Year, y = Features)) +
  geom_tile(aes(fill = IncMSE)) +
  scale_fill_gradient(low = "white", high = "steelblue")+
  theme_bw(base_size = 14) + # Size of fonts and customization of theme down below
  theme(
    plot.margin = unit(c(0.2, 0, 0.2, 0.4), "cm"),
    #        axis.title.y=element_text(margin=margin(0,17,0,0)), # space between y label and scale
    panel.background= element_rect(fill = NA, colour = "black", size=1),  # frame around plot
    panel.border= element_rect(fill = NA, colour = "black", size=1),
    #strip.background =  element_rect(fill = "#D8D8D8", colour = "black", size=1), # frame around title
    strip.text = element_text(colour = "black"),
    #panel.grid.major.y = element_blank(), # remove lines on plot
    #panel.grid.minor.y = element_blank(),
    #panel.grid.major.x = element_blank(),
    #panel.grid.minor.x = element_blank(),
    legend.title = element_text(size=14),
    legend.position="right"
  )

heatmap.reg

#plot boxplot all years combined

box.reg <- ggplot(data=top.features.reg, aes(x=Features, y=IncMSE))+
  geom_boxplot()+
  geom_point()+
  theme_bw(base_size = 14) + # Size of fonts and customization of theme down below
  theme(
    plot.margin = unit(c(0.2, 0.2, 0.2, 0.4), "cm"),
    #        axis.title.y=element_text(margin=margin(0,17,0,0)), # space between y label and scale
    panel.background= element_rect(fill = NA, colour = "black", size=1),  # frame around plot
    panel.border= element_rect(fill = NA, colour = "black", size=1),
    #strip.background =  element_rect(fill = "#D8D8D8", colour = "black", size=1), # frame around title
    strip.text = element_text(colour = "black"),
    #panel.grid.major.y = element_blank(), # remove lines on plot
    #panel.grid.minor.y = element_blank(),
    #panel.grid.major.x = element_blank(),
    #panel.grid.minor.x = element_blank(),
    legend.title = element_text(size=14),
    legend.position="right"
  )

box.reg

### CLASSIFICATION ###

#random forest classification on data set for 2014

set.seed(101)
classification1.2014 <- randomForest(x= training_set.2014[,1:24], y= as.factor(training_set.2014$class), mtry = 4, ntree= 51, importance = TRUE, do.trace = TRUE)

set.seed(42)
classification2.2014 <- randomForest(x= training_set.2014[,1:24], y= as.factor(training_set.2014$class), mtry = 4, ntree= 51, importance = TRUE, do.trace = TRUE)

set.seed(666)
classification3.2014 <- randomForest(x= training_set.2014[,1:24], y= as.factor(training_set.2014$class), mtry = 4, ntree= 51, importance = TRUE, do.trace = TRUE)

set.seed(1988)
classification4.2014 <- randomForest(x= training_set.2014[,1:24], y= as.factor(training_set.2014$class), mtry = 4, ntree= 51, importance = TRUE, do.trace = TRUE)

set.seed(1984)
classification5.2014 <- randomForest(x= training_set.2014[,1:24], y= as.factor(training_set.2014$class), mtry = 4, ntree= 51, importance = TRUE, do.trace = TRUE)

classifier.final.2014<-combine(classification1.2014, classification2.2014, classification3.2014, classification4.2014, classification5.2014)
pred.class.goldeneye.2014 <- predict(classifier.final.2014, test_set.2014[,1:24]) #necessary to NOT have type="prob" for GoldenEye++ package
pred.class.ROC.2014 <- predict(classifier.final.2014, test_set.2014[,1:24], type="prob") #necessary to have type="prob" for AUC with ROCR package
print(classifier.final.2014)

##variable importance
varImpPlot(classifier.final.2014, main="Attribute importance by classification RF on for 2014 dataset", scale = FALSE)

##Plot ROC curve and AUC for classification 2014 dataset
ROC.pred.2014 = prediction(pred.class.ROC.2014[,2], test_set.2014$class)
ROC.perf.2014 = performance(ROC.pred.2014,"tpr","fpr")
AUC.class.2014 <- performance(ROC.pred.2014, "auc")
ACC.class.2014 <- performance(ROC.pred.2014, "acc")
AUC.class.2014@y.values[[1]]
plot(ROC.perf.2014,main="ROC Curve for 2014 dataset",col=2,lwd=2, cex.lab=1.5, cex.axis=1.5, cex.main=1.5, cex.sub=1.5)
text(0.3, 0.6, paste("AUC=",signif(AUC.class.2014@y.values[[1]], 5), sep=" "), pos=3, cex=1.5) 
abline(a=0,b=1,lwd=2,lty=2,col="gray")

#random forest classification on data set for 2015

set.seed(101)
classification1.2015 <- randomForest(x= training_set.2015[,1:24], y= as.factor(training_set.2015$class), mtry = 4, ntree= 51, importance = TRUE, do.trace = TRUE)

set.seed(42)
classification2.2015 <- randomForest(x= training_set.2015[,1:24], y= as.factor(training_set.2015$class), mtry = 4, ntree= 51, importance = TRUE, do.trace = TRUE)

set.seed(666)
classification3.2015 <- randomForest(x= training_set.2015[,1:24], y= as.factor(training_set.2015$class), mtry = 4, ntree= 51, importance = TRUE, do.trace = TRUE)

set.seed(1988)
classification4.2015 <- randomForest(x= training_set.2015[,1:24], y= as.factor(training_set.2015$class), mtry = 4, ntree= 51, importance = TRUE, do.trace = TRUE)

set.seed(1984)
classification5.2015 <- randomForest(x= training_set.2015[,1:24], y= as.factor(training_set.2015$class), mtry = 4, ntree= 51, importance = TRUE, do.trace = TRUE)

classifier.final.2015<-combine(classification1.2015, classification2.2015, classification3.2015, classification4.2015, classification5.2015)
pred.class.goldeneye.2015 <- predict(classifier.final.2015, test_set.2015[,1:24])
pred.class.ROC.2015 <- predict(classifier.final.2015, test_set.2015[,1:24], type="prob")
print(classifier.final.2015)

##variable importance
varImpPlot(classifier.final.2015, main="Attribute importance by classification RF on for 2015 dataset", scale = FALSE)

##Plot ROC curve and AUC for classification 2015 dataset
ROC.pred.2015 = prediction(pred.class.ROC.2015[,2], test_set.2015$class)
ROC.perf.2015 = performance(ROC.pred.2015,"tpr","fpr")
AUC.class.2015 <- performance(ROC.pred.2015, "auc")
ACC.class.2015 <- performance(ROC.pred.2015, "acc")
AUC.class.2015@y.values[[1]]
plot(ROC.perf.2015,main="ROC Curve for 2015 dataset",col=2,lwd=2, cex.lab=1.5, cex.axis=1.5, cex.main=1.5, cex.sub=1.5)
text(0.3, 0.6, paste("AUC=",signif(AUC.class.2015@y.values[[1]], 5), sep=" "), pos=3, cex=1.5)
abline(a=0,b=1,lwd=2,lty=2,col="gray")


#random forest classification on data set for 2016

set.seed(101)
classification1.2016 <- randomForest(x= training_set.2016[,1:24], y= as.factor(training_set.2016$class), mtry = 4, ntree= 51, importance = TRUE, do.trace = TRUE)

set.seed(42)
classification2.2016 <- randomForest(x= training_set.2016[,1:24], y= as.factor(training_set.2016$class), mtry = 4, ntree= 51, importance = TRUE, do.trace = TRUE)

set.seed(666)
classification3.2016 <- randomForest(x= training_set.2016[,1:24], y= as.factor(training_set.2016$class), mtry = 4, ntree= 51, importance = TRUE, do.trace = TRUE)

set.seed(1988)
classification4.2016 <- randomForest(x= training_set.2016[,1:24], y= as.factor(training_set.2016$class), mtry = 4, ntree= 51, importance = TRUE, do.trace = TRUE)

set.seed(1984)
classification5.2016 <- randomForest(x= training_set.2016[,1:24], y= as.factor(training_set.2016$class), mtry = 4, ntree= 51, importance = TRUE, do.trace = TRUE)

classifier.final.2016<-combine(classification1.2016, classification2.2016, classification3.2016, classification4.2016, classification5.2016)
pred.class.goldeneye.2016 <- predict(classifier.final.2016, test_set.2016[,1:24])
pred.class.ROC.2016 <- predict(classifier.final.2016, test_set.2016[,1:24], type="prob")
print(classifier.final.2016)

##variable importance
varImpPlot(classifier.final.2016, main="Attribute importance by classification RF on for 2016 dataset", scale = FALSE)

##Plot ROC curve and AUC for classification 2016 dataset
ROC.pred.2016 = prediction(pred.class.ROC.2016[,2], test_set.2016$class)
ROC.perf.2016 = performance(ROC.pred.2016,"tpr","fpr")
AUC.class.2016 <- performance(ROC.pred.2016, "auc")
ACC.class.2016 <- performance(ROC.pred.2016, "acc")
AUC.class.2016@y.values[[1]]
plot(ROC.perf.2016,main="ROC Curve for 2016 dataset",col=2,lwd=2, cex.lab=1.5, cex.axis=1.5, cex.main=1.5, cex.sub=1.5)
text(0.3, 0.6, paste("AUC=",signif(AUC.class.2016@y.values[[1]], 5), sep = " "), pos=3, cex=1.5) 
abline(a=0,b=1,lwd=2,lty=2,col="gray")

#random forest regression on data set with omitted NA for 2017

set.seed(101)
classification1.2017 <- randomForest(x= training_set.2017[,1:24], y= as.factor(training_set.2017$class), mtry = 4, ntree= 51, importance = TRUE, do.trace = TRUE)

set.seed(42)
classification2.2017 <- randomForest(x= training_set.2017[,1:24], y= as.factor(training_set.2017$class), mtry = 4, ntree= 51, importance = TRUE, do.trace = TRUE)

set.seed(666)
classification3.2017 <- randomForest(x= training_set.2017[,1:24], y= as.factor(training_set.2017$class), mtry = 4, ntree= 51, importance = TRUE, do.trace = TRUE)

set.seed(1988)
classification4.2017 <- randomForest(x= training_set.2017[,1:24], y= as.factor(training_set.2017$class), mtry = 4, ntree= 51, importance = TRUE, do.trace = TRUE)

set.seed(1984)
classification5.2017 <- randomForest(x= training_set.2017[,1:24], y= as.factor(training_set.2017$class), mtry = 4, ntree= 51, importance = TRUE, do.trace = TRUE)

classifier.final.2017<-combine(classification1.2017, classification2.2017, classification3.2017, classification4.2017, classification5.2017)
pred.class.goldeneye.2017 <- predict(classifier.final.2017, test_set.2017[,1:24])
pred.class.ROC.2017 <- predict(classifier.final.2017, test_set.2017[,1:24], type="prob")
print(classifier.final.2017)

##variable importance
varImpPlot(classifier.final.2017, main="Attribute importance by classification RF on for 2017 dataset", scale = FALSE)

##Plot ROC curve and AUC for classification 2017 dataset
ROC.pred.2017 = prediction(pred.class.ROC.2017[,2], test_set.2017$class) 
ROC.perf.2017 = performance(ROC.pred.2017,"tpr","fpr")
AUC.class.2017 <- performance(ROC.pred.2017, "auc")
ACC.class.2017 <- performance(ROC.pred.2017, "acc")
AUC.class.2017@y.values[[1]]
plot(ROC.perf.2017,main="ROC Curve for 2017 dataset", col=2,lwd=2, cex.lab=1.5, cex.axis=1.5, cex.main=1.5, cex.sub=1.5)
text(0.3, 0.6, paste("AUC=",signif(AUC.class.2017@y.values[[1]],5), sep=" "), pos=3, cex=1.5) 
abline(a=0,b=1,lwd=2,lty=2,col="gray")

##Top 9 features Mean Decrease in Accuracy over the years

top.features.class <- data.frame(Features<- rep(c("F5", "F6", "F9", "F10", "F16", "F17", "F18", "F23", "F27"), each=4),
                               Year <- rep(c("2014", "2015", "2016", "2017"), 9),
                               MeanDecAcc <- c(classifier.final.2014$importance[5,3], classifier.final.2015$importance[5,3], classifier.final.2016$importance[5,3], classifier.final.2017$importance[5,3],
                                           classifier.final.2014$importance[6,3], classifier.final.2015$importance[6,3], classifier.final.2016$importance[6,3], classifier.final.2017$importance[6,3],
                                           classifier.final.2014$importance[9,3], classifier.final.2015$importance[9,3], classifier.final.2016$importance[9,3], classifier.final.2017$importance[9,3],
                                           classifier.final.2014$importance[10,3], classifier.final.2015$importance[10,3], classifier.final.2016$importance[10,3], classifier.final.2017$importance[10,3],
                                           classifier.final.2014$importance[15,3], classifier.final.2015$importance[15,3], classifier.final.2016$importance[15,3], classifier.final.2017$importance[15,3],
                                           classifier.final.2014$importance[16,3], classifier.final.2015$importance[16,3], classifier.final.2016$importance[16,3], classifier.final.2017$importance[16,3],
                                           classifier.final.2014$importance[17,3], classifier.final.2015$importance[17,3], classifier.final.2016$importance[17,3], classifier.final.2017$importance[17,3],
                                           classifier.final.2014$importance[22,3], classifier.final.2015$importance[22,3], classifier.final.2016$importance[22,3], classifier.final.2017$importance[22,3],
                                           classifier.final.2014$importance[24,3], classifier.final.2015$importance[24,3], classifier.final.2016$importance[24,3], classifier.final.2017$importance[24,3]),
                               
                               SD <-c(classifier.final.2014$importanceSD[5,3], classifier.final.2015$importanceSD[5,3], classifier.final.2016$importanceSD[5,3], classifier.final.2017$importanceSD[5,3],
                                      classifier.final.2014$importanceSD[6,3], classifier.final.2015$importanceSD[6,3], classifier.final.2016$importanceSD[6,3], classifier.final.2017$importanceSD[6,3],
                                      classifier.final.2014$importanceSD[9,3], classifier.final.2015$importanceSD[9,3], classifier.final.2016$importanceSD[9,3], classifier.final.2017$importanceSD[9,3],
                                      classifier.final.2014$importanceSD[10,3], classifier.final.2015$importanceSD[10,3], classifier.final.2016$importanceSD[10,3], classifier.final.2017$importanceSD[10,3],
                                      classifier.final.2014$importanceSD[15,3], classifier.final.2015$importanceSD[15,3], classifier.final.2016$importanceSD[15,3], classifier.final.2017$importanceSD[15,3],
                                      classifier.final.2014$importanceSD[16,3], classifier.final.2015$importanceSD[16,3], classifier.final.2016$importanceSD[16,3], classifier.final.2017$importanceSD[16,3],
                                      classifier.final.2014$importanceSD[17,3], classifier.final.2015$importanceSD[17,3], classifier.final.2016$importanceSD[17,3], classifier.final.2017$importanceSD[17,3],
                                      classifier.final.2014$importanceSD[22,3], classifier.final.2015$importanceSD[22,3], classifier.final.2016$importanceSD[22,3], classifier.final.2017$importanceSD[22,3],
                                      classifier.final.2014$importanceSD[24,3], classifier.final.2015$importanceSD[24,3], classifier.final.2016$importanceSD[24,3], classifier.final.2017$importanceSD[24,3]))

Features <- ordered(Features, levels = c("F5", "F6", "F9", "F10", "F16", "F17", "F18", "F23", "F27"))# required to give levels for the vector features F27>F23>...>F5 in order for the heatmap y-axis to be in right order
colnames(top.features.class) <- c("features","year", "MeanDecAcc", "SD") #tidy up the structure of the dataframe and give nice names to columns


#plot heatmap
heatmap.class <- ggplot(data = top.features.class, aes(x = Year, y = Features)) +
  geom_tile(aes(fill = MeanDecAcc)) +
  scale_fill_gradient(low = "white", high = "steelblue")+
  theme_bw(base_size = 14) + # Size of fonts and customization of theme down below
  theme(
    plot.margin = unit(c(0.2, 0, 0.2, 0.4), "cm"),
    #        axis.title.y=element_text(margin=margin(0,17,0,0)), # space between y label and scale
    panel.background= element_rect(fill = NA, colour = "black", size=1),  # frame around plot
    panel.border= element_rect(fill = NA, colour = "black", size=1),
    #strip.background =  element_rect(fill = "#D8D8D8", colour = "black", size=1), # frame around title
    strip.text = element_text(colour = "black"),
    #panel.grid.major.y = element_blank(), # remove lines on plot
    #panel.grid.minor.y = element_blank(),
    #panel.grid.major.x = element_blank(),
    #panel.grid.minor.x = element_blank(),
    legend.title = element_text(size=9.5),
    legend.position="right"
  )

#hclust(dist(top.features.class, method = "euclidean"), method = "ward.D")$order #Check DAMI course for cluseting methods other than Ward distance!

heatmap.class

#plot boxplot all years combined

box.reg <- ggplot(data=top.features.class, aes(x=Features, y=MeanDecAcc))+
  geom_boxplot()+
  geom_point()+
  theme_bw(base_size = 14) + # Size of fonts and customization of theme down below
  theme(
    plot.margin = unit(c(0.2, 0.2, 0.2, 0.4), "cm"),
    #        axis.title.y=element_text(margin=margin(0,17,0,0)), # space between y label and scale
    panel.background= element_rect(fill = NA, colour = "black", size=1),  # frame around plot
    panel.border= element_rect(fill = NA, colour = "black", size=1),
    #strip.background =  element_rect(fill = "#D8D8D8", colour = "black", size=1), # frame around title
    strip.text = element_text(colour = "black"),
    #panel.grid.major.y = element_blank(), # remove lines on plot
    #panel.grid.minor.y = element_blank(),
    #panel.grid.major.x = element_blank(),
    #panel.grid.minor.x = element_blank(),
    legend.title = element_text(size=14),
    legend.position="right"
  )

box.reg

###################################### FEATURE DEPENDENCIES STUDY WITH GOLDENEYE++ ######################################

## add PClass column (gives predictions to goldeneye++)
#2014
test_set.2014[,"PClass"]<- pred.class.goldeneye.2014 #Watch out for type of predict element... GoldenEye doesn't accept type="prob"

#2015
test_set.2015[,"PClass"]<- pred.class.goldeneye.2015

#2016
test_set.2016[,"PClass"]<- pred.class.goldeneye.2016

#2017
test_set.2017[,"PClass"]<- pred.class.goldeneye.2017

## Run GoldenEye algo for F24 label model

#on 2014 dataset
set.seed(1988)
test_set.2014.goldeneye <- subset(test_set.2014, select = -c(F24))
dep1 <- goldeneye(data = test_set.2014.goldeneye, real.class.name = "class", model= classifier.final.2014, classifier = randomForest, goodness.function = class_probability_correlation_randomforest)
str(dep1)

#on 2015 dataset
test_set.2015.goldeneye <- subset(test_set.2015, select = -c(F24))
dep2 <- goldeneye(data = test_set.2015.goldeneye, real.class.name = "class", model= classifier.final.2015, classifier = randomForest, goodness.function = class_probability_correlation_randomforest)
str(dep2)

#on 2016 dataset
test_set.2016.goldeneye <- subset(test_set.2016, select = -c(F24))
dep3 <- goldeneye(data = test_set.2016.goldeneye, real.class.name = "class", model= classifier.final.2016, classifier = randomForest, goodness.function = class_probability_correlation_randomforest)
str(dep3)

#on 2017 dataset
test_set.2017.goldeneye <- subset(test_set.2017, select = -c(F24))
dep4 <- goldeneye(data = test_set.2017.goldeneye, real.class.name = "class", model= classifier.final.2017, classifier = randomForest, goodness.function = class_probability_correlation_randomforest)
str(dep4)

#################################################### models with F16 as label ####################################################

#Put F16 in the first column of the dataframe
normal_df.2014 <- subset(normal_df.2014, select=c(F16,1:25))
normal_df.2014 <- subset(normal_df.2014, select = -c(F16.1))
normal_df.2015 <- subset(normal_df.2015, select=c(F16,1:25))
normal_df.2015 <- subset(normal_df.2015, select = -c(F16.1))
normal_df.2016 <- subset(normal_df.2016, select=c(F16,1:25))
normal_df.2016 <- subset(normal_df.2016, select = -c(F16.1))
normal_df.2017 <- subset(normal_df.2017, select=c(F16,1:25))
normal_df.2017 <- subset(normal_df.2017, select = -c(F16.1))

#create categories based on quantiles to transform the regression problem into a classification problem for 2014 for F16 label
normal_df.2014["classF16"]<- NA

for(k in 1:nrow(normal_df.2014)){
  if (normal_df.2014[k,1] <=90){
    normal_df.2014[k,26] <- "<=90%"
  }
  else if (normal_df.2014[k,1] >90){
    normal_df.2014[k,26] <- ">90%"
  }
  else{
    normal_df.2014[k,26] <- NA
  }
}

normal_df.2015["classF16"]<- NA

for(k in 1:nrow(normal_df.2015)){
  if (normal_df.2015[k,1] <=90){
    normal_df.2015[k,26] <- "<=90%"
  }
  else if (normal_df.2015[k,1] >90){
    normal_df.2015[k,26] <- ">90%"
  }
  else{
    normal_df.2015[k,26] <- NA
  }
}

normal_df.2016["classF16"]<- NA

for(k in 1:nrow(normal_df.2016)){
  if (normal_df.2016[k,1] <=90){
    normal_df.2016[k,26] <- "<=90%"
  }
  else if (normal_df.2016[k,1] >90){
    normal_df.2016[k,26] <- ">90%"
  }
  else{
    normal_df.2016[k,26] <- NA
  }
}

normal_df.2017["classF16"]<- NA

for(k in 1:nrow(normal_df.2017)){
  if (normal_df.2017[k,1] <=90){
    normal_df.2017[k,26] <- "<=90%"
  }
  else if (normal_df.2017[k,1] >90){
    normal_df.2017[k,26] <- ">90%"
  }
  else{
    normal_df.2017[k,26] <- NA
  }
}


#split between test set and training set

# Splitting the dataset into the Training set and Test set for 2014
# install.packages('caTools')
set.seed(123)
split <- sample.split(normal_df.2014$F24, SplitRatio = 0.75)
training_set.2014 <- subset(normal_df.2014, split == TRUE)
test_set.2014 <- subset(normal_df.2014, split == FALSE)

# Splitting the dataset into the Training set and Test set for 2015
# install.packages('caTools')
set.seed(123)
split <- sample.split(normal_df.2015$F24, SplitRatio = 0.75)
training_set.2015 <- subset(normal_df.2015, split == TRUE)
test_set.2015 <- subset(normal_df.2015, split == FALSE)

# Splitting the dataset into the Training set and Test set for 2016
# install.packages('caTools')
set.seed(123)
split <- sample.split(normal_df.2016$F24, SplitRatio = 0.75)
training_set.2016 <- subset(normal_df.2016, split == TRUE)
test_set.2016 <- subset(normal_df.2016, split == FALSE)

# Splitting the dataset into the Training set and Test set for 2017
# install.packages('caTools')
set.seed(123)
split <- sample.split(normal_df.2017$F24, SplitRatio = 0.75)
training_set.2017 <- subset(normal_df.2017, split == TRUE)
test_set.2017 <- subset(normal_df.2017, split == FALSE)

#random forest regression on data set for 2014 with F16 as independent variable

set.seed(101)
regression1.2014 <- randomForest(x= training_set.2014[,2:25], y= training_set.2014$F16, mtry = 4, ntree= 51, importance = TRUE)

set.seed(42)
regression2.2014 <- randomForest(x= training_set.2014[,2:25], y= training_set.2014$F16, mtry = 4, ntree= 51, importance = TRUE)

set.seed(666)
regression3.2014 <- randomForest(x= training_set.2014[,2:25], y= training_set.2014$F16, mtry = 4, ntree= 51, importance = TRUE)

set.seed(1988)
regression4.2014 <- randomForest(x= training_set.2014[,2:25], y= training_set.2014$F16, mtry = 4, ntree= 51, importance = TRUE)

set.seed(1984)
regression5.2014 <- randomForest(x= training_set.2014[,2:25], y= training_set.2014$F16, mtry = 4, ntree= 51, importance = TRUE)

reg.final.2014<-combine(regression1.2014, regression2.2014, regression3.2014, regression4.2014, regression5.2014)
pred.reg.2014 <- predict(reg.final.2014, test_set.2014[,2:25], type = "response") 
print(reg.final.2014)

##variable importance
varImpPlot(reg.final.2014, main="Attribute importance by classification RF for on for 2014 dataset (for F16 label)", scale = FALSE)

##Regression curve 2014 with F16 as independent variable
fit2014 <- lm(pred.reg.2014 ~ test_set.2014$F16)

ggplotRegression.2014 <- function (fit2014) {
  
  require(ggplot2)
  
  ggplot(fit2014$model, aes_string(x = names(fit2014$model)[2], y = names(fit2014$model)[1])) + 
    geom_point() +
    stat_smooth(method = "lm", col = "red") +
    labs(title = paste("Adj R2 = ",signif(summary(fit2014)$adj.r.squared, 4),
                       " P =",signif(summary(fit2014)$coef[2,4], 4),
                       " Slope =",signif(fit2014$coef[[2]], 4),
                       " RMSE =", signif(sqrt(mean((test_set.2014$F16-pred.reg.2014)^2)), 4)),
         x="test set satisfaction values",
         y= "predicted satisfaction values")+
    theme_bw(base_size = 13)
}

ggplotRegression.2014(lm( pred.reg.2014 ~ test_set.2014$F16))

#random forest regression on data set for 2015 with F16 as independent variable

set.seed(101)
regression1.2015 <- randomForest(x= training_set.2015[,2:25], y= training_set.2015$F16, mtry = 4, ntree= 51, importance = TRUE)

set.seed(42)
regression2.2015 <- randomForest(x= training_set.2015[,2:25], y= training_set.2015$F16, mtry = 4, ntree= 51, importance = TRUE)

set.seed(666)
regression3.2015 <- randomForest(x= training_set.2015[,2:25], y= training_set.2015$F16, mtry = 4, ntree= 51, importance = TRUE)

set.seed(1988)
regression4.2015 <- randomForest(x= training_set.2015[,2:25], y= training_set.2015$F16, mtry = 4, ntree= 51, importance = TRUE)

set.seed(1984)
regression5.2015 <- randomForest(x= training_set.2015[,2:25], y= training_set.2015$F16, mtry = 4, ntree= 51, importance = TRUE)

reg.final.2015<-combine(regression1.2015, regression2.2015, regression3.2015, regression4.2015, regression5.2015)
pred.reg.2015 <- predict(reg.final.2015, test_set.2015[,2:25], type = "response") 
print(reg.final.2015)

##variable importance
varImpPlot(reg.final.2015, main="Attribute importance by classification RF for on for 2015 dataset (for F16 label)", scale = FALSE)

##Regression curve 2015 with F16 as independent variable
fit2015 <- lm(pred.reg.2015 ~ test_set.2015$F16)

ggplotRegression.2015 <- function (fit2015) {
  
  require(ggplot2)
  
  ggplot(fit2015$model, aes_string(x = names(fit2015$model)[2], y = names(fit2015$model)[1])) + 
    geom_point() +
    stat_smooth(method = "lm", col = "red") +
    labs(title = paste("Adj R2 = ",signif(summary(fit2015)$adj.r.squared, 4),
                       " P =",signif(summary(fit2015)$coef[2,4], 4),
                       " Slope =",signif(fit2015$coef[[2]], 4),
                       " RMSE =", signif(sqrt(mean((test_set.2015$F16-pred.reg.2015)^2)), 4)),
         x="test set satisfaction values",
         y= "predicted satisfaction values")+
    theme_bw(base_size = 13)
}

ggplotRegression.2015(lm( pred.reg.2015 ~ test_set.2015$F16))


#random forest regression on data set for 2016 with F16 as independent variable

set.seed(101)
regression1.2016 <- randomForest(x= training_set.2016[,2:25], y= training_set.2016$F16, mtry = 4, ntree= 51, importance = TRUE)

set.seed(42)
regression2.2016 <- randomForest(x= training_set.2016[,2:25], y= training_set.2016$F16, mtry = 4, ntree= 51, importance = TRUE)

set.seed(666)
regression3.2016 <- randomForest(x= training_set.2016[,2:25], y= training_set.2016$F16, mtry = 4, ntree= 51, importance = TRUE)

set.seed(1988)
regression4.2016 <- randomForest(x= training_set.2016[,2:25], y= training_set.2016$F16, mtry = 4, ntree= 51, importance = TRUE)

set.seed(1984)
regression5.2016 <- randomForest(x= training_set.2016[,2:25], y= training_set.2016$F16, mtry = 4, ntree= 51, importance = TRUE)

reg.final.2016<-combine(regression1.2016, regression2.2016, regression3.2016, regression4.2016, regression5.2016)
pred.reg.2016 <- predict(reg.final.2016, test_set.2016[,2:25], type = "response") 
print(reg.final.2016)

##variable importance
varImpPlot(reg.final.2016, main="Attribute importance by classification RF for on for 2016 dataset (for F16 label)", scale = FALSE)

##Regression curve 2016 with F16 as independent variable
fit2015 <- lm(pred.reg.2016 ~ test_set.2016$F16)

ggplotRegression.2016 <- function (fit2016) {
  
  require(ggplot2)
  
  ggplot(fit2016$model, aes_string(x = names(fit2016$model)[2], y = names(fit2016$model)[1])) + 
    geom_point() +
    stat_smooth(method = "lm", col = "red") +
    labs(title = paste("Adj R2 = ",signif(summary(fit2016)$adj.r.squared, 4),
                       " P =",signif(summary(fit2016)$coef[2,4], 4),
                       " Slope =",signif(fit2016$coef[[2]], 4),
                       " RMSE =", signif(sqrt(mean((test_set.2016$F16-pred.reg.2016)^2)), 4)),
         x="test set satisfaction values",
         y= "predicted satisfaction values")+
    theme_bw(base_size = 13)
}

ggplotRegression.2016(lm( pred.reg.2016 ~ test_set.2016$F16))

#random forest regression on data set for 2017 with F16 as independent variable

set.seed(101)
regression1.2017 <- randomForest(x= training_set.2017[,2:25], y= training_set.2017$F16, mtry = 4, ntree= 51, importance = TRUE)

set.seed(42)
regression2.2017 <- randomForest(x= training_set.2017[,2:25], y= training_set.2017$F16, mtry = 4, ntree= 51, importance = TRUE)

set.seed(666)
regression3.2017 <- randomForest(x= training_set.2017[,2:25], y= training_set.2017$F16, mtry = 4, ntree= 51, importance = TRUE)

set.seed(1988)
regression4.2017 <- randomForest(x= training_set.2017[,2:25], y= training_set.2017$F16, mtry = 4, ntree= 51, importance = TRUE)

set.seed(1984)
regression5.2017 <- randomForest(x= training_set.2017[,2:25], y= training_set.2017$F16, mtry = 4, ntree= 51, importance = TRUE)

reg.final.2017<-combine(regression1.2017, regression2.2017, regression3.2017, regression4.2017, regression5.2017)
pred.reg.2017 <- predict(reg.final.2017, test_set.2017[,2:25], type="response")
print(reg.final.2017)

##variable importance
varImpPlot(reg.final.2017, main="Attribute importance by classification RF for on for 2017 dataset (for F16 label)", scale = FALSE)

##Regression curve 2017 with F16 as independent variable
fit2017 <- lm(pred.reg.2017 ~ test_set.2017$F16)

ggplotRegression.2017 <- function (fit2017) {
  
  require(ggplot2)
  
  ggplot(fit2017$model, aes_string(x = names(fit2017$model)[2], y = names(fit2017$model)[1])) + 
    geom_point() +
    stat_smooth(method = "lm", col = "red") +
    labs(title = paste("Adj R2 = ",signif(summary(fit2017)$adj.r.squared, 4),
                       " P =",signif(summary(fit2017)$coef[2,4], 4),
                       " Slope =",signif(fit2017$coef[[2]], 4),
                       " RMSE =", signif(sqrt(mean((test_set.2017$F16-pred.reg.2017)^2)), 4)),
         x="test set satisfaction values",
         y= "predicted satisfaction values")+
    theme_bw(base_size = 13)
}

ggplotRegression.2015(lm( pred.reg.2017 ~ test_set.2017$F16))


#random forest classification on data set for 2014 with F16 as independent variable

set.seed(101)
classification1.2014 <- randomForest(x= training_set.2014[,2:25], y= as.factor(training_set.2014$classF16), mtry = 4, ntree= 51, importance = TRUE)

set.seed(42)
classification2.2014 <- randomForest(x= training_set.2014[,2:25], y= as.factor(training_set.2014$classF16), mtry = 4, ntree= 51, importance = TRUE)

set.seed(666)
classification3.2014 <- randomForest(x= training_set.2014[,2:25], y= as.factor(training_set.2014$classF16), mtry = 4, ntree= 51, importance = TRUE)

set.seed(1988)
classification4.2014 <- randomForest(x= training_set.2014[,2:25], y= as.factor(training_set.2014$classF16), mtry = 4, ntree= 51, importance = TRUE)

set.seed(1984)
classification5.2014 <- randomForest(x= training_set.2014[,2:25], y= as.factor(training_set.2014$classF16), mtry = 4, ntree= 51, importance = TRUE)

classifier.final.2014<-combine(classification1.2014, classification2.2014, classification3.2014, classification4.2014, classification5.2014)
pred.class.goldeneye.2014 <- predict(classifier.final.2014, test_set.2014[,2:25]) #necessary to NOT have type="prob" for GoldenEye++ package
pred.class.ROC.2014 <- predict(classifier.final.2014, test_set.2014[,2:25], type="prob") #necessary to have type="prob" for AUC with ROCR package
print(classifier.final.2014)

##variable importance
varImpPlot(classifier.final.2014, main="Attribute importance by classification RF for on for 2014 dataset (for F16 label)", scale = FALSE)

##Plot ROC curve and AUC for classification 2014 dataset with F16 as independent variable
ROC.pred.2014 = prediction(pred.class.ROC.2014[,2], test_set.2014$classF16)
ROC.perf.2014 = performance(ROC.pred.2014,"tpr","fpr")
AUC.class.2014 <- performance(ROC.pred.2014, "auc")
ACC.class.2014 <- performance(ROC.pred.2014, "acc")
AUC.class.2014@y.values[[1]]
plot(ROC.perf.2014,main="ROC Curve for 2014 dataset", col=2,lwd=2, cex.lab=1.5, cex.axis=1.5, cex.main=1.5, cex.sub=1.5)
text(0.3, 0.6, paste("AUC=", signif(AUC.class.2014@y.values[[1]],5), sep = " "), pos=3, cex=1.5) 
abline(a=0,b=1,lwd=2,lty=2,col="gray")
 
#random forest classification on data set for 2015 with F16 as independent variable

set.seed(101)
classification1.2015 <- randomForest(x= training_set.2015[,2:25], y= as.factor(training_set.2015$classF16), mtry = 4, ntree= 51, importance = TRUE)

set.seed(42)
classification2.2015 <- randomForest(x= training_set.2015[,2:25], y= as.factor(training_set.2015$classF16), mtry = 4, ntree= 51, importance = TRUE)

set.seed(666)
classification3.2015 <- randomForest(x= training_set.2015[,2:25], y= as.factor(training_set.2015$classF16), mtry = 4, ntree= 51, importance = TRUE)

set.seed(1988)
classification4.2015 <- randomForest(x= training_set.2015[,2:25], y= as.factor(training_set.2015$classF16), mtry = 4, ntree= 51, importance = TRUE)

set.seed(1984)
classification5.2015 <- randomForest(x= training_set.2015[,2:25], y= as.factor(training_set.2015$classF16), mtry = 4, ntree= 51, importance = TRUE)

classifier.final.2015<-combine(classification1.2015, classification2.2015, classification3.2015, classification4.2015, classification5.2015)
pred.class.goldeneye.2015 <- predict(classifier.final.2015, test_set.2015[,2:25])
pred.class.ROC.2015 <- predict(classifier.final.2015, test_set.2015[,2:25], type="prob")
print(classifier.final.2015)

##variable importance
varImpPlot(classifier.final.2015, main="Attribute importance by classification RF on for 2015 dataset (for F16 label)", scale = FALSE)

##Plot ROC curve and AUC for classification 2015 dataset with F16 as independent variable
ROC.pred.2015 = prediction(pred.class.ROC.2015[,2], test_set.2015$classF16)
ROC.perf.2015 = performance(ROC.pred.2015,"tpr","fpr")
AUC.class.2015 <- performance(ROC.pred.2015, "auc")
ACC.class.2015 <- performance(ROC.pred.2015, "acc")
AUC.class.2015@y.values[[1]]
plot(ROC.perf.2015,main="ROC Curve for 2015 dataset", col=2,lwd=2, cex.lab=1.5, cex.axis=1.5, cex.main=1.5, cex.sub=1.5)
text(0.3, 0.6, paste("AUC=", signif(AUC.class.2015@y.values[[1]],5), sep = " "), pos=3, cex=1.5) 
abline(a=0,b=1,lwd=2,lty=2,col="gray")


#random forest classification on data set for 2016 with F16 as independent variable

set.seed(101)
classification1.2016 <- randomForest(x= training_set.2016[,2:25], y= as.factor(training_set.2016$classF16), mtry = 4, ntree= 51, importance = TRUE)

set.seed(42)
classification2.2016 <- randomForest(x= training_set.2016[,2:25], y= as.factor(training_set.2016$classF16), mtry = 4, ntree= 51, importance = TRUE)

set.seed(666)
classification3.2016 <- randomForest(x= training_set.2016[,2:25], y= as.factor(training_set.2016$classF16), mtry = 4, ntree= 51, importance = TRUE)

set.seed(1988)
classification4.2016 <- randomForest(x= training_set.2016[,2:25], y= as.factor(training_set.2016$classF16), mtry = 4, ntree= 51, importance = TRUE)

set.seed(1984)
classification5.2016 <- randomForest(x= training_set.2016[,2:25], y= as.factor(training_set.2016$classF16), mtry = 4, ntree= 51, importance = TRUE)

classifier.final.2016<-combine(classification1.2016, classification2.2016, classification3.2016, classification4.2016, classification5.2016)
pred.class.goldeneye.2016 <- predict(classifier.final.2016, test_set.2016[,2:25])
pred.class.ROC.2016 <- predict(classifier.final.2016, test_set.2016[,2:25], type="prob")
print(classifier.final.2016)

##variable importance
varImpPlot(classifier.final.2016, main="Attribute importance by classification RF on for 2016 dataset (for F16 label)", scale = FALSE)

##Plot ROC curve and AUC for classification 2016 dataset with F16 as independent variable
ROC.pred.2016 = prediction(pred.class.ROC.2016[,2], test_set.2016$classF16)
ROC.perf.2016 = performance(ROC.pred.2016,"tpr","fpr")
AUC.class.2016 <- performance(ROC.pred.2016, "auc")
ACC.class.2016 <- performance(ROC.pred.2016, "acc")
AUC.class.2016@y.values[[1]]
plot(ROC.perf.2016,main="ROC Curve for 2016 dataset", col=2,lwd=2, cex.lab=1.5, cex.axis=1.5, cex.main=1.5, cex.sub=1.5)
text(0.3, 0.6, paste("AUC=", signif(AUC.class.2016@y.values[[1]], 5), sep = " "), pos=3, cex=1.5) 
abline(a=0,b=1,lwd=2,lty=2,col="gray")

#random forest classification on data set with omitted NA for 2017 with F16 as independent variable

set.seed(101)
classification1.2017 <- randomForest(x= training_set.2017[,2:25], y= as.factor(training_set.2017$classF16), mtry = 4, ntree= 51, importance = TRUE)

set.seed(42)
classification2.2017 <- randomForest(x= training_set.2017[,2:25], y= as.factor(training_set.2017$classF16), mtry = 4, ntree= 51, importance = TRUE)

set.seed(666)
classification3.2017 <- randomForest(x= training_set.2017[,2:25], y= as.factor(training_set.2017$classF16), mtry = 4, ntree= 51, importance = TRUE)

set.seed(1988)
classification4.2017 <- randomForest(x= training_set.2017[,2:25], y= as.factor(training_set.2017$classF16), mtry = 4, ntree= 51, importance = TRUE)

set.seed(1984)
classification5.2017 <- randomForest(x= training_set.2017[,2:25], y= as.factor(training_set.2017$classF16), mtry = 4, ntree= 51, importance = TRUE)

classifier.final.2017<-combine(classification1.2017, classification2.2017, classification3.2017, classification4.2017, classification5.2017)
pred.class.goldeneye.2017 <- predict(classifier.final.2017, test_set.2017[,2:25])
pred.class.ROC.2017 <- predict(classifier.final.2017, test_set.2017[,2:25], type="prob")
print(classifier.final.2017)

##variable importance
varImpPlot(classifier.final.2017, main="Attribute importance by classification RF on for 2017 dataset (for F16 label)", scale = FALSE)

##Plot ROC curve and AUC for classification 2017 dataset with F16 as independent variable
ROC.pred.2017 = prediction(pred.class.ROC.2017[,2], test_set.2017$classF16) 
ROC.perf.2017 = performance(ROC.pred.2017,"tpr","fpr")
AUC.class.2017 <- performance(ROC.pred.2017, "auc")
ACC.class.2017 <- performance(ROC.pred.2017, "acc")
AUC.class.2017@y.values[[1]]
plot(ROC.perf.2017,main="ROC Curve for 2017 dataset", col=2,lwd=2, cex.lab=1.5, cex.axis=1.5, cex.main=1.5, cex.sub=1.5)
text(0.3, 0.6, paste("AUC=", signif(AUC.class.2017@y.values[[1]], 5), sep = " "), pos=3, cex=1.5) 
abline(a=0,b=1,lwd=2,lty=2,col="gray")


############################################ add PClass column (gives predictions to goldeneye++) ############################################ 
#2014
test_set.2014[,"PClass"]<- pred.class.goldeneye.2014 #Watch out for type of predict element... GoldenEye doesn't accept type="prob"

#2015
test_set.2015[,"PClass"]<- pred.class.goldeneye.2015

#2016
test_set.2016[,"PClass"]<- pred.class.goldeneye.2016

#2017
test_set.2017[,"PClass"]<- pred.class.goldeneye.2017


#on 2014 dataset
library(goldeneye)
dep <- goldeneye(data = test_set.2014[,2:27], real.class.name = "classF16", model= classifier.final.2014, classifier = randomForest, goodness.function = class_probability_correlation_randomforest)
str(dep)

#on 2015 dataset
library(goldeneye)
dep <- goldeneye(data = test_set.2015[,2:27], real.class.name = "classF16", model= classifier.final.2015, classifier = randomForest, goodness.function = class_probability_correlation_randomforest)
str(dep)

#on 2016 dataset
library(goldeneye)
dep <- goldeneye(data = test_set.2016[,2:27], real.class.name = "classF16", model= classifier.final.2016, classifier = randomForest, goodness.function = class_probability_correlation_randomforest)
str(dep)

#on 2017 dataset
library(goldeneye)
dep <- goldeneye(data = test_set.2017[,2:27], real.class.name = "classF16", model= classifier.final.2017, classifier = randomForest, goodness.function = class_probability_correlation_randomforest)
str(dep)

### CHECKING IMPORTANCE OF PARAMETERS USING BORUTA PCKG##
Boruta(F24~., data = normal_df.2014[,1:25], doTrace=2, getImp= getImpRfZ)->Bor2014
print(Bor2014)
plot(Bor2014, cex.axis= 0.3, cex.lab= 1)

Boruta(F24~., data = normal_df.2015[,1:25], doTrace=2, getImp= getImpRfZ)->Bor2015
print(Bor2015)
plot(Bor2015, cex.axis= 0.3, cex.lab= 1)

Boruta(F24~., data = normal_df.2016[,1:25], doTrace=2, getImp= getImpRfZ)->Bor2016
print(Bor2016)
plot(Bor2016, cex.axis= 0.3, cex.lab= 1)

Boruta(F24~., data = normal_df.2017[,1:25], doTrace=2, getImp= getImpRfZ)->Bor2017
print(Bor2017)
plot(Bor2017, cex.axis= 0.3, cex.lab= 1)
