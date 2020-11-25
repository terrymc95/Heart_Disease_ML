###### Load Packages ##########################################################


library(dplyr)
library(ggplot2)
library(raster)
library(factoextra)
library(ggpubr)
library(leaps)
library(car)
library(caret)
library(MASS)
library(boot)
library(PredPsych)
library(tree)
library(rpart)
library(randomForest)
library(gbm)


#####Custom Functions #########################################################
clamped <- function(x){ ## This clamps the upper and lower bounds of a 
                        ## variable to be the mean +/- 3 Sdev
  
  x <- as.numeric(x)
  clamp(x, lower = (mean(x, na.rm = T) - 3*sd(x, na.rm = T)),
        upper = (mean(x, na.rm = T) + 3*sd(x, na.rm = T)))
  
}

compare <- function(x1,x2 = NULL, title = NULL){
  p1 <- hist(as.numeric(x1))
  p2 <- hist(as.numeric(x2))
  plot(p1, col = "orange", main = paste("Comparison of ",
                                        title), xlab = title)
  plot(p2, col = "green", add = T)
}

log_Riot <- function(Model, data = df, prob =0.5, Print = FALSE ){ 
  ## A function to run the predict statment faster and allows the probability 
  ## cutoff to be changed for a range of values 
  
  target <- data[[16]]
  if(Print == TRUE){
      pred <- predict(Model, data, type = "response")
      
      pred.fit <- rep(0,nrow(data))
      pred.fit[pred > prob] <- 1
      x <-  table(pred.fit, target)
      y <- mean(pred.fit == target)
      print(x)
      print(y)

      
  }else{
      pred <- predict(Model, data, type = "response")
      
      pred.fit <- rep(0,nrow(data))
      pred.fit[pred > prob] <- 1
      x <-  table(pred.fit, target)
      y <- mean(pred.fit == target) 
       
    
    
   }

  
}

Top5 <- function(data){ ## Prints the top 5 accuracies for the models
  
  head(data[order(data$Accuracy, decreasing = T), c(1,2)],5)
  
}

log_MSE <- function(Model, data=df, prob = 0.5){
  
  target <- as.numeric(data[[16]])
  pred <- predict(Model, data, type = "response")
  pred.fit <- rep(0, nrow(data))
  pred.fit[pred > prob] <- 1
  mean((pred.fit - target)^2)
  
}

lda_Riot <- function(Model, data = df, prob = 0.5, Print =FALSE){
  data <- na.omit(data)
  Target <- data[[16]]
  
  if(Print == TRUE){
       
       pred <- predict(Model, data)
       predPos <- pred$posterior
       predProb <- as.factor(predPos[,1]<prob)
       levels(predProb)[1] <- 0
       levels(predProb)[2] <- 1
       
       x <- table(predProb, Target)
       y <- mean(predProb == Target)
       
       print(x)
       print(y)
  } else{
        pred <- predict(Model, data)
        predPos <- pred$posterior
        predProb <- as.factor(predPos[,1]<prob)
        levels(predProb)[1] <- 0
        levels(predProb)[2] <- 1
        
        x <- table(predProb, Target)
        y <- mean(predProb == Target)    
          
    
  }
  
  
}

lda_MSE <- function(Model, data = df, prob = 0.5){
  data <-  na.omit(data)
  target <- as.numeric(data[[16]])
  
  pred <- predict(Model, data)
  predPos <- pred$posterior
  predProb <- as.factor(predPos[,1] < prob)
  levels(predProb)[1] <- 0
  levels(predProb)[2] <- 1
  predProb <- as.numeric(predProb)
  
  mean((predProb - target)^2)
  
}

#### Load data ################################################################

df <- read.csv("HeartDisease.csv")
### 4238 Rows 16 Coloumns 


#### Summary Stats

summary(df)

str(df)

## need to correct data types 

ints <- c(1,3,6:9,16)
df[,ints] <- lapply(df[,ints], factor) ### all ints that represent nominal values changesd to factors

ints_2 <- c(2,4:5,10,14:15)
df[,ints_2] <- lapply(df[,ints_2], as.numeric) ## all ints that are continuous changed to numeric 


##### Checking for outliers ###################################################
names <- names(df)

for (i in 1:15){  ## This will plot histograms with and without the outliers, this allows them to be
                  ## removed faster and more efficient 
  
  compare(df[[i]], clamped(df[[i]]), names[i])
  
}


### Removing outliers using a clamp transformation

### Glucose, Heart Rate, sysBP, totChol, all seemed to have outliers

### Glucouse
df$glucose <- clamped(df$glucose)

## Heart Rate

df$heartRate <- clamped(df$heartRate)

##  sysBP

df$sysBP <- clamped(df$sysBP)

## totChol

df$totChol <- clamped(df$totChol)







##### Getting P-values 

p_val <- numeric(15)
test <- numeric(15)
AltHypo <- numeric(15)
i = 0

## Factor Data vs Factor data uses Chi-squared method for p-value
## Continuous vs Factor uses t-test for p-value

for (i in 1:15){
  x <- is.factor(df[[i]])
  if (x == TRUE) {
    y <- chisq.test(df[[i]], df[[16]],simulate.p.value = TRUE)
    p_val[i] <- y$p.value
    test[i] <- "Chi-squared"
    if(y$p.value < 0.05){
      AltHypo[i] = "Alternative Hypothesis"
      }else{
        AltHypo[i] = "Null Hypothesis"}
  }else{
    y <-  t.test(df[[i]] ~ df[[16]])
    p_val[i] <- y$p.value
    test[i] <- "T-test"
    if(y$p.value < 0.05){
      AltHypo[i] = "Alternative Hypothesis"
    }else{
      AltHypo[i] = "Null Hypothesis"}
  }
}

p_val_tab <- data.frame(Variable = names[1:15],'Testing Method' =  test, 'P-Value' = p_val, Hypothesis = AltHypo)
p_val_tab


## Chi-squared is approximating p-values because quite small best to use a logistic regression approach to
## get all p-values 


for( i in 1:15){
  
  form <- df[[16]]~df[[i]]
  logreg <- glm(form, family = "binomial")
  y<- coef(summary(logreg))[2,4]
  p_val[i] <- y
  if (y < 0.05){
    
    AltHypo[i] <- "Alternative Hypothesis"
    
  }else{
    AltHypo[i] <- "Null Hypothesis"
  }
  
  
}

p_val_tab_loreg <- cbind(Variable = names[1:15], 'P-Value' = p_val, Hypothesis = AltHypo)
p_val_tab_loreg


## From Both Methods of finding P-values; Current Smoker and Heart Rate are not related to having
## CHD in 10 years

##### Building Predictive Models ##############################################
## Going to use Logistic Regression; LDA; and Tree Methods to predict CHD

##### Logistic Regression #####################################################

All.Form <- TenYearCHD ~ . - currentSmoker - heartRate

Log_1 <- glm(All.Form, data =df, family = "binomial")
summary(Log_1)


exp(Log_1$coefficients) ## Most meaningful Variables 

## Using the model to predict 
Accurat <- numeric(101)
for(i in 1:101){ ## This gives the accuracy for 101 different probability cutoffs
  x <- (i-1)/100
  Accurat[i] <- log_Riot(Log_1,prob = x)
  }
  
accTab <- data.frame(Probability = c(0:100)/100, Accuracy = Accurat)
accTab
Top5(accTab)

## Gives Highest Accuracy of 85.48844% at 0.49 probability

log_MSE(Log_1, prob = 0.49) ## 1.41647

## Need to refine the model with variables removed and testing and training data

## From summary only male, age, cigsPerDay, sysBP, glucouse gave were significant in prediciting

### Checking the VIF to see multicolinnearity

vif(Log_1)
## No issues 

## Use Stepwise selection to get the best model

step <- regsubsets(TenYearCHD ~ ., data =df, nvmax = 15, method = "seqrep")
summary(step)
## Accepting Variables with 8 * or more
## Model should include male, age, cigsPerDay, sysBP, diabetes, PrevelantStroke BPMeds

Form = TenYearCHD ~ male + age + cigsPerDay + sysBP + diabetes + prevalentStroke + BPMeds
Log_2 <- glm(Form, data = df, family = "binomial")
summary(Log_2)

exp(Log_2$coefficients) ## Most Accurate 

for(i in 1:101){ ## This gives the accuracy for 101 different probability cutoffs
  x <- (i-1)/100
  Accurat[i] <- log_Riot(Log_2,prob = x)
}

accTab2 <- data.frame(Probability = c(0:100)/100, Accuracy = Accurat)
accTab2
Top5(accTab2) ## Less accurate (0.51, 85.15809%)
log_Riot(Log_2, prob = 0.51, Print = T); log_MSE(Log_2, prob = 0.51) ## 1.421189


step2 <- regsubsets(Form, data = df, method = "seqrep")
summary(step2)
 ## remove stoke an BP meds


Form2 <-  TenYearCHD ~ male + age + cigsPerDay + sysBP + diabetes


Log_3 <- glm(Form2, data = df, family = "binomial")
summary(Log_3)

exp(Log_3$coefficients) ## Most Accurate 

for(i in 1:101){ ## This gives the accuracy for 101 different probability cutoffs
  x <- (i-1)/100
  Accurat[i] <- log_Riot(Log_3,prob = x)
}

accTab3 <- data.frame(Probability = c(0:100)/100, Accuracy = Accurat)
accTab3
Top5(accTab3) ## Less accurate again (0.48, 85.1345%)
log_MSE(Log_3, prob = .48) ## 1.418594

##### Creating a training and testing datasets ################################

df$ID <- c(1:nrow(df))
set.seed(40126429)

TrainSamp <- sample(df$ID, nrow(df)*.75)

train_df <- df %>%  ## Model Training Dataframe
              filter(df$ID %in% TrainSamp)
test_df <- df %>%   ## Model Testing Dataframe
              filter(!df$ID %in% TrainSamp)


##### Logregression with training and testing data ############################

## Use first Log regression model Formula 
All.Form_2 <- TenYearCHD ~ . - currentSmoker - heartRate - ID
TrainLog_1 <- glm(All.Form_2, data= train_df, family = "binomial")

exp(TrainLog_1$coefficients)
i <- 0
New_Acc <- numeric(101)
for (i in 1:101) {
  x <- (i-1)/100
  New_Acc[i] <- log_Riot(TrainLog_1, data = test_df, prob = x)
  
}

accTabTest <- data.frame(Probability = c(0:100)/100, Accuracy = New_Acc)
accTabTest   
Top5(accTabTest)  ### Highest accuracy at 0.42 probability with 83.11321%
log_MSE(TrainLog_1, data = test_df, prob = 0.42) ## 1.468868 MSE

## Second log regression model fomula

TrainLog_2 <-  glm(Form, data= train_df, family = "binomial")
exp(TrainLog_2$coefficients)

for(i in 1:101){
  x <- (i-1)/100
  New_Acc[i] <- log_Riot(TrainLog_2, data = test_df, prob = x)
  
  
}  
accTabTest2 <- data.frame(Probability = c(0:100)/100, Accuracy = New_Acc)
accTabTest2   
Top5(accTabTest2)  ### Higest Accuracy at 0.42 probability cutoff with 83.67925%
                   ### slightly better than before 
log_Riot(TrainLog_2, data = test_df, prob = 0.42, Print = T)  ;log_MSE(TrainLog_2, data = test_df, prob = 0.42) ## 1.463208


## Third log ression model fromula 

TrainLog_3 <-  glm(Form2, data= train_df, family = "binomial")
exp(TrainLog_3$coefficients)

for(i in 1:101){
  x <- (i-1)/100
  New_Acc[i] <- log_Riot(TrainLog_3, data = test_df, prob = x)
  
  
}  
accTabTest3 <- data.frame(Probability = c(0:100)/100, Accuracy = New_Acc)
accTabTest3   
Top5(accTabTest3) ## Higest Accuracy at 0.42 probability cutoff with 83.58491%
log_MSE(TrainLog_3, data= test_df, prob = .42) ## 1.469811

## Formula 2 is the best for predicting 

##### Need to use cross validation to  better the testing accuracy ############

## Need to set up Train Control first 
set.seed(40126429)
Train_con <- trainControl(method = "cv", number = 10)

cv.log1 <- train(Form2, data = na.omit(df), trControl = Train_con, method = "glm",
                 family = binomial())

print(cv.log1)  ## 85.03838% accurate ## Most accurate model


##### LDA approach ############################################################
dfomit <- na.omit(df)
LDA_1 <- lda(TenYearCHD ~ . -ID, data = dfomit)
LDA_1
plot(LDA_1)

lda_Riot(LDA_1,Print = T)

ldaAcc <- numeric(101)

for ( i in 1:101){
  x <- (i-1)/100
  ldaAcc[i] <- lda_Riot(LDA_1, prob = x)
  
}
accTablda <- data.frame(Probability = c(0:100)/100, Accuracy = ldaAcc)
accTablda
Top5(accTablda) # Higest accuracy at 0.38 probability cutoff with 85.28446%
lda_Riot(LDA_1, prob = 0.38, Print = T); lda_MSE(LDA_1, prob = 0.38) ## 0.1471554

LDA_2 <- lda(TenYearCHD ~ . -ID, data = train_df)


for( i in 1:101){
  
  x <- (i-1)/100
  ldaAcc[i] <- lda_Riot(LDA_2, data = test_df, prob = x)
  
}

accTablda2 <- data.frame(Probability = c(0:100)/100, Accuracy = ldaAcc)
accTablda2
Top5(accTablda2) # Higest Accuracy at 0.56 probability cutoff with accuracy 82.988%
lda_Riot(LDA_2, data = test_df, prob = 0.56, Print = T); lda_MSE(LDA_2, data = test_df, prob = 0.56) ## 0.12301


## Trying a the Formula from logistic regression that was most accurate 

LDA_3 <- lda(Form, data = df)

for( i in 1:101){
  
  x <- (i-1)/ 100
  ldaAcc[i] <- lda_Riot(LDA_3, prob = x)
  
}
accTablda3 <- data.frame(Probability = c(0:100)/100, Accuracy = ldaAcc)
accTablda3
Top5(accTablda3)  ## Highest Accuracy at prob cutoff 0.32 accuracy 85.06565%
lda_MSE(LDA_3, prob = 0.32) ## 0.1493435

## Now trying with the test/train method 

LDA_4 <- lda(Form, data = train_df)

for( i in 1:101){
  
  
  x <- (i-1)/100
  ldaAcc[i] <- lda_Riot(LDA_4, data = test_df, prob = x)
  
}
accTablda4 <- data.frame(Probability = c(0:100)/100, Accuracy = ldaAcc)
accTablda4
Top5(accTablda4)  ## Higest Accuracy at 0.51 probability cutoff with 83.09706% accuracy 
lda_Riot(LDA_4, data = test_df, prob = 0.51, Print =T); lda_MSE(LDA_4, data = test_df, prob = 0.51) ## 0.1690294


## The predictions are not good for the train/test split 

## Trying k-folds Cross validation on LDA


## Have to covert all to numeric for this to work


numdf <- dfomit
numdf[,ints] <- lapply(numdf[,ints], as.numeric)

x <- LinearDA(Data= numdf, classCol = 16, selectedCols = c(1,2,5,11,9,7,6,16),
              cvType = "folds", SetSeed = T,
              nTrainFolds = 100, ntrainTestFolds = 71)
x ## 88.46154% accurate  

## Most accurate approach so far 


##### Deteriming what is the key factors in CHD ###############################

### Using Bagging Random Forest

Bagging_classifier <- randomForest(TenYearCHD ~ . - ID, data = na.omit(df),
                              ntree = 1000,mtry = 15, importance = T)
Bagging_classifier
importance(Bagging_classifier)
varImpPlot(Bagging_classifier)



### Gradient Boosted machine
set.seed(40126429)
train_df$TenYearCHD <- as.character(train_df$TenYearCHD) ## this allows gbm to work 
test_df$TenYearCHD<- as.numeric(test_df$TenYearCHD)

boost <- gbm(TenYearCHD ~. -ID, data = train_df,distribution  = "bernoulli",
             n.trees = 5000, interaction.depth = 5)
summary(boost)



##### Clustering data of pepole with CHD ######################################

set.seed(40126429)  ## Student Number

df_chd <- df %>% 
              filter(TenYearCHD != 0)

df_chd <- df_chd[-16] ## Dropping CHD ssince they all have it 
df_chd <- df_chd[-16] ## Drops ID
 ## Need to omit in missing values ffor clustering 

df_chd <- na.omit(df_chd)
 
## Finding how many clusters to use

fviz_nbclust(df_chd, kmeans, method = "wss", k.max = 15) +
  geom_vline(xintercept = 7, linetype = 2)


kmeans_CHD <- kmeans(df_chd, 7, nstart = 50)
kmeans_CHD

plot(df_chd, col = (kmeans_CHD$cluster+1))


