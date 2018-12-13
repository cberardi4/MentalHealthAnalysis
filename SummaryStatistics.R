# read in dataset
setwd("/Users/jennprosinski/Downloads/")
MHDF <- read.csv("survey.csv")

# set factors to binary values or levels
MHDF$Gender <- ifelse(MHDF$Gender == "F", 0, 1)
MHDF$mental_health_consequence <- ifelse(MHDF$mental_health_consequence == "Yes", 2, 
                                         ifelse(MHDF$mental_health_consequence == "No", 0, 1))
MHDF$phys_health_consequence <- ifelse(MHDF$phys_health_consequence == "Yes", 2, 
                                       ifelse(MHDF$phys_health_consequence == "No", 0, 1))
MHDF$coworkers <- ifelse(MHDF$coworkers == "Yes", 2, ifelse(MHDF$coworkers == "No", 0, 2))
MHDF$supervisor <- ifelse(MHDF$supervisor == "Yes", 2, ifelse(MHDF$supervisor == "No", 0, 1))
MHDF$mental_health_interview <- ifelse(MHDF$mental_health_interview == "Yes", 2,
                                       ifelse(MHDF$mental_health_interview == "No", 0, 1))
MHDF$phys_health_interview <- ifelse(MHDF$phys_health_interview == "Yes", 2, 
                                     ifelse(MHDF$phys_health_interview == "No", 0, 2))
MHDF$mental_vs_physical <- ifelse(MHDF$mental_vs_physical == "Yes", 2, 
                                  ifelse(MHDF$mental_vs_physical == "No", 0, 1))
MHDF$obs_consequence <- ifelse(MHDF$obs_consequence == "No", 0, 1)
MHDF$tech_company <-ifelse(MHDF$tech_company == "Yes", 1, 0)
MHDF$benefits <-ifelse(MHDF$benefits == "Yes", 2, 
                       ifelse(MHDF$benefits == "No", 0,1))
MHDF$care_options <-ifelse(MHDF$care_options == "Yes", 2, 
                           ifelse(MHDF$care_options == "No", 0,1))
MHDF$wellness_program <-ifelse(MHDF$wellness_program == "Yes", 2, 
                               ifelse(MHDF$wellness_program == "No", 0,1))
MHDF$seek_help <-ifelse(MHDF$seek_help == "Yes", 2, 
                        ifelse(MHDF$seek_help == "No", 0,1))
MHDF$anonymity <-ifelse(MHDF$anonymity == "Yes", 2, 
                        ifelse(MHDF$anonymity == "No", 0,1))
MHDF$leave <- ifelse(MHDF$leave == "Very easy", 1,
                     ifelse(MHDF$leave == "Somewhat easy", 2,
                            ifelse(MHDF$leave == "Don't Know",3,
                              ifelse(MHDF$leave == "Somewhat difficult",4,5))))
MHDF$self_employed <- ifelse(MHDF$self_employed == "No", 1, 0)
MHDF$family_history <- ifelse(MHDF$family_history == "No", 1, 0)
MHDF$treatment <- ifelse(MHDF$treatment == "No", 1, 0)
MHDF$work_interfere <- ifelse(is.na(MHDF$work_interfere), 0, 
                              ifelse(MHDF$work_interfere == "Often", 1, 
                                     ifelse(MHDF$work_interfere == "Rarely", 0, 
                                            ifelse(MHDF$work_interfere == "Sometimes", 1, 0))))
levels(MHDF$no_employees)
MHDF$remote_work <- ifelse(MHDF$remote_work == "No", 0, 1)

# --------------------------
# EXPLORATORY DATA ANALYSIS
# --------------------------

# find correlated values in dataaset
cor(MHDF[sapply(MHDF, function(x) !is.factor(x))])
summary(MHDF)

# -------
# PLOTS
# -------                
library(ggplot2)   
ggplot(MHDF, aes(x=seek_help)) + geom_bar() + labs(title="Frequency")  # Y axis derived from counts of X item

# barplot: wellness_program
counts<- table(MHDF$wellness_program)
barplot(counts, main="Do Companies Have Mental Wellness Programs?",
        xlab=("Wellness program"), names.arg = c("Yes", "No", "Don't Know"), col="blue")

# barplot: mental_vs_physical vs. anonymity
counts4<- table(MHDF$mental_vs_physical, MHDF$anonymity)
counts4
barplot(counts4, main="Do employer's Take Mental Health as Seriously as Physical and Are They Anonymous?",
        xlab="Mental Health Importance", col=c("black","orange","red"), ylab="Anonymity",
        legend=c("Yes","No","Don't know"),
        names.arg=c("Yes", "No", "Don't know"))

library(ISLR)

# -------
# MODELS
# -------

# -------------
# Linear Models
# -------------
                
# linear model: treatment
treatLM <- lm(treatment ~., data = MHDF)
summary(treatLM)

# linear model: seek_help
# model from presentation
helpLM <- lm(seek_help ~., data = MHDF)
summary(helpLM)

# linear model: benefits
familyLM <- lm(benefits ~., data = MHDF)
summary(familyLM)
  
# linear model: work_interfere
lm <- lm(MHDF$work_interfere~., data = MHDF)
summary(lm)

# create training and test data from MHDF dataset
trainsize <- 0.75
trainInd <- sample(1:nrow(MHDF), size = floor(nrow(MHDF) * trainsize))
trainDF <- MHDF[trainInd, ]
testDF <- MHDF[-trainInd, ]

# predictions based on treatLM model
treatLM$coefficients
predsTrain <- predict(treatLM, newdata = trainDF)
predsTest <- predict(treatLM, newdata = testDF)
plot(treatLM)

# calculates the MSE
MSE <- function(ytrue, ypreds){
  return(mean((ytrue - ypreds)^2))}

# MSE Values for model
# MSE: training data
MSEtrain <- MSE(trainDF$treatment, predsTrain)

# MSE: testing data
MSEtest <- MSE(testDF$treatment, predsTest)
              
# ------------
# TREE MODELS
# ------------

# tree model: seek_helo
library(rpart)
treeFit <- rpart(seek_help ~., data = trainDF)
summary(treeFit)
plot(treeFit); text(treeFit,pretty=0)

# predictions based on training and testing data

# training data
predsTrainTree <- predict(treeFit, newdata = trainDF)
# testing data
predsTestTree <- predict(treeFit, newdata = testDF)

 # training data               
MSEtrainTree <- MSE(trainDF$treatment, predsTrainTree)
# testing data
MSEtestTree <- MSE(testDF$treatment, predsTestTree)

# --------------
# LOGISTIC MODEL
# --------------

# logistic model: work_interfere
logitfit<- glm(MHDF$work_interfere~., data = MHDF)
summary(logitfit)

# ----------------------------
# FORWARD STEPWISE REGRESSION               
# ----------------------------

#stepwise regression: seek_help
# using leaps library
library(leaps)
reg.forward <- regsubsets(seek_help~., data = MHDF, method = c("forward"))
summary(reg.forward)
# new model with variables selected in stepwise regression
updated_model <- lm(seek_help ~ no_employees + benefits + care_options +wellness_program+
                      leave+mental_vs_physical, data = MHDF)
summary(updated_model)

# ---------------------
# LASSO REGULARIZATION
# ---------------------

# using useful and glmnet libraries
library(useful)
library(glmnet)

formula <- as.formula(seek_help~.)

Xvars <- build.x(formula = formula, data = trainDF, contrasts = TRUE)
Yvar <- build.y(formula = formula, data = trainDF)

# lasso: seek_help
# based on 1se model
LassoFit <- cv.glmnet(x = Xvars, y = Yvar, 
                      alpha = 1)
LassoFit
coef(LassoFit, s = "lambda.1se")
plot(LassoFit)

