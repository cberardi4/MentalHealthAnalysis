set.seed(1861)

MHDF <- read.csv("survey.csv")

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
                            ifelse(MHDF$leave == "Very difficult",3,
                                   ifelse(MHDF$leave == "Somewhat difficult",4,5))))

MHDF$Country <- ifelse(MHDF$Country == "United States", 1, 
                       ifelse(MHDF$Country == "United Kingdom", 2, 3))
MHDF$self_employed <- ifelse(MHDF$self_employed == "No", 1, 0)
MHDF$family_history <- ifelse(MHDF$family_history == "No", 1, 0)
MHDF$treatment <- ifelse(MHDF$treatment == "No", 1, 0)
MHDF$work_interfere <- ifelse(is.na(MHDF$work_interfere), 1, 
                              ifelse(MHDF$work_interfere == "Often", 4, 
                                     ifelse(MHDF$work_interfere == "Rarely", 2, 
                                            ifelse(MHDF$work_interfere == "Sometimes", 3, 1))))
MHDF$no_employees <- ifelse(MHDF$no_employees == "1-5", 1, 
                            ifelse(MHDF$no_employees == "6-25", 2, 
                                   ifelse(MHDF$no_employees == "26-100", 3, 
                                          ifelse(MHDF$no_employees == "100-500", 4,
                                                 ifelse(MHDF$no_employees == "500-1000", 5, 6)))))
MHDF$remote_work <- ifelse(MHDF$remote_work == "No", 0, 1)


cor(MHDF[sapply(MHDF, function(x) !is.factor(x))])
summary(MHDF)

# Correlation
cor(MHDF$seek_help, MHDF$wellness_program)

# Matrix of 2 variables
counts1<- table(MHDF$seek_help, MHDF$wellness_program)
counts1
barplot(counts1, main="Do employees seek mental health help based on their wellness program?",
        xlab="Wellness program", col=c("red","yellow","blue"), ylab="Seek Help",
        legend("topright", legend=c("Seeking help","Not seeking help","Don't know")),
        names.arg=c("Yes", "No", "Don't Know"))

# Correlation
cor(MHDF$family_history, MHDF$treatment)

# Matrix of 2 variables
counts2 <- table(MHDF$family_history, MHDF$treatment, dnn = c("Family History", "Treatment"))
counts2
barplot(counts2, main="Mental Health Issues Based on Family History",
        xlab="Treatment", col=c("red","yellow"),
        legend = c("Family History", "No Family History"),
        names.arg = c("Not Sought Help", "Sought Help"))

# Correlation
cor(MHDF$care_options,MHDF$benefits)
counts3<- table(MHDF$care_options, MHDF$benefit)
counts3
barplot(counts3, main="Are employees who have mental health benefits aware of them?",
        ylab="Care options", col=c("red","yellow","blue"), xlab="Benefits",
        legend=c("Yes","No","Don't know"),
        names.arg=c("Yes","No","Not sure"))

# Correlation
cor(MHDF$seek_help,MHDF$benefits)

# Matrix of 2 variables
counts4<- table(MHDF$seek_help, MHDF$benefits)
counts4
barplot(counts4, main="Do employers provide mental health benefits or resources to seek help?",
        xlab="Benefits", col=c("red","yellow","blue"), ylab="Helpful Resources",
        legend=c("Yes","No","Don't know"),
        names.arg=c("Yes", "No", "Don't know"))

# Correlation
cor(MHDF$mental_vs_physical, MHDF$anonymity)

# Matrix of 2 variables
counts5<- table(MHDF$mental_vs_physical, MHDF$anonymity)
counts5
barplot(counts5, main="Do employers take mental health as seriously as physical and are they anonymous?",
        xlab="Mental Health Importance", col=c("red","yellow","blue"), ylab="Anonymity",
        legend=c("Yes","No","Don't know"),
        names.arg=c("Yes", "No", "Don't know"))
# Correlation
cor(MHDF$mental_vs_physical, MHDF$leave)

# Matrix of 2 variables
counts6 <- table(MHDF$mental_vs_physical, MHDF$leave)
counts6
barplot(counts6, main = "Mental Health Leave Based on Companies' Mental Health Attitudes", 
        ylab = "Employers Take Mental Health As Seriously As Physical Health",
        xlab = "How Easy to Take Mental Health Leave",
        names.arg = c("Very Easy", "Somewhat Easy", "Very Difficult", "Somewhat Difficult", "Don't Know"),
        legend = c("Yes", "No", "Don't Know"),
        col=c("red","yellow", "blue"),
        beside = TRUE)

trainSize <- 0.75
trainInd <- sample(1:nrow(MHDF), size = floor(nrow(MHDF) * trainSize))
MHDFTrain <- MHDF[trainInd, ]
MHDFTest <- MHDF[-trainInd, ]


library(ISLR)

# linear model
treatLM <- lm(treatment ~., data = MHDF)

predsTrain <- predict(treatLM, newdata = MHDFTrain)
predsTest <- predict(treatLM, newdata = MHDFTest)
plot(treatLM)

MSE <- function(ytrue, ypreds){
  return(mean((ytrue - ypreds)^2))}
MSEtrain <- MSE(MHDFTrain$treatment, predsTrain)
# 0.1857
MSEtest <- MSE(MHDFTest$treatment, predsTest)
# 0.2091

# tree
library(rpart)
treeFit <- rpart(treatment ~., data = MHDFTrain)
summary(treeFit)
plot(treeFit); text(treeFit,pretty=0)

predsTrainTree <- predict(treeFit, newdata = MHDFTrain)
predsTestTree <- predict(treeFit, newdata = MHDFTest)
predsTestTree

MSEtrainTree <- MSE(MHDFTrain$treatment, predsTrainTree)
# 0.1184
MSEtestTree <- MSE(MHDFTest$treatment, predsTestTree)
# 0.1421

# random forest
library(randomForest)

rf.health = randomForest(treatment~.,data=MHDF,subset=trainInd,mtry=3,ntree=500)
summary(rf.health)

predsTrainRF <- predict(rf.health, newdata = MHDFTrain)
predsTestRF <- predict(rf.health, newdata = MHDFTest)
MSEtrainRF <- MSE(MHDFTrain$treatment, predsTrainRF)
# 0.0450
MSEtestRF <- MSE(MHDFTest$treatment, predsTestRF)
# 0.1506


#linear model
obsLM <- lm(obs_consequence ~., data = MHDF)

#Decision tree

#2d. Plot the fitted tree using the plot() function. Use text() to add text to the object.
install.packages("rpart")
library("rpart")
obstree<-rpart(obs_consequence~., data=MHDFTrain)
par(mar=c(0,0,0,0))
plot(obstree); text(obstree,pretty=0,use.n=TRUE)

#2f. Use the predict function to get predicted values for data in the training and validation set. Calculate MSE for both sets.
predtrain<-predict(obstree,MHDFTrain)
predval<-predict(obstree,MHDFTest)

MSE(predtrain, MHDFTrain$obs_consequence)

MSE(predval, MHDFTest$obs_consequence)



#2g. Estimate a random forest model using the randomForest function in the package of the same name.Use mtry = 3 as a parameter.

fit=randomForest(obs_consequence~., data=MHDFTrain,mtry=3)

predtrain1<-predict(fit,MHDFTrain)
predval2<-predict(fit,MHDFTest)

MSE(predtrain1, MHDFTrain$obs_consequence)

MSE(predval2, MHDFTest$obs_consequence)

# Regularization
install.packages("useful")
library(useful)
library(glmnet)
install.packages("glmnet")

formula <- as.formula(treatment~.)

Xvars <- build.x(formula = formula, data = MHDFTrain, contrasts = TRUE)
Yvar <- build.y(formula = formula, data = MHDFTrain)

# cv.glmnet() 
LassoFit <- cv.glmnet(x = Xvars, y = Yvar, 
                      alpha = 1)
LassoFit
coef(LassoFit, s = "lambda.min")
plot(LassoFit)

