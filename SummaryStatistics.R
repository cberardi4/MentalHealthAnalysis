MHDF <- read.csv("survey.csv")

MHDF$Gender <- ifelse(MHDF$Gender == "F", 0, 1)
MHDF$mental_health_consequence <- ifelse(MHDF$mental_health_consequence == "Yes", 1, 
                                         ifelse(MHDF$mental_health_consequence == "No", 2, 3))
MHDF$phys_health_consequence <- ifelse(MHDF$phys_health_consequence == "Yes", 1, 
                                       ifelse(MHDF$phys_health_consequence == "No", 2, 3))
MHDF$coworkers <- ifelse(MHDF$coworkers == "Yes", 1, ifelse(MHDF$coworkers == "No", 2, 3))
MHDF$supervisor <- ifelse(MHDF$supervisor == "Yes", 1, ifelse(MHDF$supervisor == "No", 2, 3))
MHDF$mental_health_interview <- ifelse(MHDF$mental_health_interview == "Yes", 1,
                                       ifelse(MHDF$mental_health_interview == "No", 2, 3))
MHDF$phys_health_interview <- ifelse(MHDF$phys_health_interview == "Yes", 1, 
                                     ifelse(MHDF$phys_health_interview == "No", 2, 3))
MHDF$mental_vs_physical <- ifelse(MHDF$mental_vs_physical == "Yes", 1, 
                                  ifelse(MHDF$mental_vs_physical == "No", 2, 3))
MHDF$obs_consequence <- ifelse(MHDF$obs_consequence == "No", 0, 1)
MHDF$tech_company <-ifelse(MHDF$tech_company == "Yes", 1, 0)
MHDF$benefits <-ifelse(MHDF$benefits == "Yes", 1, 
                       ifelse(MHDF$benefits == "No", 2,3))
MHDF$care_options <-ifelse(MHDF$care_options == "Yes", 1, 
                           ifelse(MHDF$care_options == "No", 2,3))
MHDF$wellness_program <-ifelse(MHDF$wellness_program == "Yes", 1, 
                               ifelse(MHDF$wellness_program == "No", 2,3))
MHDF$seek_help <-ifelse(MHDF$seek_help == "Yes", 1, 
                        ifelse(MHDF$seek_help == "No", 2,3))
MHDF$anonymity <-ifelse(MHDF$anonymity == "Yes", 1, 
                        ifelse(MHDF$anonymity == "No", 2,3))
MHDF$leave <- ifelse(MHDF$leave == "Very easy", 1,
              ifelse(MHDF$leave == "Somewhat easy", 2,
              ifelse(MHDF$leave == "Very difficult",3,
              ifelse(MHDF$leave == "Somewhat difficult",4,5))))

MHDF$Country <- ifelse(MHDF$Country == "United States", 1, 
                       ifelse(MHDF$Country == "United Kingdom", 2, 3))
MHDF$self_employed <- ifelse(MHDF$self_employed == "No", 1, 0)
MHDF$family_history <- ifelse(MHDF$family_history == "No", 1, 0)
MHDF$treatment <- ifelse(MHDF$treatment == "No", 1, 0)
MHDF$work_interfere <- ifelse(is.na(MHDF$work_interfere), 5, 
                              ifelse(MHDF$work_interfere == "Often", 2, 
                                     ifelse(MHDF$work_interfere == "Rarely", 3, 
                                            ifelse(MHDF$work_interfere == "Sometimes", 4, 1))))
MHDF$no_employees <- ifelse(MHDF$no_employees == "1-5", 1, 
                            ifelse(MHDF$no_employees == "6-25", 2, 
                                   ifelse(MHDF$no_employees == "26-100", 3, 
                                          ifelse(MHDF$no_employees == "100-500", 4,
                                                 ifelse(MHDF$no_employees == "500-1000", 5, 6)))))
MHDF$remote_work <- ifelse(MHDF$remote_work == "No", 0, 1)


cor(MHDF[sapply(MHDF, function(x) !is.factor(x))])
summary(MHDF)

# plots
library(ggplot2)
ggplot(MHDF, aes(x=seek_help)) + geom_bar() + labs(title="Frequency")  # Y axis derived from counts of X item

counts<- table(MHDF$seek_help, MHDF$wellness_program)
barplot(counts, main="Do employee's seek mental health help based on their wellness program?",
        xlab="Wellness program", col=c("black","orange","red"), ylab="Seek Help",
        legend=c("Seeking help","Not seeking help","Don't know"),
        names.arg=c("Yes", "No", "Don't Know"))

counts2<- table(MHDF$seek_help, MHDF$benefits)
barplot(counts2, main="Do employer's provide mental health benefits or resources to seek help?",
        xlab="Benefits", col=c("black","orange","red"), ylab="Helpful Resources",
        legend=c("Yes","No","Don't know"),
        names.arg=c("Yes", "No", "Don't know"))

counts3 <- table(MHDF$family_history, MHDF$treatment, dnn = c("Family History", "Treatment"))
barplot(counts3, main="Mental Health Issues Based on Family History",
        xlab="Treatment", col=c("darkblue","red"),
        legend = c("Family History", "No Family History"),
        names.arg = c("Not Saught Help", "Saught Help"))

counts4<- table(MHDF$mental_vs_physical, MHDF$anonymity)
counts4
barplot(counts4, main="Do employer's take mental health as seriously as physical and are they anonymous?",
        xlab="Mental Health Importance", col=c("black","orange","red"), ylab="Anonymity",
        legend=c("Yes","No","Don't know"),
        names.arg=c("Yes", "No", "Don't know"))