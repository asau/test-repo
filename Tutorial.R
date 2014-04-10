#setwd("~/Documents/School/Kaggle/Titanic/Data")
testData <- read.csv("~/Documents/School/Kaggle/Titanic/Data/test.csv")
trainData <- read.csv("~/Documents/School/Kaggle/Titanic/Data/train.csv")

plot(density(trainData$Age, na.rm = TRUE))
plot(density(trainData$Fare, na.rm = TRUE))
plot(density(trainData$Survived, na.rm = TRUE))
plot(density(trainData$Pclass, na.rm = TRUE))
plot(density(trainData$Parch, na.rm = TRUE))

counts <- table(trainData$Survived, trainData$Sex)
barplot(counts, xlab = "Gender", ylab = "Number of People", main = "survived and deceased between male and female")
womensurv<-counts[2] / (counts[1] + counts[2])
mensurv<-counts[4] / (counts[3] + counts[4])

Pclass_survival <- table(trainData$Survived, trainData$Pclass)
barplot(Pclass_survival, xlab = "Cabin Class", ylab = "Number of People",
        main = "survived and deceased between male and female")
Pclass_survival[2] / (Pclass_survival[1] + Pclass_survival[2])
Pclass_survival[4] / (Pclass_survival[3] + Pclass_survival[4])
Pclass_survival[6] / (Pclass_survival[5] + Pclass_survival[6])

#Cleaning Data
trainData <- trainData[-c(1,9:12)]
trainData$Sex <- gsub("female", 1, trainData$Sex)
trainData$Sex <- gsub("^male", 0, trainData$Sex)

master_vector <- grep("Master.",trainData$Name)
miss_vector <- grep("Miss.", trainData$Name)
mrs_vector <- grep("Mrs.", trainData$Name)
mr_vector <- grep("Mr.", trainData$Name)
dr_vector <- grep("Dr.", trainData$Name)

trainData$Name<-as.vector(trainData$Name)

trainData$Name[master_vector]="Master"
trainData$Name[miss_vector]<-"Miss"
trainData$Name[mrs_vector]<-"Mrs"
trainData$Name[mr_vector]<-"Mr"
trainData$Name[dr_vector]<-"Dr"

trainData<-trainData[-c(8)]

#imputation
master_age <- round(mean(trainData$Age[trainData$Name == "Master"], na.rm = TRUE), digits = 2)
miss_age <- round(mean(trainData$Age[trainData$Name == "Miss"], na.rm = TRUE), digits =2)
mrs_age <- round(mean(trainData$Age[trainData$Name == "Mrs"], na.rm = TRUE), digits = 2)
mr_age <- round(mean(trainData$Age[trainData$Name == "Mr"], na.rm = TRUE), digits = 2)
dr_age <- round(mean(trainData$Age[trainData$Name == "Dr"], na.rm = TRUE), digits = 2)

for (i in 1:nrow(trainData)) {
  if (is.na(trainData[i,5])) {
    if (trainData$Name[i] == "Master") {
      trainData$Age[i] <- master_age
    } else if (trainData$Name[i] == "Miss") {
      trainData$Age[i] <- miss_age
    } else if (trainData$Name[i] == "Mrs") {
      trainData$Age[i] <- mrs_age
    } else if (trainData$Name[i] == "Mr") {
      trainData$Age[i] <- mr_age
    } else if (trainData$Name[i] == "Dr") {
      trainData$Age[i] <- dr_age
    } else {
      print("Uncaught Title")
    }
  }
}

trainData["Child"] <- NA

for (i in 1:nrow(trainData)) {
  if (trainData$Age[i] <=12){
    trainData$Child[i]<-1
  } else {
    trainData$Child[i] <- 2
  }
}
trainData["Family"] <- NA

for(i in 1:nrow(trainData)) {
  x <- trainData$SibSp[i]
  y <- trainData$Parch[i]
  trainData$Family[i] <- x + y + 1
}

trainData["Mother"] <- NA
for(i in 1:nrow(trainData)) {
  if(trainData$Name[i] == "Mrs" & trainData$Parch[i] > 0) {
    trainData$Mother[i] <- 1
  } else {
    trainData$Mother[i] <- 2
  }
}
