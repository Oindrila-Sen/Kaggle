library(dplyr)
library(ggplot2)
######################################
# 1. Load Data
######################################
# Set Working Directory
setwd("/Users/oindrilasen/WORK_AREA/Data Science/Kaggle/HeartDiseaseUCI")
# Read train.csv data file
disease<-read.csv("heart.csv",
                  header = TRUE
                  ,na.strings = "",
                  stringsAsFactors = FALSE
)
# Let's take a look at the data
dim(disease) 
glimpse(disease) 

######################################
# 2. Data Cleaning
######################################
# 1. Check for NA values
na_count <- sort(sapply(disease, function(x) sum(is.na(x))),
                 decreasing = TRUE)
na_count
# 2. Check the different featutes
#age
summary(disease$age)
# sex
unique(disease$sex)
table(disease$sex)
#Modify Sex Values to M and F
disease$sex <- ifelse(disease$sex == 1, "M","F") 
# cp
unique(disease$cp)
table(disease$cp)
#trestbps
summary(disease$trestbps)
#chol
summary(disease$chol)
# fbs = (fasting blood sugar > 120 mg/dl) 1 = true; 0 = false) 
table(disease$fbs)
# restecg(resting electrocardiographic results)
unique(disease$restecg)
table(disease$restecg)
#thalach
summary(disease$thalach)
#exang
summary(disease$exang)
table(disease$exang)
#oldpeak
summary(disease$oldpeak)
#slope
unique(disease$slope)
#ca
summary(disease$ca)
#thal
summary(disease$thal)
#target
unique(disease$target)
table(disease$target)

# 3. Convert to Factors
disease$target <- as.factor(disease$target)
disease$sex <- as.factor(disease$sex)
disease$cp <- as.factor(disease$cp)
disease$fbs <- as.factor(disease$fbs)
disease$restecg <- as.factor(disease$restecg)
disease$exang <- as.factor(disease$exang)
disease$slope <- as.factor(disease$slope)

glimpse(disease)

######################################
# 3. Exploratory Data Analysis
######################################
# Target
# (1=yes, 0=no)
ggplot(disease, aes(x = target)) +
  geom_bar()
# Age
summary(disease$age)
ggplot(disease, aes(x = target, y = age)) +
  geom_boxplot(na.rm = TRUE) +
  ylim(20, 100)

#Sex
table(disease$sex, disease$target)
ggplot(disease, aes(x = sex, fill = target)) +
  geom_bar(position = "dodge") 

# cp
ggplot(disease, aes(x = cp, fill= target)) +
  geom_bar(position = "dodge")

# trestbps
ggplot(disease, aes(x = trestbps,fill = target)) +
  geom_histogram(position = "dodge")

# chol
ggplot(disease, aes(x = chol,fill = target)) +
  geom_histogram(position = "dodge")

#fbs
#fasting blood sugar > 120 mg/dl) (1 = true; 0 = false) 
ggplot(disease, aes(x = fbs, fill= target)) +
  geom_bar(position = "dodge")

#restecg
ggplot(disease, aes(x = restecg, fill= target)) +
  geom_bar(position = "dodge")

#thalach
ggplot(disease, aes(x = thalach,fill = target)) +
  geom_histogram(position = "dodge")

# exang
ggplot(disease, aes(x = exang, fill= target)) +
  geom_bar(position = "dodge")

#oldpeak
ggplot(disease, aes(x = oldpeak,fill = target)) +
  geom_histogram(position = "dodge")+
  scale_x_continuous(limits = c(0, 6), breaks = seq(0, 6, 0.50)) 

#slope
ggplot(disease, aes(x = slope, fill= target)) +
  geom_bar(position = "dodge")

#ca
ggplot(disease, aes(x = ca, fill= target)) +
  geom_bar(position = "dodge")

#thal
ggplot(disease, aes(x = thal, fill= target)) +
  geom_bar(position = "dodge")

######################################
# 4. Data Model
######################################
set.seed(150)
#Sample Indexes
indexes = sample(1:nrow(disease), size = 0.2 * nrow(disease))
# Split dataset into training and test set
test_data = disease[indexes, ]
train_data = disease[-indexes, ]

dim(train_data)
dim(test_data)

lg_model <- glm(target ~ ., 
                data = train_data,
                family = binomial(link = "logit"))

summary(lg_model)

lg_model_rev <- glm(target ~ sex+ cp + ca + thal + exang + oldpeak, 
                    data = train_data,
                    family = binomial(link = "logit"))
summary(lg_model_rev)

######################################
# 4. Prediction
######################################
test_data$predicted_target <- predict(lg_model_rev,
                                      test_data, 
                                      type =  "response")

summary(test_data$predicted_target )

test_data$predicted_target  <- round(test_data$predicted_target )

table(test_data$target)

table(test_data$predicted_target)

# Check Accuracy of the Model
lconfMat <- table(`Actual Class` = test_data$target,`Predicted Class` =test_data$predicted_target) 
laccuracy <- sum(diag(lconfMat))/sum(lconfMat)
laccuracy

# Test prediction at a random record
# Generate a Random Number
dim(test_data)
randomNum <- sample(1:60, 1)
test_data$target[randomNum]
test_data$predicted_target[randomNum]