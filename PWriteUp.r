
# Required libraries ------------------------------------------------------

packages = c("mi", "caret")  # Here, other tabplotd3           

ipak <- function(pkg){                                          
  new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])] 
  if (length(new.pkg))                                          
    install.packages(new.pkg, dependencies = TRUE)              
  sapply(pkg, require, character.only = TRUE)                   
}

ipak(packages)

# Reading datasets --------------------------------------------------------

url_training = "pml-training.csv"
training = read.csv(url_training, sep=",", header = TRUE, na.strings= c("NA",""," "), nrow=19622)

url_testing = "pml-testing.csv"
testing = read.csv(url_testing, sep=",", header = TRUE, na.strings= c("NA",""," "), nrow=20)

# In the first instance, are removed attributes related to: timestam, X, usern_name, new_window
removeIndex = grep("timestamp|X|user_name|new_window",names(training));
training = training[,-removeIndex];
testing = testing[,-removeIndex];

summary(training)

# function de mi package
missing.pattern.plot(training[1:200,2:80], ylab = "")

# We can see many missing values with a curious pattern: 
# attributes which have missing values present exactly 19216 each one
# If we remove registers with missing values we will suffer an enormous decreasing
# from 19622 to 406. For this reason, we only delete the column attribute 
# which present NA (missing values).
# Doing this, we got a new dataset: [19216x53]
# Namely, nrow(training)-sum(c) attributes left out

a = colSums(is.na(training))
c = a!= 19216 # index of columnas with 19216 missing values each one
training = training[, c]
testing = testing[, c]

# Esta cantidad de columnas quedan fuera de analisis: 

summary(training) # Ya no observamos missing values

# Atributos a explorar:
names(training)

 

# Split into Train & Validation -------------------------------------------
# In this part we are going to split training dataset into Train and Validation
# to evaluate the performance in some models

set.seed(625)

trainIndex = createDataPartition(training$classe, p = .6,
                                  list = FALSE,
                                  times = 1)

train = training[trainIndex, ]
validation = training[-trainIndex, ]

# Model Fitting -----------------------------------------------------------

rpartModel = train(classe ~ .,
                   data = train,
                   method = "rpart",
                   tuneLength = 30)

ctreeModel = train(classe ~ .,
                   data = train,
                   method = "ctree",
                   tuneLength = 10)

treebagModel = train(classe ~ .,
                     data = train,
                     method = "treebag")

library(caret)

trControl = trainControl(method = "cv", number = 2, allowParallel =TRUE)
modFitRF = train(classe ~.,data = training,method="rf",trControl=trControl)

predictedValues = predict(modFitRF,testing);
View(predictedValues);

pRes = postResample(predictedValues, testing$classe)
cfM = confusionMatrix(predictedValues, testing$classe)

# Save predicted values
answers = predictedValues
