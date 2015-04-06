# Requirement for hidden markov model
install.packages("HMM")
require(HMM)
# Library for confusion matrix
install.packages("caret")
require(caret)
# Library for fitting distributions
install.packages("MASS")
require(MASS)

# set your own working directory
setwd("C:/Users/Stapose/CDT/Group-project/HMM")

# load train data
trainData = read.csv("hipTraining.csv")
# load test data
testData = read.csv("RawaHip.csv")
# define annotation for test data
annotation = testData$activity

# Create the emission distributions
# Emission matrix for HMM require discrete data
# Movement data is continuous
# Need to put each piece of data into a bin representing a range of values
emmisNames = character(32)
for (i in 1:32){
  emmisNames[i] = paste("Bin", i, sep = "")
}

# Want to establish range of values we think are plausible
minIntens = min(trainData$intensity)
maxIntens = max(trainData$intensity)
# Split the data into 30 blocks so we can still get an idea of the distribution
blocks = (maxIntens - minIntens)/30
# Need to allow for values either side of the range in the case of test outliers
dist = c(-Inf, seq(minIntens, maxIntens, by=blocks), Inf)
# Create a new column that groups the values based on the given blocks
trainData$groups = cut(trainData$intensity, dist, labels=emmisNames)

# Different state names
stateNames = c("0", "1", "2")

# Initial stationarity we wish to be stationary
initialDist = c(19/24, 3.5/24, 1.5/24)

# Seperate data frames into various activities
trainRestData = trainData[trainData$activity == 0,]
trainLightData = trainData[trainData$activity == 1,]
trainHeavyData = trainData[trainData$activity == 2,]

# Changed the names to save typing but kept above so I knew what they represented
tr0 = trainRestData
tr1 = trainLightData
tr2 = trainHeavyData

# Examining the distribution of each type of activity
plot(tr0$groups)
plot(tr1$groups)
plot(tr2$groups)

# Put the group data into table to easily work with frequenices
# Divided by the total to find the probabilities
tr0tab = table(tr0$groups)/(sum(table(tr0$groups)))
tr1tab = table(tr1$groups)/(sum(table(tr1$groups)))
tr2tab = table(tr2$groups)/(sum(table(tr2$groups)))

# Really none of the groups should have a zero probability
# Ideally would find the best fitting normal fitting distribution
# But a quick fix for now is just assign all the negligable probabilities to 0.001
# Normalise after to ensure the sum equals 1
# Convert to vector for the HMM function

tr0tab[tr0tab<0.001] = 0.001
tr0tab = tr0tab/sum(tr0tab)
tr0Vec = as.vector(tr0tab)
tr1tab[tr1tab<0.001] = 0.001
tr1tab = tr1tab/sum(tr1tab)
tr1Vec = as.vector(tr1tab)
tr2tab[tr2tab<0.001] = 0.001
tr2tab = tr2tab/sum(tr2tab)
tr2Vec = as.vector(tr2tab)

plot(tr0tab)
plot(tr1tab)
plot(tr2tab)

# Create the emission matrix
emissionDist = rbind(tr0Vec, tr1Vec, tr2Vec)

# Create the transition matrix
Transition = rbind(c(9/10, 61/1140, 53/1140),
                   c(2/7, 2/3, 1/21),
                   c(6/10, 1/10, 3/10))

# Function creates a hidden markov model object
actHMM = initHMM(States=stateNames, Symbols=emmisNames, startProbs=initialDist, 
                  transProbs=Transition, emissionProbs=emissionDist)
# Add the group column to test data
testData$groups = cut(testData$intensity, dist, labels=emmisNames)

# Run the viterbi algorithm
testVit = viterbi(actHMM, testData$groups)

compare = cbind(testVit, testData$activity)
compare

# Need to incoportate Peters code below to find summaries

# Machine learning summaries
confMatrix = getConfMatrix(testData$intensity, annotation, trainData)
accuracy = getAccuracy(confMatrix)
precision = getPrecision(confMatrix)
recall = getRecall(confMatrix)
fmeasure = getFmeasure(confMatrix)

# returns a confusion matrix
getConfMatrix = function(testData, annotation, trainData) {
  fit.hmm = knn(data.frame(trainData$intensity, 1), data.frame(testData, 1), 
                 factor(trainData$activity), k = 3, prob=TRUE)
  # build confusion matrix
  # confusionMatrix(fit.knn, annotation)
  
  # do it manually
  confMatrix = matrix(rep(0, 9), nrow = 3, ncol = 3)
  # to be able to add fill up confMatrix
  fit.hmm.numeric = as.numeric(fit.hmm)
  # print(fit.knn.numeric)
  
  # to match the levels
  annotation = annotation + 1
  # print(annotation)
  for(i in 1:length(fit.hmm.numeric)) {
    confMatrix[fit.hmm.numeric[i],annotation[i]] = confMatrix[fit.hmm.numeric[i],annotation[i]] + 1
  }
  return(confMatrix)
}

# calculates accuracy for each class
getAccuracy = function(confMatrix) {
  acc = rep(0,ncol(confMatrix))
  for (i in 1:ncol(confMatrix)) {
    # true positive
    TP = confMatrix[i,i]
    tempCM = confMatrix
    tempCM[i,i] = 0
    # false positive
    FP = sum(tempCM[,i])
    # false negative
    FN = sum(tempCM[i,])
    tempCM[i,] = 0
    tempCM[,i] = 0
    # true negative
    TN = sum(tempCM)
    # accuracy
    acc[i] = (TP + TN) / (TP + FP + FN + TN)
  }
  return(acc)
}

# calculates precision for each class
getPrecision = function(confMatrix) {
  p = rep(0,ncol(confMatrix))
  for (i in 1:ncol(confMatrix)) {
    # true positive
    TP = confMatrix[i,i]
    tempCM = confMatrix
    tempCM[i,i] = 0
    # false positive
    FP = sum(tempCM[,i])
    # precision
    p[i] = TP / (TP + FP)
  }
  return(p)
}

# calculates recall for each class
getRecall = function(confMatrix) {
  r = rep(0,ncol(confMatrix))
  for (i in 1:ncol(confMatrix)) {
    # true positive
    TP = confMatrix[i,i]
    tempCM = confMatrix
    tempCM[i,i] = 0
    # false negative
    FN = sum(tempCM[i,])
    # accuracy
    r[i] = TP / (TP + FN)
  }
  return(r)
}

# calculates F-measure for each class
getFmeasure = function(confMatrix) {
  fMeasure = rep(0,ncol(confMatrix))
  for (i in 1:ncol(confMatrix)) {
    # true positive
    TP = confMatrix[i,i]
    tempCM = confMatrix
    tempCM[i,i] = 0
    # false positive
    FP = sum(tempCM[,i])
    # false negative
    FN = sum(tempCM[i,])
    # accuracy
    fMeasure[i] = (2 * TP) / (2 * TP + FP + FN)
  }
  return(fMeasure)
}