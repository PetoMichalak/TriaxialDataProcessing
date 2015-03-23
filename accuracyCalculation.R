# library for kNN
require(class)
# library for conf matrix
require(caret)

# set your own working directory
setwd("/home/pet5o/Dropbox/uni/09-csc8625-GroupProject/dataEvaluation/annotated")

# load train data
trainDataHip = read.csv("hip/hipTraining.csv")
trainDataWrist = read.csv("wrist/wristTraining_noResistance.csv")
# load test data
testDataHip = read.csv("RawaHip.csv")
testDataWrist = read.csv("RawaWrist.csv")
# define annotation for test data
annotation = testDataHip$activity

confMatrix <- getConfMatrix(testDataHip$intensity, annotation, trainDataHip, 3)
accuracy <- getAccuracy(confMatrix)
precision <- getPrecision(confMatrix)
recall <- getRecall(confMatrix)
fmeasure <- getFmeasure(confMatrix)

# returns a confusion matrix
getConfMatrix = function(testData, annotation, trainData, k = 3) {
  fit.knn <- knn(data.frame(trainData$intensity, 1), data.frame(testData, 1), 
                 factor(trainData$activity), k = k, prob=TRUE)
  print(fit.knn)
  cat("Stream quality: ", mean(attributes(fit.knn)$prob))
  # build confusion matrix
  # confusionMatrix(fit.knn, annotation)
  
  # do it manually
  confMatrix = matrix(rep(0, 9), nrow = 3, ncol = 3)
  # to be able to add fill up confMatrix
  fit.knn.numeric = as.numeric(fit.knn)
  print(fit.knn.numeric)
  
  # to match the levels
  annotation = annotation + 1
  print(annotation)
  for(i in 1:length(fit.knn.numeric)) {
    confMatrix[fit.knn.numeric[i],annotation[i]] = confMatrix[fit.knn.numeric[i],annotation[i]] + 1
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