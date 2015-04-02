# script takes in hip and wrist training data sets (must include annotation)
# and raw hip and wrist stream of data (must be of same sampling rate)
# kNN - 3,5,7,11,13,17 and 19 is performed on a datasets
# the stream with the best quality will be picked and 
# statistics summaries will be produced (conf.matrix, accuracy, precision, F-measure and recall)

# these functions are essential - CHANGE TO MATCH YOUR PATH to 'activityRecognitionFunctions.R'
# source("/home/pet5o/workspace/TDP/R/group-har/activityRecognitionFunctions.R")
source("C:/Users/localadmin/Documents/workspace/TDP/classification/activityRecognitionFunctions.R")

setwd("C:/Users/localadmin/Dropbox/uni/09-csc8625-GroupProject/dataEvaluation/annotated_w_fft/testing")
# setwd("/home/pet5o/Dropbox/uni/09-csc8625-GroupProject/dataEvaluation/annotated_w_fft/testing")
hipTrainFile = "HipTrain.csv"
wristTrainFile = "WristTrain.csv"
hipTestStream = "RawaHip.csv"
wristTestStream = "RawaWrist.csv"
testAnnotation = read.csv("RawaHip.csv")$activity

fit.knn <- maxClassAccuracy_46f(hipTrainFile, wristTrainFile, hipTestStream, wristTestStream, 
                            testAnnotation = testAnnotation, testDataInCSV = TRUE)

fit.knn <- maxClassAccuracy_76f(hipTrainFile, wristTrainFile, hipTestStream, wristTestStream, 
                                testAnnotation = testAnnotation, testDataInCSV = TRUE)

fit.knn <- maxClassAccuracy_226f(hipTrainFile, wristTrainFile, hipTestStream, wristTestStream, 
                                 testAnnotation = testAnnotation, testDataInCSV = TRUE)

# using 3 top freq from each 3 * 3 * 5 = 45 FFT features
# works (so far) only for CSV test/train set 
maxClassAccuracy_46f = function(hipTrainFile, wristTrainFile, hipTestStream, 
                                wristTestStream, testAnnotation = c(), testDataInCSV = FALSE) {
  require(class)
  
  filter = c(TRUE, TRUE, rep(TRUE,3), rep(FALSE,12), rep(TRUE,3), rep(FALSE,12), rep(TRUE,3), rep(FALSE,12),
             rep(TRUE,3), rep(FALSE,12), rep(TRUE,3), rep(FALSE,12), rep(TRUE,3), rep(FALSE,12),
             rep(TRUE,3), rep(FALSE,12), rep(TRUE,3), rep(FALSE,12), rep(TRUE,3), rep(FALSE,12),
             rep(TRUE,3), rep(FALSE,12), rep(TRUE,3), rep(FALSE,12), rep(TRUE,3), rep(FALSE,12),
             rep(TRUE,3), rep(FALSE,12), rep(TRUE,3), rep(FALSE,12), rep(TRUE,3), rep(FALSE,12))
  
  # load training data
  trainHip = read.csv(hipTrainFile)[,filter]
  trainWrist = read.csv(wristTrainFile)[,filter]
  
  # statistics summaries
  testHipSS = c()
  testWristSS = c()
  
  if (testDataInCSV) {
    # load testing data (if in CSV - just read the statistics summaries)
    testHip = read.csv(hipTestStream)[,filter]
    # take all features except of classification
    testHipSS = testHip[,-1]
    testWrist = read.csv(wristTestStream)[,filter]
    testWristSS = testWrist[,-1]
  } else {
    # calculate statistic summaries for testing streams
    # testHipSS = getStatSummary(hipTestStream)
    # testWristSS = getStatSummary(wristTestStream)  
  }
  
  # calculate stream quality for given data
  kNN = c(3,5,7,11,13,17,19)
  testHipSQ = rep(0, length(kNN))
  testWristSQ = rep(0, length(kNN))
  
  # loop through all kNN options
  for(i in 1:length(kNN)) {
    cat("kNN: ", kNN[i])
    testHipSQ[i] = getStreamQuality_df(trainHip[,-1], trainHip[,1], testHipSS, kNN[i])
    cat("; Hip: ", testHipSQ[i])
    testWristSQ[i] = getStreamQuality_df(trainWrist[,-1], trainWrist[,1], testWristSS, kNN[i])
    cat("; Wrist: ", testWristSQ[i], "\n")
  }
  
  # get the highest stream quality
  maxHipSQ = max(testHipSQ)
  maxWristSQ = max(testWristSQ)
  
  # make assumption that hip has better SQ; get final prediction at the same time
  hipBetter = TRUE
  fit.knn <- c()
  if (maxHipSQ < maxWristSQ) { 
    hipBetter = FALSE 
    cat("Wrist stream - kNN(", match(maxWristSQ, testWristSQ), ") performs the best - ", maxWristSQ, "\n")
    fit.knn <- knn(trainWrist[,-1], testWristSS, 
                   factor(trainWrist[,1]), k = match(maxWristSQ, testWristSQ), prob=FALSE)
  } else {
    cat("Hip stream - kNN(", kNN[match(maxHipSQ, testHipSQ)], ") performs the best - ", maxHipSQ, "\n")
    fit.knn <- knn(trainHip[,-1], testHipSS, 
                   factor(trainHip[,1]), k = match(maxHipSQ, testHipSQ), prob=FALSE)
  }
  
  # predictions
  pred <- as.numeric(fit.knn)
  
  # produce plots of given data with the best prediction
  par(mfrow=c(2,1))
  # TODO - nasty ylim constants - should really set it dynamically
  plot(testWristSS$intensity, xlab = "Time", ylab="Intensity", type="l", main="Wrist data", ylim=c(0.07,0.09))
  points(x=1:length(pred),y=rep(0.07,length(pred)),col=pred, pch=16)
  plot(testHipSS$intensity, xlab = "Time", ylab="Intensity", type="l", main="Hip data", ylim=c(0.07,0.09))
  points(x=1:length(pred),y=rep(0.07,length(pred)),col=pred, pch=16)
  
  # provide statitics summaries of a model if test annotation provided
  if (length(testAnnotation) == 0) { return(fit.knn) }
  confMatrix = getConfMatrix(fit.knn, testAnnotation)
  print("===Accuracy measures for best model===")
  print("Confusion matrix: ")
  print(confMatrix)
  cat("Accuracy: ", getAccuracy(confMatrix))
  cat("\nF-measure: ", getFmeasure(confMatrix))
  cat("\nPrecision: ", getPrecision(confMatrix))
  cat("\nRecall: ", getRecall(confMatrix), "\n")
  print("======================================")
  
  # return best kNN model
  return(fit.knn)
}

# using 5 top freq from each 75 FFT features
# works (so far) only for CSV test/train set 
maxClassAccuracy_76f = function(hipTrainFile, wristTrainFile, hipTestStream, 
                                 wristTestStream, testAnnotation = c(), testDataInCSV = FALSE) {
  require(class)

  filter = c(TRUE, TRUE, rep(TRUE,5),rep(FALSE,10), rep(TRUE,5), rep(FALSE,10), rep(TRUE,5), rep(FALSE,10),
             rep(TRUE,5), rep(FALSE,10), rep(TRUE,5), rep(FALSE,10), rep(TRUE,5), rep(FALSE,10),
             rep(TRUE,5), rep(FALSE,10), rep(TRUE,5), rep(FALSE,10), rep(TRUE,5), rep(FALSE,10),
             rep(TRUE,5), rep(FALSE,10), rep(TRUE,5), rep(FALSE,10), rep(TRUE,5), rep(FALSE,10),
             rep(TRUE,5), rep(FALSE,10), rep(TRUE,5), rep(FALSE,10), rep(TRUE,5), rep(FALSE,10))
  
  # load training data
  trainHip = read.csv(hipTrainFile)[,filter]
  trainWrist = read.csv(wristTrainFile)[,filter]
  
  # statistics summaries
  testHipSS = c()
  testWristSS = c()
  
  if (testDataInCSV) {
    # load testing data (if in CSV - just read the statistics summaries)
    testHip = read.csv(hipTestStream)[,filter]
    # take all features except of classification
    testHipSS = testHip[,-1]
    testWrist = read.csv(wristTestStream)[,filter]
    testWristSS = testWrist[,-1]
  } else {
    # calculate statistic summaries for testing streams
    # testHipSS = getStatSummary(hipTestStream)
    # testWristSS = getStatSummary(wristTestStream)  
  }
  
  # calculate stream quality for given data
  kNN = c(3,5,7,11,13,17,19)
  testHipSQ = rep(0, length(kNN))
  testWristSQ = rep(0, length(kNN))
  
  # loop through all kNN options
  for(i in 1:length(kNN)) {
    cat("kNN: ", kNN[i])
    testHipSQ[i] = getStreamQuality_df(trainHip[,-1], trainHip[,1], testHipSS, kNN[i])
    cat("; Hip: ", testHipSQ[i])
    testWristSQ[i] = getStreamQuality_df(trainWrist[,-1], trainWrist[,1], testWristSS, kNN[i])
    cat("; Wrist: ", testWristSQ[i], "\n")
  }
  
  # get the highest stream quality
  maxHipSQ = max(testHipSQ)
  maxWristSQ = max(testWristSQ)
  
  # make assumption that hip has better SQ; get final prediction at the same time
  hipBetter = TRUE
  fit.knn <- c()
  if (maxHipSQ < maxWristSQ) { 
    hipBetter = FALSE 
    cat("Wrist stream - kNN(", match(maxWristSQ, testWristSQ), ") performs the best - ", maxWristSQ, "\n")
    fit.knn <- knn(trainWrist[,-1], testWristSS, 
                   factor(trainWrist[,1]), k = match(maxWristSQ, testWristSQ), prob=FALSE)
  } else {
    cat("Hip stream - kNN(", kNN[match(maxHipSQ, testHipSQ)], ") performs the best - ", maxHipSQ, "\n")
    fit.knn <- knn(trainHip[,-1], testHipSS, 
                   factor(trainHip[,1]), k = match(maxHipSQ, testHipSQ), prob=FALSE)
  }
  
  # predictions
  pred <- as.numeric(fit.knn)
  
  # produce plots of given data with the best prediction
  par(mfrow=c(2,1))
  # TODO - nasty ylim constants - should really set it dynamically
  plot(testWristSS$intensity, xlab = "Time", ylab="Intensity", type="l", main="Wrist data", ylim=c(0.07,0.09))
  points(x=1:length(pred),y=rep(0.07,length(pred)),col=pred, pch=16)
  plot(testHipSS$intensity, xlab = "Time", ylab="Intensity", type="l", main="Hip data", ylim=c(0.07,0.09))
  points(x=1:length(pred),y=rep(0.07,length(pred)),col=pred, pch=16)
  
  # provide statitics summaries of a model if test annotation provided
  if (length(testAnnotation) == 0) { return(fit.knn) }
  confMatrix = getConfMatrix(fit.knn, testAnnotation)
  print("===Accuracy measures for best model===")
  print("Confusion matrix: ")
  print(confMatrix)
  cat("Accuracy: ", getAccuracy(confMatrix))
  cat("\nF-measure: ", getFmeasure(confMatrix))
  cat("\nPrecision: ", getPrecision(confMatrix))
  cat("\nRecall: ", getRecall(confMatrix), "\n")
  print("======================================")
  
  # return best kNN model
  return(fit.knn)
}


# classification based on intensity and 225 FFT features
# works (so far) only for CSV test/train set 
maxClassAccuracy_226f = function(hipTrainFile, wristTrainFile, hipTestStream, 
                                 wristTestStream, testAnnotation = c(), testDataInCSV = FALSE) {
  require(class)
  
  # load training data
  trainHip = read.csv(hipTrainFile)
  trainWrist = read.csv(wristTrainFile)
  
  # statistics summaries
  testHipSS = c()
  testWristSS = c()
  
  if (testDataInCSV) {
    # load testing data (if in CSV - just read the statistics summaries)
    testHip = read.csv(hipTestStream)
    # take all features except of classification
    testHipSS = testHip[,-1]
    testWrist = read.csv(wristTestStream)
    testWristSS = testWrist[,-1]
  } else {
    # calculate statistic summaries for testing streams
    # testHipSS = getStatSummary(hipTestStream)
    # testWristSS = getStatSummary(wristTestStream)  
  }
  
  # calculate stream quality for given data
  kNN = c(3,5,7,11,13,17,19)
  testHipSQ = rep(0, length(kNN))
  testWristSQ = rep(0, length(kNN))
  
  # loop through all kNN options
  for(i in 1:length(kNN)) {
    cat("kNN: ", kNN[i])
    testHipSQ[i] = getStreamQuality_df(trainHip[,-1], trainHip[,1], testHipSS, kNN[i])
    cat("; Hip: ", testHipSQ[i])
    testWristSQ[i] = getStreamQuality_df(trainWrist[,-1], trainWrist[,1], testWristSS, kNN[i])
    cat("; Wrist: ", testWristSQ[i], "\n")
  }
  
  # get the highest stream quality
  maxHipSQ = max(testHipSQ)
  maxWristSQ = max(testWristSQ)
  
  # make assumption that hip has better SQ; get final prediction at the same time
  hipBetter = TRUE
  fit.knn <- c()
  if (maxHipSQ < maxWristSQ) { 
    hipBetter = FALSE 
    cat("Wrist stream - kNN(", match(maxWristSQ, testWristSQ), ") performs the best - ", maxWristSQ, "\n")
    fit.knn <- knn(trainWrist[,-1], testWristSS, 
                   factor(trainWrist[,1]), k = match(maxWristSQ, testWristSQ), prob=FALSE)
  } else {
    cat("Hip stream - kNN(", kNN[match(maxHipSQ, testHipSQ)], ") performs the best - ", maxHipSQ, "\n")
    fit.knn <- knn(trainHip[,-1], testHipSS, 
                   factor(trainHip[,1]), k = match(maxHipSQ, testHipSQ), prob=FALSE)
  }
  
  # predictions
  pred <- as.numeric(fit.knn)
  
  # produce plots of given data with the best prediction
  par(mfrow=c(2,1))
  # TODO - nasty ylim constants - should really set it dynamically
  plot(testWristSS$intensity, xlab = "Time", ylab="Intensity", type="l", main="Wrist data", ylim=c(0.07,0.09))
  points(x=1:length(pred),y=rep(0.07,length(pred)),col=pred, pch=16)
  plot(testHipSS$intensity, xlab = "Time", ylab="Intensity", type="l", main="Hip data", ylim=c(0.07,0.09))
  points(x=1:length(pred),y=rep(0.07,length(pred)),col=pred, pch=16)
  
  # provide statitics summaries of a model if test annotation provided
  if (length(testAnnotation) == 0) { return(fit.knn) }
  confMatrix = getConfMatrix(fit.knn, testAnnotation)
  print("===Accuracy measures for best model===")
  print("Confusion matrix: ")
  print(confMatrix)
  cat("Accuracy: ", getAccuracy(confMatrix))
  cat("\nF-measure: ", getFmeasure(confMatrix))
  cat("\nPrecision: ", getPrecision(confMatrix))
  cat("\nRecall: ", getRecall(confMatrix), "\n")
  print("======================================")
  
  # return best kNN model
  return(fit.knn)
}

# classification based on only one feature
maxClassAccuracy_1f = function(hipTrainFile, wristTrainFile, hipTestStream, 
                            wristTestStream, testAnnotation = c(), testDataInCSV = FALSE) {
  require(class)
  
  # load training data
  trainHip = read.csv(hipTrainFile)
  trainWrist = read.csv(wristTrainFile)
  
  # statistics summaries
  testHipSS = c()
  testWristSS = c()
  
  if (testDataInCSV) {
    # load testing data (if in CSV - just read the statistics summaries)
    testHip = read.csv(hipTestStream)
    testHipSS = testHip$intensity
    testWrist = read.csv(wristTestStream)
    testWristSS = testWrist$intensity
  } else {
    # calculate statistic summaries for testing streams
    testHipSS = getStatSummary(hipTestStream)
    testWristSS = getStatSummary(wristTestStream)  
  }
    
  # calculate stream quality for given data
  kNN = c(3,5,7,11,13,17,19)
  testHipSQ = rep(0, length(kNN))
  testWristSQ = rep(0, length(kNN))
  
  # loop through all kNN options
  for(i in 1:length(kNN)) {
    cat("kNN: ", kNN[i])
    testHipSQ[i] = getStreamQuality_vector(trainHip, testHipSS, kNN[i])
    cat("; Hip: ", testHipSQ[i])
    testWristSQ[i] = getStreamQuality_vector(trainWrist, testWristSS, kNN[i])
    cat("; Wrist: ", testWristSQ[i], "\n")
  }
  
  # get the highest stream quality
  maxHipSQ = max(testHipSQ)
  maxWristSQ = max(testWristSQ)
  
  # make assumption that hip has better SQ; get final prediction at the same time
  hipBetter = TRUE
  fit.knn <- c()
  if (maxHipSQ < maxWristSQ) { 
    hipBetter = FALSE 
    cat("Wrist stream - kNN(", match(maxWristSQ, testWristSQ), ") performs the best - ", maxWristSQ, "\n")
    fit.knn <- knn(data.frame(trainWrist$intensity, 1), data.frame(testWristSS, 1), 
                   factor(trainWrist$activity), k = match(maxWristSQ, testWristSQ), prob=FALSE)
  } else {
    cat("Hip stream - kNN(", kNN[match(maxHipSQ, testHipSQ)], ") performs the best - ", maxHipSQ, "\n")
    fit.knn <- knn(data.frame(trainHip$intensity, 1), data.frame(testHipSS, 1), 
                   factor(trainHip$activity), k = match(maxHipSQ, testHipSQ), prob=FALSE)
  }
  
  # predictions
  pred <- as.numeric(fit.knn)
  
  # produce plots of given data with the best prediction
  par(mfrow=c(2,1))
  # TODO - nasty ylim constants - should really set it dynamically
  plot(testWristSS, xlab = "Time", ylab="Intensity", type="l", main="Wrist data", ylim=c(0.07,0.09))
  points(x=1:length(pred),y=rep(0.07,length(pred)),col=pred, pch=16)
  plot(testHipSS, xlab = "Time", ylab="Intensity", type="l", main="Hip data", ylim=c(0.07,0.09))
  points(x=1:length(pred),y=rep(0.07,length(pred)),col=pred, pch=16)
  
  # provide statitics summaries of a model if test annotation provided
  if (length(testAnnotation) == 0) { return(fit.knn) }
  confMatrix = getConfMatrix(fit.knn, testAnnotation)
  print("===Accuracy measures for best model===")
  print("Confusion matrix: ")
  print(confMatrix)
  cat("Accuracy: ", getAccuracy(confMatrix))
  cat("\nF-measure: ", getFmeasure(confMatrix))
  cat("\nPrecision: ", getPrecision(confMatrix))
  cat("\nRecall: ", getRecall(confMatrix), "\n")
  print("======================================")
  
  # return best kNN model
  return(fit.knn)
}