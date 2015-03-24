# script takes in hip and wrist training data sets (must include annotation)
# and raw hip and wrist stream of data (must be of same sampling rate)
# kNN - 3,5,7,11,13,17 and 19 is performed on a datasets
# the stream with the best quality will be picked and 
# statistics summaries will be produced (conf.matrix, accuracy, precision, F-measure and recall)

# these functions are essential - CHANGE TO MATCH YOUR PATH to 'activityRecognitionFunctions.R'
source("/home/pet5o/workspace/TDP/R/group-har/activityRecognitionFunctions.R")

setwd("/home/pet5o/Dropbox/uni/09-csc8625-GroupProject/dataEvaluation/annotated/testing")
hipTrainFile = "hipTraining.csv"
wristTrainFile = "wristTraining.csv"
hipTestStream = "RawaHip.csv"
wristTestStream = "RawaWrist.csv"
testAnnotation = read.csv("RawaHip.csv")$activity

fit.knn <- maxClassAccuracy(hipTrainFile, wristTrainFile, hipTestStream, wristTestStream, 
                            testAnnotation = testAnnotation, testDataInCSV = TRUE)

maxClassAccuracy = function(hipTrainFile, wristTrainFile, hipTestStream, 
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
    testHipSQ[i] = getStreamQuality(trainHip, testHipSS, kNN[i])
    cat("; Hip: ", testHipSQ[i])
    testWristSQ[i] = getStreamQuality(trainWrist, testWristSS, kNN[i])
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
  plot(testWristSS, xlab = "Time", ylab="Intensity", type="l", main="Wrist data", ylim=c(0.07,0.08))
  points(x=1:length(pred),y=rep(0.07,length(pred)),col=pred, pch=16)
  plot(testHipSS, xlab = "Time", ylab="Intensity", type="l", main="Hip data", ylim=c(0.07,0.08))
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