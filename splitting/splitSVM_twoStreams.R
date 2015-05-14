# SVM - split
require(tools)

# load project specific libraries
source("/home/pet5o/workspace/TDP/R/group-har/activityRecognitionFunctions.R")
path="/home/pet5o/workspace/TDP/DataEvaluation/_reportRun/SVM"
# TRAINING DATA
wristTrainPath = "/home/pet5o/workspace/TDP/DataEvaluation/final_dataset_run/trainingSets/wrist"
hipTrainPath = "/home/pet5o/workspace/TDP/DataEvaluation/final_dataset_run/trainingSets/hip"

setwd(path)
# prepare partial result file
logPath = paste(path, "/partialResult_SVM.log", sep="")
write("filename,source,cost,gamma",file=logPath, append=FALSE)

# performs kNN classification on two streams of triaxial data (preprocessed)
SVM_twoStreams = function(wristDataPath, logPath) {
  require(e1071)
  # === modify to suit your needs
  # path = "/home/pet5o/workspace/TDP/data/Jack_weekend17-19April"
  # wristDataPath = "Jack_wrist_sample_annotated_features.csv"
  hipDataPath = gsub("WRIST", "HIP", wristDataPath)
  
  # list of booleans to specify number of features to work with
  fftCount = 15
  filterTestData = c(rep(TRUE, 9), rep(c(rep(TRUE, fftCount), rep(FALSE, 15-fftCount)), 15))
  filterTrainData = c(rep(TRUE, 9), rep(c(rep(TRUE, fftCount), rep(FALSE, 15-fftCount)), 15))
  # ===
  
  # load data
  print(paste("Loading data", wristDataPath))
  testWrist = read.csv(wristDataPath)
  testHip = read.csv(hipDataPath)
  
  # filter features to be used - default [ALL]
  filterTest = rep(TRUE, dim(testWrist)[2])

  # if user specified filter - override default
  if (length(filterTestData)) {
    filterTest = filterTestData
  }
  if (length(filterTrainData)) {
    filterTrain = filterTrainData
  }
  testWrist = testWrist[,filterTest]
  testHip = testHip[,filterTest]
  
  # run SVM
  testWristSQ = NA
  testHipSQ = NA
  
  svm.pred.wrist <- predict(svm.model.wrist, testWrist[,-c(1,2)], probability = TRUE)
  svm.pred.hip <- predict(svm.model.hip, testHip[,-c(1,2)], probability = TRUE)
  
  # calculate Stream Quality
  svm_SQ = rep(NA, length(svm.pred.wrist))
  for (i in 1:length(svm.pred.wrist)) {
    svm_SQ[i] = attributes(svm.pred.wrist)$prob[i,as.character(svm.pred.wrist[i])]
  }
  testWristSQ = mean(svm_SQ)
  
  svm_SQ = rep(NA, length(svm.pred.hip))
  for (i in 1:length(svm.pred.hip)) {
    svm_SQ[i] = attributes(svm.pred.hip)$prob[i,as.character(svm.pred.hip[i])]
  }
  testHipSQ = mean(svm_SQ)
  
  # compare SQ and pick best stream
  # make assumption that hip has better SQ
  hipBetter = TRUE
  # placeholder for for final predictions
  pred = NA
  if (testHipSQ < testWristSQ) { 
    hipBetter = FALSE
    pred <- as.numeric(svm.pred.wrist)
    cat("Wrist stream performs better; SQ: ", testWristSQ, "\n")
    log = paste(wristDataPath, "wrist", tuned.wrist$best.parameters[1,2], 
                tuned.wrist$best.parameters[1,1], sep=",")
    write(log, file=logPath, append=TRUE)
  } else {
    pred <- as.numeric(svm.pred.hip)
    cat("Hip stream performs better; SQ: ", testHipSQ, "\n")
    log = paste(hipDataPath, "hip", tuned.hip$best.parameters[1,2], 
                tuned.hip$best.parameters[1,1], sep=",")
    write(log, file=logPath, append=TRUE)
  }
  
  # save the graph
  if (hipBetter) {
    pdf(paste(file_path_sans_ext(hipDataPath),"_class_features",sum(filterTest),".pdf",sep=""))
  } else {
    pdf(paste(file_path_sans_ext(wristDataPath),"_class_features",sum(filterTest),".pdf",sep=""))
  }
  # produce plots of given data with the best prediction
  par(mfrow=c(2,1))
  # TODO - nasty ylim constants - should really set it dynamically
  plot(testWrist$statSummary, xlab = "Time", ylab="Intensity", type="l", main="Wrist data")
  points(x=1:length(pred),y=rep(0.07,length(pred)),col=pred, pch=16)
  plot(testHip$statSummary, xlab = "Time", ylab="Intensity", type="l", main="Hip data")
  points(x=1:length(pred),y=rep(0.07,length(pred)),col=pred, pch=16)
  dev.off()
  
  # save the graph
  if (hipBetter) {
    pdf(paste(path,"/SVM/", file_path_sans_ext(basename(as.character(hipDataPath))),"_class_features",
              (sum(filterTest)-2),".pdf",sep=""))
  } else {
    pdf(paste(path,"/SVM/", file_path_sans_ext(basename(as.character(wristDataPath))),"_class_features",
              (sum(filterTest)-2),".pdf",sep=""))
  }
  # produce plots of given data with the best prediction
  par(mfrow=c(2,1))
  plot(testWrist$statSummary, xlab = "Time", ylab="Intensity", 
       type="l", main="Wrist data", ylim=c(0.07,max(testWrist$statSummary)))
  points(x=1:length(pred),y=rep(0.07,length(pred)),col=pred, pch=16)
  legend("topright", c("Rest", "Light", "High"), cex=1, fill=1:3)
  plot(testHip$statSummary, xlab = "Time", ylab="Intensity", 
       type="l", main="Hip data", ylim=c(0.07,max(testHip$statSummary)))
  points(x=1:length(pred),y=rep(0.07,length(pred)),col=pred, pch=16)
  legend("topright", c("Rest", "Light", "High"), cex=1, fill=1:3)
  dev.off()
  
  # save the result
  # take away one from prediction as it counts as 1,2,3 isntead of 0,1,2 - see 'pred <- as.numeric(fit.knn)'
  outputWrist = data.frame(timestamp=testWrist$timestamp, activity=testWrist$activity, prediction=pred-1)
  outputHip = data.frame(timestamp=testHip$timestamp, activity=testHip$activity, prediction=pred-1)
  
  write.csv(outputWrist, 
            paste(file_path_sans_ext(wristDataPath),"_prediction.csv",sep=""), 
            row.names=FALSE)
  
  write.csv(outputHip, 
            paste(file_path_sans_ext(hipDataPath),"_prediction.csv",sep=""), 
            row.names=FALSE)
}

# lists all wrist data files which are paired up with hip files later in function
filenames <- list.files(file.path(path, "featureData"), pattern="*WRIST*", full.names=TRUE)

# create output dir
dir.create(file.path(path, "SVM"), showWarnings = FALSE)

# load training sets
trainWrist = loadTrainingData(wristTrainPath)
trainHip = loadTrainingData(hipTrainPath)

# tune parameters - gamma and cost
# Stream Quality placeholders
testWristSQ = NA
testHipSQ = NA

# Wrist stream
trainWrist$activity = factor(trainWrist$activity)
trainHip$activity = factor(trainHip$activity)

# find best cost and gamma parameters for SVM
tuned.wrist <- tune.svm(activity ~ ., data = trainWrist[,-1],
                        gamma = 10^(-3:3), cost = 10^(-3:3)) 
tuned.hip <- tune.svm(activity ~ ., data = trainHip[,-1],
                      gamma = 10^(-3:3), cost = 10^(-3:3)) 

# train model (ignoring timestamp)
svm.model.wrist <- svm(activity ~ ., data = trainWrist[,-1], 
                       gamma=tuned.wrist$best.parameters[1,1], 
                       cost=tuned.wrist$best.parameters[1,2], 
                       probability = TRUE)

svm.model.hip <- svm(activity ~ ., data = trainHip[,-1], 
                     gamma=tuned.hip$best.parameters[1,1], 
                     cost=tuned.hip$best.parameters[1,2], 
                     probability = TRUE)

# train SVM model

require(class)
require(parallel)
cl <- makeCluster(4)
clusterExport(cl, c("loadTrainingData", "knn", "file_path_sans_ext","path",
                    "svm.model.wrist", "svm.model.hip", "predict", "tuned.wrist", "tuned.hip"))
parLapply(cl, filenames, SVM_twoStreams, logPath)
stopCluster(cl)
