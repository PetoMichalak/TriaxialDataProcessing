# kNN - split
require(tools)

# load project specific libraries
source("/home/pet5o/workspace/TDP/R/group-har/activityRecognitionFunctions.R")
path="/home/pet5o/workspace/TDP/DataEvaluation/final_dataset_runII/fragmentedFeatureData_fft0"
# TRAINING DATA
wristTrainPath = "/home/pet5o/workspace/TDP/DataEvaluation/final_dataset_run/trainingSets/wrist"
hipTrainPath = "/home/pet5o/workspace/TDP/DataEvaluation/final_dataset_run/trainingSets/hip"

setwd(path)
# prepare partial result file
logPath = paste(path, "/partialResult.log", sep="")
write("filename,source,kNN",file=logPath, append=FALSE)
      
# performs kNN classification on two streams of triaxial data (preprocessed)
kNN_twoStreams = function(wristDataPath, wristTrainPath, hipTrainPath, logPath) {
  # === modify to suit your needs
  # path = "/home/pet5o/workspace/TDP/data/Jack_weekend17-19April"
  # wristDataPath = "Jack_wrist_sample_annotated_features.csv"
  hipDataPath = gsub("WRIST", "HIP", wristDataPath)
  
  # list of booleans to specify number of features to work with
  fftCount = 0
  filterTestData = c(rep(TRUE, 9), rep(c(rep(TRUE, fftCount), rep(FALSE, 15-fftCount)), 15))
  filterTrainData = c(rep(TRUE, 9), rep(c(rep(TRUE, fftCount), rep(FALSE, 15-fftCount)), 15))
  kNN_classifiers = c(3,5,7,11,13,17,19,23)
  # ===
  
  # load data
  print(paste("Loading data", wristDataPath))
  testWrist = read.csv(wristDataPath)
  testHip = read.csv(hipDataPath)
  trainWrist = loadTrainingData(wristTrainPath)
  trainHip = loadTrainingData(hipTrainPath)
  
  # filter features to be used - default [ALL]
  filterTest = rep(TRUE, dim(testWrist)[2])
  filterTrain = rep(TRUE, dim(trainWrist)[2])
  # if user specified filter - override default
  if (length(filterTestData)) {
    filterTest = filterTestData
  }
  if (length(filterTrainData)) {
    filterTrain = filterTrainData
  }
  testWrist = testWrist[,filterTest]
  testHip = testHip[,filterTest]
  trainWrist = trainWrist[,filterTrain]
  trainHip = trainHip[,filterTrain]
  
  # run kNN
  testWristSQ = rep(0, length(kNN_classifiers))
  testHipSQ = rep(0, length(kNN_classifiers))
  # drop timestamp and activity
  drops = c("timestamp", "activity")
  
  # loop through all kNN options
  for(i in 1:length(kNN_classifiers)) {
    # cat("kNN: ", kNN_classifiers[i])
    testHipSQ[i] = getStreamQuality_df(trainHip[,!(names(trainHip) %in% drops)], 
                                       trainHip[,"activity"], 
                                       testHip[,!(names(testHip) %in% drops)], 
                                       kNN_classifiers[i])
    # cat("; Hip: ", testHipSQ[i])
    testWristSQ[i] = getStreamQuality_df(trainWrist[,!(names(trainWrist) %in% drops)],
                                         trainWrist[,"activity"], 
                                         testWrist[,!(names(testWrist) %in% drops)], 
                                         kNN_classifiers[i])
    # cat("; Wrist: ", testWristSQ[i], "\n")
  }
  
  # pick the best stream
  # get the highest stream quality
  maxHipSQ = max(testHipSQ)
  maxWristSQ = max(testWristSQ)
  
  # make assumption that hip has better SQ; get final prediction at the same time
  hipBetter = TRUE
  fit.knn <- c()
  if (maxHipSQ < maxWristSQ) { 
    hipBetter = FALSE 
    cat(wristDataPath, "wrist stream - kNN(", kNN_classifiers[match(maxWristSQ, testWristSQ)], ") performs the best - ", maxWristSQ, "\n")
    fit.knn <- knn(trainWrist[,!(names(trainWrist) %in% drops)], testWrist[,!(names(testWrist) %in% drops)], 
                   factor(trainWrist[,"activity"]), k = match(maxWristSQ, testWristSQ), prob=FALSE)
    log = paste(wristDataPath, "wrist", kNN_classifiers[match(maxWristSQ, testWristSQ)], sep=",")
    write(log, file=logPath, append=TRUE)
  } else {
    cat(hipDataPath, "hip stream - kNN(", kNN_classifiers[match(maxHipSQ, testHipSQ)], ") performs the best - ", maxHipSQ, "\n")
    fit.knn <- knn(trainHip[,!(names(trainHip) %in% drops)], testHip[,!(names(testHip) %in% drops)], 
                   factor(trainHip[,"activity"]), k = match(maxHipSQ, testHipSQ), prob=FALSE)
    log = paste(hipDataPath, "hip", kNN_classifiers[match(maxHipSQ, testHipSQ)], sep=",")
    write(log, file=logPath, append=TRUE)    
  }
  
  # predictions
  pred <- as.numeric(fit.knn)
  
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
filenames <- list.files(path, pattern="*WRIST*", full.names=TRUE)

require(class)
require(parallel)
cl <- makeCluster(4)
clusterExport(cl, c("loadTrainingData", "getStreamQuality_df", "knn","file_path_sans_ext"))
parLapply(cl, filenames, kNN_twoStreams, wristTrainPath, hipTrainPath, logPath)
stopCluster(cl)