# svm for two streams of triaxial data
require(e1071)
require(tools)

# === modify to suit your needs
# path to a case study
path = "TestCase"
wristDataPath = "TestWrist_annotated_features.csv"
hipDataPath = "TestHip_annotated_features.csv"
# TRAINING DATA FOLDERS
wristTrainPath = "TrainData/wrist"
hipTrainPath = "TrainData/hip"
# list of booleans to specify number of features to work with
# add up to 234
fftCount = 15
# ===

# log file
logPath = paste(path, "/partialResult_svm.log", sep="")
write("filename,source,cost,gamma",file=logPath, append=FALSE)

# setup the filter
filterTestData = c(rep(TRUE, 9),
                   rep(c(rep(TRUE, fftCount), rep(FALSE, 15-fftCount)), 15))
filterTrainData = c(rep(TRUE, 9),
                    rep(c(rep(TRUE, fftCount), rep(FALSE, 15-fftCount)), 15))

# load project specific libraries
source("activityRecognitionFunctions.R")

# load data
print("Loading data")
testWrist = read.csv(file.path(path,"featureData",wristDataPath))
testHip = read.csv(file.path(path,"featureData",hipDataPath))
trainWrist = loadTrainingData(file.path(path,wristTrainPath))
trainHip = loadTrainingData(file.path(path,hipTrainPath))

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

###########
# run SVM #
###########

# Stream Quality placeholders
testWristSQ = NA
testHipSQ = NA

# Wrist stream
trainWrist$activity = factor(trainWrist$activity)

# find best cost and gamma parameters for SVM
tuned.wrist <- tune.svm(activity ~ ., data = trainWrist[,-1],
                        gamma = 10^(-10:10), cost = 10^(-3:3)) 

# train model (ignoring timestamp)
svm.model.wrist <- svm(activity ~ ., data = trainWrist[,-1], 
                       gamma=tuned.wrist$best.parameters[1,1], 
                       cost=tuned.wrist$best.parameters[1,2], 
                       probability = TRUE)

# make prediction (ignoring timestamp and annotation)
svm.pred.wrist <- predict(svm.model.wrist, testWrist[,-c(1,2)], probability = TRUE)

# calculate Stream Quality
svm_SQ = rep(NA, length(svm.pred.wrist))
for (i in 1:length(svm.pred.wrist)) {
  svm_SQ[i] = attributes(svm.pred.wrist)$prob[i,as.character(svm.pred.wrist[i])]
}
testWristSQ = mean(svm_SQ)

# Hip stream
trainHip$activity = factor(trainHip$activity)

# find best cost and gamma parameters for SVM
tuned.hip <- tune.svm(activity ~ ., data = trainHip[,-1],
                      gamma = 10^(-10:10), cost = 10^(-3:3)) 

# train model (ignoring timestamp)
svm.model.hip <- svm(activity ~ ., data = trainHip[,-1], 
                     gamma=tuned.hip$best.parameters[1,1], 
                     cost=tuned.hip$best.parameters[1,2], 
                     probability = TRUE)

# make prediction (ignoring timestamp and annotation)
svm.pred.hip <- predict(svm.model.hip, testHip[,-c(1,2)], probability = TRUE)

# calculate Stream Quality
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

# create output dir
dir.create(file.path(path, "SVM"), showWarnings = FALSE)

# save the graph
if (hipBetter) {
  pdf(paste(path,"/SVM/", file_path_sans_ext(hipDataPath),"_class_features",
            (sum(filterTest)-2),".pdf",sep=""))
} else {
  pdf(paste(path,"/SVM/", file_path_sans_ext(wristDataPath),"_class_features",
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
outputWrist = data.frame(timestamp=testWrist$timestamp, 
                         activity=testWrist$activity, prediction=pred-1)
outputHip = data.frame(timestamp=testHip$timestamp, 
                       activity=testHip$activity, prediction=pred-1)

write.csv(outputWrist, 
          paste(path,"/SVM/",file_path_sans_ext(wristDataPath),"_",sum(filterTest),"_prediction.csv",sep=""), 
          row.names=FALSE)

write.csv(outputHip, 
          paste(path,"/SVM/",file_path_sans_ext(hipDataPath),"_",sum(filterTest),"prediction.csv",sep=""), 
          row.names=FALSE)