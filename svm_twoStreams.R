# svm for two streams of triaxial data

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
fftCount = 1
# ===

# log file
logPath = paste(path, "/partialResult_svm.log", sep="")
write("filename,source,SVM",file=logPath, append=FALSE)

# setup the filter
filterTestData = c(rep(TRUE, 9), rep(c(rep(TRUE, fftCount), rep(FALSE, 15-fftCount)), 15))
filterTrainData = c(rep(TRUE, 9), rep(c(rep(TRUE, fftCount), rep(FALSE, 15-fftCount)), 15))

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

# run kNN
testWristSQ = rep(0, length(kNN_classifiers))
testHipSQ = rep(0, length(kNN_classifiers))
# drop timestamp and activity
drops = c("timestamp", "activity")

tuned <- tune.svm(activity~., data = trainHip[,!(names(trainHip) %in% c("timestamp"))], 
                  gamma = 10^(-6:-1), cost = 10^(-1:1)) 
model  <- svm(activity~., data = trainHip[,!(names(trainHip) %in% c("timestamp"))], 
              gamma=tuned$best.parameters[1,1], 
              cost=tuned$best.parameters[1,2])
prediction <- predict(model, testHip[,!(names(testHip) %in% drops)])
tab <- table(pred = prediction, true = testHip[,"activity"])
tab

trainDF$activity = factor(trainDF$activity)
svm.model <- svm(activity ~ ., data = trainDF, cost = 100, gamma = 1, probability = TRUE)
svm.pred <- predict(svm.model, testDF["statSummary"], probability = TRUE)
svm.pred

svm_SQ = rep(NA, length(svm.pred))
for (i in 1:length(svm.pred)) {
  svm_SQ[i] = attributes(svm.pred)$prob[i,as.character(svm.pred[i])]
}