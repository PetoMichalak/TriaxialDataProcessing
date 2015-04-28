# kNN - module will use extracted features from two data streams 
# to perform k nearest neighbours classification
# program will run classification routines to determine the optimal configuration

# === modify to suit your needs
path = "/home/pet5o/workspace/TDP/DataEvaluation/pet_01"
wristDataPath = "Peter_003_right wrist_015800_2015-03-10 18-30-03_annotated_features.csv"
hipDataPath = "Peter_003_left hip_020088_2015-03-10 18-40-35_annotated_features.csv"
wristTrainPath = "/home/pet5o/workspace/TDP/DataEvaluation/pet_01/trainingSets/wrist"
hipTrainPath = "/home/pet5o/workspace/TDP/DataEvaluation/pet_01/trainingSets/hip"
# list of booleans to specify number of features to work with
filterTestData = c()
filterTrainData = c()
kNN_classifiers = c(3,5,7,11,13,17,19,23)
# ===

# load project specific libraries
source("/home/pet5o/workspace/TDP/R/group-har/activityRecognitionFunctions.R")

# load data
setwd(path)
print("Loading data")
testWrist = read.csv(file.path(path,"featureData",wristDataPath))
testHip = read.csv(file.path(path,"featureData",hipDataPath))
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
  cat("kNN: ", kNN_classifiers[i])
  testHipSQ[i] = getStreamQuality_df(trainHip[,!(names(trainHip) %in% drops)], 
                                     trainHip[,"activity"], 
                                     testHip[,!(names(testHip) %in% drops)], 
                                     kNN_classifiers[i])
  cat("; Hip: ", testHipSQ[i])
  testWristSQ[i] = getStreamQuality_df(trainWrist[,!(names(trainWrist) %in% drops)],
                                       trainWrist[,"activity"], 
                                       testWrist[,!(names(testWrist) %in% drops)], 
                                       kNN_classifiers[i])
  cat("; Wrist: ", testWristSQ[i], "\n")
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
  cat("Wrist stream - kNN(", kNN_classifiers[match(maxWristSQ, testWristSQ)], ") performs the best - ", maxWristSQ, "\n")
  fit.knn <- knn(trainWrist[,!(names(trainWrist) %in% drops)], testWrist[,!(names(testWrist) %in% drops)], 
                 factor(trainWrist[,"activity"]), k = match(maxWristSQ, testWristSQ), prob=FALSE)
} else {
  cat("Hip stream - kNN(", kNN_classifiers[match(maxHipSQ, testHipSQ)], ") performs the best - ", maxHipSQ, "\n")
  fit.knn <- knn(trainHip[,!(names(trainHip) %in% drops)], testHip[,!(names(testHip) %in% drops)], 
                 factor(trainHip[,"activity"]), k = match(maxHipSQ, testHipSQ), prob=FALSE)
}

# predictions
pred <- as.numeric(fit.knn)

# save the graph
if (hipBetter) {
  pdf(paste("kNN", file_path_sans_ext(hipDataPath),"_class_features",(sum(filterTest)-2),".pdf",sep=""))
} else {
  pdf(paste("kNN", file_path_sans_ext(wristDataPath),"_class_features",(sum(filterTest)-2),".pdf",sep=""))
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

# create output dir
dir.create(file.path(path, "kNN"), showWarnings = FALSE)

write.csv(outputWrist, 
          paste("kNN/",file_path_sans_ext(wristDataPath),"features",sum(filterTest),"_prediction.csv",sep=""), 
          row.names=FALSE)

write.csv(outputHip, 
          paste("kNN/",file_path_sans_ext(hipDataPath),"_",sum(filterTest),"prediction.csv",sep=""), 
          row.names=FALSE)