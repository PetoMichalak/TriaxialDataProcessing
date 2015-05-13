# kNN - module will use extracted features from two data streams 
# to perform k nearest neighbours classification
# program will run classification routines to determine the optimal configuration

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
kNN_classifiers = c(3,5,7,11,13,17,19,23)
# ===

# log file
logPath = paste(path, "/partialResult.log", sep="")
write("filename,source,kNN",file=logPath, append=FALSE)

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
  log = paste(wristDataPath, "wrist", kNN_classifiers[match(maxWristSQ, testWristSQ)], sep=",")
  write(log, file=logPath, append=TRUE)
} else {
  cat("Hip stream - kNN(", kNN_classifiers[match(maxHipSQ, testHipSQ)], ") performs the best - ", maxHipSQ, "\n")
  fit.knn <- knn(trainHip[,!(names(trainHip) %in% drops)], testHip[,!(names(testHip) %in% drops)], 
                 factor(trainHip[,"activity"]), k = match(maxHipSQ, testHipSQ), prob=FALSE)
  log = paste(hipDataPath, "hip", kNN_classifiers[match(maxHipSQ, testHipSQ)], sep=",")
  write(log, file=logPath, append=TRUE)
}

# predictions
pred <- as.numeric(fit.knn)

# create output dir
dir.create(file.path(path, "kNN"), showWarnings = FALSE)

# save the graph
if (hipBetter) {
  pdf(paste(path,"/kNN/", file_path_sans_ext(hipDataPath),"_class_features",(sum(filterTest)-2),".pdf",sep=""))
} else {
  pdf(paste(path,"/kNN/", file_path_sans_ext(wristDataPath),"_class_features",(sum(filterTest)-2),".pdf",sep=""))
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
          paste(path,"/kNN/",file_path_sans_ext(wristDataPath),"_",sum(filterTest),"_prediction.csv",sep=""), 
          row.names=FALSE)

write.csv(outputHip, 
          paste(path,"/kNN/",file_path_sans_ext(hipDataPath),"_",sum(filterTest),"prediction.csv",sep=""), 
          row.names=FALSE)