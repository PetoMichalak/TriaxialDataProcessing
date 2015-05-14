# kNN cross validation
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

factor(trainWrist[,"activity"])
for (k in kNN_classifiers) {
  print(k)
  print(knn.cv(trainWrist[, -c(1,2)], factor(trainWrist[,"activity"]), k = k, prob = FALSE))
}
