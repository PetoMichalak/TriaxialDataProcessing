# kNN - module will use extracted features from two data streams 
# to perform k nearest neighbours classification
# program will run classification routines to determine the optimal configuration

# === modify to suit your needs
path = "/home/pet5o/workspace/TDP/data/Jack_weekend17-19April"
wristDataPath = "Jack_wrist_sample_annotated_features.csv"
hipDataPath = "Jack_hip_sample_annotated_features.csv"
wristTrainPath = "training/wristTrain.csv"
hipTrainPath = "training/hipTrain.csv"
# list of booleans to specify number of features to work with
filterTestData = c()
filterTrainData = c()
kNN_classifiers = c(3,5,7,11,13,17,19,23)
# ===

# load data
setwd(path)
print("Loading data")
testWrist = read.csv(wristDataPath)
testHip = read.csv(hipDataPath)
trainWrist = read.csv(wristTrainPath)
trainHip = read.csv(hipTrainPath)

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

# pick the best stream

# save the graph

# save the result