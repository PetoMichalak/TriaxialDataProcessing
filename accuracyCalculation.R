# library for kNN
require(class)
# library for conf matrix
require(caret)

# set your own working directory
setwd("/home/pet5o/Dropbox/uni/09-csc8625-GroupProject/dataEvaluation/annotated")

# load train data
trainData = read.csv("hip/hipTraining.csv")
# load test data
testData = read.csv("RawaHip.csv")
# define annotation for test data
annotation = testData$activity

confMatrix <- getConfMatrix(testData$intensity, annotation, trainData)

# returns a confusion matrix
getConfMatrix = function(testData, annotation, trainData) {
  fit.knn <- knn(data.frame(trainData$intensity, 1), data.frame(testData, 1), 
                 factor(trainData$activity), k = 3, prob=TRUE)
  # build confusion matrix
  # confusionMatrix(fit.knn, annotation)
  
  # do it manually
  confMatrix = matrix(rep(0, 9), nrow = 3, ncol = 3)
  # to be able to add fill up confMatrix
  fit.knn.numeric = as.numeric(fit.knn)
  # print(fit.knn.numeric)
  
  # to match the levels
  annotation = annotation + 1
  # print(annotation)
  for(i in 1:length(fit.knn.numeric)) {
    confMatrix[fit.knn.numeric[i],annotation[i]] = confMatrix[fit.knn.numeric[i],annotation[i]] + 1
  }
  return(confMatrix)
}


