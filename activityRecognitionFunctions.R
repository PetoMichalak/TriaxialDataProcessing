# a collection of activity recognition functions

# returns a statistic summaries for given data stream
# must be a valid GENEActiv binary format
getStatSummary = function(dataStream) {
  require(GENEAread)
  data=read.bin(dataStream)
  
  # number of seconds for the output
  SPLIT_INTERVAL=5
  # sampling frequency
  FREQUENCY=100
  # set boundary for the inputted data
  SET_BOUNDARY=8
  
  # get ready for statistics summary calculation
  counter=1
  max_interval=SPLIT_INTERVAL * FREQUENCY
  frameCount = floor(nrow(dataSnippet)/max_interval)
  # data holder for statistics summary
  statsSummary = rep(NA, frameCount)
  boundaryConstant = sqrt(3 * SET_BOUNDARY*SET_BOUNDARY)
  
  # vectorized ED calc
  for (i in 1:frameCount) {
    startIndex = 1 + max_interval * (i - 1)
    endIndex = i * max_interval
    tempDF = dataSnippet[startIndex:endIndex,]
    # normalized Euclidian distance  
    statsSummary[i] = mean(sqrt(tempDF[,2]*tempDF[,2] + tempDF[,3]*tempDF[,3] + tempDF[,4]*tempDF[,4]) / 
                           boundaryConstant)
  }
  return(statsSummary)
}

# runs kNN on given data and returns stream quality
getStreamQuality = function(trainData, testData, k = 3) {
  fit.knn <- knn(data.frame(trainData$intensity, 1), data.frame(testData, 1), 
                 factor(trainData$activity), k = k, prob=TRUE)
  # return mean of individual classification probabilities
  return(mean(attributes(fit.knn)$prob))
}

# returns a confusion matrix
getConfMatrix = function(fit, annotation, size = 3) {
  confMatrix = matrix(rep(0, size*size), nrow = size, ncol = size)
  # to be able to add fill up confMatrix
  fit.numeric = as.numeric(fit)
  
  # to match the levels
  annotation = annotation + 1
  for(i in 1:length(fit.numeric)) {
    confMatrix[fit.numeric[i], annotation[i]] = confMatrix[fit.numeric[i],annotation[i]] + 1
  }
  return(confMatrix)
}

# calculates accuracy for each class
getAccuracy = function(confMatrix) {
  acc = rep(0,ncol(confMatrix))
  for (i in 1:ncol(confMatrix)) {
    # true positive
    TP = confMatrix[i,i]
    tempCM = confMatrix
    tempCM[i,i] = 0
    # false positive
    FP = sum(tempCM[,i])
    # false negative
    FN = sum(tempCM[i,])
    tempCM[i,] = 0
    tempCM[,i] = 0
    # true negative
    TN = sum(tempCM)
    # accuracy
    acc[i] = (TP + TN) / (TP + FP + FN + TN)
  }
  return(acc)
}

# calculates precision for each class
getPrecision = function(confMatrix) {
  p = rep(0,ncol(confMatrix))
  for (i in 1:ncol(confMatrix)) {
    # true positive
    TP = confMatrix[i,i]
    tempCM = confMatrix
    tempCM[i,i] = 0
    # false positive
    FP = sum(tempCM[,i])
    # precision
    p[i] = TP / (TP + FP)
  }
  return(p)
}

# calculates recall for each class
getRecall = function(confMatrix) {
  r = rep(0,ncol(confMatrix))
  for (i in 1:ncol(confMatrix)) {
    # true positive
    TP = confMatrix[i,i]
    tempCM = confMatrix
    tempCM[i,i] = 0
    # false negative
    FN = sum(tempCM[i,])
    # accuracy
    r[i] = TP / (TP + FN)
  }
  return(r)
}

# calculates F-measure for each class
getFmeasure = function(confMatrix) {
  fMeasure = rep(0,ncol(confMatrix))
  for (i in 1:ncol(confMatrix)) {
    # true positive
    TP = confMatrix[i,i]
    tempCM = confMatrix
    tempCM[i,i] = 0
    # false positive
    FP = sum(tempCM[,i])
    # false negative
    FN = sum(tempCM[i,])
    # accuracy
    fMeasure[i] = (2 * TP) / (2 * TP + FP + FN)
  }
  return(fMeasure)
}