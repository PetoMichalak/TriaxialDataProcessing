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