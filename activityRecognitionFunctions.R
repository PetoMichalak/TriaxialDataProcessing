# a collection of activity recognition functions
require(class)

SET_BOUNDARY = 8
BOUNDARY_CONSTANT = sqrt(3 * SET_BOUNDARY*SET_BOUNDARY)

# returns best classification for the inputted data stream
# classification based on only one feature
maxClassAccuracy = function(hipTrainFile, wristTrainFile, hipTestStream, fftFeatures = 5,
                            wristTestStream, testAnnotation = c()) {
  # allows for FFT filtering of training data
  filter = c(TRUE, TRUE, rep(TRUE,fftFeatures), rep(FALSE,15-fftFeatures), 
             rep(TRUE,fftFeatures), rep(FALSE,15-fftFeatures), rep(TRUE,fftFeatures), 
             rep(FALSE,15-fftFeatures), rep(TRUE,fftFeatures), rep(FALSE,15-fftFeatures), 
             rep(TRUE,fftFeatures), rep(FALSE,15-fftFeatures), rep(TRUE,fftFeatures), 
             rep(FALSE,15-fftFeatures), rep(TRUE,fftFeatures), rep(FALSE,15-fftFeatures), 
             rep(TRUE,fftFeatures), rep(FALSE,15-fftFeatures), rep(TRUE,fftFeatures), 
             rep(FALSE,15-fftFeatures), rep(TRUE,fftFeatures), rep(FALSE,15-fftFeatures), 
             rep(TRUE,fftFeatures), rep(FALSE,15-fftFeatures), rep(TRUE,fftFeatures), 
             rep(FALSE,15-fftFeatures), rep(TRUE,fftFeatures), rep(FALSE,15-fftFeatures), 
             rep(TRUE,fftFeatures), rep(FALSE,15-fftFeatures), rep(TRUE,fftFeatures), rep(FALSE,15-fftFeatures))
  
  # load training data
  trainHip = read.csv(hipTrainFile)[,filter]
  trainWrist = read.csv(wristTrainFile)[,filter]
  
  # partition the data
  p = data$data.out[1:500,]
  
  # feature extraction
  testHipFeatures = featureExtraction(p, fftCount = fftFeatures)
  testWristFeatures = featureExtraction(wristTestStream)  
  
  # calculate stream quality for given data
  kNN = c(3,5,7,11,13,17,19)
  testHipSQ = rep(0, length(kNN))
  testWristSQ = rep(0, length(kNN))
  
  # loop through all kNN options
  for(i in 1:length(kNN)) {
    cat("kNN: ", kNN[i])
    testHipSQ[i] = getStreamQuality_df(trainHip[,-1], trainHip[,1], testHipSS, kNN[i])
    cat("; Hip: ", testHipSQ[i])
    testWristSQ[i] = getStreamQuality_df(trainWrist[,-1], trainWrist[,1], testWristSS, kNN[i])
    cat("; Wrist: ", testWristSQ[i], "\n")
  }
  
  # get the highest stream quality
  maxHipSQ = max(testHipSQ)
  maxWristSQ = max(testWristSQ)
  
  # make assumption that hip has better SQ; get final prediction at the same time
  hipBetter = TRUE
  fit.knn <- c()
  if (maxHipSQ < maxWristSQ) { 
    hipBetter = FALSE 
    cat("Wrist stream - kNN(", match(maxWristSQ, testWristSQ), ") performs the best - ", maxWristSQ, "\n")
    fit.knn <- knn(trainWrist[,-1], testWristSS, 
                   factor(trainWrist[,1]), k = match(maxWristSQ, testWristSQ), prob=FALSE)
  } else {
    cat("Hip stream - kNN(", kNN[match(maxHipSQ, testHipSQ)], ") performs the best - ", maxHipSQ, "\n")
    fit.knn <- knn(trainHip[,-1], testHipSS, 
                   factor(trainHip[,1]), k = match(maxHipSQ, testHipSQ), prob=FALSE)
  }
  
  # predictions
  pred <- as.numeric(fit.knn)
  
  # produce plots of given data with the best prediction
  par(mfrow=c(2,1))
  # TODO - nasty ylim constants - should really set it dynamically
  plot(testWristSS$intensity, xlab = "Time", ylab="Intensity", type="l", main="Wrist data", ylim=c(0.07,0.09))
  points(x=1:length(pred),y=rep(0.07,length(pred)),col=pred, pch=16)
  plot(testHipSS$intensity, xlab = "Time", ylab="Intensity", type="l", main="Hip data", ylim=c(0.07,0.09))
  points(x=1:length(pred),y=rep(0.07,length(pred)),col=pred, pch=16)
  
  # provide statitics summaries of a model if test annotation provided
  if (length(testAnnotation) == 0) { return(fit.knn) }
  confMatrix = getConfMatrix(fit.knn, testAnnotation)
  print("===Accuracy measures for best model===")
  print("Confusion matrix: ")
  print(confMatrix)
  cat("Accuracy: ", getAccuracy(confMatrix))
  cat("\nF-measure: ", getFmeasure(confMatrix))
  cat("\nPrecision: ", getPrecision(confMatrix))
  cat("\nRecall: ", getRecall(confMatrix), "\n")
  print("======================================")
  
  # return best kNN model
  return(fit.knn)
}

# === FEATURE EXTRACTION ===
# extracts FFT from given dataset
extractFFT = function(data, n = 15, sampling_rate = FREQUENCY) {
  number_of_runs = length(data)/sampling_rate
  fft.extract = rep(NA, (number_of_runs * n))
  # extract n highest FFTs; ignore the first one as we handle it with stat summary (static zero)
  for (i in 1:number_of_runs) {
    # data index
    startIndex = i + (i-1) * sampling_rate - (i - 1)
    # result index
    fftStart = i + (i-1) * n - (i - 1)
    fft.extract[fftStart:(i*n)] = fft.profile(data[startIndex:(i*sampling_rate)], n + 1)[-1]
  }
  return(fft.extract)
}

# creates a colnames for fft matrix
getColNames = function(seconds, points) {
  colNames = c()
  for (i in 1:seconds) {
    colNames = c(colNames, paste("fft_",i,"x",1:points, sep=""))
  }
  for (i in 1:seconds) {
    colNames = c(colNames, paste("fft_",i,"y",1:points, sep=""))
  }
  for (i in 1:seconds) {
    colNames = c(colNames, paste("fft_",i,"z",1:points, sep=""))
  }
  return(colNames)
}

# calculates normalized euclidian distance
# calculates FFT and returns given number of features
featureExtraction = function(data, fftCount = 25) {
  statsSummary = mean(sqrt(data[,2]*data[,2] + data[,3]*data[,3] + data[,4]*data[,4]) / BOUNDARY_CONSTANT)
  fft.data = calcFFT(data, fftCount)
  return(c(statsSummary, fft.data))
}

# calculates FFT for x,y,z - awfully hardcoded just to see if it works
# n - number of high freq to take from the second
calcFFT = function(data, n = 15) {
  fft.summary = rep(NA, n * 3 * 5)
  # 1 second intervals for x
  fft.summary[1:n] = fft.profile(data[,2][1:100], n)
  fft.summary[(n+1):(2*n)] = fft.profile(data[,2][101:200], n)
  fft.summary[(2*n+1):(3*n)] = fft.profile(data[,2][201:300], n)
  fft.summary[(3*n+1):(4*n)] = fft.profile(data[,2][301:400], n)
  fft.summary[(4*n+1):(5*n)] = fft.profile(data[,2][401:500], n)
  
  # 1 second intervals for y
  fft.summary[(5*n+1):(6*n)] = fft.profile(data[,3][1:100], n)
  fft.summary[(6*n+1):(7*n)] = fft.profile(data[,3][101:200], n)
  fft.summary[(7*n+1):(8*n)] = fft.profile(data[,3][201:300], n)
  fft.summary[(8*n+1):(9*n)] = fft.profile(data[,3][301:400], n)
  fft.summary[(9*n+1):(10*n)] = fft.profile(data[,3][401:500], n)
  
  # 1 second intervals for z
  fft.summary[(10*n+1):(11*n)] = fft.profile(data[,4][1:100], n)
  fft.summary[(11*n+1):(12*n)] = fft.profile(data[,4][101:200], n)
  fft.summary[(12*n+1):(13*n)] = fft.profile(data[,4][201:300], n)
  fft.summary[(13*n+1):(14*n)] = fft.profile(data[,4][301:400], n)
  fft.summary[(14*n+1):(15*n)] = fft.profile(data[,4][401:500], n)
  return(fft.summary)
}

# extracts n highest frequencies from the data
fft.profile <- function (dataset, n)
{
  fft.data <- fft(dataset)
  amplitude <- Mod(fft.data[1:(length(fft.data)/2)])
  frequencies <- seq(0, 100, length.out=length(fft.data)/2)
  # plot(amplitude ~ frequencies, t="l", xlim=c(0,100), ylim=c(0,20))
  sorted <- sort.int(amplitude, decreasing=TRUE, index.return=TRUE)
  top <- sorted$ix[1:n] # indexes of the largest n components
  return (frequencies[top]) # convert indexes to frequencies
}

# extracts features from the datastream
# normalized Euclidian distance, fft
partitionData = function(Stream, windowsSize = 500) {
  require(GENEAread)
  data=read.bin(dataStream)
  
  # partition input data
  windowCount = floor(nrow(data)/windowSize)
  partitions = rep(NA, windowCount)
  for (i in 1:windowCount) {
    startIndex = 1 + windowSize * (i - 1)
    endIndex = i * windowSize
    partitions[i] = data[startIndex:endIndex,]
  }
  return(partitions)
}

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

# runs kNN on given data and returns stream quality (train/test data are data frames)
getStreamQuality_df = function(trainData, trainAnnotation, testData, k = 3) {
  fit.knn <- knn(trainData, testData, factor(trainAnnotation), k = k, prob=TRUE)
  # return mean of individual classification probabilities
  return(mean(attributes(fit.knn)$prob))
}

# runs kNN on given data and returns stream quality (train/test data is only vector)
getStreamQuality_vector = function(trainData, testData, k = 3) {
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