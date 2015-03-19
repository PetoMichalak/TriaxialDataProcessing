# calculating our own euclidian statistics
require(GENEAread)

# set to your own path
# datapath = "C:/Users/localadmin/Documents/Projects/BBetter/TDP/officialTests/"
datapath = "/home/pet5o/workspace/TDP/ThreeTrainingSets/data"

# change working directory 
setwd(datapath)

# all training sets
trainDataJackWrist = "Jack_002_right_wrist_020164_2015-03-10 17-50-10.bin"
trainDataJackHip = "Jack_002_left_hip_020091_2015-03-10 18-28-16.bin"
trainDataPeterWrist = "Peter_003_right wrist_015800_2015-03-10 18-30-03.bin"
trainDataPeterHip = "Peter_003_left hip_020088_2015-03-10 18-40-35.bin"
trainDataSimonWrist = "Simon_001_right wrist_020163_2015-03-10 17-58-54.bin"
trainDataSimonHip = "Simon_001_left hip_020097_2015-03-10 17-54-46.bin"

# calculate statistical summaries for every splitInterval
# (manually input start and end index)
data=read.bin(trainDataPeterWrist)
dataSnippet=data$data.out[8920000:9228000,]

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

# plot summary
plot(tempED[128:250], xlab = "Time", ylab="Intensity", type="l", main="Peter train wrist")
# save it to the file
write.csv(data.frame(intensity=tempED[90:128],activity="1"), 
          "010_wrist_walking_suitcase.csv", row.names=FALSE)

# kNN fitting - supplied arguments must be at least 2 dimensional data
# knn(train data, test data, annotation, k, whether to calculate probababilities)
fit.knn <- knn(data.frame(df$intensity,1), data.frame(tempED,1), factor(df$activity), k = 3, prob=TRUE)
summary(fit.knn)

# visualize the classification
points(x=1:158,y=rep(0.072,158),col=fit.knn[1:158], pch=16)
# histogram
hist(tempED, breaks = 25)

# smoothing
Average = (c(cumsum(tempED), 0,0,0,0,0) - 
           c(0,0,0,0,0,cumsum(tempED)))/5

Average = Average[3:(length(Average)-5)]
plot(tempED, xlab = "Time", ylab="Intensity", type="l", main="Smoothing - hip")
lines(Average, col = 2)  