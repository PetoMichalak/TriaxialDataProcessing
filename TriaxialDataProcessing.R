# calculating our own euclidian statistics
require(GENEAread)

# set to your own path
# datapath = "C:/Users/localadmin/Documents/Projects/BBetter/TDP/officialTests/"
datapath = "/home/pet5o/workspace/TDP/data/ThreeTrainingSets/data"
# datapath = "/home/pet5o/workspace/TDP/data/WednesdayData/data"
# datapath = "/home/pet5o/workspace/TDP/data/15-03-23"

# change working directory 
setwd(datapath)

# all training sets
trainDataJackWrist = "Jack_002_right_wrist_020164_2015-03-10 17-50-10.bin"
trainDataJackHip = "Jack_002_left_hip_020091_2015-03-10 18-28-16.bin"
trainDataPeterWrist = "Peter_003_right wrist_015800_2015-03-10 18-30-03.bin"
trainDataPeterHip = "Peter_003_left hip_020088_2015-03-10 18-40-35.bin"
trainDataSimonWrist = "Simon_001_right wrist_020163_2015-03-10 17-58-54.bin"
trainDataSimonHip = "Simon_001_left hip_020097_2015-03-10 17-54-46.bin"

testRawaWrist = "A_020164_2015-03-11 12-21-03.bin"
testRawaHip = "A_020091_2015-03-11 12-18-13.bin"

freeDataPeter="Peter_right wrist_015800_2015-03-23 10-20-36.bin"

# calculate statistical summaries for every splitInterval
# (manually input start and end index)
data=read.bin(trainDataPeterWrist)
dataSnippet=data$data.out[17190000:17460000,]

# number of seconds for the output
SPLIT_INTERVAL=5
# sampling frequency
FREQUENCY=100
# set boundary for the inputted data
SET_BOUNDARY=8
# number of highest frequencies to take from FFT
TOP_FREQ = 15

# get ready for statistics summary calculation
counter=1
max_interval=SPLIT_INTERVAL * FREQUENCY
frameCount = floor(nrow(dataSnippet)/max_interval)
# data holder for statistics summary
statsSummary = data.frame(activity = 1, intensity = rep(NA, frameCount), 
           fft_x_1_1 = 0, fft_x_1_2 = 0, fft_x_1_3 = 0, fft_x_1_4 = 0, fft_x_1_5 = 0, fft_x_1_6 = 0, fft_x_1_7 = 0, fft_x_1_8 = 0, fft_x_1_9 = 0, fft_x_1_10 = 0, fft_x_1_11 = 0, fft_x_1_12 = 0, fft_x_1_13 = 0, fft_x_1_14 = 0, fft_x_1_15 = 0,
           fft_x_2_1 = 0, fft_x_2_2 = 0, fft_x_2_3 = 0, fft_x_2_4 = 0, fft_x_2_5 = 0, fft_x_2_6 = 0, fft_x_2_7 = 0, fft_x_2_8 = 0, fft_x_2_9 = 0, fft_x_2_10 = 0, fft_x_2_11 = 0, fft_x_2_12 = 0, fft_x_2_13 = 0, fft_x_2_14 = 0, fft_x_2_15 = 0,
           fft_x_3_1 = 0, fft_x_3_2 = 0, fft_x_3_3 = 0, fft_x_3_4 = 0, fft_x_3_5 = 0, fft_x_3_6 = 0, fft_x_3_7 = 0, fft_x_3_8 = 0, fft_x_3_9 = 0, fft_x_3_10 = 0, fft_x_3_11 = 0, fft_x_3_12 = 0, fft_x_3_13 = 0, fft_x_3_14 = 0, fft_x_3_15 = 0,
           fft_x_4_1 = 0, fft_x_4_2 = 0, fft_x_4_3 = 0, fft_x_4_4 = 0, fft_x_4_5 = 0, fft_x_4_6 = 0, fft_x_4_7 = 0, fft_x_4_8 = 0, fft_x_4_9 = 0, fft_x_4_10 = 0, fft_x_4_11 = 0, fft_x_4_12 = 0, fft_x_4_13 = 0, fft_x_4_14 = 0, fft_x_4_15 = 0,
           fft_x_5_1 = 0, fft_x_5_2 = 0, fft_x_5_3 = 0, fft_x_5_4 = 0, fft_x_5_5 = 0, fft_x_5_6 = 0, fft_x_5_7 = 0, fft_x_5_8 = 0, fft_x_5_9 = 0, fft_x_5_10 = 0, fft_x_5_11 = 0, fft_x_5_12 = 0, fft_x_5_13 = 0, fft_x_5_14 = 0, fft_x_5_15 = 0,
           fft_y_1_1 = 0, fft_y_1_2 = 0, fft_y_1_3 = 0, fft_y_1_4 = 0, fft_y_1_5 = 0, fft_y_1_6 = 0, fft_y_1_7 = 0, fft_y_1_8 = 0, fft_y_1_9 = 0, fft_y_1_10 = 0, fft_y_1_11 = 0, fft_y_1_12 = 0, fft_y_1_13 = 0, fft_y_1_14 = 0, fft_y_1_15 = 0,
           fft_y_2_1 = 0, fft_y_2_2 = 0, fft_y_2_3 = 0, fft_y_2_4 = 0, fft_y_2_5 = 0, fft_y_2_6 = 0, fft_y_2_7 = 0, fft_y_2_8 = 0, fft_y_2_9 = 0, fft_y_2_10 = 0, fft_y_2_11 = 0, fft_y_2_12 = 0, fft_y_2_13 = 0, fft_y_2_14 = 0, fft_y_2_15 = 0,
           fft_y_3_1 = 0, fft_y_3_2 = 0, fft_y_3_3 = 0, fft_y_3_4 = 0, fft_y_3_5 = 0, fft_y_3_6 = 0, fft_y_3_7 = 0, fft_y_3_8 = 0, fft_y_3_9 = 0, fft_y_3_10 = 0, fft_y_3_11 = 0, fft_y_3_12 = 0, fft_y_3_13 = 0, fft_y_3_14 = 0, fft_y_3_15 = 0,
           fft_y_4_1 = 0, fft_y_4_2 = 0, fft_y_4_3 = 0, fft_y_4_4 = 0, fft_y_4_5 = 0, fft_y_4_6 = 0, fft_y_4_7 = 0, fft_y_4_8 = 0, fft_y_4_9 = 0, fft_y_4_10 = 0, fft_y_4_11 = 0, fft_y_4_12 = 0, fft_y_4_13 = 0, fft_y_4_14 = 0, fft_y_4_15 = 0,
           fft_y_5_1 = 0, fft_y_5_2 = 0, fft_y_5_3 = 0, fft_y_5_4 = 0, fft_y_5_5 = 0, fft_y_5_6 = 0, fft_y_5_7 = 0, fft_y_5_8 = 0, fft_y_5_9 = 0, fft_y_5_10 = 0, fft_y_5_11 = 0, fft_y_5_12 = 0, fft_y_5_13 = 0, fft_y_5_14 = 0, fft_y_5_15 = 0,
           fft_z_1_1 = 0, fft_z_1_2 = 0, fft_z_1_3 = 0, fft_z_1_4 = 0, fft_z_1_5 = 0, fft_z_1_6 = 0, fft_z_1_7 = 0, fft_z_1_8 = 0, fft_z_1_9 = 0, fft_z_1_10 = 0, fft_z_1_11 = 0, fft_z_1_12 = 0, fft_z_1_13 = 0, fft_z_1_14 = 0, fft_z_1_15 = 0,
           fft_z_2_1 = 0, fft_z_2_2 = 0, fft_z_2_3 = 0, fft_z_2_4 = 0, fft_z_2_5 = 0, fft_z_2_6 = 0, fft_z_2_7 = 0, fft_z_2_8 = 0, fft_z_2_9 = 0, fft_z_2_10 = 0, fft_z_2_11 = 0, fft_z_2_12 = 0, fft_z_2_13 = 0, fft_z_2_14 = 0, fft_z_2_15 = 0,
           fft_z_3_1 = 0, fft_z_3_2 = 0, fft_z_3_3 = 0, fft_z_3_4 = 0, fft_z_3_5 = 0, fft_z_3_6 = 0, fft_z_3_7 = 0, fft_z_3_8 = 0, fft_z_3_9 = 0, fft_z_3_10 = 0, fft_z_3_11 = 0, fft_z_3_12 = 0, fft_z_3_13 = 0, fft_z_3_14 = 0, fft_z_3_15 = 0,
           fft_z_4_1 = 0, fft_z_4_2 = 0, fft_z_4_3 = 0, fft_z_4_4 = 0, fft_z_4_5 = 0, fft_z_4_6 = 0, fft_z_4_7 = 0, fft_z_4_8 = 0, fft_z_4_9 = 0, fft_z_4_10 = 0, fft_z_4_11 = 0, fft_z_4_12 = 0, fft_z_4_13 = 0, fft_z_4_14 = 0, fft_z_4_15 = 0,
           fft_z_5_1 = 0, fft_z_5_2 = 0, fft_z_5_3 = 0, fft_z_5_4 = 0, fft_z_5_5 = 0, fft_z_5_6 = 0, fft_z_5_7 = 0, fft_z_5_8 = 0, fft_z_5_9 = 0, fft_z_5_10 = 0, fft_z_5_11 = 0, fft_z_5_12 = 0, fft_z_5_13 = 0, fft_z_5_14 = 0, fft_z_5_15 = 0)
boundaryConstant = sqrt(3 * SET_BOUNDARY*SET_BOUNDARY)

# vectorized ED calc
for (i in 1:frameCount) {
  startIndex = 1 + max_interval * (i - 1)
  endIndex = i * max_interval
  tempDF = dataSnippet[startIndex:endIndex,]
  # extract FFT from the 5 sec interval
  statsSummary[i,][3:dim(statsSummary)[2]] = calcFFT(tempDF, TOP_FREQ)
  # normalized Euclidian distance  
  statsSummary[i,][2] = mean(sqrt(tempDF[,2]*tempDF[,2] + tempDF[,3]*tempDF[,3] + tempDF[,4]*tempDF[,4]) / 
                         boundaryConstant)
}

# plot summary
plot(statsSummary[,2][42100:42500], xlab = "Time", ylab="Intensity", type="l", main="Peter free run")
# save it to the file
write.csv(statsSummary[42100:42500,], "Peter_rest_SleepData.csv", row.names=FALSE)

# kNN fitting - supplied arguments must be at least 2 dimensional data
# knn(train data, test data, annotation, k, whether to calculate probababilities)
fit.knn <- knn(data.frame(df$intensity,1), data.frame(statsSummary,1), factor(df$activity), k = 3, prob=TRUE)
fit.knn <- knn(data.frame(trainDataWrist$intensity, 1), data.frame(statsSummary, 1), factor(trainDataWrist$activity), k=3, prob=TRUE)
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

# raw data collection
rawData = data.frame()
plot(dataSnippet[,2][186000:201000], type="l")
rawData = cbind(dataSnippet[186000:201000,], activity = 0)
head(rawData)
nrow(rawData)

write.csv(rawData, 
          "extractedRawData/PeterTrainWrist_rest_001.csv", 
          row.names=FALSE)
