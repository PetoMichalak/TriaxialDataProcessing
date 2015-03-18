# calculating our own euclidian statistics
require(GENEAread)

# our own euclidian statistics
# datafile="/home/pet5o/workspace/TDP/WednesdayData/resting/data/left_hip.bin"
datafile="C:/Users/localadmin/Documents/Projects/BBetter/TDP/testingWednesday/A_020091_2015-03-11 12-18-13.bin"
datafile="C:/Users/localadmin/Documents/Projects/BBetter/TDP/testingWednesday/A_020164_2015-03-11 12-21-03.bin"
datafile="C:/Users/localadmin/Documents/Projects/BBetter/TDP/data/005_pet5o__015800_2015-03-03_14-16-07.bin"

# training sets
datafile="/home/pet5o/workspace/TDP/ThreeTrainingSets/data/Jack_002_right_wrist_020164_2015-03-10 17-50-10.bin"
datafile="/home/pet5o/workspace/TDP/ThreeTrainingSets/data/Jack_002_left_hip_020091_2015-03-10 18-28-16.bin"
datafile="/home/pet5o/workspace/TDP/ThreeTrainingSets/data/Peter_003_right wrist_015800_2015-03-10 18-30-03.bin"
datafile="/home/pet5o/workspace/TDP/ThreeTrainingSets/data/Peter_003_left hip_020088_2015-03-10 18-40-35.bin"

# training set
datafileHip="C:/Users/localadmin/Documents/Projects/BBetter/TDP/officialTests/Simon_001_left hip_020097_2015-03-10 17-54-46.bin"
datafileWrist="C:/Users/localadmin/Documents/Projects/BBetter/TDP/officialTests/Simon_001_right wrist_020163_2015-03-10 17-58-54.bin"
# number of seconds for the output
SPLIT_INTERVAL=5
# sampling frequency
FREQUENCY=100
# set boundary for the inputted data
SET_BOUNDARY=8

# read data
data=read.bin(datafile)
dataHip=read.bin(datafileHip)
dataWrist=read.bin(datafileWrist)
# fist timestamp
as.POSIXct(data$data.out[1,1], origin="1970-01-01")
# last timestamp
as.POSIXct(data$data.out[nrow(data$data.out),1], origin="1970-01-01")

# calculate statistical summaries for every splitInterval
as.POSIXct(data$data.out[490000,1], origin="1970-01-01")
dataSnippet=data$data.out[5953000:6032000,]
counter=1
max_interval=SPLIT_INTERVAL * FREQUENCY
frameCount = floor(nrow(dataSnippet)/max_interval)
tempED = rep(NA, frameCount)
boundaryConstant = sqrt(3 * SET_BOUNDARY*SET_BOUNDARY)

Sys.time()
# vectorized ED calc
for (i in 1:frameCount) {
  startIndex = 1 + max_interval * (i - 1)
  endIndex = i * max_interval
  tempDF = dataSnippet[startIndex:endIndex,]
  # normalized Euclidian distance  
  tempED[i] = mean(sqrt(tempDF[,2]*tempDF[,2] + tempDF[,3]*tempDF[,3] + tempDF[,4]*tempDF[,4]) / 
                     boundaryConstant)
}
Sys.time()

plot(tempED, xlab = "Time", ylab="Intensity", type="l", main="Rawa test set - hip")
abline(v=49, col=2)
abline(v=99, col=2)
text(20, 0.09, "walk - no resistance", cex = .8, col=2)
text(70, 0.09, "walk - suitcase", cex = .8, col=2)
text(115, 0.09, "walk - ball", cex = .8, col=2)

# annotate the data in CSV
require(class)
df = data.frame(intensity=tempED,activity=0)
write.csv(df, "PeterHip.csv", row.names=FALSE)
# manual classification modification 0.75;0.8;0.1
# df = read.csv(file = "/home/pet5o/Dropbox/uni/09-csc8625-GroupProject/dataEvaluation/annotated/PeterHip.csv")
df = read.csv(file = "C:/Users/localadmin/Dropbox/uni/09-csc8625-GroupProject/dataEvaluation/annotated/PeterHip.csv")

write.csv(tempED, "RawaHip.csv", row.names=FALSE)

fit.knn <- knn(data.frame(df$intensity,1), data.frame(tempED,1), factor(df$activity), k = 3, prob=TRUE)
summary(fit.knn)
points(x=1:158,y=rep(0.072,158),col=fit.knn[1:158], pch=16)
abline(v=126, col = 3)
hist(tempED, breaks = 25)

# smoothing
Average = (c(cumsum(tempED), 0,0,0,0,0) - 
           c(0,0,0,0,0,cumsum(tempED)))/5

Average = Average[3:(length(Average)-5)]
plot(tempED, xlab = "Time", ylab="Intensity", type="l", main="Smoothing - hip")
lines(Average, col = 2)

# test
data = c(1,1.1,2.2,1.8,5,6.3,3.2,4.2)
testdata = c(1.5,5.3)
fit.knn <- knn(data.frame(data,1), data.frame(testdata,1), factor(c(1,1,1,1,2,2,2,2)), k = 3, prob = TRUE)
fit.knn

# normalized Euclidian distance
calcEuclidian = function(x,y,z,boundary=1) {
  sqrt((x*x) + (y*y) + (z*z)) / sqrt(3 * boundary*boundary)
}
  
  