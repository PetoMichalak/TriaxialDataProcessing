# FFT for 5 sec windows

# read a bin file with triaxial data
require(GENEAread)
# library for kNN
require(class)
# library for fft
require(stats)

# set to your own path
# datapath = "C:/Users/localadmin/Documents/Projects/BBetter/TDP/officialTests/"
datapath = "/home/pet5o/workspace/TDP/data/ThreeTrainingSets/data"
# change working directory 
setwd(datapath)

trainDataPeterHip = "Peter_003_left hip_020088_2015-03-10 18-40-35.bin"

# calculate statistical summaries for every splitInterval
# (manually input start and end index)
data=read.bin(trainDataPeterHip)
dataSnippet=data$data.out[325000:620000,]

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

# cs is the vector of complex points to convert
convert.fft <- function(cs, sample.rate=1) {
  cs <- cs / length(cs) # normalize
  
  distance.center <- function(c)signif( Mod(c),        4)
  angle           <- function(c)signif( 180*Arg(c)/pi, 3)
  
  df <- data.frame(cycle    = 0:(length(cs)-1),
                   freq     = 0:(length(cs)-1) * sample.rate / length(cs),
                   strength = sapply(cs, distance.center),
                   delay    = sapply(cs, angle))
  df
}

convert.fft(fft(tempDF[,2]))
