# FFT for 5 sec windows

# read a bin file with triaxial data
require(GENEAread)
# library for kNN
require(class)
# library for fft
require(stats)
library(GeneCycle)
require(seewave)

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
  spectrum(tempDF)  
  # normalized Euclidian distance  
  statsSummary[i] = mean(sqrt(tempDF[,2]*tempDF[,2] + tempDF[,3]*tempDF[,3] + tempDF[,4]*tempDF[,4]) / 
                           boundaryConstant)
}

# http://cowlet.org/2013/09/15/understanding-data-science-feature-extraction-with-r.html
x.fft <- fft(tempDF[,2])
# Ignore the 2nd half, which are complex conjugates of the 1st half, 
# and calculate the Mod (magnitude of each complex number)
amplitude <- Mod(x.fft[1:(length(x.fft)/2)])
# Calculate the frequencies
frequency <- seq(0, 10000, length.out=length(x.fft)/2)
# Plot!
plot(amplitude ~ frequency, t="l")
# zoom in
plot(amplitude ~ frequency, t="l", xlim=c(0,5000), ylim=c(0,100))
axis(1, at=seq(0,5000,250), labels=FALSE)  # add more ticks
# dc term (at 0 Hz)


fft.profile <- function (dataset, n)
{
  fft.data <- fft(dataset)
  amplitude <- Mod(fft.data[1:(length(fft.data)/2)])
  frequencies <- seq(0, 10000, length.out=length(fft.data)/2)
  
  sorted <- sort.int(amplitude, decreasing=TRUE, index.return=TRUE)
  top <- sorted$ix[1:n] # indexes of the largest n components
  return (frequencies[top]) # convert indexes to frequencies
}

fft.profile(tempDF[,2], 15)
