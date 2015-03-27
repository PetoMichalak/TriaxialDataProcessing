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
# number of highest frequencies to take from FFT
TOP_FREQ = 15

# get ready for statistics summary calculation
counter=1
max_interval=SPLIT_INTERVAL * FREQUENCY
frameCount = floor(nrow(dataSnippet)/max_interval)
# data holder for statistics summary
statsSummary = rep(NA, frameCount)
# data holder for fft data
fftData = rep(NA, frameCount * TOP_FREQ)

boundaryConstant = sqrt(3 * SET_BOUNDARY*SET_BOUNDARY)
# for each dataframe we have 5 seconds for each x,y,z coord
fftWidth = TOP_FREQ*5*3

# vectorized ED calc
# TODO probably problem with final frame size : (
# sapply and data.table should improve the performance dramatically
for (i in 1:frameCount) {
  startIndex = 1 + max_interval * (i - 1)
  endIndex = i * max_interval
  tempDF = dataSnippet[startIndex:endIndex,]
  startIndex = i + (i-1) * fftWidth - (i - 1)
  # cat(startIndex,";")
  fftData[startIndex:(startIndex + fftWidth)] = calcFFT(tempDF, TOP_FREQ)
  # print(fftData[startIndex:(startIndex + fftWidth)])
  # normalized Euclidian distance  
  statsSummary[i] = mean(sqrt(tempDF[,2]*tempDF[,2] + tempDF[,3]*tempDF[,3] + tempDF[,4]*tempDF[,4]) / 
                         boundaryConstant)
}

# http://cowlet.org/2013/09/15/understanding-data-science-feature-extraction-with-r.html
x.fft <- fft(tempDF[,2][500])
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
