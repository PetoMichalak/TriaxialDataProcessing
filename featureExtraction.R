# feature extraction

# === modify to suit your needs
path = "/home/pet5o/workspace/TDP/data/Jack_weekend17-19April"
dataPath = "Jack_wrist_sample_annotated.csv"
# size of the window (in seconds) for feature extraction
SPLIT_INTERVAL=5
# sampling frequency
FREQUENCY=100
# set boundary for the inputted data (theorethical the highest value)
SET_BOUNDARY=8
# number of highest frequencies to take from FFT
TOP_FREQ = 15

# load data
setwd(path)
print("Loading data")
data = read.csv(dataPath)

# load project specific libraries
source("/home/pet5o/workspace/TDP/R/group-har/activityRecognitionFunctions.R")

# calculate normalized euclidian distance and FFT
# get ready for statistics summary calculation
counter=1
max_interval=SPLIT_INTERVAL * FREQUENCY
frameCount = floor(nrow(data)/max_interval)
# data holder for statistics summary
statsSummary = rep(NA, frameCount)
# data holder for fft data
fftData = rep(NA, frameCount * TOP_FREQ)

boundaryConstant = sqrt(3 * SET_BOUNDARY*SET_BOUNDARY)
# for each dataframe we have 5 seconds for each x,y,z coord
fftWidth = TOP_FREQ*5*3

# vectorized ED calc
# TODO vectorize - probably problem with final frame size : (
# sapply and data.table should improve the performance dramatically
for (i in 1:frameCount) {
  startIndex = 1 + max_interval * (i - 1)
  endIndex = i * max_interval
  tempDF = data[startIndex:endIndex,]
  startIndex = i + (i-1) * fftWidth - (i - 1)
  # cat(startIndex,";")
  fftData[startIndex:(startIndex + fftWidth)] = calcFFT(tempDF, TOP_FREQ)
  # print(fftData[startIndex:(startIndex + fftWidth)])
  # normalized Euclidian distance  
  statsSummary[i] = mean(sqrt(tempDF[,2]*tempDF[,2] + tempDF[,3]*tempDF[,3] + tempDF[,4]*tempDF[,4]) / 
                           boundaryConstant)
}

# calculate mean, variance, standard deviation

# if annotation was included in source file, copy it further