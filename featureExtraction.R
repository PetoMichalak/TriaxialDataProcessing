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
TOP_FREQ = 3

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
fftCount = TOP_FREQ * SPLIT_INTERVAL * 3
fftData = matrix(0, ncol = fftCount, nrow = frameCount)

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
  # calculate top n FFTs (count depends on sampling_rate)
  # x
  fftData[i,1:(fftCount/3)] = extractFFT(tempDF[,2], TOP_FREQ, sampling_rate = FREQUENCY)
  # y
  fftData[i,(fftCount/3+1):(fftCount/3*2)] = extractFFT(tempDF[,3], TOP_FREQ, sampling_rate = FREQUENCY)
  # z
  fftData[i,(fftCount/3*2+1):(fftCount)] = extractFFT(tempDF[,4], TOP_FREQ, sampling_rate = FREQUENCY)
  # normalized Euclidian distance  
  statsSummary[i] = mean(sqrt(tempDF[,2]*tempDF[,2] + tempDF[,3]*tempDF[,3] + tempDF[,4]*tempDF[,4]) / 
                           boundaryConstant)
}

# calculate mean, variance, standard deviation

# if annotation was included in source file, copy it further


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