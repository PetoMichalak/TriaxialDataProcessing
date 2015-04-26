# feature extraction

# === modify to suit your needs
path = "/home/pet5o/workspace/TDP/data/150426_1136_workflowTests/testingSets/synced/annotated"
dataPath = "Peter_003_left hip_020088_2015-03-10 18-40-35_annotated.csv"
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
# data holder for starting timestamp
timestamps = rep(NA, frameCount)
# data holder for mean, variance and standard deviations
means_x = rep(NA, frameCount)
means_y = rep(NA, frameCount)
means_z = rep(NA, frameCount)
vars_x = rep(NA, frameCount)
vars_y = rep(NA, frameCount)
vars_z = rep(NA, frameCount)
# sds_x = rep(NA, frameCount)
# sds_y = rep(NA, frameCount)
# sds_z = rep(NA, frameCount)
# data holder for annotation
activities = rep(NA, frameCount)
# data holder for statistics summary
statsSummary = rep(NA, frameCount)
# data holder for fft data
fftCount = TOP_FREQ * SPLIT_INTERVAL * 3
fftData = matrix(0, ncol = fftCount, nrow = frameCount, dimnames = list(NULL, getColNames(SPLIT_INTERVAL, TOP_FREQ)))

boundaryConstant = sqrt(3 * SET_BOUNDARY*SET_BOUNDARY)
# for each dataframe we have 5 seconds for each x,y,z coord
fftWidth = TOP_FREQ*5*3

# vectorized ED calc
# TODO vectorize - probably problem with final frame size : (
# sapply and data.table should improve the performance dramatically
for (i in 1:frameCount) {
  # calculate the data range 
  startIndex = 1 + max_interval * (i - 1)
  endIndex = i * max_interval
  tempDF = data[startIndex:endIndex,]
  # capture the timestamp
  timestamps[i] = tempDF[1,1]
    
  # calculate top n FFTs (count depends on sampling_rate)
  startIndex = i + (i-1) * fftWidth - (i - 1)
  # x
  fftData[i,1:(fftCount/3)] = extractFFT(tempDF[,2], TOP_FREQ, sampling_rate = FREQUENCY)
  # y
  fftData[i,(fftCount/3+1):(fftCount/3*2)] = extractFFT(tempDF[,3], TOP_FREQ, sampling_rate = FREQUENCY)
  # z
  fftData[i,(fftCount/3*2+1):(fftCount)] = extractFFT(tempDF[,4], TOP_FREQ, sampling_rate = FREQUENCY)
  
  # normalized Euclidian distance  
  statsSummary[i] = mean(sqrt(tempDF[,2]*tempDF[,2] + tempDF[,3]*tempDF[,3] + tempDF[,4]*tempDF[,4]) / 
                           boundaryConstant)

  # calculate mean, variance, standard deviation
  means_x[i] = mean(tempDF[,2])
  means_y[i] = mean(tempDF[,3])
  means_z[i] = mean(tempDF[,4])
  vars_x[i] = var(tempDF[,2])
  vars_y[i] = var(tempDF[,3])
  vars_z[i] = var(tempDF[,4])
  # sds_x[i] = sd(tempDF[,2])
  # sds_y[i] = sd(tempDF[,3])
  # sds_z[i] = sd(tempDF[,4])
  
  # if annotation was included in source file, copy it further
  # TODO pick the most frequent value
  if("activity" %in% colnames(tempDF)) {
    activities[i] = tempDF$activity[1]
  }
}

# bundle all the info together (with timestamp)
output = data.frame()
if("activity" %in% colnames(data)) {
  fftDF = as.data.frame(as.table(fftData))
  output = data.frame(timestamp = timestamps, 
                      activity = activities, statSummary = statsSummary, 
                      mean_x = means_x, mean_y = means_y, mean_z = means_z,
                      var_x = vars_x, var_y = vars_y, var_z = vars_z)
                      # sd_x = sds_x, sd_y = sds_y, sd_z = sds_z)
  output = cbind(output, as.data.frame(fftData))
} else {
  fftDF = as.data.frame(as.table(fftData))
  output = data.frame(timestamp = timestamps, 
                      activity = -1, statSummary = statsSummary, 
                      mean_x = means_x, mean_y = means_y, mean_z = means_z,
                      var_x = vars_x, var_y = vars_y, var_z = vars_z)
                      # sd_x = sds_x, sd_y = sds_y, sd_z = sds_z)
  output = cbind(output, as.data.frame(fftData))
}

# save to the disk
write.csv(output, 
          paste(file_path_sans_ext(dataPath),"_features.csv",sep=""), 
          row.names=FALSE)