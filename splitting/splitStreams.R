# splitting the data file into smaller

# === modify to suit your needs
path = "/home/pet5o/workspace/TDP/data/150426_1136_workflowTests/testingSets/synced/annotated"
wristDataPath = "Peter_003_right wrist_015800_2015-03-10 18-30-03_annotated.csv"
hipDataPath = "Peter_003_left hip_020088_2015-03-10 18-40-35_annotated.csv"
# size of the window (in minutes) for feature extraction
WINDOW_SIZE=15
# sampling frequency
FREQUENCY=100

# load data
setwd(path)
print("Loading data")
wristData = read.csv(wristDataPath)
hipData = read.csv(hipDataPath)

# d <- split(wristData, rep(1:11, each=90000))
# plyr looks much faster - further study needed
# require plyr
# llply(wristData[1:5,2:5], "x", mean)

cutsize = WINDOW_SIZE * FREQUENCY * 60
frameCount = ceiling(nrow(wristData) / cutsize)

for (i in 1:frameCount) {
  startIndex = i + (i-1) * cutsize - (i - 1)
  write.csv(wristData[startIndex:(i*cutsize),], 
            paste(file_path_sans_ext(wristDataPath),"_WRIST_part",i,".csv",sep=""), 
            row.names=FALSE)
  write.csv(hipData[startIndex:(i*cutsize),], 
            paste(file_path_sans_ext(wristDataPath),"_HIP_part",i,".csv",sep=""), 
            row.names=FALSE)
}