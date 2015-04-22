# sync up the data
# insert two streams
# if date is insert - cut it down
# if no date inserted - find later beginning find first end

# load required libraries
require(GENEAread)

# === modify to suit your needs
path = "C:/Users/localadmin/Documents/workspace/TDP/Jack_weekend17-19April"
wristDataPath = "Jack_left hip_020163_2015-04-20 14-51-02.bin"
hipDataPath = "Jack_right wrist_020164_2015-04-20 15-04-16.bin"
timeStart = c() 
endStart = c()
# ===

# load data
setwd(path)
print("Loading wrist data")
wristData = read.bin(wristDataPath)

print("Loading hip data")
hipData = read.bin(hipDataPath)

# print the data start timestamps
wristStart = wristData$data.out[1,1]
wristEnd = wristData$data.out[(nrow(wristData$data.out)-1),1]

hipStart = hipData$data.out[1,1]
hipEnd = hipData$data.out[(nrow(hipData$data.out)-1),1]

# pick the later start
monitoringStart = 0
if (wristStart > hipStart) {
  monitoringStart = wristStart
} else {
  monitoringStart = hipStart
}

monitoringEnd = 0
if (wristEnd < hipEnd) {
  monitoringEnd = wristEnd
} else {
  monitoringEnd = hipEnd
}

# save the data between start and stop interval
write.csv(wristData$data.out[wristData$data.out[,1]>monitoringStart & wristData$data.out[,1]<monitoringEnd,], paste(wristDataPath,".csv",sep=""), row.names=FALSE)
write.csv(hipData$data.out[hipData$data.out[,1]>monitoringStart & hipData$data.out[,1]<monitoringEnd,], paste(hipDataPath,".csv",sep=""), row.names=FALSE)
