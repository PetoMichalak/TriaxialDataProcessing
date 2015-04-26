# synchronise two datastreams from binary format 

# load required libraries
require(tools)
require(GENEAread)

# === modify to suit your needs
path = "/home/pet5o/workspace/TDP/data/150426_1136_workflowTests/testingSets"
wristDataPath = "Peter_003_right wrist_015800_2015-03-10 18-30-03.bin"
hipDataPath = "Peter_003_left hip_020088_2015-03-10 18-40-35.bin"
# timeStart and timeEnd allow to make a snippet of data
# for now they need to be in seconds from epoch origin="1970-01-01"
# print(as.numeric(Sys.time())) might help to determine the values
timeStart = 0 
# default end time is now
timeEnd = as.numeric(Sys.time())
# ===

# load data
setwd(path)
print("Loading wrist data")
wristData = read.bin(wristDataPath)

print("Loading hip data")
hipData = read.bin(hipDataPath)

# print the data start timestamps
wristStart = wristData$data.out[1,1]
wristEnd = wristData$data.out[nrow(wristData$data.out),1]

hipStart = hipData$data.out[1,1]
hipEnd = hipData$data.out[nrow(hipData$data.out),1]

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

# adjust time and end time if necessary
if (monitoringStart < timeStart) {
  monitoringStart = timeStart
}
if (monitoringEnd > timeEnd) {
  monitoringEnd = timeEnd
}

# add annotation column
wristDF = (as.data.frame(wristData$data.out))
wristDF["activity"] = NA
hipDF = (as.data.frame(hipData$data.out))
hipDF["activity"] = NA

# save the data between start and stop interval
#write.csv(wristData$data.out[wristData$data.out[,1]>monitoringStart & 
#                               wristData$data.out[,1]<monitoringEnd,], 
#          paste(file_path_sans_ext(wristDataPath),".csv",sep=""), row.names=FALSE)
#write.csv(hipData$data.out[hipData$data.out[,1]>monitoringStart & 
#                             hipData$data.out[,1]<monitoringEnd,], 
#          paste(file_path_sans_ext(hipDataPath),".csv",sep=""), row.names=FALSE)

write.csv(wristDF[wristDF[,1]>monitoringStart & wristDF[,1]<monitoringEnd,], 
          paste(file_path_sans_ext(wristDataPath),".csv",sep=""), row.names=FALSE)
write.csv(hipDF[hipDF[,1]>monitoringStart & hipDF[,1]<monitoringEnd,], 
          paste(file_path_sans_ext(hipDataPath),".csv",sep=""), row.names=FALSE)
