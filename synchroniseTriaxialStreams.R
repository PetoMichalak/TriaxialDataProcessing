# synchronise two datastreams from binary format 

# load required libraries
require(tools)
require(GENEAread)

# === modify to suit your needs
# path to a case study
path = "TestCase"
wristDataPath = "TestData/TestWrist.csv"
hipDataPath = "TestData/TestHip.csv"
# timeStart and timeEnd allow to make a snippet of data
# for now they need to be in seconds from epoch origin="1970-01-01"
# print(as.numeric(Sys.time())) might help to determine the values
timeStart = 0 
# default end time is now
timeEnd = as.numeric(Sys.time())
# switch to load from binary or CSV
IsBinary = FALSE
# ===

setwd(path)
# data placeholders
wristData = data.frame()
hipData = data.frame()
# load data
if (IsBinary) {
  print("Loading wrist data")
  wristData = read.bin(wristDataPath)
  wristData = wristData$data.out
  
  print("Loading hip data")
  hipData = read.bin(hipDataPath)
  hipData = hipData$data.out
} else {
  print("Loading wrist data")
  wristData = read.csv(wristDataPath)
  
  print("Loading hip data")
  hipData = read.csv(hipDataPath)
}

# print the data start timestamps
wristStart = wristData[1,1]
wristEnd = wristData[nrow(wristData),1]

hipStart = hipData[1,1]
hipEnd = hipData[nrow(hipData),1]

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
wristDF = (as.data.frame(wristData))
wristDF["activity"] = NA
hipDF = (as.data.frame(hipData))
hipDF["activity"] = NA

# create output dir
dir.create(file.path(getwd(), "syncedData"), showWarnings = FALSE)

# save data on disk
write.csv(wristDF[wristDF[,1]>monitoringStart & wristDF[,1]<monitoringEnd,], 
          paste("syncedData/", 
                file_path_sans_ext(basename(as.character(wristDataPath))),
                ".csv",sep=""), row.names=FALSE)

write.csv(hipDF[hipDF[,1]>monitoringStart & hipDF[,1]<monitoringEnd,], 
          paste("syncedData/", 
                file_path_sans_ext(basename(as.character(hipDataPath))),
                ".csv",sep=""), row.names=FALSE)
