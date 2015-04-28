# automatically annotate the stream from external anotation file
require(tools)

# === modify to suit your needs
path = "/home/pet5o/workspace/TDP/DataEvaluation/pet_01"
streamDataPath = "Peter_003_right wrist_015800_2015-03-10 18-30-03.csv"
annotationPath = "annotation.csv"

# load data
setwd(path)
print("Loading data")
streamData = read.csv(file.path(path,"syncedData",streamDataPath), header = TRUE, sep = ",")
annotation = read.csv(file.path(path, annotationPath), header = TRUE, sep = ",")

# subset only relevant annotation records
annotation = annotation[annotation$filename==file_path_sans_ext(streamDataPath),]

# set default annotation to -1
streamData["activity"]=NA

# convert start and end time of annotation record
# TODO vectorize
for (i in 1:nrow(annotation)) {
  record = annotation[i,]
  startTime = as.numeric(as.POSIXct(record$starttime, tz="BST"))
  endTime = as.numeric(as.POSIXct(record$endtime, tz="BST"))
  streamData$activity[streamData$timestamp > startTime & 
                        streamData$timestamp < endTime] = record$activity
}

# put the data in the same folder
write.csv(streamData, 
          paste("syncedData/", file_path_sans_ext(streamDataPath), "_annotated.csv",sep=""), 
          row.names=FALSE)

# nice histogram to get a feeling on how much data is annotated
# hist(streamData$activity, main=paste("Histogram of", streamDataPath))
