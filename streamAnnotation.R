Load# automatically annotate the stream from external anotation file
require(tools)

# === modify to suit your needs
path = "/home/pet5o/workspace/TDP/data/Jack_weekend17-19April"
streamDataPath = "Jack_hip_sample.csv"
annotationPath = "annotation.csv"

# load data
setwd(path)
print("Loading data")
streamData = read.csv(streamDataPath, header = TRUE, sep = ",")
annotation = read.csv(annotationPath, header = TRUE, sep = ",")

# subset only relevant annotation records
annotation = annotation[annotation$filename==file_path_sans_ext(streamDataPath),]

# set default annotation to -1
streamData["activity"]=-1

# convert start and end time of annotation record
# TODO vectorize
for (i in 1:nrow(annotation)) {
  record = annotation[i,]
  startTime = as.numeric(as.POSIXct(record$starttime, tz="BST"))
  endTime = as.numeric(as.POSIXct(record$endtime, tz="BST"))
  streamData$activity[streamData$timestamp > startTime & 
                        streamData$timestamp < endTime] = record$activity
}

write.csv(streamData, 
          paste(file_path_sans_ext(streamDataPath),"_annotated.csv",sep=""), 
          row.names=FALSE)

# nice histogram to get a feeling on how much data is annotated
# hist(streamData$activity, main=paste("Histogram of", streamDataPath))
