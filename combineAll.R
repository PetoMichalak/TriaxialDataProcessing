# r combine the final dataset
require(GENEAread)
path = "/home/pet5o/workspace/TDP/DataEvaluation/final_dataset_runII/syncedData"
setwd(path)
# load of files
filenames <- list.files(path, pattern="*wrist*", full.names=TRUE)

# fullDataFrame = data.frame()
fullDataFrame = read.csv("wrist_all_test_data_timeSynced.csv")

tempDF = read.csv("000_wrist_rest_desk.csv")
tempDF["activity"] = 0
# head(tempDF)

fullDataFrame = rbind(tempDF, fullDataFrame)
# head(fullDataFrame)
dim(fullDataFrame)

write.csv(fullDataFrame, "wrist_all_testdata_withRest.csv", row.names=FALSE)

# temp = read.csv("04_wrist_walk002.csv")
# dim(temp)
