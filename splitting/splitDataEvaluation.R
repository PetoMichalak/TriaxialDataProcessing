# evaluates all available prediction files in the folder
# require(ggplot2)
require(tools)

# === modify to suit your needs
path = "/home/pet5o/workspace/TDP/DataEvaluation/final_dataset_runII/fragmentedFeatureData_fft0"
# ===

# load project specific libraries
source("/home/pet5o/workspace/TDP/R/group-har/activityRecognitionFunctions.R")

# load data
setwd(path)

# calculates confusion matrix a statsy things 
singleStreamEvaluation = function(dataitem) {
  print("Loading data")
  data = read.csv(dataitem)
  
  # strip unannotated data (-1)
  data = data[complete.cases(data[,"activity"]),]
  activity = data$activity + 1
  prediction = data$prediction + 1
  
  # confusion matrix
  size = 3
  confMatrix = matrix(rep(0, size*size), nrow = size, ncol = size)
  for(i in 1:length(prediction)) {
    confMatrix[prediction[i], activity[i]] = confMatrix[prediction[i], activity[i]] + 1
  }
  
  # test measures
  print("===Accuracy measures for best model===")
  print("Confusion matrix: ")
  print(confMatrix)
  statsy = data.frame(accuracy = rep(NA,size), fmeasure=rep(NA,size), 
                      precision=rep(NA,size), recall=rep(NA,size))
  rownames(statsy) = c("0", "1", "2")
  statsy["accuracy"] = getAccuracy(confMatrix)
  statsy["fmeasure"] = getFmeasure(confMatrix)
  statsy["precision"] = getPrecision(confMatrix)
  statsy["recall"] = getRecall(confMatrix)
  print(statsy)
  print("======================================")
  
  # save the output
  df = data.frame(confMatrix)
  rownames(df) = c("0", "1", "2")
  colnames(df) = c("0", "1", "2")
  write.csv(df, 
            paste(file_path_sans_ext(dataitem),"_evaluation_confMatrix.csv",sep=""), 
            row.names=TRUE)
  write.csv(statsy,
            paste(file_path_sans_ext(dataitem),"_evaluation_stats.csv",sep=""), 
            row.names=TRUE)
}

# get all prediction files
filenames <- list.files(path, pattern="*prediction*", full.names=TRUE)

require(class)
require(parallel)
cl <- makeCluster(4)
clusterExport(cl, c("getAccuracy","getPrecision","getFmeasure","getRecall","complete.cases","file_path_sans_ext"))
parLapply(cl, filenames, singleStreamEvaluation)
stopCluster(cl)
