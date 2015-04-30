# this module evaluates the prediction against annotation, if available
require(tools)
library(reshape2)
library(ggplot2)


# === modify to suit your needs
path = "/home/pet5o/workspace/TDP/DataEvaluation/final_dataset_runII/kNN_fft5"
dataPath = "hip_all_testdata_withRest_features_84prediction.csv"
# ===

# load project specific libraries
source("/home/pet5o/workspace/TDP/R/group-har/activityRecognitionFunctions.R")

# load data
setwd(path)
print("Loading data")
data = read.csv(dataPath)

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

pdf(paste(file_path_sans_ext(dataPath),"_ConfMatrix.pdf",sep=""))
# make a pie chart of results
slices <- c(confMatrix[1,1], confMatrix[1,2], confMatrix[1,3], 
            confMatrix[2,1], confMatrix[2,2], confMatrix[2,3], 
            confMatrix[3,1], confMatrix[3,2], confMatrix[3,3])
lbls <- c("0:0", "1:0", "2:0", 
          "1:0", "1:1", "1:2",
          "2:0", "2:1", "2:2")
pie(slices, labels = lbls, clockwise = TRUE, radius = 1,
    col = c(3,2,2,
            5,3,2,
            5,5,3),
    main="Confusion matrix visualized (predicted:annotated)")
legend("topright", lbls, cex=0.8, fill=c(3,2,2,5,3,2,5,5,3))
dev.off()

# == get a confusion matrix raster 
# confMatrix = read.csv("/home/pet5o/workspace/TDP/DataEvaluation/pet_01/kNN_fft0/Peter_003_right wrist_015800_2015-03-10 18-30-03_annotated_featuresfeatures9_prediction_evaluation_confMatrix.csv")[,2:4]

pdf(paste(file_path_sans_ext(dataPath),"_ConfMatrixRaster.pdf",sep=""))
# normalise the confusion matrix
normaliseConf = function(confMatrix) {
  for (i in 1:nrow(confMatrix)) {
    confMatrix[i,] = confMatrix[i,] / sum(confMatrix[i,])
  }
  return(confMatrix)
}

# normalise the values
normConfMatrix = normaliseConf(confMatrix)

conf = data.frame(Predicted = c(0,1,2,0,1,2,0,1,2), Actual = c(0,0,0,1,1,1,2,2,2), Proportion = unlist(as.list(normConfMatrix)))
ggplot(conf, aes(Predicted, Actual, fill = Proportion)) + geom_raster() + ggtitle("Confusion Matrix") +
  theme(plot.title=element_text(family="Times", face="bold", size=20))
dev.off()

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
          paste(file_path_sans_ext(dataPath),"_evaluation_confMatrix.csv",sep=""), 
          row.names=TRUE)
write.csv(statsy,
          paste(file_path_sans_ext(dataPath),"_evaluation_stats.csv",sep=""), 
          row.names=TRUE)