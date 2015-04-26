# this module evaluates the prediction against annotation, if available
# require(ggplot2)
require(tools)

# === modify to suit your needs
path = "/home/pet5o/workspace/TDP/data/150426_1136_workflowTests/testingSets/synced/annotated/features/prediction"
dataPath = "Peter_003_left hip_020088_2015-03-10 18-40-35_annotated_features_234prediction.csv"
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

# TODO draw this pie chart in ggplot
# http://stackoverflow.com/questions/8952077/pie-plot-getting-its-text-on-top-of-each-other
# pie <- ggplot(slices, aes(x = "", y = lbls, fill = Type)) + 
#   geom_bar(width = 1) + 
#   geom_text(aes(y = val/2 + c(0, cumsum(slices)[-length(slices)]), label = percent), size=10)
# pie + coord_polar(theta = "y")

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
cat("Accuracy: ", accuracy)
cat("\nF-measure: ", fmeasure)
cat("\nPrecision: ", precision)
cat("\nRecall: ", recall, "\n")
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