# reads all available confusion matrices 
# aggregagates results and draws a pie chart

# === modify to suit your needs
path = "/home/pet5o/workspace/TDP/data/150426_1136_workflowTests/testingSets/synced/annotated/stream_split/features/prediction/evaluation"
# ===

# load project specific libraries
source("/home/pet5o/workspace/TDP/R/group-har/activityRecognitionFunctions.R")

# get all prediction files
filenames <- list.files(path, pattern="*confMatrix*", full.names=TRUE)

# confusion matrix
size = 3
aggregatedConfMatrix = matrix(rep(0, size*size), nrow = size, ncol = size)

for (i in 1:length(filenames)) {
  # filter out just the data
  tempConfMatrix = read.csv(filenames[i])[,2:4]
  aggregatedConfMatrix = aggregatedConfMatrix + tempConfMatrix
}

# make a pie chart of results
slices <- c(aggregatedConfMatrix[1,1], aggregatedConfMatrix[1,2], aggregatedConfMatrix[1,3], 
            aggregatedConfMatrix[2,1], aggregatedConfMatrix[2,2], aggregatedConfMatrix[2,3], 
            aggregatedConfMatrix[3,1], aggregatedConfMatrix[3,2], aggregatedConfMatrix[3,3])
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
print(aggregatedConfMatrix)
statsy = data.frame(accuracy = rep(NA,size), fmeasure=rep(NA,size), 
                    precision=rep(NA,size), recall=rep(NA,size))
rownames(statsy) = c("0", "1", "2")
statsy["accuracy"] = getAccuracy(aggregatedConfMatrix)
statsy["fmeasure"] = getFmeasure(aggregatedConfMatrix)
statsy["precision"] = getPrecision(aggregatedConfMatrix)
statsy["recall"] = getRecall(aggregatedConfMatrix)
print(statsy)
print("======================================")

# save the output
df = data.frame(aggregatedConfMatrix)
rownames(df) = c("0", "1", "2")
colnames(df) = c("0", "1", "2")
write.csv(df, 
          paste(file_path_sans_ext(filenames[1]),"_evaluation_confMatrix_aggregated.csv",sep=""), 
          row.names=TRUE)
write.csv(statsy,
          paste(file_path_sans_ext(filenames[1]),"_evaluation_stats_aggregated.csv",sep=""), 
          row.names=TRUE)