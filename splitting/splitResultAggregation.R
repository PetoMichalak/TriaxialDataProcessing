# reads all available confusion matrices 
# aggregagates results and draws a pie chart
require(ggplot2)

# === modify to suit your needs
path = "/home/pet5o/workspace/TDP/data/150426_1136_workflowTests/testingSets/synced/annotated/stream_split/features/prediction/evaluation"
logpath = "/home/pet5o/workspace/TDP/data/150426_1136_workflowTests/testingSets/synced/annotated/stream_split/features/partialResult.log"
SHOW_KNN_PLOTS = TRUE
# ===

setwd(path)

# load project specific libraries
source("/home/pet5o/workspace/TDP/R/group-har/activityRecognitionFunctions.R")

# get all prediction files
logFiles <- read.csv(logpath)["filename"]
filenames = rep(NA, dim(logFiles)[1])
for (i in 1:dim(logFiles)[1]) {
  # strips the extension and the path from the absolute path
  item = file_path_sans_ext(basename(as.character(logFiles[i,1])))
  filenames[i] = paste(item, "_prediction_evaluation_confMatrix.csv", sep="")
}

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

#confusion <- data.frame(labels = lbls, values = slices)

#p = ggplot(data=confusion, aes(x=labels, y=slices))
#p = p + geom_bar(width=1)
#p = p + facet_grid(facets=. ~ slices)

#ggplot(data=confusion, 
#       aes(x=factor(1),
#           y=values,
#           fill = factor(labels))) + 
#  geom_bar(width = 1) + 
#  coord_polar(theta="y") +
#  xlab('Males') +
#  ylab('') +
#  labs(fill='Response')

#pie <- ggplot(confusion, aes(x = factor(1), fill = complete.cases(values))) +
#  geom_bar(width = 1)
#pie + coord_polar(theta = "y")
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

if (SHOW_KNN_PLOTS) {
  # barplots to show use of kNN
  par(mfrow=c(1,2))
  x <- summary[summary$source=="hip",3]
  uval <- sort(unique(x))
  if (length(uval) > 1) {
    counts <- rowSums(sapply(x, function(x) x==uval))
  } else {
    counts <- length(x)
  }
  barplot(counts, names=uval, main = "kNN() count - hip", xlab = "k", ylab = "Count")
  
  x <- summary[summary$source=="wrist",3]
  uval <- sort(unique(x))
  if (length(uval) > 1) {
    counts <- rowSums(sapply(x, function(x) x==uval))
  } else {
    counts <- length(x)
  }
  barplot(counts, names=uval, main = "kNN() count - wrist", xlab = "k", ylab = "Count")
}
