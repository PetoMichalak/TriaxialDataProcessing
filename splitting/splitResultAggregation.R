# reads all available confusion matrices 
# aggregagates results and draws a pie chart
require(ggplot2)

# === modify to suit your needs
path = "/home/pet5o/workspace/TDP/DataEvaluation/_reportRun/SVM/featureData"
logpath = "/home/pet5o/workspace/TDP/DataEvaluation/_reportRun/SVM/partialResult_SVM.log"
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
  # load log summary
  logSummary <- read.csv(logpath)
  # barplots to show use of kNN
  par(mfrow=c(1,2))
  x <- logSummary[logSummary$source=="hip",3]
  # uval <- sort(unique(x))
  uval <- c(3,5,7,11,13,17,19,23)
  if (length(uval) > 1) {
    counts <- rowSums(sapply(x, function(x) x==uval))
  } else {
    counts <- length(x)
  }
  barplot(counts, names=uval, main = "kNN() count - hip", xlab = "k", ylab = "Count", ylim=c(0,10))
  
  x <- logSummary[logSummary$source=="wrist",3]
  # uval <- sort(unique(x))
  uval <- c(3,5,7,11,13,17,19,23)
  if (length(uval) > 1) {
    counts <- rowSums(sapply(x, function(x) x==uval))
  } else {
    counts <- length(x)
  }
  barplot(counts, names=uval, main = "kNN() count - wrist", xlab = "k", ylab = "Count", ylim=c(0,10))
  
  # Generate data
#  pp <- function (n,r=4) {
#    x <- seq(-r*pi, r*pi, len=n)
#    df <- expand.grid(x=x, y=x)
#    df$r <- sqrt(df$x^2 + df$y^2)
#    df$z <- cos(df$r^2)*exp(-df$r/6)
#    df
#  }
  
#  x <- c(74 / (74+176+110), 0, 0, 
#         176 / (74+176+110), 255 / (255+9), 12 / (12), 
#         110 / (74+176+110), 9 / (255+9), 0)
#  qplot(x, y, data = pp(3), fill = z, geom = "raster")    

#  library(reshape2)
#  library(ggplot2)
#  m = matrix(x,3)
#  conf = data.frame(Predicted = c(2,1,0,2,1,0,2,1,0), Actual = c(2,2,2,1,1,1,0,0,0), Value = x)
#  ggplot(conf, aes(Predicted, Actual, fill = Value)) + geom_raster()
}

# normalise the confusion matrix
normaliseConf = function(confMatrix) {
  for (i in 1:nrow(confMatrix)) {
    confMatrix[i,] = confMatrix[i,] / sum(confMatrix[i,])
  }
  return(confMatrix)
}

# normalise the values
normConfMatrix = normaliseConf(aggregatedConfMatrix)
pdf(paste(path,"/aggregatedConfMatrixRaster.pdf",sep=""))
conf = data.frame(Predicted = c(0,1,2,0,1,2,0,1,2), Actual = c(0,0,0,1,1,1,2,2,2), Proportion = unlist(as.list(normConfMatrix)))
ggplot(conf, aes(Predicted, Actual, fill = Proportion)) + geom_raster() + ggtitle("Confusion Matrix") +
  theme(plot.title=element_text(family="Times", face="bold", size=20))
dev.off()

# produce a merged prediction graph
# get all intensity levels
# lists all wrist data files which 
filenames <- list.files(getwd(), pattern="*WRIST*", full.names=TRUE)
wristData = data.frame()
for (filename in filenames) {
  wristData <- rbind(wristData, read.csv(filename)[,c("timestamp", "statSummary")])
}
# sort the dataframe by timestamp
wristData = wristData[with(wristData, order(-timestamp)), ]

# lists all hip data files which 
filenames <- list.files(getwd(), pattern="*HIP*", full.names=TRUE)
hipData = data.frame()
for (filename in filenames) {
  hipData <- rbind(hipData, read.csv(filename)[,c("timestamp", "statSummary")])
}
# sort the dataframe by timestamp
hipData = hipData[with(hipData, order(-timestamp)), ]

# get all prediction files
logFiles <- read.csv(logpath)["filename"]
filenames = rep(NA, dim(logFiles)[1])
for (i in 1:dim(logFiles)[1]) {
  # strips the extension and the path from the absolute path
  item = file_path_sans_ext(basename(as.character(logFiles[i,1])))
  filenames[i] = paste("predictions/", item, "_prediction.csv", sep="")
}

wristPred = data.frame()
hipPred = data.frame()
# load all predictions into data frames
for (filename in filenames) {
  if(grepl("HIP", filename)) {
    hipPred <- rbind(hipPred, read.csv(filename))
  }
  # don't use else in case some other filename formatting creeps in
  if(grepl("WRIST", filename)) {
    wristPred <- rbind(wristPred, read.csv(filename))
  }
}
# sort the dataframes by timestamp
hipPred = hipPred[with(hipPred, order(-timestamp)), ]
wristPred = wristPred[with(wristPred, order(-timestamp)), ]


par(mfrow=c(2,1))
# TODO - nasty ylim constants - should really set it dynamically
plot(wristData$timestamp, wristData$statSummary, xlab = "Time", 
     ylab="Intensity", type="l", main="Wrist data",
     ylim=c(0.06, 0.3))
legend("topright", c("rest", "light", "heavy"), cex=1.5, fill=c(1:3))
points(x=wristPred$timestamp,y=rep(0.06,length(wristPred$prediction)),col=wristPred$prediction + 1, pch=16)
plot(hipData$timestamp, hipData$statSummary, xlab = "Time", 
     ylab="Intensity", type="l", main="Hip data",
     ylim=c(0.06, 0.3))
legend("topright", c("rest", "light", "heavy"), cex=1.5, fill=c(1:3))
points(x=hipPred$timestamp,y=rep(0.06,length(hipPred$prediction)),col=hipPred$prediction + 1, pch=16)
