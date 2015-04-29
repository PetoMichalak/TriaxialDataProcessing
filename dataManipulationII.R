# aggregate 4 x 15 minutes of different types of activities
# load libraries
library(GENEAread)


##########################################################
# Edit these:

#File you wish to use
path = "/home/simon/Downloads/RealScience/001_Simon"
fileName = "S001lefthip.bin"
fulFilePath = paste(path,fileName,sep="/")

#Time window you wish to view - Start time, End Time
window = c( "2015-03-10 16:40:00","2015-03-10 16:45:00")

fileToWrite = "01_wrist_behindDesk.csv"


##########################################################

# Load the Data
data = read.bin(fulFilePath)
data = data$data.out

#### TIME ### 
#60000 milliseconds = 1 minute

####  Start Times
as.POSIXct(data[1], origin="1970-01-01", tz = "BST")

### Window
windowMillis = as.integer( as.POSIXct( window, tz = "BST" ) )

#Get the index of the start time
startIndex = which(abs(data[,1]-windowMillis[1]) == min(abs(data[,1]-windowMillis[1])) )

#Get the index of the end time
endIndex = which(abs(data[,1]-windowMillis[2]) == min(abs(data[,1]-windowMillis[2])) )


plot(data[,2][startIndex:endIndex], type="l")

# save the snippet
write.csv(data[startIndex:endIndex,], fileToWrite, row.names=TRUE)

