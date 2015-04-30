# aggregate 4 x 15 minutes of different types of activities
# load libraries
library(GENEAread)

path = "/home/pet5o/workspace/TDP/DataEvaluation/pet_presentation_comparison"

# set path
setwd(path)

#== working by desk 
wristPath = "data/walking_and_workByTheDesk/Peter_right wrist_020163_2015-04-29 09-29-22.bin"
hipPath = "data/walking_and_workByTheDesk/Peter_left hip_020164_2015-04-29 09-36-32.bin"

# load data
wristData = read.bin(wristPath)
hipData = read.bin(hipPath)
wristData = wristData$data.out
hipData = hipData$data.out

# plot
plot(x=wristData[,1][5040000:5140000], y=wristData[,2][5040000:5140000], type="l")
plot(x=hipData[,1][5040000:5140000], y=hipData[,2][5040000:5140000], type="l")

# save the snippet
write.csv(wristData[5040000:5140000,], "01_wrist_behindDesk.csv", row.names=TRUE)
write.csv(hipData[5040000:5140000,], "01_hip_behindDesk.csv", row.names=TRUE)


#== walking with briefcase
wristPath = "data/walkingBriefcase/A_020097_2015-03-11 12-31-32.bin"
hipPath = "data/walkingBriefcase/A_020163_2015-03-11 12-23-14.bin"

# load data
wristData = read.bin(wristPath)
hipData = read.bin(hipPath)
wristData = wristData$data.out
hipData = hipData$data.out

# plot
plot(wristData[,2][6122000:6142500], type="l")
plot(x=hipData[,2][6100000:6150000], type="l")

# save the snippet
write.csv(wristData[5040000:5140000,], "01_wrist_behindDesk.csv", row.names=TRUE)
write.csv(hipData[5040000:5140000,], "01_hip_behindDesk.csv", row.names=TRUE)

# === running data
wristPath = "data/run/run_right wrist_020163_2015-04-29 12-33-13.bin"
hipPath = "data/run/run_left hip_020164_2015-04-29 12-33-38.bin"

# load data
wristData = read.bin(wristPath)
hipData = read.bin(hipPath)
wristData = wristData$data.out
hipData = hipData$data.out

# plot
plot(wristData[,2][24000:97500], type="l")
plot(hipData[,2][23500:97000], type="l")

# save the snippet
write.csv(wristData[24000:97500,], "03_wrist_run.csv", row.names=TRUE)
write.csv(hipData[23500:97000,], "03_hip_run.csv", row.names=TRUE)

# === walking data
wristPath = "data/walking_and_workByTheDesk/Peter_right wrist_020163_2015-04-29 09-29-22.bin"
hipPath = "data/walking_and_workByTheDesk/Peter_left hip_020164_2015-04-29 09-36-32.bin"

# load data
wristData = read.bin(wristPath)
hipData = read.bin(hipPath)
wristData = wristData$data.out
hipData = hipData$data.out

# plot
plot(wristData[,2][1:200000], type="l")
plot(hipData[,2][1:200000], type="l")

# save the snippet
write.csv(wristData[1:10000,], "03_wrist_walk.csv", row.names=FALSE)
write.csv(hipData[23500:97000,], "03_hip_walk.csv", row.names=FALSE)


# timestamp conversion
as.POSIXct(hipData[1,1], origin="1970-01-01", tz = "BST")
as.POSIXct(1430296000, origin="1970-01-01")
