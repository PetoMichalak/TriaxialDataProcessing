#grab the svm function from doSVM
source("/home/simon/Code/GROUP_PRJ/group-har/doSVM.R")

# ********************** #
# *** Load & Prepare *** #
# ********************** #
testset <- read.csv('/home/simon/Data/GRP/testing/RawaHip.csv',head=TRUE)[1:2]

trainset <- read.csv('/home/simon/Data/GRP/training/merged/HipTrain.csv',head=TRUE)[1:2]

### HACKY MESS ###
#Make sure the headers are removed, factors are factors, and columns have the right naming.... 
colnames(testset)[colnames(testset)=="intensity"] <- "V1"
colnames(testset)[colnames(testset)=="activity"] <- "V2"

colnames(trainset)[colnames(trainset)=="intensity"] <- "V1"
colnames(trainset)[colnames(trainset)=="activity"] <- "V2"

testset$V2 = factor(testset$V2)
trainset$V2 = factor(trainset$V2)
### /HACKY MESS ###


# ****************** #
# *** Do the SVM *** #
# ****************** #
tab = doSVM(V2~., trainset, testset)
