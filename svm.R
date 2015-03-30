#SVM library
require(e1071)

# ********************** #
# *** Load & Prepare *** #
# ********************** #
dataset <- read.csv('/home/simon/Data/GRP/test/wdbc.data',head=FALSE)

index <- 1:nrow(dataset)

testindex <- sample(index, trunc(length(index)*30/100))

testset <- dataset[testindex,]

trainset <- dataset[-testindex,]

doSVM(V2~., trainset, testset)

doSVM = function(formula, trainset, testset){
  
  # ******************************* #
  # *** Choosing the Parameters *** #
  # ******************************* #
  environment(as.formula("y ~ x"))
  tuned <- tune.svm(formula, data = trainset, gamma = 10^(-6:-1), cost = 10^(-1:1)) 
  
  cat("Gamma: ",tuned$best.parameters[1,1],"\n")
  cat("Cost:  ",tuned$best.parameters[1,2],"\n")

  # ************************** #
  # *** Training the model *** #
  # ************************** #
  #From the tuned data we can see that the best gamma and cost are tuned$best.parameters[1,1] & tuned$best.parameters[1,2]
  #model  <- svm(V2~., data = trainset, kernel="radial", gamma=tuned$best.parameters[1,1], cost=tuned$best.parameters[1,2]) 

  
}







# ************************** #
# *** Testing the Model *** #
# ************************** #
prediction <- predict(model, testset[,-2])

tab <- table(pred = prediction, true = testset[,2])
tab

#Produce SVM Model on the training set

#Run against the test set

#Evaluate