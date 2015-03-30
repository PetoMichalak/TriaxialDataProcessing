#SVM library
require(e1071)
doSVM = function(formula, trainset, testset){
  
  # ******************************* #
  # *** Choosing the Parameters *** #
  # ******************************* #
  tuned <- tune.svm(formula, data = trainset, gamma = 10^(-6:-1), cost = 10^(-1:1)) 
  
  cat("Gamma: ",tuned$best.parameters[1,1],"\n")
  cat("Cost:  ",tuned$best.parameters[1,2],"\n")

  # ************************** #
  # *** Training the model *** #
  # ************************** #
  #From the tuned data we can see that the best gamma and cost are tuned$best.parameters[1,1] & tuned$best.parameters[1,2]
  model  <- svm(formula, data = trainset, kernel="radial", gamma=tuned$best.parameters[1,1], cost=tuned$best.parameters[1,2]) 
  
  # ************************** #
  # *** Testing the Model *** #
  # ************************** #ls
  prediction <- predict(model, testset[,-2])
  
  tab <- table(pred = prediction, true = testset[,2])
  return(tab)
}