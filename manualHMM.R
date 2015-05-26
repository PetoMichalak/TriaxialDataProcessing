# RUN THIS FIRST
# set your own working directory
setwd("C:/Users/Stapose/CDT/Group-project/HMM")

bigWrist2 = read.csv("wrist_all_testdata_withRest_features.csv")
bigHip2 = read.csv("hip_all_testdata_withRest_features.csv")

th0 = bigHip2[bigHip2$activity==0,]
th1 = bigHip2[bigHip2$activity==1,]
th2 = bigHip2[bigHip2$activity==2,]

tw0 = bigWrist2[bigWrist2$activity==0,]
tw1 = bigWrist2[bigWrist2$activity==1,]
tw2 = bigWrist2[bigWrist2$activity==2,]



#emission is the training data, intensities and annotation

hmmStrQual = function(observations, initialDist, transition, states, body="w") {
  totalStates = length(states)
  duration = length(observations)
  dfQual = data.frame(intensity=observations, currentState=integer(duration))
  
  # Want to establish range of values we think are plausible
  minIntens = 0.07
  maxIntens = 0.25
  # Split the data into 30 blocks so we can still get an idea of the distribution
  blocks = (maxIntens - minIntens)/31
  # Need to allow for values either side of the range in the case of test outliers
  dist = seq(minIntens, maxIntens, by=blocks)
  # Create a new column that groups the values based on the given blocks
  emmisNames = character(31)
  for (i in 1:31){
    emmisNames[i] = paste("Bin", i, sep = "")
  }
  
  observations$groups = cut(observations$intens, dist, labels=emmisNames)
  
  # Create the emission matrix
  
  if(body=="h"){
    # Hip0
    minH0 = 0.07
    maxH0 = 0.25
    x = seq(minH0,maxH0,length=30)
    x=c(-Inf, x, Inf)
    meanH0 = mean(th0$statSummary)
    sdH0 = sd(th0$statSummary)
    distH0 = pnorm(x[2:32], meanH0, sdH0) - pnorm(x[1:31], meanH0, sdH0)
    distH0[1] = 0.05
    distH0 = distH0/(sum(distH0))
    
    # Hip1
    bcH1 = ((th1$statSummary)^(-2)-1)/(-2)
    minH1 = ((0.07)^(-2)-1)/(-2)
    maxH1 = ((0.25)^(-2)-1)/(-2)
    x = seq(minH1,maxH1,length=30)
    x=c(-Inf, x, Inf)
    meanH1 = mean(bcH1)
    sdH1 = sd(bcH1)
    distH1 = pnorm(x[2:32], meanH1, sdH1) - pnorm(x[1:31], meanH1, sdH1)
    
    # Hip2
    bcH2 = ((th2$statSummary)^(1.5)-1)/(1.5)
    minH2 = ((0.07)^(1.5)-1)/(1.5)
    maxH2 = ((0.25)^(1.5)-1)/(1.5)
    x = seq(minH2,maxH2,length=30)
    x=c(-Inf, x, Inf)
    meanH2 = mean(bcH2)
    sdH2 = sd(bcH2)
    distH2 = pnorm(x[2:32], meanH2, sdH2) - pnorm(x[1:31], meanH2, sdH2)
    
    emissionDistH = rbind(distH0, distH1, distH2) 
    
  } else {
    # Wrist0
    bcW0 = ((tw0$statSummary)^(-3.8)-1)/(-3.8)
    minW0 = ((0.07)^(-3.8)-1)/(-3.8)
    maxW0 = ((0.25)^(-3.8)-1)/(-3.8)
    x = seq(minW0,maxW0,length=30)
    x=c(-Inf, x, Inf)
    meanW0 = mean(bcW0)
    sdW0 = sd(bcW0)
    distW0 = pnorm(x[2:32], meanW0, sdW0) - pnorm(x[1:31], meanW0, sdW0)
    
    # Wrist1
    bcW1 = ((tw1$statSummary)^(-3)-1)/(-3)
    minW1 = ((0.07)^(-3)-1)/(-3)
    maxW1 = ((0.25)^(-3)-1)/(-3)
    x = seq(minW1,maxW1,length=30)
    x=c(-Inf, x, Inf)
    meanW1 = mean(bcW1)
    sdW1 = sd(bcW1)
    distW1 = pnorm(x[2:32], meanW1, sdW1) - pnorm(x[1:31], meanW1, sdW1)
    
    # Wrist2
    bcW2 = ((tw2$statSummary)^(-0.8)-1)/(-0.8)
    minW2 = ((0.07)^(-0.8)-1)/(-0.8)
    maxW2 = ((0.25)^(-0.8)-1)/(-0.8)
    x = seq(minW2,maxW2,length=30)
    x=c(-Inf, x, Inf)
    meanW2 = mean(bcW2)
    sdW2 = sd(bcW2)
    distW2 = pnorm(x[2:32], meanW2, sdW2) - pnorm(x[1:31], meanW2, sdW2)
    
    emissionDistW = rbind(distW0, distW1, distW2) 
  }
  
  
  for(i in 1:totalStates){
    title = paste("probability of", states[i], sep=" ")
    dfQual = cbind(dfQual, title = numeric(duration))
  }
  dfQualDim=dim(dfQual)
  if(body=="h"){
    
    for(j in 1:duration) {
      location = as.integer(substr(as.character(observations$groups[j]), 4, nchar(as.character(observations$groups[j]))))
      # location = which(as.character(emission$groups) == as.character(observations$groups[j]))[1:3]
      # location = location[location < dim(emissionDist)[2]]
      if(j == 1){
        
        dfQual[1,3:dfQualDim[2]] = log(initialDist) + log(emissionDistH[,location])
        dfQual[1,2] = which(dfQual[1,3:dfQualDim[2]] == max(dfQual[1,3:dfQualDim[2]])) - 1
        
      } else {
        print(j)
        dfQual[j,3:dfQualDim[2]] = max(dfQual[(j-1),3:dfQualDim[2]]) + log(emissionDistH[,location]) + log(transition[dfQual[(j-1),2]+1,])
        dfQual[j,2] = which(dfQual[j,3:dfQualDim[2]] == max(dfQual[j,3:dfQualDim[2]])) - 1
      }
    }
  } else {
    for(j in 1:duration) {
      location = as.integer(substr(as.character(observations$groups[j]), 4, nchar(as.character(observations$groups[j]))))
      # location = which(as.character(emission$groups) == as.character(observations$groups[j]))[1:3]
      # location = location[location < dim(emissionDist)[2]]
      if(j == 1){
        
        dfQual[1,3:dfQualDim[2]] = log(initialDist) + log(emissionDistW[,location])
        dfQual[1,2] = which(dfQual[1,3:dfQualDim[2]] == max(dfQual[1,3:dfQualDim[2]])) - 1
        
      } else {
        print(j)
        dfQual[j,3:dfQualDim[2]] = max(dfQual[(j-1),3:dfQualDim[2]]) + log(emissionDistW[,location]) + log(transition[dfQual[(j-1),2]+1,])
        dfQual[j,2] = which(dfQual[j,3:dfQualDim[2]] == max(dfQual[j,3:dfQualDim[2]])) - 1
      }
    }
  }
  return(dfQual)
}



# examples of inputs for 'hmmStrQual'
initialDist = c(19/24, 3.5/24, 1.5/24)
transition1 = rbind(c(9/10, 61/1140, 53/1140),
                    c(2/7, 2/3, 1/21),
                    c(6/10, 1/10, 3/10))

transition2 = rbind(c(3215/3216, 1/3216, 0),
                    c(1/180, 1073/1080, 1/1080),
                    c(0, 1/120, 119/120))

# setup states
states = c(0,1,2)

out1 = hmmStrQual(bigHip2$statSummary, initialDist, transition1, states, body="h")
out2 = hmmStrQual(bigWrist2$statSummary, initialDist, transition1, states, body="w")
out3 = hmmStrQual(bigHip2$statSummary, initialDist, transition2, states, body="h")
out4 = hmmStrQual(bigWrist2$statSummary, initialDist, transition2, states, body="w")

