#emission is the training data, intensities and annotation

hmmStrQual = function(observations, initialDist, transition, emission, states){
  totalStates = length(states)
  duration = length(observations)
  dfQual = data.frame(intensity=observations, currentState=integer(duration))
  
  
  
  # Setting up the emission probabilities
  
  # Create the emission distributions
  # Emission matrix for HMM require discrete data
  # Movement data is continuous
  # Need to put each piece of data into a bin representing a range of values
  emmisNames = character(32)
  for (i in 1:32){
    emmisNames[i] = paste("Bin", i, sep = "")
  }
  
  # Want to establish range of values we think are plausible
  minIntens = min(emission$intensity)
  maxIntens = max(emission$intensity)
  # Split the data into 30 blocks so we can still get an idea of the distribution
  blocks = (maxIntens - minIntens)/30
  # Need to allow for values either side of the range in the case of test outliers
  dist = c(-Inf, seq(minIntens, maxIntens, by=blocks), Inf)
  # Create a new column that groups the values based on the given blocks
  emission$groups = cut(emission$intensity, dist, labels=emmisNames) 
  observations$groups = cut(emission$intensity, dist, labels=emmisNames)
    
  # Seperate data frames into various activities
  trainRestData = emission[emission$activity == 0,]
  trainLightData = emission[emission$activity == 1,]
  trainHeavyData = emission[emission$activity == 2,]
  
  # Changed the names to save typing but kept above so I knew what they represented
  tr0 = trainRestData
  tr1 = trainLightData
  tr2 = trainHeavyData

  # Put the group data into table to easily work with frequenices
  # Divided by the total to find the probabilities
  tr0tab = table(tr0$groups)/(sum(table(tr0$groups)))
  tr1tab = table(tr1$groups)/(sum(table(tr1$groups)))
  tr2tab = table(tr2$groups)/(sum(table(tr2$groups)))
  
  # Really none of the groups should have a zero probability
  # Ideally would find the best fitting normal fitting distribution
  # But a quick fix for now is just assign all the negligable probabilities to 0.001
  # Normalise after to ensure the sum equals 1
  # Convert to vector for the HMM function
  
  tr0tab[tr0tab<0.001] = 0.001
  tr0tab = tr0tab/sum(tr0tab)
  tr0Vec = as.vector(tr0tab)
  tr1tab[tr1tab<0.001] = 0.001
  tr1tab = tr1tab/sum(tr1tab)
  tr1Vec = as.vector(tr1tab)
  tr2tab[tr2tab<0.001] = 0.001
  tr2tab = tr2tab/sum(tr2tab)
  tr2Vec = as.vector(tr2tab)
  
  # Create the emission matrix
  emissionDist = rbind(tr0Vec, tr1Vec, tr2Vec)  
   
  for(i in 1:totalStates){
    title = paste("probability of", states[i], sep=" ")
    dfQual = cbind(dfQual, title = numeric(duration))
  }
  dfQualDim=dim(dfQual)
  for(j in 1:duration){
    location = which(emission$groups == observations$groups[j])
    if(j = 1){
      
      dfQual[1,3:dfQualDim[2]] = initialDist * emissionDist[,location]
      dfQual[1,2] = which(dfQual[1,3:dfQualDim[2]] == max(dfQual[1,3:dfQualDim[2]])) - 3
      
    } else {
      
      dfQual[j,3:dfQualDim[2]] = max(dfQual[(j-1),4:dfQualDim[2]]) * emission[,location] * 
        transition[dfQual[(j-1),2],]
      dfQual[j,2] = which(dfQual[j,3:dfQualDim[2]] == max(dfQual[j,3:dfQualDim[2]])) - 3
    }
  }
  return(dfQual)
}