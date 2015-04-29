#emission is the training data, intensities and annotation

hmmStrQual = function(observations, initialDist, transition, emission, states) {
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
  # minIntens = range(emission$intesity)[1]
  maxIntens = max(emission$intensity)
  # maxIntens = range(emission$intesity)[2]
  # Split the data into 30 blocks so we can still get an idea of the distribution
  blocks = (maxIntens - minIntens)/30
  # Need to allow for values either side of the range in the case of test outliers
  dist = c(-Inf, seq(minIntens, maxIntens, by=blocks), Inf)
  # Create a new column that groups the values based on the given blocks
  emission$groups = cut(emission$intensity, dist, labels=emmisNames) 
  observations$groups = cut(observations, dist, labels=emmisNames)
    
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
  
  tr0tab[tr0tab<0.001] = unique(runif(0.0005, 0.0015, n=2*length(tr0tab[tr0tab<0.001])))[1:length(tr0tab[tr0tab<0.001])]
  tr0tab = tr0tab/sum(tr0tab)
  tr0Vec = as.vector(tr0tab)
  tr1tab[tr1tab<0.001] = unique(runif(0.0005, 0.0015, n=2*length(tr1tab[tr1tab<0.001])))[1:length(tr1tab[tr1tab<0.001])]
  tr1tab = tr1tab/sum(tr1tab)
  tr1Vec = as.vector(tr1tab)
  tr2tab[tr2tab<0.001] = unique(runif(0.0005, 0.0015, n=2*length(tr2tab[tr2tab<0.001])))[1:length(tr2tab[tr2tab<0.001])]
  tr2tab = tr2tab/sum(tr2tab)
  tr2Vec = as.vector(tr2tab)
  
  # Create the emission matrix
  emissionDist = rbind(tr0Vec, tr1Vec, tr2Vec)  
   
  for(i in 1:totalStates){
    title = paste("probability of", states[i], sep=" ")
    dfQual = cbind(dfQual, title = numeric(duration))
  }
  dfQualDim=dim(dfQual)
  for(j in 1:duration) {
    location = as.integer(substr(as.character(observations$groups[j]), 4, nchar(as.character(observations$groups[j]))))
    # location = which(as.character(emission$groups) == as.character(observations$groups[j]))[1:3]
    # location = location[location < dim(emissionDist)[2]]
    if(j == 1){
      
      dfQual[1,3:dfQualDim[2]] = initialDist * emissionDist[,location]
      dfQual[1,2] = which(dfQual[1,3:dfQualDim[2]] == max(dfQual[1,3:dfQualDim[2]])) - 1
      
    } else {
      print(j)
      dfQual[j,3:dfQualDim[2]] = max(dfQual[(j-1),3:dfQualDim[2]]) * emissionDist[,location] * transition[dfQual[(j-1),2]+1,]
      dfQual[j,2] = which(dfQual[j,3:dfQualDim[2]] == max(dfQual[j,3:dfQualDim[2]])) - 1
    }
  }
  return(dfQual)
}

# load project specific libraries
source("/home/pet5o/workspace/TDP/R/group-har/activityRecognitionFunctions.R")

# examples of inputs for 'hmmStrQual'
initialDist = c(19/24, 3.5/24, 1.5/24)
transition = rbind(c(9/10, 61/1140, 53/1140),
                   c(2/7, 2/3, 1/21),
                   c(6/10, 1/10, 3/10))

# load the data - observations
path = "/home/pet5o/workspace/TDP/DataEvaluation/pet_01/featureData/Peter_003_left hip_020088_2015-03-10 18-40-35_annotated_features.csv"
observations_all = read.csv(path)

# strip unannotated data 
# observations = observations[complete.cases(observations[,"activity"]),]

# load training data
hipDataPath = "Peter_003_left hip_020088_2015-03-10 18-40-35_annotated_features.csv"
# wristTrainPath = "/home/pet5o/workspace/TDP/DataEvaluation/pet_01/trainingSets/wrist"
hipTrainPath = "/home/pet5o/workspace/TDP/DataEvaluation/pet_01/trainingSets/hip"
trainHip = loadTrainingData(hipTrainPath)
emission = data.frame(intensity=trainHip$statSummary, activity=trainHip$activity)

# setup states
states = c(0,1,2)

out = hmmStrQual(observations$statSummary, initialDist, transition, emission, states)
observations=observations$statSummary
result = data.frame(timestamp=head(observations_all[,1], n=335), activity= observations_all$activity[1:335],prediction = dfQual[1:335,2])
# write.csv(result, "HMM_first335observations.csv", row.names=TRUE)

for(j in 1:25){
  print(j)
  location = which(emission$groups == observations$groups[j])[1:3]
  print(location)
}