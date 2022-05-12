#
# Assignment - Probability
# Cian Farrell
#


forename = "Cian"
surname = "Farrell"
studentID = 20094046
studentProgramme = "Games Development"

q1 = function(){
#exactP = pbinom(2,29, 6/29,lower=F)
exactP = choose(6,2)*choose(23,4)/choose(29,6)
results = c()
runs = 50
for(run in 1:runs){
  loops = 200
  twocount = 0
  for(loop in 1:loops){
    ransample = sample(29,6)
    if(length(ransample[ransample<=6])==2){
      twocount=twocount+1
    }
  }
  estimate = twocount/200
  results = c(results,estimate)
}
estimateP = mean(results)
precisionP = sd(results)
errorP = abs(exactP-estimateP)
errorCheck = sqrt(50)*errorP/precisionP
round(c(exactP, estimateP, precisionP, errorP, errorCheck),4)
}
q2 = function(){
  exactP = 1- (choose(7,4)*factorial(4))/(7^4)
  results = c()
  runs = 50
  for(run in 1:runs){
    loops = 200
    count = 0
    for(loop in 1:loops){
      sample = sample(7,4,replace=T)
      if(length(unique(sample))!=4){
        count = count +1
      }
    }
    estimate = count/200
    results = c(results,estimate)
  }
  estimateP = mean(results)
  precisionP = sd(results)
  errorP = abs(exactP-estimateP)
  errorCheck = sqrt(50)*errorP/precisionP
  round(c(exactP, estimateP, precisionP, errorP, errorCheck),4)
}
q3 = function(){
  exactP = pbinom(2,126,1/choose(8,3),lower=F)
  results = c()
  runs = 50
  for(run in 1:runs){
    loops = 200
    count=0
    for(loop in 1:loops){
      fcount=0
      for(i in 1:126){
        
        sample = sample(8,3)
        if(length(sample[sample<=3])==3){
          fcount=fcount+1
        }
      }
      if(fcount>=3){
        count=count+1
      }
      
    }
   # sample = rbinom(126,200,1/choose(8,3))
   # count = length(sample[sample<3])
    estimate = count/200
    results = c(results,estimate)
  }
  estimateP = mean(results)
  precisionP = sd(results)
  errorP = abs(exactP-estimateP)
  errorCheck = sqrt(50)*errorP/precisionP
  round(c(exactP, estimateP, precisionP, errorP, errorCheck),4)
}

q4 = function(){
  #111 hours/ticket
  #1/111 tickets per hour
  #64/37 tickets per 8 days
  
  exactP = ppois(1,64/37,lower = F)
  results = c()
  runs = 50
  for(run in 1:runs){
    sample = rpois(200,64/37)
    count = length(sample[sample>=2])
    estimate = count/200
    results = c(results,estimate)
  }
  estimateP = mean(results)
  precisionP = sd(results)
  errorP = abs(exactP-estimateP)
  errorCheck = sqrt(50)*errorP/precisionP
  round(c(exactP, estimateP, precisionP, errorP, errorCheck),4)
}
q5 = function(){
  exactP = pnorm(193,136,44)-pnorm(114,136,44)
  results = c()
  runs = 50
  for(run in 1:runs){
    sample = rnorm(200,136,44)
    count = sample[114<=sample]
    count = length(count[count<=193])
    estimate = count/200
    results = c(results,estimate)
  }
  estimateP = mean(results)
  precisionP = sd(results)
  errorP = abs(exactP-estimateP)
  errorCheck = sqrt(50)*errorP/precisionP
  round(c(exactP, estimateP, precisionP, errorP, errorCheck),4)
}
pick = function(n,k){
       value = n
       for(run in 1:(k-1)){
             value = value*(n-run)
         }
       value
   }