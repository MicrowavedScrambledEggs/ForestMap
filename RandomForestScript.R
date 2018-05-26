#put covertype data into a txt file and add directory into line below.
covertype <- read.csv("Project Resources/covtype.data/covtype.data", header=FALSE)
library(randomForest)
#number of variables
mdim = 55
#number of cases in data
nsample = 581012
#number of classes
nclass = 7
#largest number of values assumed by a categorical variable
maxcat = 1
#number of cases in the test set
ntest = 11340
#0 if no class labels, 1 if labels.
labelts = 0
#iaddcl = 2
iaddcl = 1
#number of trees
jbt = 100
#can change this to get better results
mtry = 5
#how often to chek prediction error
  look = 5
  #minnimum node size (1 works fine)
  ndsize = 1
  cutoff <- 1:7
 output.Forest <- randomForest(covertype, ntree= jbt, mtry = 7)
 