#read covertype data
covertype <- read.csv("Project Resources/covtype.data/covtype.data", header=FALSE)
# make covertype data a tidy format for randomForest
colnames(covertype)[55] = "forestCoverType"
# Convert forestCoverType to a factor vector as otherwise randomForest assumes you're doing regression
covertype$forestCoverType[covertype$forestCoverType == 2] <- "Lodgepole_Pine"
covertype$forestCoverType[covertype$forestCoverType == 1] <- "Spruce_Fir"
covertype$forestCoverType[covertype$forestCoverType == 3] <- "Ponderosa_Pine"
covertype$forestCoverType[covertype$forestCoverType == 4] <- "Cottonwood_Willow"
covertype$forestCoverType[covertype$forestCoverType == 5] <- "Aspen"
covertype$forestCoverType[covertype$forestCoverType == 6] <- "Douglas_Fir"
covertype$forestCoverType[covertype$forestCoverType == 7] <- "Krummholz"
covertype$forestCoverType <- as.factor(covertype$forestCoverType)

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
 output.Forest <- randomForest(forestCoverType ~ ., data=covertype, ntree= jbt, mtry = 7)
 