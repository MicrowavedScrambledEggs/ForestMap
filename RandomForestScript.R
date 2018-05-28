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

#covertype$V55 <- as.factor(covertype$V55) instead of above?

#seperating dataset into training and testing
#randomise the data set
set.seed(123)
#create 2 sets of data where the rows of covertype are partitioned by the ratio given at the end of the line
ind <- sample(3, nrow(covertype), replace = TRUE, prob = c(0.09,0.01,0.9 ))
traindata <- covertype[ind==1,]
testdata <- covertype[ind==2,]

library(randomForest)
set.seed(222)
#number of variables
#mdim = 55
#number of cases in data
#nsample = 581012
#number of classes
#nclass = 7
#largest number of values assumed by a categorical variable
#maxcat = 1
#number of cases in the test set
#ntest = 1482
#0 if no class labels, 1 if labels.
#labelts = 0
#iaddcl = 2
#iaddcl = 1
#number of trees
#ntree = 500
#can change this to get better results (by default should be around sqrt of number of varaibles in model)
#mtry = 7
#how often to chek prediction error
 # look = 5
  #minnimum node size (1 works fine)

 # ndsize = 1
 # cutoff <- 1:7
# Note from badi, Need the oob data for ExtractingRuleRF, so setting keep.inbag to true
output.Forest <- randomForest(forestCoverType ~ ., data=traindata, ntree = 500, mtry = 17, keep.inbag= TRUE)
#output.Forest <- randomForest(forestCoverType ~ ., data=traindata, ntree = 100, mtry = 17, maxcat = 1, look= 5 )
print(output.Forest)
#check confusions matrix
output.Forest$confusion
#for some predictions
library(caret)
predict1 <- predict(output.Forest, traindata)
#look to see if its matching initially
head(predict1)
head(traindata$forestCoverType)
#check confussion matrix
confusionMatrix(predict1, traindata$forestCoverType)

predict2 <- predict(output.Forest, testdata)
head(predict2)
head(testdata$forestCoverType)
confusionMatrix(predict2, testdata$forestCoverType)

#check if error is reducing as ntrees increases
plot(output.Forest)

#These trees are massive btw
getTree(output.Forest,1,labelVar = TRUE)



