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

#create 2 sets of data where the rows of covertype are partitioned by the ratio given at the end of the line
ind <- sample(3, nrow(covertype), replace = TRUE, prob = c(0.004,0.006,0.99 ))
trainMini <- covertype[ind==1,]
testMini <- covertype[ind==2,]
library(randomForest)
set.seed(222)
miniForest <- randomForest(forestCoverType ~ ., data=trainMini, ntree = 50, mtry = 17, keep.inbag= TRUE)
source('~/Uni Stuff/COMP760/ForestMap/ExtractingRuleRF.R')
source('~/Uni Stuff/COMP760/ForestMap/HackinginTrees.R')
miniRUles <- ruleExtraction(miniForest, trainMini, "forestCoverType")
