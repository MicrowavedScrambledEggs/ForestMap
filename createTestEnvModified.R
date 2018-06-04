#read covertype data
covertype <- read.csv("covtype.data", header=FALSE)
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
source("ExtractingRuleRF.R")
source("HackinginTrees.R")
miniRUles <- ruleExtraction(miniForest, trainMini, "forestCoverType")
refMiniRules <- ruleRefinement(miniRUles, trainMini)


# Stuff for the Mc Nemar thing
library(exact2x2)
source("McNemar_FalsePos_falseNeg.r")

# Splitting into individual conditions
refRuleConditions <- sapply(strsplit(as.character(refMiniRules[,"condition"]),"&"),"[")

# Need to be reset before testing with a new condition
a <- 0
b <- 0
c <- 0
d <- 0

for (i in 1:NROW(trainMini)) {
  testRuleCondition(trainMini[i,], refRuleConditions[[1]][1], refMiniRules[1,], "forestCoverType")
}

# Table should 
contingencyTable <- matrix(c(a,c,b,d), nrow = 2, dimnames = list(Condition = c("True", "False"), Prediction = c("True", "False")))

# Adding margines: a + b, c + d, a + c, b + d
addmargins(contingencyTable)

# "Exact" Mc Nemar test
mcnemar.exact(contingencyTable)

# Null hypothesis: the specific rule condition and the rule conclusion ARE independant 
# Alternate hypothesis: the specific rule condition and the rule concluse ARE NOT independant
# If the "Mc Nemar's chi-squared vaule" is larger than the "p-value", reject the null hypothesis of independence 