library(inTrees)
library(randomForest)

# predictVarMat is the data used to train the RF without the class labels
ruleExtraction <- function(rfobj, predictVarMat)
{
  treeList <- RF2List(rfobj)
  # Setting ntree and maxdepth values to ensure rule set perfectly represents
  # raw rule set so we can do our own rule trimming. Need to check if 
  # ntree$maxdepth is actually how you get the depth of the deepest tree 
  rawRules <- extractRules(treeList, predictVarMat, ntree = treeList$ntree,
                           maxdepth = ntree$maxdepth)
  return(rawRules)
}