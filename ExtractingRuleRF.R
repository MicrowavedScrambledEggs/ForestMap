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
  # need to know which tree each rule came from so know what oob data to 
  # measure their accuracy on. Then ideally return a table with rules mapped
  # to tree ids
  return(rawRules)
}

# oobData would ideally be a map of tree ids to the out of bag data for 
# each tree
ruleRefinement <- function(rawRules, oobData)
{
  # Initialise weight of each rule to 1
  weightRules = data.frame(rawRules)
  weightRules$weight = rep(1, nrow(weightRules))
  # add values to frame for ranking
  weightRules$accuracy = apply(weightRules, 1, measureRuleAcc, oobData)
}

