library(inTrees)
library(randomForest)

# predictVarMat is the data used to train the RF
ruleExtraction <- function(rfobj, predictVarMat, classColName)
{
  # data without the class label
  X = predictVarMat[ , -which(names(predictVarMat) %in% c(classColName))]
  # class Label vector
  classLab = subset(predictVarMat, select=c(classColName))
  treeList <- RF2List(rfobj)
  # Setting ntree and maxdepth values to ensure rule set perfectly represents
  # raw rule set so we can do our own rule trimming. Need to check if 
  # ntree$maxdepth is actually how you get the depth of the deepest tree 
  # Just set maxdepth to 100 for now
  rawConditions <- extractRules(treeList, X, ntree = treeList$ntree, maxdepth = 100)
  rawRules <- measureRule(rawConditions, X, classLab)
  # need to know which tree each rule came from so know what oob data to 
  # measure their accuracy on. Then ideally return a table with rules mapped
  # to tree ids
  return(rawRules)
}

# oobData would ideally be a list of out of bag data sets where their 
# position in the list corresponds to the tree id they're associated with
ruleRefinement <- function(rawRules, oobData)
{
  # Initialise weight of each rule to 1
  weightRules = as.data.frame(rawRules)
  weightRules$weight = rep(1, nrow(weightRules))
  # add values to frame for ranking
  weightRules$accuracy = apply(weightRules, 1, measureRuleAcc, oobData)
  weightRules$coverage = apply(weightRules, 1, measureRuleCov, oobData)
  # ExtractingRuleRF also uses 'variable importance in tree' and 'variable
  # importance in rule' but lets not use thouse just yet as they are complex
  # to measure
  # rank rules by reordering dataframe
  weightRules = weightRules[order(weightRules$accuracy, weightRules$coverage), ]
  # removing redundant conditions and rules I can't do untill I know what 
  # the rules look like
}

measureRuleAcc <- function(ruleRow, oobData)
{
  # get the associated oobData set and measure
  # the frequency of correct predictions
  correctPredict = sapply(oobData[ruleRow["tree"]], testRule, ruleRow["rule"])
  freqCorrect = table(correctPredict)
  return(freqCorrect[names(freqCorrect)==1] / length(freqCorrect))
}

# Will have to rewrite this once I know how what the hell extracted rules
# look like and how to apply them
testRule <- function(case, rule)
{
  # True Positive
  # If the case meets the conditions of the rule and is the class it says 
  # it should be, return 1 for correct
  
  # True Negative
  # If the case does not meet the conditions of the rule and is not the 
  # class that cases that meet those conditions will be classified as by the
  # rule, return 1 for correct
  
  # False positive and false negative, return 0 for incorrect
}

measureRuleCov <- function(ruleRow, oobData)
{
  covered = sapply(oobData[ruleRow["tree"]], testRuleCov, ruleRow["rule"])
  freqCovered = table(covered)
  return(freqCovered[names(freqCovered)==1] / length(freqCovered))
}

# Will have to rewrite this once I know how what the hell extracted rules
# look like and how to apply them
testRuleCov <- function(case, rule)
{
  # return 1 if case satisfies all conditions of rule, regardless if rule 
  # says the same class. 0 if not
}


