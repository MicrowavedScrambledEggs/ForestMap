library(inTrees)
library(randomForest)

# predictVarMat is the data used to train the RF
ruleExtraction <- function(rfobj, predictVarMat, classColName)
{
  # data without the class label
  X = predictVarMat[ , -which(names(predictVarMat) %in% c(classColName))]
  # class Label vector
  classLab = predictVarMat[, classColName]
  treeList <- RF2List(rfobj)
  # Setting ntree and maxdepth values to ensure rule set perfectly represents
  # raw rule set so we can do our own rule trimming. Need to check if 
  # ntree$maxdepth is actually how you get the depth of the deepest tree 
  # Just set maxdepth to 1000 for now
  # Need to call it for each tree so can keep track of which rules come from which tree
  nTree <- treeList$ntree
  for(i in 1:nTree){
    tempList <- treeList$list[i:nTree]
    tempTreeList <- treeList
    tempTreeList$list <- tempList
    rawConditions <- extractRules(tempTreeList, X, ntree = 1, maxdepth = 1000)
    rawRulesFromTree <- as.data.frame(getRuleMetric(rawConditions, X, classLab))
    rawRulesFromTree$tree <- rep(i, nrow(rawRulesFromTree))
    if(i==1) rawRules <- rawRulesFromTree
    else rawRules <- rbind(rawRules, rawRulesFromTree)
  }
  return(rawRules)
}

ruleRefinement <- function(rawRules, rfobj, predictVarMat)
{
  # Initialise weight of each rule to 1
  weightRules = as.data.frame(rawRules)
  weightRules$weight = rep(1, nrow(weightRules))
  oobIndexList <- oobIndexToTreeList(rfobj$inbag)
  # add values to frame for ranking
  weightRules$accuracy = apply(weightRules, 1, measureRuleAcc, oobIndexList, predictVarMat)
  weightRules$coverage = apply(weightRules, 1, measureRuleCov, oobIndexList, predictVarMat)
  # ExtractingRuleRF also uses 'variable importance in tree' and 'variable
  # importance in rule' but lets not use thouse just yet as they are complex
  # to measure
  # rank rules by reordering dataframe
  weightRules = weightRules[order(weightRules$accuracy, weightRules$coverage), ]
  # removing redundant conditions and rules I can't do untill I know what 
  # the rules look like
}

# Returns a list of vectors where each vector lists the row numbers of the OOB examples
# in the data used to train the random forest (predictVarMat) for the tree whose row
# number in a randomForest object's inbag matrix is the same as the vector's position 
# in the list
# Usage: oobCasesForTree2 <- trainingData[oobIndexToTreeList(rfobj$inbag)[2], ]
oobIndexToTreeList <- function(inbagMat)
{
  oobIndexList <- apply(inbagMat, 1, 
                        function(x) as.numeric(colnames(which(sapply(x, function(y) any(x==0))))))
}

measureRuleAcc <- function(ruleRow, oobIndexList, predictVarMat)
{
  # get the associated oobData set
  # treeid = ruleRow["tree"]
  # oobDataIndex = rfobj$inbag[treeid, which(apply(rfobj$inbag[treeid,],2,function(x) any(x==0)))]
  # oobDataIndex = as.numeric(colnames(oobDataIndex))
  oobData = predictVarMat[oobIndexList[ruleRow["tree"]], ]
  
  # measure the frequency of correct predictions
  correctPredict = sapply(oobData, testRule, ruleRow["rule"])
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

measureRuleCov <- function(ruleRow, oobIndexList, predictVarMat)
{
  oobData = predictVarMat[oobIndexList[ruleRow["tree"]], ]
  covered = sapply(oobData, testRuleCov, ruleRow["rule"])
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


