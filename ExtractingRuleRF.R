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
  # raw rule set so we can do our own rule trimming. 
  # Just set maxdepth to 1000 untill I know how to get the depth of the deepest tree
  nTree <- treeList$ntree
  rawConditions <- as.data.frame(extractRulesHack(treeList, X, ntree = nTree, maxdepth = 1000))
  # TODO: Do a modified getRuleMetric to only look at oob data
  rawRulesFromTree <- getRuleMetric(rawConditions, X, classLab)
  # Remove redundant conditions from rules (easier to do it here)
  rawRulesFromTree <- as.data.frame(pruneRule(rawRulesFromTree, X, classLab))
  rawRulesFromTree$tree <- rep(i, nrow(rawRulesFromTree))
  if(i==1) rawRules <- rawRulesFromTree
  else rawRules <- rbind(rawRules, rawRulesFromTree)

  return(rawRules)
}

ruleRefinement <- function(rawRules, rfobj, predictVarMat, classColName)
{
  # Initialise weight of each rule to 1
  weightRules = as.data.frame(rawRules)
  weightRules$weight = rep(1, nrow(weightRules))
  oobGetTime <- Sys.time()
  oobIndexList <- oobIndexToTreeList(rfobj$inbag)
  cat("Time to get oob indexes", Sys.time() - oobGetTime, '\n')
  # add values to frame for ranking
  accuracyAllTime <- Sys.time() 
  weightRules$accuracy = apply(weightRules, 1, measureRuleAcc, oobIndexList, 
                               predictVarMat, classColName)
  cat("Time to measure oob accuracy of all rules", Sys.time() - oobGetTime, '\n')
  weightRules$coverage = apply(weightRules, 1, measureRuleCov, oobIndexList, predictVarMat)
  # ExtractingRuleRF also uses 'variable importance in tree' and 'variable
  # importance in rule' but lets not use thouse just yet as they are complex
  # to measure
  # rank rules by reordering dataframe
  weightRules = weightRules[order(weightRules$accuracy, weightRules$coverage), ]
  # removing redundant rules
  
  return(weightRules)
}

# Returns a list of vectors where each vector lists the row numbers of the OOB examples
# in the data used to train the random forest (predictVarMat) for the tree whose row
# number in a randomForest object's inbag matrix is the same as the vector's position 
# in the list
# Usage: oobCasesForTree2 <- trainingData[unlist(oobIndexToTreeList(rfobj$inbag)[2]), ]
oobIndexToTreeList <- function(inbagMat)
{
  oobIndexList <- apply(inbagMat, 2, 
                        function(x) which(sapply(x, function(y) any(y==0))))
}

measureRuleAcc <- function(ruleRow, oobIndexList, predictVarMat, classColName)
{
  # get the associated oobData set
  # treeid = ruleRow["tree"]
  # oobDataIndex = rfobj$inbag[treeid, which(apply(rfobj$inbag[treeid,],2,function(x) any(x==0)))]
  # oobDataIndex = as.numeric(colnames(oobDataIndex))
  oobData = predictVarMat[unlist(oobIndexList[as.numeric(ruleRow["tree"])]), ]
  
  # measure the frequency of correct predictions
  correctPredict = apply(oobData, 1, testRule, ruleRow, classColName)
  freqCorrect = table(correctPredict)
  return(freqCorrect[names(freqCorrect)==1] / length(freqCorrect))
}

# Will have to rewrite this once I know how what the hell extracted rules
# look like and how to apply them
testRule <- function(X, rule, classColName)
{
  # True Positive
  # If the case meets the conditions of the rule and is the class it says 
  # it should be, return 1 for correct
  if(eval(parse(text=gsub(",", "", rule["condition"]))) & X[classColName] == rule["pred"]) return(1)
  # True Negative
  # If the case does not meet the conditions of the rule and is not the 
  # class that cases that meet those conditions will be classified as by the
  # rule, return 1 for correct
  if(!eval(parse(text=gsub(",", "", rule["condition"]))) & X[classColName] != rule["pred"]) return(1)
  # False positive and false negative, return 0 for incorrect
  return(0)
}

measureRuleCov <- function(ruleRow, oobIndexList, predictVarMat)
{
  oobData = predictVarMat[unlist(oobIndexList[as.numeric(ruleRow["tree"])]), ]
  covered = sapply(oobData, testRuleCov, ruleRow)
  freqCovered = table(covered)
  return(freqCovered[names(freqCovered)==1] / length(freqCovered))
}

# Will have to rewrite this once I know how what the hell extracted rules
# look like and how to apply them
testRuleCov <- function(case, rule)
{
  # return 1 if case satisfies all conditions of rule, regardless if rule 
  # says the same class. 0 if not
  if(eval(parse(text=gsub(",", "", rule["condition"])))) return(1)
  return(0)
}


