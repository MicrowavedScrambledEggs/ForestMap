# Requires the functions from HackinginTrees.R

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
  rawConditions <- as.data.frame(extractRulesHack(treeList, X, classLab, ntree = nTree, 
                                                  maxdepth = 1000))
  # TODO: Do a modified getRuleMetric to only look at oob data
  # Get oob case indexes for each tree
  oobIndexes <- oobIndexesFromEachTree(rfobj$inbag)
  
  rawRules <- getRuleMetricHack(rawConditions, X, classLab, oobIndexes)

  return(rawRules)
}

ruleRefinement <- function(rawRules, predictVarMat)
{
  # timeStart <- Sys.time() 
  # numRules <- nrow(rawRules)
  rawRules <- as.data.frame(rawRules)
  # remove all rules with f score of 0
  #rawRules <- rawRules[rawRules[,'f_score'] != 0, ]
  
  # removing redundant rules
  # Build a string of 1s and 0s where each position represents the presence or absence
  # of an attribute-operation combo in a rule condition, for each rule 
  # Example: a "1" in the first character of the string means the rule has
  #           "X[,1]<=" in its condition
  condStrings <- paste0("X[,", 1:ncol(predictVarMat), "]<=")
  condStrings <- c(condStrings, paste0("X[,", 1:ncol(predictVarMat), "]>"))
  condno <- function(condition, condStrings)
  {
    condOneNo <- function(condStr, condition)
    {
      num <- rep(0, ceiling(length(condStrings)*2/30))
      if (grepl(condStr, condition, fixed = TRUE))
      {
        matchNo <- match(condStr, condStrings)-1
        num[floor(matchNo/30)+1] <- 2**(matchNo %% 30)
      }
      return(num)
    }
    condAllNo <- apply(sapply(condStrings, condOneNo, condition),1,sum)
    toReturn <- paste0(intToBits(condAllNo), collapse = "")
    return(toReturn)
  }
  reducedRules <- NULL
  rawRules <- cbind(rawRules, condNums = sapply(rawRules[, 'condition'], condno, condStrings))
  
  ## Variant 4: This is apparently the fastest way to do rule refinement
  
  ruleWeights <- table(rawRules$pred, rawRules$condNums)
  # rank rules by reordering dataframe
  rawRules = rawRules[order(-as.numeric(rawRules$f_score), rawRules$err,
                                    -as.numeric(rawRules$freq)), ]
  # Pick just the highest ranked one of each condNum prediction combo
  reducedRules <- rawRules[row.names(unique(rawRules[,c("condNums", "pred")])), ]
  #assign weights
  reducedRules$weight <- apply(reducedRules,1, function(x) ruleWeights[x['pred'], x['condNums']])
  reducedRules <- reducedRules[, -which(names(reducedRules) %in% c('condNums'))]
  
  # timeTaken <- Sys.time() - timeStart
  # print(paste("Time taken with", numRules, "rules:", timeTaken))
  return(reducedRules)
}

condAttriValMat <- function(condition) 
{
  conds <- unlist(strsplit(condition, "&"))
  attris <- unlist(regmatches(condition, gregexpr("X\\[,\\d+\\](<=|>)", condition)))
  matr <- t(rbind(conds, attris))
  vals <- as.numeric(apply(matr, 1, function(a) gsub(a[2], "", a[1], fixed = TRUE)))
  return(t(rbind(conds, attris, vals)))
}

# Returns a list of vectors where each vector lists the row numbers of the OOB examples
# in the data used to train the random forest (predictVarMat) for the tree whose row
# number in a randomForest object's inbag matrix is the same as the vector's position 
# in the list
# Usage: oobCasesForTree2 <- trainingData[unlist(oobIndexesFromEachTree(rfobj$inbag)[2]), ]
oobIndexesFromEachTree <- function(inbagMat)
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

testRuleCov <- function(X, rule)
{
  # return 1 if case satisfies all conditions of rule, regardless if rule 
  # says the same class. 0 if not
  if(eval(parse(text=gsub(",", "", rule["condition"])))) return(1)
  return(0)
}

ruleSetPredict <- function(weightedRuleSet, testSet)
{
  votes <- matrix(0, nrow = nrow(testSet), 
                  ncol = length(levels(weightedRuleSet$pred)))
  colnames(votes) <- levels(weightedRuleSet$pred)
  condChars <- as.character(weightedRuleSet$condition)
  condChars <- paste0("which(", condChars, ")")
  condToPred <- cbind(condChars, weightedRuleSet$pred, 
                      weightedRuleSet$weight)
  X <- testSet
  matches <- sapply(condChars, function(v) eval(parse(text = v)))
  
  for(i in 1:nrow(condToPred)) {
    match <- matches[[i]]
    pred <- as.numeric(condToPred[i,2])
    weight <- as.numeric(condToPred[i,3])
    if(length(match) > 0){
      votes[match, pred] <- votes[match, pred] + weight
    }
  }

  return(apply(votes, 1, function(v) names(which.max(v))))
}
