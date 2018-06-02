library(inTrees)

# Modifying the inTrees functions to work for ExtractingRuleRF

# Modified to also retun the rule's predictions and the tree the rule came from
extractRulesHack <- function (treeList, X, classLab, ntree = 100, maxdepth = 6, random = FALSE, 
                              digits = NULL) 
{
  if (is.numeric(digits)) 
    digits <- as.integer(abs(digits))
  levelX = list()
  for (iX in 1:ncol(X)) levelX <- c(levelX, list(levels(X[, 
                                                          iX])))
  ntree = min(treeList$ntree, ntree)
  allRulesList = list()
  allRulesPredList = list()
  allRulesTreeList = list()
  for (iTree in 1:ntree) {
    if (random == TRUE) {
      max_length = sample(1:maxdepth, 1, replace = FALSE)
    }
    else {
      max_length = maxdepth
    }
    rule = list()
    count = 0
    rowIx = 1
    tree <- treeList$list[[iTree]]
    if (nrow(tree) <= 1) 
      next
    ruleSet = vector("list", length(which(tree[, "status"] == -1)))
    predSet = vector("list", length(which(tree[, "status"] == -1)))
    res = treeVisitHack(tree, rowIx = rowIx, count, ruleSet, predSet, 
                    rule, levelX, length = 0, max_length = max_length, 
                    digits = digits)
    allRulesList = c(allRulesList, res$ruleSet)
    allRulesPredList = c(allRulesPredList, res$predSet)
    allRulesTreeList = c(allRulesTreeList, rep(iTree, length(res$ruleSet)))
  }
  allRulesList <- allRulesList[!unlist(lapply(allRulesList, is.null))]
  allRulesPredList <- allRulesPredList[!unlist(lapply(allRulesList, is.null))]
  # In the tree list the predictions are the number of the factor's level
  # So need to convert to class names
  allRulesPredList <- sapply(allRulesPredList, function(x) levels(classLab)[x])
  allRulesTreeList <- allRulesTreeList[!unlist(lapply(allRulesList, is.null))]
  cat(paste(length(allRulesList), " rules (length<=", max_length, 
            ") were extracted from the first ", ntree, " trees.", 
            "\n", sep = ""))
  rulesExec <- ruleList2Exec(X, allRulesList)
  rulesExec <- cbind(rulesExec, allRulesPredList, allRulesTreeList)
  colnames(rulesExec) <- c("condition", "prediction", "tree")
  return(rulesExec)
}

# Modified to return the rule prediction and ignore redundant conditions
treeVisitHack <- function (tree, rowIx, count, ruleSet, predSet, rule, levelX, length, 
                           max_length, digits = NULL) 
{
  if (tree[rowIx, "status"] == -1 | length == max_length) {
    count = count + 1
    ruleSet[[count]] = rule
    predSet[[count]] = tree[rowIx, "prediction"]
    return(list(ruleSet = ruleSet, count = count, predSet = predSet))
  }
  xIx <- tree[rowIx, "split var"]
  xValue <- tree[rowIx, "split point"]
  if (is.integer(digits)) 
    xValue <- round(tree[rowIx, "split point"], digits)
  if (is.null(levelX[[xIx]])) {
    lValue <- paste("X[,", xIx, "]<=", xValue, sep = "")
    rValue <- paste("X[,", xIx, "]>", xValue, sep = "")
  }
  else {
    xValue <- which(as.integer(intToBits(as.integer(xValue))) > 0)
    lValue <- levelX[[xIx]][xValue]
    rValue <- setdiff(levelX[[xIx]], lValue)
  }
  ruleleft <- rule
  if (length(ruleleft) == 0) {
    ruleleft[[as.character(xIx)]] <- lValue
  }
  else {
    if (as.character(xIx) %in% ls(ruleleft)) {
      if (!is.null(levelX[[xIx]])) {
        lValue <- intersect(ruleleft[[as.character(xIx)]], 
                            lValue)
        ruleleft[[as.character(xIx)]] <- lValue
      }
      else {
        # Removing redundant conditions
        if (grepl("<=", ruleleft[[as.character(xIx)]]) ) {
          # Extract the current value for the condition on the attribute
          cond <- ruleleft[[as.character(xIx)]]
          match <- regexpr("<=\\s?-?\\d+(\\.\\d+)?", cond)
          old_val <- as.numeric(substr(cond, match+2, match+attributes(match)$match.length-1))
          if (is.na(xValue <= old_val)){
            print("WHATT!!")
          }
          if (xValue <= old_val) {
            # replace the value on the attribute
            ruleleft[[as.character(xIx)]] <- gsub("<=\\s?-?\\d+(\\.\\d+)?", paste("<=", xValue), cond) 
          }
          # else leave ruleleft alone
        }
        else 
          ruleleft[[as.character(xIx)]] <- paste(ruleleft[[as.character(xIx)]], 
                                                 "&", lValue)
      }
    }
    else {
      ruleleft[[as.character(xIx)]] <- lValue
    }
  }
  ruleright <- rule
  if (length(ruleright) == 0) {
    ruleright[[as.character(xIx)]] <- rValue
  }
  else {
    if (as.character(xIx) %in% ls(ruleright)) {
      if (!is.null(levelX[[xIx]])) {
        rValue <- intersect(ruleright[[as.character(xIx)]], 
                            rValue)
        ruleright[[as.character(xIx)]] <- rValue
      }
      else {
        # Removing redundant conditions
        if (grepl(">", ruleright[[as.character(xIx)]]) ) {
          # Extract the current value for the condition on the attribute
          cond <- ruleright[[as.character(xIx)]]
          match <- regexpr(">\\s?-?\\d+(\\.\\d+)?", cond)
          old_val <- as.numeric(substr(cond, match+1, match+attributes(match)$match.length-1))
          if (xValue > old_val) {
            # replace the value on the attribute
            ruleright[[as.character(xIx)]] <- gsub(">\\s?-?\\d+(\\.\\d+)?", paste(">", xValue), cond) 
          }
          # else leave ruleRight alone
        }
        else 
          ruleright[[as.character(xIx)]] <- paste(ruleright[[as.character(xIx)]], 
                                                  "&", rValue)
      }
    }
    else {
      ruleright[[as.character(xIx)]] <- rValue
    }
  }
  thisList = treeVisitHack(tree, tree[rowIx, "left daughter"], 
                       count, ruleSet, predSet, ruleleft, levelX, length + 1, max_length, 
                       digits)
  ruleSet = thisList$ruleSet
  count = thisList$count
  predSet = thisList$predSet
  thisList = treeVisitHack(tree, tree[rowIx, "right daughter"], 
                       count, ruleSet, predSet, ruleright, levelX, length + 1, max_length, 
                       digits)
  ruleSet = thisList$ruleSet
  count = thisList$count
  predSet = thisList$predSet
  return(list(ruleSet = ruleSet, count = count, predSet = predSet))
}

getRuleMetricHack <- function(ruleExec, X, target, oobIndexes)
{
  ruleMetric <- t(apply(ruleExec, 1, 
                         measureRuleHack, X, target, oobIndexes))
  ###### Comment out this code if you want more rules ######
  dIx <- which(ruleMetric[, "freq"] == "0")
  if (length(dIx) > 0) {
    ruleMetric <- ruleMetric[-dIx, ]
    print(paste(length(dIx), " paths are ignored.", sep = ""))
  }
  ###########################################################
  return(ruleMetric)
}

measureRuleHack <- function (ruleExec, X, target, oobIndexes) 
{
  len <- length(unlist(strsplit(ruleExec$condition, split = " & ")))
  origRule <- ruleExec$condition
  tree_id <- ruleExec$tree
  ysMost <- ruleExec$prediction
  ruleExec <- paste("which(", origRule, ")")
  X <- X[unlist(oobIndexes[tree_id]), ]
  ixMatch <- eval(parse(text = ruleExec))
  metric_names <- c("len", "freq", "err", "condition", "pred", "tree", 
                    "oobSize", "numMatches", "numTP", "numTN", "precision",
                    "recall", "f_score")
  
  target <- target[unlist(oobIndexes[tree_id])]
  
  if (length(ixMatch) == 0) {
    ys <- NULL
    no <- target
    freq <- 0
    true_positives <- 0
    false_positives <- 0
    precision <- 0
  } else {
    ys <- target[ixMatch]
    no <- target[-ixMatch]
    freq <- round(length(ys)/nrow(X), digits = 3)
    
    ## Something to try increasing rule set accuracy:
    ## Replace the prediction from the tree with the class that has the most
    ## cases in the OOB that satisfy the rule's conditions 
    # ysMost <- names(which.max(table(ys)))
    
    true_positives <- sum(as.character(ys) == ysMost)
    false_positives <- length(ys) - true_positives
    precision <- round(true_positives/length(ys), digits = 3)
  }

  true_negatives <- sum(as.character(no) != ysMost)
  false_negatives <- length(no) - true_negatives
  conf <- round((true_positives+true_negatives)/length(target), digits = 3)
  err <- 1 - conf
  recall <- round(true_positives/(true_positives+false_negatives), digits = 3)
  if( true_positives == 0) f_score = 0
  else f_score <- round(2*precision*recall/(precision+recall), digits = 3)
  
  rule <- origRule
  v <- c(len, freq, err, rule, ysMost, tree_id, length(target), length(ys), 
         true_positives, true_negatives, precision, recall, f_score)
  names(v) <- metric_names
  return(v)
}