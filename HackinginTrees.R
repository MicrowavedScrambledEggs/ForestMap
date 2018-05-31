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

# Modified to return the rule prediction
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
    xValue <- which(as.integer(intToBits(as.integer(xValue))) > 
                      0)
    lValue <- levelX[[xIx]][xValue]
    rValue <- setdiff(levelX[[xIx]], lValue)
  }
  xValue <- NULL
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
  dIx <- which(ruleMetric[, "len"] == "-1")
  if (length(dIx) > 0) {
    ruleMetric <- ruleMetric[-dIx, ]
    print(paste(length(dIx), " paths are ignored.", sep = ""))
  }
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
    v <- c("-1", "-1", "-1", "", "", tree_id, length(target), "0", "0", "0",
           "0", "0", "0")
    names(v) <- metric_names
    return(v)
  }
  
  ys <- target[ixMatch]
  no <- target[-ixMatch]
  freq <- round(length(ys)/nrow(X), digits = 3)

  true_positives <- sum(as.character(ys) == ysMost)
  true_negatives <- sum(as.character(no) != ysMost)
  false_positives <- length(ys) - true_positives
  false_negatives <- length(no) - true_negatives
  conf <- round((true_positives+true_negatives)/length(target), digits = 3)
  err <- 1 - conf
  precision <- round(true_positives/length(ys), digits = 3)
  recall <- round(true_positives/(true_positives+false_negatives), digits = 3)
  if( true_positives == 0) f_score = 0
  else f_score <- round(2*precision*recall/(precision+recall), digits = 3)
  
  rule <- origRule
  v <- c(len, freq, err, rule, ysMost, tree_id, length(target), length(ys), 
         true_positives, true_negatives, precision, recall, f_score)
  names(v) <- metric_names
  return(v)
}