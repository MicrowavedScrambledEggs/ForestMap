	# MC NEMAR TEST FOR RULES RETURNING A FALSE POSITIVE OR FALSE NEGATIVE
	
	# Group rules together based their conclusion statement?
	
	# Use a test data set on the rules to determine which ones return a false positive or false negative
	# Badi's "testRule"  and "testRuleCov" function

	# Create individual 2x2 contingency tables for each condition/antecedent of each rule
	
	#-----------------------|----------------------------|------------------------------|-----------|
	#						| Column 1 Conclusion "true" |	Column 2 Conclusion "false" | 			|		
	#-----------------------|----------------------------|------------------------------|-----------|
	#row 1 Condition true	|				a			 |				b				|	a + b   |
	#-----------------------|----------------------------|------------------------------|-----------|
	#row 2 Condition false	|				c			 |				d				|	c + d   |
	#-----------------------|----------------------------|------------------------------|-----------|
	#						|			  a + c			 |			  b + d				|  a+b+c+d  |
	#-----------------------|----------------------------|------------------------------|-----------|
	
	# Use the same test data on each rule recording the output for each condition and the conclusion
	# e.g.  if condition is true and conclusion is "true", a++
	#		if condition is true and conclusion is "false", b++
	# 		if condition is false and conclusion is "true", c++
	#		if condition is false and conclusion is "false", d++
	
	# Can use addmargins() function on each contingency table
	
	# On each table run mcnemar.test(table, correction == FALSE), I don't know yet if the continuity correction should be set TRUE or FALSE for our case

	# Getting the individual conditions after obtaining the raw set of rules
  #rawRuleConditions <- sapply(strsplit(rawRules[,"condition"],"&"),"[")
  #rawRulePredictions <- rawRules[,"pred"]
  
  # Getting the individual conditions after obtaining the refined set of rules
  #refRuleConditions <- sapply(strsplit(as.character(refRules[,"condition"]),"&"),"[")
  #refRulePredictions <- refRules[,"pred"]

  # To access all condition of rule x 
  #rawRuleConditions[[x]]
  #refRuleConditions[[x]]

  # To access condition y of rule x
  #rawRuleConditions[[x]][y]
  #refRuleConditions[[x]][y]
  
  # To access the prediction of rule x, corresponds to the same index as the conditions
  #rawRulePrediction[x]
  #refRulePrediction[x]
  
  # Setting up global variables for table
  #a <- 0
  #b <- 0
  #c <- 0
  #d <- 0
  
  # After performing function "testRuleCondition" for all data instances on one condition create the contingency table
  #contingencyTable <- matrix(c(a,c,b,d), nrow = 2, dimnames = list(Condition = c("True", "False"), Prediction = c("True", "False")))
  
  # Addsum margins to the contingency table
  #addmargins(contingencyTable)
  
  library(exact2x2)
  # Perform "exact" Mc Nemar's test, "exact" means continuity correction does not need to be specified 
  #mcnemar.exact(contingencyTable)
  
  # Null hypothesis: the specific rule condition and the rule conclusion ARE independant 
  # Alternate hypothesis: the specific rule condition and the rule concluse ARE NOT independant
  # If the "Mc Nemar's chi-squared vaule" is larger than the "p-value", reject the null hypothesis of independence 

  # Modified Badi's testRule function
  # Inputs:
  #       - X, the data instance with all variables and class label
  #       - ruleCondition, a single condition of the rule
  #       - rulePredction, the prediction of the rule which the above condition belongs to
  #       - classColName, the name of the column of the class label variable; in this case "forestCoverType"
  
  testRuleCondition <- function(X, ruleCondition, rule, classColName)
  {
    if (eval(parse(text = ruleCondition)) & X[classColName] == rule["pred"]) {
      a <<- a + 1
      return(1)
    }
    if (eval(parse(text = ruleCondition)) & X[classColName] != rule["pred"]){
      b <<- b + 1
      return(1)
    }
    if (!eval(parse(text = ruleCondition)) & X[classColName] == rule["pred"]){
      c <<- c + 1
      return(1)
    }
    if (!eval(parse(text = ruleCondition)) & X[classColName] != rule["pred"]){
      d <<- d + 1
      return(1)
    }
    return(0)
  }
  
  