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

	