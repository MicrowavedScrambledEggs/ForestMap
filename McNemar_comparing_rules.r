	# MC NEMAR TEST BETWEEN RULES to find their "equality" in the forest
	
	# Should the only rules testing for the same case be compared to each other
	
	# Not sure about this:
	# Badi's "testRuleCov" function where "Rule no. true" is for when the conditions are true and 
	# "Rule no. false" is for when all the rule condition are false
	# OR
	# maybe it should use "testRule", like the McNemar_FalsePos_FalseNeg, to see if the rule gives a
	# correct conclusion compared to another rule

	
	# Create individual 2x2 contingency tables for each condition/antecedent of each rule
	
	#-----------------------|----------------------------|------------------------------|-----------|
	#						|  Column 1 Rule 2 correct	 |	 Column 2 Rule 2 incorrect	| 			|		
	#-----------------------|----------------------------|------------------------------|-----------|
	#row 1 Rule 1 correct	|				a			 |				b				|	a + b   |
	#-----------------------|----------------------------|------------------------------|-----------|
	#row 2 Rule 1 incorrect	|				c			 |				d				|	c + d   |
	#-----------------------|----------------------------|------------------------------|-----------|
	#						|			  a + c			 |			  b + d				|  a+b+c+d  |
	#-----------------------|----------------------------|------------------------------|-----------|
	
	# Once again:
	# Can use addmargins() function on each contingency table
	# On each table run mcnemar.exact(table)
	
	