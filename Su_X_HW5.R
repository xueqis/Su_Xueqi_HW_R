---
title: "Su_X_HW5"
author: "Xueqi Su"
output: pdf_document
---
#MATH 510
#HW5

#1.Print to the console all methods and attributes associates with	a	dataframe. Write code	to determine the number	of columns in	a dataframe


library(ggplot2)
data("diamonds")
#use diamonds data to develop my code
mydata = diamonds
#get the general information of mydata
str(mydata)
summary(mydata)

#print all methods and attributes that are associated with mydata
methods(class=data.frame)
attributes(mydata)$names
#print the number of columns
ncol(mydata)


#2.Write code to determine how many rows are in a dataframe

#print the number of rows
nrow(mydata)


#3.Write code to extract the column names from the dataframe and print the names of the columns(one per line) to the console

#because it asks the names should be displayed one per line
#I use "\n" to seperate each names.
listname = function(dataframe){
  cat(names(dataframe), sep = "\n")
}
#check
listname(mydata)



#4.Write	code to	determine	the	type of	each column	(numeric,	factor, logical, etc.).	Print	the	type of	each column	to the	console

#use function lapply to get the type of each column and print them out
types = function(dataframe){
  lapply(dataframe, class)
}

#check
types(mydata)

#5.Write	code that	will loop	through	any	dataframe	and	calculate	the	mean of	every	numeric	column. Label	the	output with	the	name of	the	column.

#Method 1 
colmean <- function(data.frame){
  #define a function that accepts dataframe as parameter
  #define a term indexnum to store the index of the columns that are
  #numeric
  indexnum = which(types == "numeric")
  #use function colMean to get the mean of every numeric column
  #and also label the output with the name of the column
  colMeans(data.frame[indexnum])
}
#check
colmean(mydata)
#Method 2
#apply sapply and colMeans
#is.numeric here indicates the type of columns we want

colmean2 = function(dataframe){
  colMeans(dataframe[sapply(dataframe,is.numeric)])
}
#check
colmean2(mydata)

#the difference between these two methods is that the first method
#would not include the result of price because the type of price was
#marked as "integer".
#while the Method 2 would recognize the column price as 
#numeric column and then return the mean of the column price.


#6.Write	code that	will loop	through	any	dataframe	and	create a frequency	table	for	every	factor column. Label the output	with the name of	the	column.

#use sapply function here
#is.factor indicates the type of columns we want
#summary produces the frequency table

freq_table = function(dataframe){
  summary(dataframe[sapply(dataframe,is.factor)])
}

#check
freq_table(mydata)


#7.Write	code that	will loop	through	any	dataframe	and	determine	the	number of	rows containing	NA (missing	value) in	each column and the	percentage of rows containing	an NA in any of	the	columns. HINT: In	a	single row,	zero or more columns	may	contain	an	NA.	For	the	percentage of	rows	containing NA in	any	column,	do	not	double	count	NA in	rows that	contain	more than	one	column with	an NA. Print	the	results	to	the	console.

#use apply and sapply function here
#use is.na to check if there exists NAs in columns
#2 indicates columns
#use sum to get the number of rows containing NA in each column.

row_na = function(dataframe){
  apply(sapply(dataframe,is.na),2,sum)
}
#check

row_na(mydata)

#use rowSums here to get the if there exists NAs.
#the result consists of T and F. T means the row has NA; F means not. #divided by the number of rows to get the percentage. 

percent_na = function(dataframe){
  sum(rowSums(is.na(dataframe))>0)/nrow(dataframe)
}

#check
percent_na(mydata)

#8.Create an	R	function that	can	accept any dataframe as	a	parameter	and	returns	a dataframe	that contains	each pair	of column names	in	the	first	column in	a	single string	separated	by	a	-, e.g. for the	variables	x	and	y, you should	form the string	???x-y??? (HINT: Look	at	the	help provided	for	the	paste	function)	and	their	corresponding	Pearson	correlation	coefficient	in the second	column.	(HINT: There	is	a	function that calculates correlation coefficients	??? look carefully	at what	is returned	and	optimize how you extract the correlation	coefficients). Do	not	repeat any pairs.

pairname = NULL
numcor = NULL
#define two terms for futher use

paircor = function(dataframe){
  #define a function that accepts dataframe as parameter
  #get the numeric date subsets
  num = dataframe[sapply(dataframe,is.numeric)]
  #get the column names of the subset above
  numname = colnames(num)
  #use two for loops here to pair each column
  for (i in 1:(length(numname)-1)){
    for (j in (i+1):length(numname)){
      #use paste function to connect each two column names with "-"
      
      pairname = c(pairname, paste(numname[i], numname[j], sep = "-")) 
      #calculate the correlation of each pair of the subset num
      
      numcor = c(numcor, cor(num[i], num[j], method = "pearson"))
      
    }
  }
  #return a dataframe that consists of the names of each pair 
  #and their correlation coefficients 
  
  return(data.frame(pairname, numcor))
}

#check
paircor(mydata)


