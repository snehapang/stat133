
Midterm 1

# Please load in the Kaiser babies dataset included in your midterm1
# directory. This dataset includes information on mothers and the children born
# to those mothers. You will need this data to perform the tasks for this quiz.
load("/Users/snehapang/Downloads/KaiserBabies.rda")

# calculate the mean and standard deviation of birthweights (bwt) for all
# observations in the dataset. Store these as the variables <mean.bwt> and
# <sd.bwt> respectively.

# mean.bwt <- your code here
# sd.bwt <- your code here

mean.bwt=mean(infants$bwt)
sd.bwt=sd(infants$bwt)



# For each observation in the dataset, subtract <mean.bwt> from the observations
# bwt and divide by <sd.bwt>. Store this as the variable <std.bwts>. Note that
# this should be a numeric vector whose length is equal to the number of
# observations in the dataset.

# std.bwts <- your code here

function1=function(x){
  mean.bwt=mean(infants$bwt)
  sd.bwt=sd(infants$bwt)
  y=((x-mean.bwt)/sd.bwt)
  return(y)
}
std.bwts=function1(infants$bwt)



# Create the following two subsets and store them as variables with the
# indicated names:
# 1) Mothers whose smoking status is never: <subset.nonsmoke>
# 2) Mothers whose smoking status is now: <subset.smoke>

# subset.nonsmoke <- your code here
# subset.smoke <- your code here

subset.nonsmoke=infants[infants$smoke=="Never",]
subset.smoke=infants[infants$smoke=="Now",]



# For each of your subsets, create a vector giving the age of the mother. Store
# these as variables <subset.nonsmoke.age> and <subset.smoke.age>.

# subset.smoke.age <- your code here
# subset.nonsmoke.age <- your code here

subset.smoke.age=c(subset.smoke$age)
subset.nonsmoke.age=c(subset.nonsmoke$age)



# Implement the function gestByAge. Your function should take the following
# arguments:
#
# <age.cutoff>: a numeric constant giving a cutoff to subset by
# <ages>: a numeric vector of ages for each observation
# <gestation>: a numeric vector of gestation period length for each observation
#   (this should be the same length as <ages>)
#
# Your function should return the average gestation period for every observation
# whose value in <ages> is strictly less that <age.cutoff>.





gestByAge <- function(age.cutoff, ages, gestation){

  
}

# Please produce a plot of birthweight (y-axis) against gestation period. Your
# plot should contain the following features:
# 1) the title: "Birthweight v gestation"
# 2) points of mothers whose smoking status is never should be colored red

plot(x=infants$gestation,y=infants$bwt,
     type="p",xlab="Gestation Period",ylab="Birthweight",
     main="Birthweight vs. Gestation",
     col=ifelse(infants$smoke=="Never","red","black"))








## Stat 133 Midterm 2
## Thursday April 2nd

## General R commands

present="yes"

# [1 pt]
# Create [x], a numeric vector of length 2000 with 
# entries: 6, 12, 18, etc.

#x <- <your code here>
x=seq(from=6,length.out=2000,by=6)


# [1 pt]
# Create [y], a logical vector of length 2000 
# with y[i]=T if x[i] is divisible by 10, otherwise F

# y <- <your code here>
y=rep(0,times=2000)
for(i in 1:length(x)){
  if(floor(x[i]/10)==(x[i]/10)){
    y[i]=TRUE}
  else{y[i]=FALSE}
  }



# [1 pt]
# Create [w], a random permutation of the numeric values of a deck of cards
# (i.e. just the numbers 1 through 13 each repeated 4 times)
set.seed(2718)
#w <- <your code here>
w=sample(rep(c(1:13),times=4),size=52)

# [1 pt]
# Create [m], a matrix of size 10x10 with entries that are 
# Exponential random variables (hint: rexp) with rate 3
# (arrange the values by column, as per default)
set.seed(344)

#m <- <your code here>
m=matrix(rexp(100,3),nrow=10,ncol=10)

# [1 pt]
# Create [l], a list with 12 elements, each a vector of length 100.
# Each vector of length 100 of Poisson (hint:rpois) random variables with mean 5
set.seed(71)
#l <- <your code here>
l=list(rpois(100,5),rpois(100,5),rpois(100,5),rpois(100,5),rpois(100,5)
         ,rpois(100,5),rpois(100,5),rpois(100,5),rpois(100,5),rpois(100,5),
         rpois(100,5),rpois(100,5))


# for the next two tasks you will use the data frame infants (size 1236x15)
# LEAVE AS IS:
load("KaiserBabies.rda") 

# [2 pt]
# Create a table [t] of the education level ($ed) of all married ($marital) first time ($parity=1) mothers:
#t <- <your code here>
t.1=subset(infants,infants$marital=="Married")
t.2=subset(t.1,t.1$parity==1)
t=c(t.2$ed)

# [2 pt]
# Calculate [mw], the average birthweight ($bwt) of all babies whose were full term, i.e. gestation equal or more than 259 days.
#mw <- <your code here>
m1=subset(infants,infants$gestation>259)
mw=mean(m1$bwt)

# For the next few tasks you will use the data frame family (size 14x5)
# LEAVE AS IS:
load("family.rda")

# [1 pt]
# Create [f1] a subset of family with only women over age 50
#f <- <your code here>
f1=family[family$age>50,]


# [1 pt]
# Create [f2] a subset of family with only men 6 foot tall or more
#fm <- <your code here>
fm=family[family$gender=="m",]
f2=fm[fm$height>72,]


# [1 pt]
# Create [f3] a subset of family of people whose name starts with T
#f3 <- <your code here>
f3=family[grep("^T",family$name),]


# [1 pt]
# Create [f4] a subset of family with just the youngest individual (so just one row)
#f4 <- <your code here>
f4=family[family$age==min(family$age),]




## Plotting

# We will now use the dataset "iris" which is icluded in the R package.
# To look at the dataframe you can just type "iris" at the prompt
# It is a data frame of size 150x5 with measurements of 4 attributes
# for 150 flowers, 50 each of 3 different species of irises.

# [2 pts]
# Make a box plot of Sepal Length by Species (so 3 boxplots in one plot)
boxplot(Sepal.Length ~ Species, data=iris)


# [3 pts]
# Make a scatterplot of petal width (y-axis) versus petal length (x-axis)
# The axes labels should be "Petal Length" and "Petal Width",
# Color the plotting symbol by Species (any 3 colors)
plot(x=iris$Petal.Length,y=iris$Petal.Width,
     xlab="Petal Length",ylab="Petal Width",type="p",
     col=ifelse(iris$Species=="setosa","red",ifelse(iris$Species=="versicolor","green","blue")))
     
     


# [3 pt]
# Make a scatterplot of ( sepal length / petal length) as a function of index (order)
# Color the plotting symbol by Species (any 3 colors)
plot(x=iris$Sepal.Length,y=iris$Petal.Length,type="p",
     col=ifelse(iris$Species=="setosa","pink",ifelse(iris$Species=="versicolor","green","orange")),
     xlab="Sepal Length", ylab="Petal Length")



##  apply statements

# For the next few tasks you will use the list Cache500 
# (list of length 500, each element is a numeric vector of various lengths)
# LEAVE AS IS:
load("Cache500.rda")

# [3 pts]
# Create [first.cache], a vector where each entry is the _first_ element of the
# corresponding vector in the list Cache500

#first.cache <- <your code here>
the_first_element=function(x){
 vec=rep(0,times=500)
  for(i in 1:length(x)){
   vec[i]=(x[[i]][1])
 }
 return(vec)
}



# [3 pts]
# Create [mean.cache], a vector of length 500 where each entry is the mean 
# of the corresponding element of the list Cache500

#mean.cache <- <your code here>
mean.cache=lapply(Cache500,mean)


# [2 pts]
# Create [sd.cache], a vector of length 500 where each entry is the sd
# of the corresponding element of the list Cache500

#sd.cache <- <your code here>
sd.cache=lapply(Cache500,sd)


# [4 pts]
# Create [mean.long.cache], a vector where 
# mean.long.cache[i] is:
# the mean of Cache500[[i]] IF it has 50 or more entries.
# NA IF Cache500[[i]] has less than 50 entries.

#mean.long.cache <- <your code here>
myfunction=function(x){
  if(length(x)>50){
    return(mean(x))
  }
  if(length(x)<50){
    return("NA")
  }
}
mean.long.cache=lapply(Cache500,myfunction)












# Midterm 3

# Write a function called numDotElements. Your function should take the following
# arguments
#   <chvec>: A character vector containing strings of letters, possibly 
#     with the "." symbol
#
# and return the following
#   <num.dot>: an integer indicating how many elements of <chvec> contain the "."
#     symbol. For example: numDotElements(c('USA', 'U.S.A', '...')) should return 2

numDotElements=function(string){
  x=grep(pattern=".",x=string)
  return(length(x))
}
#that was on my midterm and it's wrong

numDotElements=function(string){
  count=rep(0,times=length(string))
  for(i in 1:length(string)){
    if(grepl("\\.",string[i])==TRUE)
     {count[i]=1}
    else{count[i]=0}
  }
  return(sum(count))
}

teststring=c("USA","U.S.A","dog","doggy","sneha","ms. pang", "solid", "g.t.f.o")
#this works





# Write a function called sumDigits that computes the sum of all the digits in
# a string.  The function should return 0 if there is no digit in the
# string. Your function should take the following arguments:
#   <chvec>: A character vector of length 1 (chvec contains only 1 string)
#
# and return the following
#   <total>: A single number (the sum of all the digits in chvec)

sumDigits=function(chvec){
  total=sum(as.numeric(unlist(strsplit(gsub("[^0-9]","",x=chvec), "")))) 
  return(total)
}


# Some test cases:
all.equal(sumDigits("1z3p ! 21"), 7)
all.equal(sumDigits("abcdefg"), 0)




# Write a function called hisToHer that converts every instance of 
# him in a string to her; every instance of he to she and every instance 
# of his to her. You can assume everything is lower case. Be careful not 
# to replace words that contain him/he/his (eg you don't want to
# replace the with ther). Your function should take the argument
#   <chvec>: A character vector
#
# and return
#   <herchvec>: The same character vector with the required substitutions.


hisToHers=function(chvec){
  herchvec=strsplit(chvec," ")
  herchvec=gsub("\\bhim\\b","her",herchvec)
  herchvec=gsub("\\bhe\\b","she",herchvec)
  herchvec=gsub("\\bhis\\b","her",herchvec)
  herchvec=paste(herchvec,sep="")
  return(herchvec)
}
#didn't work




# The symbols \< and \> match the empty string at the beginning and end of a word.


# A test case
hisToHers("he went to the store his mother gave him")
hisToHers("she went to the store her mother gave her")

hisToHers=function(chvec){
herchvec1=gsub("\\<he\\>","she",chvec)
herchvec2=gsub("\\<him\\>","her",herchvec1)
herchvec=gsub("\\<his\\>","her",herchvec2)
return(herchvec)}

hisToHers("While he went to the zoo, he put his heart next to himself")



#Write a function called mostCommonLetter that finds the most common 
# letter in a string. If there is a tie for most common letter return 
# all of the letters that were most common. Your function should 
# convert all letters in the string to *lower case* and you should 
# remove  everything other than letters. 
# Your function has the argument
#  <chvec>: A character vector of length 1 (chvec contains only 1 string)
#
# and it should return
#  <letter> The most common letter or letters in the string.
# For example mostCommonLetter("aabbccccdddd") should return 
# [1] "c" "d"

mostCommonLetter=function(chvec){
x=table(unlist(strsplit(gsub("[^[:alpha:]]","",tolower(chvec)),split="")))
return(names(x[x==max(x)]))}








mostCommonLetter=function(string){
  string=tolower(gsub("[[:digit:]]","",string))
  string=string[string!=""]
  tab=table(unique(strsplit(string,"")))
  return(which(tab==max(tab)))
}

mostCommonLetter("aaaaaa222222222bhw")
#didn't work







###########################################################
#### Stat 133 Midterm 4

# leave this here:
set.seed(123456)


#### Simulation
# Write a function, [dice_sum()], that takes as input:
# [k] : the number of dice rolled
# [B] : the number of rolls
# and returns:
# [dsum] : a vector of length B where each element is the sum of a roll of k dice

# So if k=1 pick a number between 1 and 6 at random B times and return them,
# if k=2 then in each roll you pick twice a number between 1 and 6 at random,
# calculate their sum, do this B times and return
# and so on.

# We've set the default inputs to k=2 and B=100
#I changed it to whatever input you want

dice_sum=function(k,B){
  dsum=rep(0,times=B)
  for(i in 1:length(dsum)){
    dsum[i]=sum(sample(1:6,size=k,replace=TRUE))
  }
  return(dsum)
}



#### String manipulation

phrases <- c("dog", "doggy", "den", "good boy", "Really?", "How much?", "Only $8", "dogdogdog", "Oh god")

# Create a vector [text1] that lists the elements in phrases 
# where the SECOND TO LAST character is "o" (lower case o).
#text1 <- <your code here>

find.o=function(string){
  emptyvec=rep(0,times=length(string))
  for(i in 1:length(string)){
    if(substr(string[i],nchar(string[i])-1,nchar(string[i])-1)=="o"){
      emptyvec[i]=string[i]}
    else{emptyvec[i]=0}
}
emptyvec=emptyvec[emptyvec!=0]
return(emptyvec)
}

text1=find.o(phrases)


# Create a vector [text2] that lists the elements in phrases that
# START with the letter "d"
#text2 <- <your code here>

find.d=function(string){
  emptyvec=rep(0,times=length(string))
  for(i in 1:length(string)){
    if(substr(string[i],1,1)=="d"){
      emptyvec[i]=string[i]}
    else{emptyvec[i]=0}
  }
  emptyvec=emptyvec[emptyvec!=0]
  return(emptyvec)
}

text2=find.d(phrases)


# Create a variable [no.punct] that equals the number of phrases with a punctuation mark in it.
# no.punct <- <your code here>
phrases2=c("hello", "how are you", "I'm good!", "What about you?", "I'm great!", "That's good", "Nice!")

punct.funct=function(string){
  emptyvec=rep(0,times=length(string))
  for(i in 1:length(string)){
    if(gsub("[^!]","",string[i])=="!"){
      emptyvec[i]=1}
    else{emptyvec[i]=0}
  }
  return(sum(emptyvec))
}


# Create a vector [even] that is of length 1000 and has the entries
# "even2", "even4", ...
# with no separation between the word and the letter

#even <- <your code here>
even1=rep("even",times=1000)
even2=seq(from=2,to=2000,by=2)
even=paste(even1,even2,sep="")

# Start with [hotelCal] which is a character string, create 
# a _vector_ (not list) [hotelCal.split] which 
# stores the words of [hotelCal] each as a separate element.
# Also, convert all upper case letters to lower case.
# Please remove all punctuation marks.

hotelCal <- "On a dark desert highway, cool wind in my hair. Warm smell of colitas, rising up through the air. Up ahead in the distance, I saw a shimmering light. My head grew heavy and my sight grew dim I had to stop for the night.  There she stood in the doorway; I heard the mission bell.  And I was thinking to myself: 'This could be heaven or this could be hell'. Then she lit up a candle and she showed me the way."

# hotelCal.split <- <your code here> 
hotelCal.split=unlist(strsplit(tolower(gsub("[[:punct:]]","",hotelCal))," "))


# Write a function called updateDate. Your function should take the following
# arguments
#   <dates>: a character vector of dates of the form "month, year" (e.g. "May, 2001")
#   <old.yr>: a string indicating the year for which elements will be updated
#     (e.g. "2002")
#
# and return the following
#   <updated.dates>: a character vector of dates where <old.yr> has been replaced by
#     '2015'. This vector should only contain the elements whose date has been
#     updated. For example updateDate(c('May, 2008', 'June, 2011'), '2008') should
#     return 'May, 2015'.
updateDate <- function(dates, old.yr) {
  vec=rep(0,times=length(dates))
  for(i in 1:length(dates)){
    if(grepl(old.yr,dates[i])==TRUE){
      vec[i]=gsub(old.yr,"2015",dates[i])
    }
  }
vec=vec[vec!=0]
return(vec)
}


# Write a function called [abbreviate] that takes in a vector of strings and returns
# a vector of the same length with only the first [k] characters from the orignal vector entries.

abbreviate <- function(vector, k){
  vec=rep(0,times=length(vector))
  for(i in 1:length(vector)){
    vec[i]=substr(vector[i],1,k)}
    
return(vec)
}

sample.vector=c("hello", "my", "name", "is", "sneha")

nicknames=function(x){
  y=rep(0,times=length(x))
  for(i in 1:length(x)){
    x[i]=substr(x[i],1,4)
    y[i]=paste(x[i],"s",sep="")
  }
  return(y)
    
}






