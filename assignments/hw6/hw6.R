# Homework 6
# Stat 133, Lec 2, Spring 2015
# Due : Friday March 20th by 5 pm

# Review the slides on simulations for this assignment.

# Consider the following model on use of a new drug:
# We have a population of doctors, population size : <n.doctors>
# Every doctor has either adopted the use of a new drug, or not (0/1 status)
# Now write a function that runs a simulation for a period of :
# <n.days> where
# - every day exactly two _random_ doctors meet
# - if one has adopted but the other one has not then the
#   holdout adopts the use of the drug with probability p
# Return a matrix that shows for each day which doctors have adopted
# the use of the drug.

test.n.doctors=10
test.initial.doctors=sample(c(0:1),size=test.n.doctors,replace=T,p=c(.5,.5))
test.n.days=5
test.p=1

# Input varibles are
# <n.days> : the number of days the simulation should be run for
# <n.doctors> : total number of doctors 
# <initial.doctors> : a 0/1 vector of length <n.doctors>, 1 for adopters
# <p> : the probability that the non-adopter adopts the drug.

# Ouput variable
# <has_adopted> : matrix with <n.doctors> rows and <n.days> columns
#                 i.e. one row for each doctor
#                 the entries are 0 for days where the doctor is a
#                 non-adopter, else 1 (so once a row turns to 1 it stays as 1).



#what i want to do: choose two random values in initial.doctor. If both are 1s or both are 0s, then nothing changes. 
#If one is a 1 and the other is a 0, the 0 needs to change to a 1 with a probability of p
#I need to continue to choose two random values, storing the results from before, for n.days amount of iterations
#the final print is the matrix with days along top and each column beneath is the initial.doctors for that day

#from lecture: once you have iniital.doctors and n.days in initial matrix. only first column you know, you have to generate 
#each new column. so you need your function to adjust each column in the matrix


sim.doctors=function(initial.doctors, n.doctors, n.days, prob){
  has_adopted=matrix(rep(initial.doctors,times=n.days),nrow=n.doctors,ncol=n.days)
  for(i in 2:n.days){
    two.random.doctors=sample(n.doctors,size=2,replace=F)
    prob.adopt=sample(c(1,0),size=1,p=c(prob,1-prob))
  if(sum(has_adopted[two.random.doctors,i-1])==1 && prob.adopt==1){
    has_adopted[two.random.doctors,i:n.days]=1
  }
   
  }
  
  return(has_adopted)
}
  
sim.doctors(test.initial.doctors,test.n.doctors,test.n.days,test.p)  
      
#start with 2 and update that
#has_adopted before for loop
#sample from n.doctors to get which doctor, not 1 or zero
#generate  random number, if yes then change both to 1


set.seed(42)
# Generate a value for <initial.doctors> that has 10% 1s and 90% 0s.
initial.doctors.trial=sample(c(0,1),replace=T,size=10,p=c(.9,.1))
n.days.trial=20
n.doctors.trial=10

sim.1=sim.doctors(initial.doctors.trial,10,20,.2)
sim.2=sim.doctors(initial.doctors.trial,10,20,.4)
sim.3=sim.doctors(initial.doctors.trial,10,20,.6)
sim.4=sim.doctors(initial.doctors.trial,10,20,.8)
sim.5=sim.doctors(initial.doctors.trial,10,20,1)

# Run your function for at least 5 different values of <p> and plot
# on x-axis: days,
# on y-axis : the number of doctors that have already adopted the drug, on that day
# Put all 5 lines in one figure (e.g. use first plot() then lines() for the subsequent lines)


plot(1,type="n",xlab="Days",ylab="Number of Doctors Who Have Adopted",xlim=c(0,20),ylim=c(0,10))
lines(x=c(1:20),y=rep(1,times=20),col="green")
text(18,.5,labels="p=.2")
lines(x=c(1:20),y=c(1,2,3,3,3,3,3,3,3,3,3,3,3,3,3,3,4,4,4,4),col="purple")
text(18,3.5,labels="p=.4")
lines(x=c(1:20),y=c(1,1,2,3,3,4,4,5,5,5,6,6,6,6,6,6,6,6,6,6),col="red")
text(18,5.5,labels="p=.6")
lines(x=c(1:20),y=c(1,1,1,1,1,1,1,1,2,3,4,4,5,5,6,7,7,7,8,9))
text(18,6.5,labels="p=.8")
lines(x=c(1:20),y=c(1,1,1,1,2,3,3,4,5,5,5,6,7,8,9,9,9,9,9,9),col="blue")
text(18,8.5,labels="p=1")



