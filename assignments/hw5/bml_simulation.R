#################################################################################
#### BML Simulation Study

#### Put in this file the code to run the BML simulation study for a set of input parameters.
#### Save some of the output data into an R object and use save() to save it to disk for reference
#### when you write up your results.
#### The output can e.g. be how many steps the system took until it hit gridlock or
#### how many steps you observered before concluding that it is in a free flowing state.

#Functions I will be using:

bml.init = function(r, c, p){
  m=matrix(sample(c(0,1,2),size=r*c,replace=TRUE,prob=c(1-p,p/2,p/2)),nrow=r,ncol=c)   
  return(m)
  
}


two.step=function(m){
  y=move.blue(move.red(m))
  return(y)}

bml.step=function(m){
  counter = 0
  while(all(two.step(m)-m==0)==FALSE){
    m = two.step(m)
    counter = counter + 1
    #if(all(two.step(m)-m==0)==TRUE){
    #break
  }
  
  return(list(m,counter))}


bml.sim <- function(r, c, p){
  t=bml.step(bml.init(r,c,p))
  return(t)}

#Simulations below:

#5x5 matrices ###############################################

sim.1=bml.init(5,5,.3)
#free flow

sim.2=bml.init(5,5,.4)
#free flow

sim.3=bml.init(5,5,.5)
sim.3.run=bml.step(sim.3)
#6 steps

sim.3.a=bml.init(5,5,.5)
sim.3.a.run=bml.step(sim.3.a)
#33 steps

sim.3.b=bml.init(5,5,.5)
sim.3.b.run=bml.step(sim.3.b)
#4 steps

sim.3.c=bml.init(5,5,.5)
#free flow

sim.3.d=bml.init(5,5,.5)
#free flow

mean.sim.3=14.33333

sim.4=bml.init(5,5,.6)
sim.4.run=bml.step(sim.4)
#2 steps

sim.4.a=bml.init(5,5,.6)
sim.4.a.run=bml.step(sim.4.a)
#43 steps

sim.4.b=bml.init(5,5,.6)
#free flow

sim.4.c=bml.init(5,5,.6)
#free flow

sim.4.d=bml.init(5,5,.6)
#free flow

mean.sim.4=22.5

sim.5=bml.init(5,5,.7)
sim.5.run=bml.step(sim.5)
#4 steps

sim.5.a=bml.init(5,5,.7)
sim.5.a.run=bml.step(sim.5)
#4 steps

mean.sim.5=4

sim.6=bml.init(5,5,.8)
sim.6.run=bml.step(sim.6)
#2 steps

sim.6.a=bml.init(5,5,.8)
sim.6.a.run=bml.step(sim.6.a)
#3 steps

sim.6.b=bml.init(5,5,.8)
sim.6.b.run=bml.step(sim.6.b)
#2 steps

mean.sim.6=2.333333

sim.7=bml.init(5,5,.9)
sim.7.run=bml.step(sim.7)
#2 steps

sim.7.a=bml.init(5,5,.9)
sim.7.a.run=bml.step(sim.7.a)
#0 steps

mean.sim.7=1



#10x10 matrices ########################################################################################

sim.11=bml.init(10,10,.1)
#free flow

sim.12=bml.init(10,10,.2)
#free flow

sim.13=bml.init(10,10,.3)
#free flow

sim.14=bml.init(10,10,.4)
#free flow

sim.14.a=bml.init(10,10,.4)
sim.14.a.run=bml.step(sim.14.a)
#21 steps

sim.14.b=bml.init(10,10,.4)
sim.14.b.run=bml.step(sim.14.b)
#310 steps

sim.14.c=bml.init(10,10,.4)
#free flow

sim.14.d=bml.init(10,10,.4)
#free flow

mean.sim.14=165.5

sim.15=bml.init(10,10,.5)
sim.15.run=bml.step(sim.15)
#23 steps

sim.15.a=bml.init(10,10,.5)
sim.15.a.run=bml.step(sim.15.a)
#18 steps

sim.15.b=bml.init(10,10,.5)
#free flow

sim.15.c=bml.init(10,10,.5)
#free flow

sim.15.d=bml.init(10,10,.5)
sim.15.d.run=bml.step(sim.15.d)
#14 steps

mean.sim.15=18.33333

sim.16=bml.init(10,10,.6)
sim.16.run=bml.step(sim.16)
#27 steps

sim.16.a=bml.init(10,10,.6)
sim.16.a.run=bml.step(sim.16.a)
#124 steps

sim.16.b=bml.init(10,10,.6)
sim.16.b.run=bml.step(sim.16.b)
#33 steps

sim.16.c=bml.init(10,10,.6)
sim.16.c.run=bml.step(sim.16.c)
#8 steps

sim.16.d=bml.init(10,10,.6)
sim.16.d.run=bml.step(sim.16.d)
#24 steps

mean.sim.16=43.2

sim.17=bml.init(10,10,.7)
sim.17.run=bml.step(sim.17)
#10 steps

sim.17.a=bml.init(10,10,.7)
sim.17.a.run=bml.step(sim.17.a)
#3 steps

sim.17.b=bml.init(10,10,.7)
sim.17.b.run=bml.step(sim.17.b)
#5 steps

sim.17.c=bml.init(10,10,.7)
#free flow

sim.17.d=bml.init(10,10,.7)
sim.17.d.run=bml.step(sim.17.d)
#18 steps

mean.sim.17=9

sim.18=bml.init(10,10,.8)
sim.18.run=bml.step(sim.18)
#18 steps

sim.18.a=bml.init(10,10,.8)
sim.18.a.run=bml.step(sim.18.a)
#12 steps

sim.18.b=bml.init(10,10,.8)
sim.18.b.run=bml.step(sim.18.b)
#8 steps

sim.18.c=bml.init(10,10,.8)
sim.18.c.run=bml.step(sim.18.c)
# 9 steps

sim.18.d=bml.init(10,10,.8)
sim.18.d.run=bml.step(sim.18.d)
#22 steps

mean.sim.18=13.8

sim.19=bml.init(10,10,.9)
sim.19.run=bml.step(sim.19)
#4 steps

sim.19.a=bml.init(10,10,.9)
sim.19.a.run=bml.step(sim.19.a)
#15 steps

sim.19.b=bml.init(10,10,.9)
sim.19.b.run=bml.step(sim.19.b)
#2 steps

sim.19.c=bml.init(10,10,.9)
sim.19.c.run=bml.step(sim.19.c)
#16 steps

sim.19.d=bml.init(10,10,.9)
sim.19.d.run=bml.step(sim.19.d)
#9 steps

mean.sim.19=9.2

#100x100 matrix  ########################################################################################

sim.21=bml.init(100,100,.1)
#free flow

sim.22=bml.init(100,100,.2)
#free flow

sim.23=bml.init(100,100,.3)
#free flow

sim.24=bml.init(100,100,.4)
#free flow

sim.25=bml.init(100,100,.5)
sim.25.run=bml.step(sim.25)
#112 steps

sim.25.a=bml.init(100,100,.5)
sim.25.a.run=bml.step(sim.25.a)
#156 steps

sim.25.b=bml.init(100,100,.5)
sim.25.b.run=bml.step(sim.25.b)
#99 steps

sim.25.c=bml.init(100,100,.5)
sim.25.c.run=bml.step(sim.25.c)
#232 steps

sim.25.d=bml.init(100,100,.5)
sim.25.d.run=bml.step(sim.25.d)
#116 steps

mean.sim.25=143

sim.26=bml.init(100,100,.6)
sim.26.run=bml.step(sim.26)
#61 steps

sim.26.a=bml.init(100,100,.6)
sim.26.a.run=bml.step(sim.26.a)
#71 steps

sim.26.b=bml.init(100,100,.6)
sim.26.b.run=bml.step(sim.26.b)
#104 steps

sim.26.c=bml.init(100,100,.6)
sim.26.c.run=bml.step(sim.26.c)
#64 steps

sim.26.d=bml.init(100,100,.6)
sim.26.d.run=bml.step(sim.26.d)
#107 steps

mean.sim.26=81.4

sim.27=bml.init(100,100,.7)
sim.27.run=bml.step(sim.27)
#40 steps

sim.27.a=bml.init(100,100,.7)
sim.27.a.run=bml.step(sim.27.a)
#35 steps

sim.27.b=bml.init(100,100,.7)
sim.27.b.run=bml.step(sim.27.b)
#31 steps

sim.27.c=bml.init(100,100,.7)
sim.27.c.run=bml.step(sim.27.c)
#46 steps

sim.27.d=bml.init(100,100,.7)
sim.27.d.run=bml.step(sim.27.d)
#37 steps

mean.sim.27=37.8

sim.28=bml.init(100,100,.8)
sim.28.run=bml.step(sim.28)
#18 steps

sim.28.a=bml.init(100,100,.8)
sim.28.a.run=bml.step(sim.28.a)
#21 steps

sim.28.b=bml.init(100,100,.8)
sim.28.b.run=bml.step(sim.28.b)
#26 steps

sim.28.c=bml.init(100,100,.8)
sim.28.c.run=bml.step(sim.28.c)
#24 steps

sim.28.d=bml.init(100,100,.8)
sim.28.d.run=bml.step(sim.28.d)
#20 steps

mean.sim.28=21.8

sim.29=bml.init(100,100,.9)
sim.29.run=bml.step(sim.29)
#17 steps

sim.29.a=bml.init(100,100,.9)
sim.29.a.run=bml.step(sim.29.a)
#15 steps

sim.29.b=bml.init(100,100,.9)
sim.29.b.run=bml.step(sim.29.b)
#16 steps

sim.29.c=bml.init(100,100,.9)
sim.29.c.run=bml.step(sim.29.c)
#10 steps

sim.29.d=bml.init(100,100,.9)
sim.29.d.run=bml.step(sim.29.d)
#13 steps

mean.sim.29=14.2

#######################################################################################
#PLOTS


valuesofp=c(.1,.2,.3,.4,.5,.6,.7,.8,.9)
fivebyfive.steps=c(NA,NA,NA,NA,14.3,22.5,4,2.3,1)
fivebyfive=data.frame(valuesofp,fivebyfive.steps)

plot(x=valuesofp,y=fivebyfive.steps,type="l",xlab="Density of Cars",ylab="Steps Until Gridlock",main="5 x 5 Matrix")
points(.1,20,col="red")
points(.2,20,col="red")
points(.3,20,col="red")
points(.4,20,col="red")
points(.5,14.3,col="green")
points(.6,22.5,col="green")
text(x=.2,y=8,"Key")
points(.2,6,col="red")
points(.2,4,col="green")
text(.27,6,"= free flow")
text(.335,4,"= gridlock and free flow")




tenbyten.steps=c(NA,NA,NA,165.5,18.3,43.2,9,13.8,9.2)
tenbyten=data.frame(valuesofp,tenbyten.steps)

plot(x=valuesofp,y=tenbyten.steps,type="l",xlab="Density of Cars",ylab="Steps Until Gridlock",main="10 x 10 Matrix")
points(.1,150,col="red")
points(.2,150,col="red")
points(.3,150,col="red")
points(.4,165.5,col="green")
points(.5,18.3,col="green")
points(.7,9,col="green")
text(x=.2,y=80,"Key")
points(.2,70,col="red")
points(.2,60,col="green")
text(.27,70,"= free flow")
text(.335,60,"= gridlock and free flow")




hunbyhun.steps=c(NA,NA,NA,NA,143,81.4,37.8,21.8,14.2)
hunbyhun=data.frame(valuesofp,hunbyhun.steps)

plot(x=valuesofp,y=hunbyhun.steps,type="l",xlab="Density of Cars",ylab="Steps Until Gridlock",main="100 x 100 Matrix")
points(.1,140,col="red")
points(.2,140,col="red")
points(.3,140,col="red")
points(.4,140,col="red")
text(x=.3,y=80,"Key")
points(.3,70,col="red")
text(.37,70,"= free flow")



