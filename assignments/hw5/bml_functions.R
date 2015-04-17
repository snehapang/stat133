#################################################################################
#### Functions for BML Simulation Study


#### Initialization function.
## Input : size of grid [r and c] and density [p]
## Output : A matrix [m] with entries 0 (no cars) 1 (red cars) or 2 (blue cars)
## that stores the state of the system (i.e. location of red and blue cars)



bml.init = function(r, c, p){
  m=matrix(sample(c(0,1,2),size=r*c,replace=TRUE,prob=c(1-p,p/2,p/2)),nrow=r,ncol=c)   
             return(m)
 
}

my_palette <- colorRampPalette(c("white", "red", "blue"))(n = 3)
x=bml.init(10,10,.6) #naming a certain trial x stores it so image below will correspond particularly to that matrix
#you can change values for r,c,p, above later or whenever
#without x, you can run bml.init for same parameters and you will get dif matrix every time
image(x,col=my_palette)


#### Function to move the system one step (east and north)
## Input : a matrix [m] of the same type as the output from bml.init()
## Output : TWO variables, the updated [m] and a logical variable
## [grid.new] which should be TRUE if the system changed, FALSE otherwise.

## NOTE : the function should move the red cars once and the blue cars once,
## you can write extra functions that do just a step north or just a step east.

move.red=function(m){
  numr=nrow(m)
  numc=ncol(m)
  for(i in 1:(numr-1)){
    for(j in 1:numc){
      if(m[i,j]==0 && m[i+1,j]==1){
        m[i,j]=1
        m[i+1,j]=0
      }
    }
  }
  for(i in 1:numr){
    for(j in 1:numc){
      if(m[1,j]==1 && m[numr,j]==0){
        m[1,j]=0
        m[numr,j]=1
      }
    }
  }
  
  return(m)
}

move.blue=function(m){
  numr=nrow(m)
  numc=ncol(m)
  for(j in 2:numc){
    for(i in 1:numr){
      if(m[i,j]==0 && m[i,j-1]==2){
        m[i,j]=2
        m[i,j-1]=0
      }
    }
  }
  for(i in 1:numr){
    for(j in 1:numc){
      if(m[i,numc]==2 && m[i,1]==0){
        m[i,numc]=0
        m[i,1]=2
      }
    }
  }
  
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
  
  return(list(m,counter))
}
#counter tells you how many moves until gridlock











#### Function to do a simulation for a given set of input parameters
## Input : size of grid [r and c] and density [p]
## Output : *up to you* (e.g. number of steps taken, did you hit gridlock, ...)

bml.sim <- function(r, c, p){
  t=bml.step(bml.init(r,c,p))
  return(t)}



