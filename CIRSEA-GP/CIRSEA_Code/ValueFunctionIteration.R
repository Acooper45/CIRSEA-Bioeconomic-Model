rm(list = ls(all = TRUE)) 
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

library(ggplot2)   # Data visualization
library(tidyr)   # Tidy data management
library(dplyr)
library(cowplot)


sizex = 100 #size of the state grid

T=20 #time horizon for backward induction

a=20
b=.1
delta=1/1.2
r=.8
K=100
small=K/1000

xgrid = seq(small,K,length.out=sizex) # Defines the grid of states; initialization

f = function(h,x) # System dynamics (growth)
{
  xnext = x + r*x*(1-x/K) - h
}

pi = function(h) # Computes the profit function
{
  profit = a*h-b*h^2
}

Payoff = function(h,x,V) # Compute the payoff function w/r/t stock dynamics and the value function
{
  xnext = f(h,x)
  Vnext = spline(x=xgrid,y=V,xout=xnext)
  negout = -(pi(h) + delta*Vnext$y) 
  return(negout)
}

DFall = data.frame() # Initializes an empty df to store results
Vnext = vector()
V = seq(0,0,length.out=sizex) # Initializes the value function

#Try payoff function
z=Payoff(.17992,.1,V)

for(t in T:1) # Backwards Induction Loop from T to t = 1
{
  print(t)
  for(i in 1:sizex) #Iterates over 100 states per sizex
  {
    x = xgrid[i] # Iterate over each state in the stategrid
    guess = x/2 # Starting guess is MSY, or half of the stock at a given time
    low = 0 # Lower bound on harvest
    high = x + r*x*(1-x/K) # Upper bound on harvest
    Thing = optim(par=guess,fn=Payoff,lower=low,upper=high,x=x,V=V,method='L-BFGS-B') # Use the optimization to find the optimal harvest level (hstar) for each state
    hstar = Thing$par # parameter = optimal harvest
    Vstar = -Thing$value
    Vnext[i] = Vstar # Update the value function (V) based on the harvest
    DFnow = data.frame(time=t,x=x,hstar=hstar,Vstar=Vstar)
    DFall = bind_rows(DFall,DFnow)
  }
  V = Vnext
}

Ph = ggplot(data=DFall) +
  geom_path(aes(x=x,y=hstar,color=factor(time)),size=1.3) +
  xlab("Stock, x") +
  ylab("Harvest, h") +
  scale_color_discrete(name="Year") +
  theme_bw() +
  theme(legend.position = "none") +
  ylim(c(0,100))
Ph

PV = ggplot(data=DFall) +
  geom_path(aes(x=x,y=Vstar,color=factor(time)),size=1.3) +
  xlab("Stock, x") +
  ylab("Value Function, V") +
  scale_color_discrete(name="Year") +
  theme_bw()
PV

#Forward Simulation
DFopt = DFall %>% filter(time==1)
hpol = DFopt$hstar # Extract the optimal harvest policy associated with the last time step
xpol = DFopt$x # Extract the optimal stock state associated with the last time step
xsim = vector() # Initialize xsim for the stock state simulation
hsim = vector() # Initiliaze hsim for the harvest state simulation

xsim[1]=K/10
Tsim = seq(1,20)

for(tt in Tsim) # Iterate forward in time (Tsim)
{
  Thing = spline(x=xpol,y=hpol,xout=xsim[tt]) # Use spline interpolation to find the optimal harvest (hsim) given the current state (xsim)
  hsim[tt] = Thing$y # Update the state and the next time step using the stock dynamics in function (f)
  # xsim[tt] is the current state at time tt. The interpolation estimates the optimal harvest hsim[tt]
  
  if(tt<max(Tsim)) # checks the current time period is less than the maximum time step in Tsim
  {
     xsim[tt+1] = f(h=hsim[tt],x=xsim[tt]) # The state for the next time step (xsim[tt + 1]) is updated using the dynamics function. This ensures the loop continues until the end of the simulation horizon.
  }

}

DFsim = data.frame(time=Tsim,xsim=xsim,hsim=hsim)

Pxsim = ggplot(data=DFsim) +
  geom_line(aes(x=time,y=xsim),color="skyblue3",size=1.5) +
  geom_point(aes(x=time,y=xsim),color="black") +
  xlab("Time") +
  ylab("Stock, x") +
  theme_bw()
#Pxsim

Phsim = ggplot(data=DFsim) +
  geom_line(aes(x=time,y=hsim),color="skyblue3",size=1.5) +
  geom_point(aes(x=time,y=hsim),color="black") +
  xlab("Time") +
  ylab("Harvest, h") +
  theme_bw()
Phsim

Pall = plot_grid(Ph,PV,Pxsim,Phsim,ncol=2,nrow=2)
Pall

ggsave(filename="../Fig1.png",plot=Pall,width=6,height=5,unit="in")



