############################

#install.packages("SoilR")
#install.packages("FME")

require(SoilR)
require(FME)
require(MASS)
require(lattice)


############################
help(package="SoilR")

?OnepModel
#one pool model; most simple model they have

t_start=0 
t_end=10 
tn=50
timestep=(t_end-t_start)/tn 
t=seq(t_start,t_end,timestep) 
k=0.8
C0=100 #initial mass
In = 30 #litter inputs by time


Ex=OnepModel(t,k,C0,In) #run model function 
Ct=getC(Ex)
Rt=getReleaseFlux(Ex) #get resp rate
Rc=getAccumulatedRelease(Ex)

plot(
  t,
  Ct,
  type="l",
  ylab="Carbon stocks (arbitrary units)",
  xlab="Time (arbitrary units)",
  lwd=2
) 

plot(
  t,
  Rt,
  type="l",
  ylab="Carbon released (arbitrary units)",
  xlab="Time (arbitrary units)",
  lwd=2
) 

plot(
  t,
  Rc,
  type="l",
  ylab="Cummulative carbon released (arbitrary units)",
  xlab="Time (arbitrary units)",
  lwd=2
) 


#########################
#Two pool Model

#This example show the difference between the three types of two-pool models  
times=seq(0,20,by=0.1)
ks=c(k1=0.8,k2=0.00605)
C0=c(C10=5,C20=5) #initial amount of carbon in the two pools

Inmean=1


Parallel=TwopParallelModel(t=times,ks=ks,C0=C0,In=Inmean,gam=0.9,xi=.5)
Series=TwopSeriesModel(t=times,ks=ks,a21=0.2*ks[1],C0=C0,In=Inmean,
                       xi=.5)
Feedback=TwopFeedbackModel(t=times,ks=ks,a21=0.2*ks[1],a12=0.5*ks[2],C0=C0,
                           In=Inmean,xi=,5)

CtP=getC(Parallel)
CtS=getC(Series)
CtF=getC(Feedback)

RtP=getReleaseFlux(Parallel)
RtS=getReleaseFlux(Series)
RtF=getReleaseFlux(Feedback)

par(mfrow=c(2,1),mar=c(4,4,1,1))
plot(times,rowSums(CtP),type="l",ylim=c(0,20),ylab="Carbon stocks (arbitrary units)",xlab=" ")
lines(times,rowSums(CtS),col=2)
lines(times,rowSums(CtF),col=3)
legend("topleft",c("Two-pool Parallel","Two-pool Series","Two-pool Feedback"),
       lty=c(1,1,1),col=c(1,2,3),bty="n")

plot(times,rowSums(RtP),type="l",ylim=c(0,3),ylab="Carbon release (arbitrary units)", xlab="Time")
lines(times,rowSums(RtS),col=2)
lines(times,rowSums(RtF),col=3)
par(mfrow=c(1,1))

#########
#Now change inputs

Inmean=1
InRand=data.frame(times,Random.inputs=rnorm(length(times),Inmean,0.2))
InSin=data.frame(times,Inmean+1*sin(times*pi*3))
InRand
InSin

FeedbackMean=TwopFeedbackModel(t=times,ks=ks,a21=0.2*ks[1],a12=0.5*ks[2],C0=C0,
                           In=Inmean,xi=,5)
FeedbackRand=TwopFeedbackModel(t=times,ks=ks,a21=0.2*ks[1],a12=0.5*ks[2],C0=C0,
                            In=InRand,xi=,5)
FeedbackSin=TwopFeedbackModel(t=times,ks=ks,a21=0.2*ks[1],a12=0.5*ks[2],C0=C0,
                            In=InSin,xi=,5)



CtFM=getC(FeedbackMean)
CtFR=getC(FeedbackRand)
CtFS=getC(FeedbackSin)

RtFM=getReleaseFlux(FeedbackMean)
RtFR=getReleaseFlux(FeedbackRand)
RtFS=getReleaseFlux(FeedbackSin)

par(mfrow=c(2,1),mar=c(4,4,1,1))
plot(times,rowSums(CtFM),type="l",ylim=c(0,20),ylab="Carbon stocks (arbitrary units)",xlab=" ")
lines(times,rowSums(CtFR),col=2)
lines(times,rowSums(CtFS),col=3)
legend("topleft",c("Mean","Random","Sin"),
       lty=c(1,1,1),col=c(1,2,3),bty="n")

plot(times,rowSums(RtFM),type="l",ylim=c(0,3),ylab="Carbon release (arbitrary units)", xlab="Time")
lines(times,rowSums(RtFR),col=2)
lines(times,rowSums(RtFS),col=3)
par(mfrow=c(1,1))

################################
#Three pool model

t_start=0 
t_end=10 
tn=50
timestep=(t_end-t_start)/tn 
t=seq(t_start,t_end,timestep) 
ks=c(k1=0.8,k2=0.4,k3=0.2)
C0=c(C10=100,C20=150, C30=50)
In = 60


Ex1=ThreepFeedbackModel(t=t,ks=ks,a21=0.5,a12=0.1,a32=0.2,a23=0.1,C0=C0,In=In)
Ct=getC(Ex1)
Rt=getReleaseFlux(Ex1)

plot(
  t,
  rowSums(Ct),
  type="l",
  ylab="Carbon stocks (arbitrary units)",
  xlab="Time (arbitrary units)",
  lwd=2,
  ylim=c(0,sum(Ct[51,]))
) 
lines(t,Ct[,1],col=2)
lines(t,Ct[,2],col=4)
lines(t,Ct[,3],col=3)
legend(
  "topleft",
  c("Total C","C in pool 1", "C in pool 2","C in pool 3"),
  lty=c(1,1,1,1),
  col=c(1,2,4,3),
  lwd=c(2,1,1,1),
  bty="n"
)

plot(
  t,
  rowSums(Rt),
  type="l",
  ylab="Carbon released (arbitrary units)",
  xlab="Time (arbitrary units)",
  lwd=2,
  ylim=c(0,sum(Rt[51,]))
) 
lines(t,Rt[,1],col=2)
lines(t,Rt[,2],col=4)
lines(t,Rt[,3],col=3)
legend(
  "topleft",
  c("Total C release",
    "C release from pool 1",
    "C release from pool 2",
    "C release from pool 3"),
  lty=c(1,1,1,1),
  col=c(1,2,4,3),
  lwd=c(2,1,1,1),
  bty="n"
)

#Random inputs
Inr=data.frame(t,Random.inputs=rnorm(length(t),50,10))
plot(Inr,type="l")

Ex2=ThreepFeedbackModel(t=t,ks=ks,a21=0.5,a12=0.1,a32=0.2,a23=0.1,C0=C0,In=Inr)
Ctr=getC(Ex2)
Rtr=getReleaseFlux(Ex2)

plot(
  t,
  rowSums(Ctr),
  type="l",
  ylab="Carbon stocks (arbitrary units)",
  xlab="Time (arbitrary units)",
  lwd=2,
  ylim=c(0,sum(Ctr[51,]))
) 
lines(t,Ctr[,1],col=2)
lines(t,Ctr[,2],col=4)
lines(t,Ctr[,3],col=3)
legend("topright",c("Total C","C in pool 1", "C in pool 2","C in pool 3"),
       lty=c(1,1,1,1),col=c(1,2,4,3),lwd=c(2,1,1,1),bty="n")

plot(t,rowSums(Rtr),type="l",ylab="Carbon released (arbitrary units)",
     xlab="Time (arbitrary units)",lwd=2,ylim=c(0,sum(Rtr[51,]))) 
lines(t,Rtr[,1],col=2)
lines(t,Rtr[,2],col=4)
lines(t,Rtr[,3],col=3)
legend(
  "topright",
  c("Total C release",
    "C release from pool 1",
    "C release from pool 2",
    "C release from pool 3"
  ),
  lty=c(1,1,1,1),
  col=c(1,2,4,3),
  lwd=c(2,1,1,1),
  bty="n")


?fT.Daycent1
help(package="SoilR")

############################

#Boreal CO2 incubation experiment

BorealCO2=subset(eCO2, subset=Sample=="AK_T25", select=-Sample)
names(BorealCO2)<-c("time","eCO2","eCO2sd")

plot(BorealCO2[,1:2], xlab="Days", ylab="Evolved CO2 (mgC g-1 soil)")
arrows(BorealCO2[,1],BorealCO2[,2]-BorealCO2[,3],BorealCO2[,1],BorealCO2[,2]+BorealCO2[,3],code=3,angle=90,length=0.1)

days=seq(0,42)
Ctotal=7.7

#eCO2 function
eCO2func=function(pars){
  mod=TwopFeedbackModel(
    t=days,
    ks=pars[1:2],
    a21=pars[3]*pars[1],
    a12=pars[4]*pars[2],
    C0=Ctotal*c(pars[5],1-pars[5]),
    In=0,
    pass=TRUE 
  )
  AccR=getAccumulatedRelease(mod)
  return(data.frame(time=days,eCO2=rowSums(AccR))) 
}

#cost function: contrast model and data and minimize the residuals
eCO2cost=function(pars){
  modelOutput=eCO2func(pars)
  return(modCost(model=modelOutput, obs=BorealCO2, err="eCO2sd")) 
}


inipars=c(k1=0.5,k2=0.05,alpha21=0.5,alpha12=0.1,gamma=0.5)
eCO2fit=modFit(f=eCO2cost,p=inipars,method="Marq",
               upper=c(Inf,Inf,1,1,1),lower=c(0,0,0,0,0))

#best set of parameter values found by the function
eCO2fit$par
fitmod=eCO2func(eCO2fit$par)

#model fit
plot(BorealCO2[,1:2], xlab="Days", ylab="Evolved CO2 (mgC g-1 soil)")
arrows(BorealCO2[,1],BorealCO2[,2]-BorealCO2[,3],BorealCO2[,1],
       BorealCO2[,2]+BorealCO2[,3],code=3,angle=90,length=0.1)
lines(fitmod)


