library(flux)# to get area under the curve


yield<- function(age){
	if(type==1){F_t<-F1_func(age)}
	if(type==2){F_t<-F2_func(age)}
	if(type==3){F_t<-F3_func(age)}
	Nr = R*exp(-M*(r))# NUMBER OF FISH RECRUITED TO FISHERY
	Nt = Nr*exp(-(M+F_t)*(age-tR))	
	Nhar = F_t*Nt
	Wt = a*(Linf*(1-exp(-k*(age-t0))))^b
	Y  = F_t*Nt*Wt
	return(Y)
	}

te_fun<- function(limit,linf,k,t0)
	{
	# RETURNS THE AGE OF A FISH GIVEN A SIZE (i.e., inverse of vbgf)
	te<-log(1-((limit/linf)))/-k + t0
	return(te)
	}

a=10^-5.102
b=3.1
M = 0.2	
Linf=351

t0= 0
tR = 4.2  # age recruited to fishery
r = tR-t0 # tR-t0
k = 0.35
R=1000

# 10, 11, 12 inches
combos<- expand.grid(A_rec= c(te_fun(254,Linf,k,t0),
	te_fun(279,Linf,k,t0), te_fun(304,Linf,k,t0)), 
	F_set=seq(0,0.6, .05), type=c(1,2,3))
out<- data.frame()
for(i in 1:nrow(combos))
	{
	type=combos$type[i]	
	F<-combos$F_set[i]	
	A_rec<- combos$A_rec[i]
	yr<-seq(0.1,13,0.1)
	F1<- rep(F,length(yr))
	xx<-auc(x=yr, y=F1)
	rel<-rep(c(0,0,0,1,1,1,1,1,1,0),13)
	yy<-auc(x=yr, y=rel)
	F2<-(rel/yy)*xx
	auc(x=yr, y=F2)
	rel<-rep(c(0,0,0,1,1,1,0,0,0,0),13)
	yy<-auc(x=yr, y=rel)
	F3<-(rel/yy)*xx
	auc(x=yr, y=F3)
	F_vals<- data.frame(yr=c(0,yr),F1=c(0,F1),F2=c(0,F2),F3=c(0,F3))
	F_vals[F_vals$yr<A_rec,]$F1<-0
	F_vals[F_vals$yr<A_rec,]$F2<-0
	F_vals[F_vals$yr<A_rec,]$F3<-0
	F1_func<- approxfun(F_vals$yr,F_vals$F1,method="constant")
	F2_func<- approxfun(F_vals$yr,F_vals$F2,method="constant")
	F3_func<- approxfun(F_vals$yr,F_vals$F3,method="constant")
	y<- integrate(yield,lower= 0, upper = 13,stop.on.error=FALSE)$value	
	app<- data.frame(F=F, Arec=A_rec, type=type, y=y) 
	out<- rbind(out,app)
	}

require(lattice)
out$type_txt<-"Constant F"
out[out$type==2,]$type_txt<- "Medium F"
out[out$type==3,]$type_txt<- "Short F"
out$ll<- 10
out[out$Arec>4 & out$Arec<5,]$ll<-11
out[out$Arec>5 & out$Arec<6,]$ll<-12
xyplot(y~F|as.factor(Arec), out,groups=type,type='l',auto.key=TRUE)
xyplot(y~F|as.factor(type_txt), out,
	groups=ll,type='l',auto.key=TRUE)
x<- seq(1,13,0.1)
plot(x,F1_func(x),ylim=c(0,1.5),type='l')
points(x,F2_func(x),col="red",type='l')
points(x,F3_func(x),col="green",type='l')

out<- out[order(out$age),]	
plot(Nt~age,out, xlim=c(0,13))
plot(Wt~age,out, xlim=c(0,13))
plot(Y~age,out, xlim=c(0,13))
