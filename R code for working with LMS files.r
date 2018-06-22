# R code for working with LMS curves, from Urlacher et al...

#This function produces centile values for given ages and LMS values.
centilesLMS<-function(Age,Sex,LMSlookuptable,cent=c(0.02,0.05,0.25,0.50,0.75,0.95,0.98)){
	useLMS<-apply(data.frame(Age,Sex), 1, function(x) which.min(abs(LMSlookuptable$Age-
	as.numeric(x[1]))+(x[2]!=LMSlookuptable$Sex)*999))
	LMS<-LMSlookuptable[useLMS,c("Lambda","Mu","Sigma")]
	getcent<-function(la,mu,si){
		sapply(cent,function(x) exp(log(qnorm(x)*la*si+1)/la+log(mu)),simplify=TRUE)
		}
	if(length(cent)==1) o<-cbind(Age,apply(LMS,1,function(x) getcent(x[1],x[2],x[3])))
	else o<-cbind(Age,t(apply(LMS,1,function(x) getcent(x[1],x[2],x[3]))))
	o<-data.frame(o)
	names(o)<-c("Age",cent)
	o
}

#This function calculates a z-score from a measure and the appropriate LMS values. 
ZfromLMS<-function(measure,la,mu,si){
	zs <- ((measure / mu)^la - 1)/(la * si)
	zs
}


#function to get z-scores for a given age and value. Sex, age, and value need to be in the same units or with the same coding scheme as is used in the LMS table.
getZ<-function(Age, Sex, Value, LMSlookuptable){
	useLMS<-apply(data.frame(Age,Sex), 1, function(x) which.min(abs(LMSlookuptable$Age-as.numeric(x[1]))+(x[2]!=LMSlookuptable$Sex)*999))
	LMSmatches<-LMSlookuptable[useLMS,]
	ZfromLMS(Value,LMSmatches$Lambda,LMSmatches$Mu,LMSmatches$Sigma)
	}

#just a wrapper. Uses pnorm to get centiles from the Z-scores in getZ
getCentile<-function(Age, Sex, Value, LMSlookuptable){
	zs<-getZ(Age, Sex, Value, LMSlookuptable)
	cent<-pnorm(zs)
	cent
	}
	


#example usage

#read in LMS files
HT.LMS<-read.csv("ShuarHeightByageLMS.csv")
WT.LMS<-read.csv("ShuarWeightByAgeLMS.csv")
BMI.LMS<-read.csv("ShuarBMIByAgeLMS.csv")

#get a z-score for an 5 year-old male, 97cm tall
getZ(5,"Male",97,HT.LMS)

#get a Centile value
getCentile(5,"Male",97,HT.LMS)

#get z-scores for three individuals
growthdat<-data.frame(Age=c(5,5,15),Sex=c("Male","Female","Male"),Weights<-c(15,15,40))
getZ(growthdat$Age,growthdat$Sex,growthdat$Weights,WT.LMS)

#what are the 5th, 50th, and 95th centiles for males at age 5 and age 10?
centilesLMS(c(5,10),"Male",HT.LMS,cent=c(0.05,0.50,0.95))

