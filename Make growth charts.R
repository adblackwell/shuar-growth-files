#Shuar growth Standard charts

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

HT.LMS<-read.csv("ShuarHeightByageLMS.csv")
WT.LMS<-read.csv("ShuarWeightByAgeLMS.csv")
BMI.LMS<-read.csv("ShuarBMIByAgeLMS.csv")

#Make grid
#Boys
centiles<-c(0.05,0.10,0.25,0.50,0.75,0.90,0.95)
HTcentB<-centilesLMS(seq(2,20,0.1),"Male",HT.LMS,cent=centiles)
WTcentB<-centilesLMS(seq(2,20,0.1),"Male",WT.LMS,cent=centiles)


jpeg(filename = "Ninos2-20.jpg", width = 8, height = 10.5, units = "in", pointsize = 12, quality = 95,res=300)
xs<-c(2,20)
ys1<-c(50,170)
ys2<-c(5,125)
offs<-ys1[1]-ys2[1]
bleft<-floor(min(HTcentB[1,-1])/5)*5
bright<-ceiling(max(WTcentB[nrow(WTcentB),-1])/5)*5+offs
par(xpd=FALSE)
par(new=FALSE)
par(mar=c(4,4,5,4))
plot(xs,ys1,type="n",xaxs="i",yaxs="i",xaxt="n",yaxt="n",ylab="",xlab="",main="")
axis(1,at=seq(xs[1],xs[2],1))
axis(3,at=seq(xs[1],xs[2],1))
axis(2,at=seq(bleft,ys1[2],5),las=1)
axis(4,at=seq(bright,ys1[2],5),las=1)
grid((xs[2]-xs[1])*4,(ys1[2]-ys1[1]),lty=1)
grid((xs[2]-xs[1]),(ys1[2]-ys1[1])/5,lty=1,lwd=2)
for(cent in 2:ncol(HTcentB)){
	lines(HTcentB$Age,HTcentB[,cent],lwd=2,col="black")
}
points(rep(19,(ncol(HTcentB)-1)), HTcentB[HTcentB$Age==19,-1],pch=15,cex=3,col="white")
text(rep(19,(ncol(HTcentB)-1)), HTcentB[HTcentB$Age==19,-1],as.numeric(colnames(HTcentB)[-1])*100)
mtext("Estatura (cm)", 2,2.5,at=mean(c(bleft,ys1[2])))
mtext("Estatura (cm)", 4,2.5,at=mean(c(bright,ys1[2])))
mtext("Edad", 1,2.5)
mtext(paste("Ni","\u00F1","os Shuar Edad 2-20",sep=""),3,4,adj=0)
mtext("Percentiles de Estatura por edad y Peso por edad",3,3,adj=0)
mtext("Nombre:________________",3,4,adj=1)

par(new=T)
plot(xs,ys2,type="n",xaxs="i",yaxs="i",xaxt="n",yaxt="n",ylab="",xlab="")
axis(2,at=seq(ys2[1],bleft-offs-2.5,5),las=1)
axis(4,at=seq(ys2[1],bright-offs-2.5,5),las=1)
#grid((xs[2]-xs[1])*4,(ys[2]-ys[1]),lty=1)
#grid((xs[2]-xs[1]),(ys[2]-ys[1])/5,lty=1,lwd=2)
for(cent in 2:ncol(WTcentB)){
	lines(WTcentB$Age,WTcentB[,cent],lwd=2,col="black")
}
points(rep(19,(ncol(WTcentB)-1)), WTcentB[WTcentB$Age==19,-1],pch=15,cex=3,col="white")
text(rep(19,(ncol(WTcentB)-1)), WTcentB[WTcentB$Age==19,-1],as.numeric(colnames(WTcentB)[-1])*100)

par(xpd=NA)
lines(c(0,2),rep(bleft-offs-2.5,2))
lines(c(20,22),rep(bright-offs-2.5,2))
mtext("Peso (kg)", 2,2.5,at=mean(c(bleft-offs,ys2[1])))
mtext("Peso (kg)", 4,2.5,at=mean(c(bright-offs,ys2[1])))
mtext("Urlacher, et al (2015) American Journal of Human Biology, in press", 1,2.5,at=18,cex=0.5)
dev.off()

HTcentB<-centilesLMS(seq(2,20,0.1),"Female",HT.LMS,cent=centiles)
WTcentB<-centilesLMS(seq(2,20,0.1),"Female",WT.LMS,cent=centiles)
jpeg(filename = "Ninas2-20.jpg", width = 8, height = 10.5, units = "in", pointsize = 12, quality = 95,res=300)
xs<-c(2,20)
ys1<-c(50,170)
ys2<-c(5,125)
offs<-ys1[1]-ys2[1]
bleft<-floor(min(HTcentB[1,-1])/5)*5
bright<-ceiling(max(WTcentB[nrow(WTcentB),-1])/5)*5+offs
par(xpd=FALSE)
par(new=FALSE)
par(mar=c(4,4,5,4))
plot(xs,ys1,type="n",xaxs="i",yaxs="i",xaxt="n",yaxt="n",ylab="",xlab="",main="")
axis(1,at=seq(xs[1],xs[2],1))
axis(3,at=seq(xs[1],xs[2],1))
axis(2,at=seq(bleft,ys1[2],5),las=1)
axis(4,at=seq(bright,ys1[2],5),las=1)
grid((xs[2]-xs[1])*4,(ys1[2]-ys1[1]),lty=1)
grid((xs[2]-xs[1]),(ys1[2]-ys1[1])/5,lty=1,lwd=2)
for(cent in 2:ncol(HTcentB)){
	lines(HTcentB$Age,HTcentB[,cent],lwd=2,col="black")
}
points(rep(19,(ncol(HTcentB)-1)), HTcentB[HTcentB$Age==19,-1],pch=15,cex=3,col="white")
text(rep(19,(ncol(HTcentB)-1)), HTcentB[HTcentB$Age==19,-1],as.numeric(colnames(HTcentB)[-1])*100)
mtext("Estatura (cm)", 2,2.5,at=mean(c(bleft,ys1[2])))
mtext("Estatura (cm)", 4,2.5,at=mean(c(bright,ys1[2])))
mtext("Edad", 1,2.5)
mtext(paste("Ni","\u00F1","as Shuar Edad 2-20",sep=""),3,4,adj=0)
mtext("Percentiles de Estatura por edad y Peso por edad",3,3,adj=0)
mtext("Nombre:________________",3,4,adj=1)

par(new=T)
plot(xs,ys2,type="n",xaxs="i",yaxs="i",xaxt="n",yaxt="n",ylab="",xlab="")
axis(2,at=seq(ys2[1],bleft-offs-2.5,5),las=1)
axis(4,at=seq(ys2[1],bright-offs-2.5,5),las=1)
#grid((xs[2]-xs[1])*4,(ys[2]-ys[1]),lty=1)
#grid((xs[2]-xs[1]),(ys[2]-ys[1])/5,lty=1,lwd=2)
for(cent in 2:ncol(WTcentB)){
	lines(WTcentB$Age,WTcentB[,cent],lwd=2,col="black")
}
points(rep(19,(ncol(WTcentB)-1)), WTcentB[WTcentB$Age==19,-1],pch=15,cex=3,col="white")
text(rep(19,(ncol(WTcentB)-1)), WTcentB[WTcentB$Age==19,-1],as.numeric(colnames(WTcentB)[-1])*100)

par(xpd=NA)
lines(c(0,2),rep(bleft-offs-2.5,2))
lines(c(20,22),rep(bright-offs-2.5,2))
mtext("Peso (kg)", 2,2.5,at=mean(c(bleft-offs,ys2[1])))
mtext("Peso (kg)", 4,2.5,at=mean(c(bright-offs,ys2[1])))
mtext("Urlacher, et al (2015) American Journal of Human Biology, in press", 1,2.5,at=18,cex=0.5)
dev.off()

BMIcentB<-centilesLMS(seq(2,20,0.1),"Male",BMI.LMS,cent=centiles)
jpeg(filename = "Ninos2-20-BMI.jpg", width = 8, height = 10.5, units = "in", pointsize = 12, quality = 95,res=300)
xs<-c(2,20)
ys1<-c(12,27)
par(xpd=FALSE)
par(new=FALSE)
par(mar=c(4,4,5,4))
plot(xs,ys1,type="n",xaxs="i",yaxs="i",xaxt="n",yaxt="n",ylab="",xlab="",main="")
axis(1,at=seq(xs[1],xs[2],1))
axis(3,at=seq(xs[1],xs[2],1))
axis(2,at=seq(ys1[1],ys1[2],1),las=1)
axis(4,at=seq(ys1[1],ys1[2],1),las=1)
grid((xs[2]-xs[1])*4,(ys1[2]-ys1[1])*4,lty=1)
grid((xs[2]-xs[1]),(ys1[2]-ys1[1]),lty=1,lwd=2)
for(cent in 2:ncol(BMIcentB)){
	lines(BMIcentB$Age,BMIcentB[,cent],lwd=2,col="black")
}
points(rep(19,(ncol(BMIcentB)-1)), BMIcentB[BMIcentB$Age==19,-1],pch=15,cex=3,col="white")
text(rep(19,(ncol(BMIcentB)-1)), BMIcentB[BMIcentB$Age==19,-1],as.numeric(colnames(BMIcentB)[-1])*100)
mtext(expression("BMI kg/m"^2), 2,2.5)
mtext(expression("BMI kg/m"^2), 4,2.5)
mtext("Edad", 1,2.5)
mtext(paste("Ni","\u00F1","os Shuar Edad 2-20",sep=""),3,4,adj=0)
mtext(expression("Percentiles de BMI: Peso(kg)/Estatura(m)"^2),3,3,adj=0)
mtext("Nombre:________________",3,4,adj=1)
mtext("Urlacher, et al (2015) American Journal of Human Biology, in press", 1,2.5,at=18,cex=0.5)
dev.off()

BMIcentB<-centilesLMS(seq(2,20,0.1),"Female",BMI.LMS,cent=centiles)
jpeg(filename = "Ninas2-20-BMI.jpg", width = 8, height = 10.5, units = "in", pointsize = 12, quality = 95,res=300)
xs<-c(2,20)
ys1<-c(12,29)
par(xpd=FALSE)
par(new=FALSE)
par(mar=c(4,4,5,4))
plot(xs,ys1,type="n",xaxs="i",yaxs="i",xaxt="n",yaxt="n",ylab="",xlab="",main="")
axis(1,at=seq(xs[1],xs[2],1))
axis(3,at=seq(xs[1],xs[2],1))
axis(2,at=seq(ys1[1],ys1[2],1),las=1)
axis(4,at=seq(ys1[1],ys1[2],1),las=1)
grid((xs[2]-xs[1])*4,(ys1[2]-ys1[1])*4,lty=1)
grid((xs[2]-xs[1]),(ys1[2]-ys1[1]),lty=1,lwd=2)
for(cent in 2:ncol(BMIcentB)){
	lines(BMIcentB$Age,BMIcentB[,cent],lwd=2,col="black")
}
points(rep(19,(ncol(BMIcentB)-1)), BMIcentB[BMIcentB$Age==19,-1],pch=15,cex=3,col="white")
text(rep(19,(ncol(BMIcentB)-1)), BMIcentB[BMIcentB$Age==19,-1],as.numeric(colnames(BMIcentB)[-1])*100)
mtext(expression("BMI kg/m"^2), 2,2.5)
mtext(expression("BMI kg/m"^2), 4,2.5)
mtext("Edad", 1,2.5)
mtext(paste("Ni","\u00F1","as Shuar Edad 2-20",sep=""),3,4,adj=0)
mtext(expression("Percentiles de BMI: Peso(kg)/Estatura(m)"^2),3,3,adj=0)
mtext("Nombre:________________",3,4,adj=1)
mtext("Urlacher, et al (2015) American Journal of Human Biology, in press", 1,2.5,at=18,cex=0.5)
dev.off()
