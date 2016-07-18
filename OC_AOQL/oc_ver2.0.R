OC=function(batch,sample,ac,new=FALSE,oc=TRUE,aoql=FALSE,xlab="",f1.ylab="",main="",xline=NA){
	x<-pretty(0:1,n=100)
	y<-NA
	z<-NA
	hoc<-pbinom(ac,size=sample,prob=xline)
	haoql<-xline*hoc
	for(i in 1:101){
		y[i]<-pbinom(ac,size=sample,prob=x[i])
		z[i]<-y[i]*x[i]
	}
	oc.data<-data.frame(x,y,z)
	if(new==TRUE){
		par(new=T)
	}
	if(oc==TRUE & aoql==FALSE){
		plot(x,y,type="l",xlab=xlab,ylab=f1.ylab,main=main)
		if(!is.na(xline)){
			points(xline,hoc,cex=1.0,pch=21,bg="red",col="red",)
			text(xline+0.05,hoc,paste("OC:(x=",xline,",y=",round(hoc,2),")"),pos=4,cex=0.8)
		}
	}
	if(oc==FALSE & aoql==TRUE){
		plot(x,z,type="l",axes=T,ann=FALSE)
		text(oc.data[,1][oc.data[3]==max(z)]+0.08,round(max(z),2)-0.005,paste("AOQL=",round(max(z),3)),pos=4,cex=0.8)
		if(!is.na(xline)){
			points(xline,haoql,cex=1.0,pch=21,bg="red",col="red",)
			text(xline+0.05,haoql,paste("AOQL:(x=",xline,",y=",round(haoql,2),")"),pos=4,cex=0.8)
		}
	}
	if(oc==TRUE & aoql==TRUE){
		plot(x,y,type="l",xlab=xlab,ylab=f1.ylab,main=main)
		if(!is.na(xline)){
			points(xline,hoc,cex=1.0,pch=21,bg="red",col="red",)
			text(xline+0.05,hoc,paste("OC:(x=",xline,",y=",round(hoc,2),")"),pos=4,cex=0.8)
		}
		par(new=TRUE)
		plot(x,z,type="l",axes=F,ann=FALSE)
		text(oc.data[,1][oc.data[3]==max(z)]+0.08,round(max(z),2)-0.005,paste("AOQL=",round(max(z),3)),pos=4,cex=0.8)
		axis(4)
		if(!is.na(xline)){
			points(xline,haoql,cex=1.0,pch=21,bg="red",col="red",)
			text(xline+0.05,haoql,paste("AOQL:(x=",xline,",y=",round(haoql,2),")"),pos=4,cex=0.8)
		}
	}
}

