b.trend=function(trend_result=NA,trend_date=NA,trend_room=NA,trend_point=NA,trend_alert=NA,trend_action=NA,point.choose=NA,limit=FALSE,add=F,start.date=(Sys.Date()-360),end.date=Sys.Date(),days=180,t.title=NA,side.legend="right"){
#############Upate information#############
#' Version 3.2.2, add point type(pch parameter)
#' Version 3.2.1, remove the presentation of data.
#' Version 3.2.0, modify code to management the plot device��creat a device just before plot.
#' Reorganized the code struction. Simplify the Code. Optimized the logitical.
#############format layout##############
   opar=par(no.readonly=T)
   invisible(dev.off())
   if(add){
      png()
      if(side.legend=="right")  par(fig=c(0,0.8,0,1),cex.axis=0.8)
      else if(side.legend=="below") par(fig=c(0,1,0.1,1),cex.axis=0.8)
      else print("Option right or below can be choose")  
   }
   else{
      pdf()
      par(mar=c(5,5,5,4),mfrow=c(2,1),cex.axis=0.8)
   }
###############Data Process###############
#' Data Process
#' Data formation & Consturction
  trend_date<-as.Date(trend_date)
  trend_room<-as.character(trend_room)
  trend_point<-as.character(trend_point)
  trend_result<-as.numeric(as.character(trend_result))
  trend_data<-data.frame(trend_date,trend_room,trend_point,trend_result,stringsAsFactors=F)
  if(!all(is.na(trend_alert)))  trend_data$trend_alert=as.numeric(trend_alert)
  if(!all(is.na(trend_action))) trend_data$trend_action=as.numeric(trend_action)
#' Data Type Testing
 if(is.factor(trend_data$trend_result)) {stop("Data:result data type should not be factor")}
  if(is.factor(trend_data$trend_alert)) {stop("Data:alert data type should not be factor")}
  if(is.factor(trend_data$trend_action)) {stop("Data:action data type should not be factor")}
  if(limit & all(is.na(trend_alert)) & all(is.na(trend_action))) { stop("Limit required but no limit data")}
#' Data Filter 
  if(!is.na(start.date) & !is.na(end.date)){
    trend_data<-trend_data[trend_data$trend_date>=as.Date(start.date) & trend_data$trend_date<=as.Date(end.date),]
  }
  if(is.na(start.date) & !is.na(end.date)){
    trend_data<-trend_data[trend_data$trend_date<=as.Date(end.date),]
  }
  if(!is.na(start.date) & is.na(end.date)){
    trend_data<-trend_data[trend_data$trend_date>=as.Date(start.date),]
  }
  if(!is.na(days)&is.na(start.date) & is.na(end.date)){
    trend_data<-trend_data[(Sys.Date()-trend_data$trend_date)<=days,]
  }
  if(!is.na(point.choose)) trend_data<-trend_data[trend_data$trend_point==point.choose,]
  if(is.na(trend_room[1])) trend_data<-trend_data[-which(names(trend_data)=="trend_room")] 
#' List Generation
  if(is.na(trend_room[1])){
  logicalcondition<-duplicated(trend_data$trend_point)
         plotlist<-trend_data$trend_point[!logicalcondition]
         listlength<-length(plotlist)
  }
  else{
  logicalcondition<-duplicated(trend_data$trend_room)
         plotlist<-trend_data$trend_room[!logicalcondition]
         listlength<-length(plotlist)
  }
#' Plot parameter
    y1<-max(trend_data$trend_result,na.rm=T)
    if(limit & !is.na(trend_alert[1])) y2=max(trend_data$trend_alert,na.rm=T)   else y2=0
    if(limit & !is.na(trend_action[1])) y3=max(trend_data$trend_action,na.rm=T)  else y3=0
    ymax=max(y1,y2,y3)
    x.interval<-seq(range(trend_data$trend_date)[1],range(trend_data$trend_date)[2],by="month")
################Overlay Plot##################
  if(add){
    y1<-max(trend_data$trend_result,na.rm=T)
    if(limit & !is.na(trend_alert[1])) y2=max(trend_data$trend_alert,na.rm=T)   else y2=0
    if(limit & !is.na(trend_action[1])) y3=max(trend_data$trend_action,na.rm=T)  else y3=0
    ymax=max(y1,y2,y3)
    pchlist<-rep(0:25,times=2)
#' Plot acoording to sample point
    if(is.na(trend_room[1])){
       plot(trend_data$trend_date[trend_data$trend_point==plotlist[1]],trend_data$trend_result[trend_data$trend_point==plotlist[1]],xaxt="n",type="b",xlim=c(range(trend_data$trend_date)[1],range(trend_data$trend_date)[2]),ylim=c(0,ymax*1.2),xlab="",ylab="",cex=0.8,col=1,pch=pchlist[1])
       for(i in 1:(listlength-1)){
           lines(trend_data$trend_date[trend_data$trend_point==plotlist[i+1]],trend_data$trend_result[trend_data$trend_point==plotlist[i+1]],xlab="",ylab="",type="b",col=i+1,pch=pchlist[i+1])      
    }
    axis.Date(1,trend_data$trend_date,format="%Y-%m-%d",las=2,at=x.interval,cex=0.8)
    }
#' Plot acoording to room
    else{
        plot(trend_data$trend_date[trend_data$trend_room==plotlist[1]],trend_data$trend_result[trend_data$trend_room==plotlist[1]],xaxt="n",type="b",xlim=c(range(trend_data$trend_date)[1],range(trend_data$trend_date)[2]),ylim=c(0,ymax*1.2),xlab="",ylab="",cex=0.8,col=1,pch=pchlist[1])
        for(i in 1:(listlength-1)){
            lines(trend_data$trend_date[trend_data$trend_room==plotlist[i+1]],trend_data$trend_result[trend_data$trend_room==plotlist[i+1]],xlab="",ylab="",type="b",col=i+1,pch=pchlist[i+1])      
    }
    axis.Date(1,trend_data$trend_date,format="%Y-%m-%d",las=2,at=x.interval,cex=0.8)
    }
    title(t.title)
#' Alert
    if(!is.na(trend_alert[1]) & limit){
       spm<-trend_data$trend_date[!duplicated(trend_data$trend_alert)]
       spm[length(spm)+1]<-trend_data$trend_date[length(trend_data$trend_date)]
       for(m in 1:(length(spm)-1)){
           segments(as.numeric(spm[m]),trend_data$trend_alert[trend_data$trend_date==spm[m]],as.numeric(spm[m+1]),trend_data$trend_alert[trend_data$trend_date==spm[m]],col="yellow")
       }
     }
#' Action
     if(!is.na(trend_action[1]) & limit){
       spj<-trend_data$trend_date[!duplicated(trend_data$trend_action)]
       spj[length(spj)+1]<-trend_data$trend_date[length(trend_data$trend_date)]
       for(j in 1:(length(spj)-1)){
          segments(as.numeric(spj[j]),trend_data$trend_action[trend_data$trend_date==spj[j]],as.numeric(spj[j+1]),trend_data$trend_action[trend_data$trend_date==spj[j]],col="red")
       }
     }
#' Legend
     if(side.legend=="right")  par(fig=c(0.7,1,0,1))
     else {
       if(side.legend=="below") {
         par(fig=c(0,1,0,0.1))
       }
         else {print("only right or below can be choosed")
       }
     }
     if(side.legend=="right")legend("topleft",plotlist,cex=0.75,pt.cex=0.75,lty=1,pch=pchlist[1:listlength],col=1:listlength,horiz=F,xpd=T,bty="o",x.intersp=c(0.2),ncol=1,seg.len=0.8)
     if(side.legend=="below")legend("center",plotlist,cex=0.75,pt.cex=0.75,lty=1,pch=pchlist[1:listlength],col=1:listlength,horiz=T,xpd=T,bty="n",x.intersp=c(0.2),seg.len=0.8)
  dev.off(which=dev.list()[names(dev.list())!="windows"])
  }
#############individual plot###############
  else{
    for(i in 1:listlength){
#' Data estibilishment
      plotseries<-plotlist[i]
      if(is.na(trend_room[1])){
        plotdata<-trend_data[trend_data$trend_point==plotseries,]
      }
      else{
        plotdata<-trend_data[trend_data$trend_room==plotseries,]
      }
#' drawing the plot
      plot(plotdata$trend_date,plotdata$trend_result,xaxt="n",type="b",ylim=c(0,ymax*1.2),xlab="",ylab="",cex=0.8)
      axis.Date(1,plotdata$trend_date,format="%Y-%m-%d",las=2,at=x.interval,cex=0.8)
#'mark title
      plottitle=as.character(plotseries)
      title(paste(t.title,plottitle))
#' Alert
      if(!is.na(trend_alert[1]) & limit){
         #alert limit sepatation and drawing
         spm<-plotdata$trend_date[!duplicated(plotdata$trend_alert)]
         spm[length(spm)+1]<-plotdata$trend_date[length(plotdata$trend_date)]
         for(m in 1:(length(spm)-1)){
           segments(as.numeric(spm[m]),plotdata$trend_alert[plotdata$trend_date==spm[m]],as.numeric(spm[m+1]),plotdata$trend_alert[plotdata$trend_date==spm[m]],col="yellow")
                       }
      }
#' Action
      if(!is.na(trend_action[1]) & limit){
         #action limit sepatation and drawing
         spj<-plotdata$trend_date[!duplicated(plotdata$trend_action)]
         spj[length(spj)+1]<-plotdata$trend_date[length(plotdata$trend_date)]
         for(j in 1:(length(spj)-1)){
             segments(as.numeric(spj[j]),plotdata$trend_action[plotdata$trend_date==spj[j]],as.numeric(spj[j+1]),plotdata$trend_action[plotdata$trend_date==spj[j]],col="red")
         }
      }
    }
  dev.off()
  }
####Close device, return runing report####
  par=opar
  print("Runing Finished, The plot PDF file is saved in work directory")
}