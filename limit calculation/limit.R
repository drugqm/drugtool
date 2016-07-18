LC=function(limit_data,way=1,AL=0.95,AC=0.997,print=FALSE){
  limit_mean=mean(limit_data)
	limit_variance=var(limit_data)
	t<-shapiro.test(limit_data)$p.value
	if(t>0.05|(limit_mean>limit_variance)){
    p_alert=limit_mean+2*sqrt(limit_variance)
	  p_action=limit_mean+3*sqrt(limit_variance)
	  Pro.alert=pnorm(p_alert,limit_mean,sqrt(limit_variance))
	  Pro.action=pnorm(p_action,limit_mean,sqrt(limit_variance))
	  print(paste("Alert limit is:",p_alert))
	  print(paste("cumulative probability is:",Pro.alert))
	  print(paste("Alert limit is:",p_action))
	  print(paste("cumulative probability is:",Pro.action))
	  print("Result output from normal distribution")
	}
	else{
		if(way==1){
			p_alert=qgamma(AL,shape=limit_mean^2/limit_variance,scale=limit_variance/limit_mean)
			p_action=qgamma(AC,shape=limit_mean^2/limit_variance,scale=limit_variance/limit_mean)
			Pro.alert=pgamma(p_alert,shape=limit_mean^2/limit_variance,scale=limit_variance/limit_mean);
			Pro.action=pgamma(p_action,shape=limit_mean^2/limit_variance,scale=limit_variance/limit_mean);
			print(paste("Alert Limit is",p_alert));
			print(paste("Cumulative probability is",Pro.alert))
			print(paste("Action Limit is",p_action));
			print(paste("Cumulative probability is",Pro.action))
			print("Result output from gamma distribution")
		}
		else{
			p_alert<-0;
			p_action<-0;
			while(pnbinom(p_alert,size=limit_mean^2/(limit_variance-limit_mean),prob=limit_mean/limit_variance)<AL){
				p_alert<-p_alert+1;
			}
			while(pnbinom(p_action,size=limit_mean^2/(limit_variance-limit_mean),prob=limit_mean/limit_variance)<AC){
				p_action<-p_action+1;
			}
			Pro.alert=pnbinom(p_alert,size=limit_mean^2/(limit_variance-limit_mean),prob=limit_mean/limit_variance);
			Pro.action=pnbinom(p_action,size=limit_mean^2/(limit_variance-limit_mean),prob=limit_mean/limit_variance);
			print(paste("Alert Limit is",p_alert));
			print(paste("Cumulative probability is",Pro.alert))
			print(paste("Action Limit is",p_action));
			print(paste("Cumulative probability is",Pro.action))
			print("Result output from negative binomial distribution")
		}
	}
	if(print){
		limit_table=data.frame(limit_mean,limit_variance,AlertLimit=p_alert,ActionLimit=p_action,Pro.alert,Pro.action)
		write.table(limit_table,"Limit calculation.csv",sep=",",row.names=FALSE)
		print("the table is saved at work directory")
	}
}