##########################################################################################
#This unique function will give replies on otherwise three different functions questions
#from the programming assignment N°1
# 
##########################################################################################

fichier<-function(location="/home/paul/R_Programming/specdata/",pollutant="sulfate",file=10,threshold){
options(warn = -1)
corel<-c();DF1<-c();DF2<-c();DF3<-c();DF4<-c();DF5<-c();DF6<-c();name<-c();
if(missing(threshold))	{
		a<-file
		for(i in a){
		if(i<10){
		name<-c(paste(location,"00",as.character(i),".csv",sep=""))
		}else if(10<=i & i<100){
		name<-c(paste(location,"0",as.character(i),".csv",sep=""))
		}else{
		name<-c(paste(location,as.character(i),".csv",sep=""))
		}		
		DF1<-na.omit(read.csv(name))
		DF6<-append(DF6,DF1[,paste(pollutant)])
		DF3<-read.csv(name)
		DF4<-append(DF4,sum(complete.cases(DF3[,paste(pollutant)])))
		DF5<-cbind(a,DF4)
	colnames(DF5)<-c("file","complete_cases")
		}
		print(mean(DF6))
		return(DF5)
} else 		{
		a<-file
		b<-threshold  
		print(a)
		print(b)   	
				for(i in a) {
				if(1<= i & i<10){
				name<-c(paste(location,"00",as.character(i),".csv",sep=""))
				}else if (10<= i & i<100){
				name<-c(paste(location,"0",as.character(i),".csv",sep=""))
				} else {
				name<-c(paste(location,as.character(i),".csv",sep=""))
				}
					DF1<-read.csv(name)
				        
					DF2<-append(DF2,sum(complete.cases(DF1)))
					DF3<-cbind(a,DF2)
			      colnames(DF3)<-c("file","complete_cases")
					DF4<-DF3[,2]>b			
					DF5<-subset(DF3,DF4)
					
				}
					print(DF5)
					c<-as.numeric(DF5[,1])
					print(c)
					for(i in c){
						if (i<10){
						name<-c(paste(location,"00",as.character(i),".csv",sep=""))
						}else if (10<= i & i<100){
						name<-c(paste(location,"0",as.character(i),".csv",sep=""))
						} else {
						name<-c(paste(location,as.character(i),".csv",sep=""))
						}
							DF6<-na.omit(read.csv(name))
							corel<-rbind(corel,cor(DF6[,"sulfate"],DF6[,"nitrate"]))	

							}
							return(corel)
							}
}




