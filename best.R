best<-function(state = "TX", outcome = "heart attack"){
options(warn = -1)
o<-c();st<-state;out<-outcome;library(data.table);
nm<-c("Hospital","State","heart attack","heart failure","pneumonia")
	if(out %in% nm == F) { stop("invalid outcome")}
		else{
H<-na.omit(fread("~/R_Programming/Assignment3/outcome-of-care-measures.csv",select=c(2,7,11,17,23),na.strings=c("NA","Not Available")))
names(H)<-c("Hospital","State","heart_attack","heart_failure","pneumonia")
K<- H[, heart_failure:=as.numeric(heart_failure)];K<- H[, heart_attack:=as.numeric(heart_attack)];K<- H[, pneumonia:=as.numeric(pneumonia)]
names(H)<-nm
	if(st %in% unique(K$State) == F) { stop("invalid state")}
		else{
setkeyv(K,outcome)
K<-head(K[State==state,mult="first"],1)
o<-unlist(K[1,1,with=F])
		    }
		    }
return(o[[1]])
}


