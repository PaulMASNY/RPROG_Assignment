rankhospital<-function(state = "TX", outcome = "heart attack", num ="best"){
# List of variables :
n<-c();o<-c();	K<-c();	st<-state; out<-outcome; nb<-num; library(data.table);
nm<-c("Hospital","State","heart attack","heart failure","pneumonia")
# variable setup :
if(out==nm[3]){out="heart_attack"}
if(out==nm[4]){out="heart_failure"}
# Invalid outcome procedure :
	if(outcome %in% nm == F) { stop("invalid outcome")}
		else{
# load procedure for a simplified data.frame :
H<-na.omit(fread("~/R_Programming/Assignment3/outcome-of-care-measures.csv",select=c(2,7,11,17,23),na.strings=c("NA","Not Available")))
names(H)<-c("Hospital","State","heart_attack","heart_failure","pneumonia")
K<- H[, heart_failure:=as.numeric(heart_failure)];K<- H[, heart_attack:=as.numeric(heart_attack)];K<- H[, pneumonia:=as.numeric(pneumonia)]
#Invalid state procedure :
	if(st %in% unique(K$State) == F) { stop("invalid state")}
		else{
# Sets the key as the outcome variable :
setkeyv(K,out)
# Best selection :
	if(nb =="best") {K<-head(K[State==st,mult="first"],1); o<-K[1,1,with=F] }
# Worst selection :
		else if(nb =="worst"){K<-tail(K[State==st,mult="last"],1);o<-K[1,1,with=F]}
# Numerically determined selection :
			else {K<-K[State==st,mult="first"];m<-K[as.numeric(paste(nb)),paste(out),with=F]
# Multiple results checking and last hospital name output ordering procedure :
			      n<-K[m]
			      setkey(n,Hospital); setorder(n,-Hospital,State,heart_attack,heart_failure,pneumonia)
			      o<-n[1,1,with=F]      
}
}
}

return(o[[1]])
}

rankhospital("TX","heart failure","best")
rankhospital("MD", "heart attack", "worst")
rankhospital("TX", "heart failure", 4)
rankhospital("MN", "heart attack", 5000)
rankhospital("WY","pneumonia","worst")

