rankall <- function(outcome = "heart attack", num = "best") {
# List of variables :
n<-c();o<-c();	K<-c();L<-c(); st<-c(); out<-outcome; nb<-num; library(data.table);
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
st<-sort(unique(K$State))
# Sets the key as the outcome variable :
setkeyv(K,out)
# Best selection :
	if(nb =="best") for(i in st) {L<-head(K[State==i,mult="first"],1); o<-rbind(o,L[1,1:5,with=F])}
	if(nb =="worst") for(i in st) {L<-head(K[State==i,mult="last"],1); o<-rbind(o,L[1,1:5,with=F])} 
# Numerically determined selection :
			else { for(i in st) { L<-K[State==i,mult="first"];m<-L[as.numeric(paste(nb)),paste(out),with=F]
# Multiple results checking and last hospital name output ordering procedure :
			      n<-K[m]
			      setkey(n,Hospital); setorder(n,-Hospital,State,heart_attack,heart_failure,pneumonia)
			      o<-rbind(o,n[1,1:5,with=F]) 
}
}
}
setkey(o,State)
o<-o[sort(State)]

write.table(o,file="Kframe.txt")
return(o)
}

tail(rankall("pneumonia", "best"), 3)
tail(rankall("heart failure"), 10)

