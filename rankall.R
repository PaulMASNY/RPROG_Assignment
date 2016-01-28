
##########################################################################################
########## mult=last is not working on the line 28 , I don't know why !
##########################################################################################

rankall <- function(outcome = "heart attack", num = "best") {
# List of variables :
m<-c(); n<-c();o<-c();	K<-c();L<-c(); st<-c(); out<-outcome; nb<-num; library(data.table);
nm<-c("Hospital","State","heart attack","heart failure","pneumonia")
# variable setup :
if(out==nm[3]){out="heart_attack"}
if(out==nm[4]){out="heart_failure"}
# Invalid outcome procedure :
	if(outcome %in% nm == F) { stop("invalid outcome")}
		else{
# load procedure for a simplified data.frame :
H<-fread("~/R_Programming/Assignment3/outcome-of-care-measures.csv",select=c(2,7,11,17,23),na.strings=c("NA","Not Available"))
names(H)<-c("Hospital","State","heart_attack","heart_failure","pneumonia")
K<- H[, heart_failure:=as.numeric(heart_failure)];K<- H[, heart_attack:=as.numeric(heart_attack)];K<- H[, pneumonia:=as.numeric(pneumonia)]
st<-sort(unique(K$State))
# Trims the NA's in function of the outcome selected and sets the key on the outcome variable :
K<-na.omit(subset(K,select=c("Hospital","State",paste(out))));
setkeyv(K,out)
# Best selection :
	if (nb =="best") { for(i in st) {L<-K[State== i,mult="first"];m<-L[L[1,3,with=F]];m<-m[order(Hospital)]
			      m<-m[1,1:2,with=F]; n<-rbind(n,m) }}

		else if (nb =="worst") { for(i in st) {L<-K[State== i,mult="last"]; m<-L[L[1,3,with=F]];m<-m[order(Hospital)]
			      m<-m[1,1:2,with=F]; n<-rbind(n,m)}} 

# Numerically determined selection :
			else  { for(i in st) { L<-K[State==i,mult="first"];m<-L[L[as.numeric(paste(nb)),3,with=F]];m<-m[order(Hospital)]
			      m<-m[1,1:2,with=F]; n<-rbind(n,m)}}

}
o<-n
write.table(o,file="Kframe.txt")
return(o)
}

tail(rankall("pneumonia", "best"), 3)
tail(rankall("heart failure"), 10)
tail(rankall("pneumonia", "worst"), 3)
head(rankall("heart attack", 20), 10)
