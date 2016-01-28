best<-function(state = "TX", outcome = "heart attack"){

o<-c();st<-state;out<-outcome;library(data.table);
nm<-c("Hospital","State","heart attack","heart failure","pneumonia")

# variable setup :
if(out==nm[3]){out="heart_attack"}
if(out==nm[4]){out="heart_failure"}
# Invalid outcome:
	if(outcome %in% nm == F) { stop("invalid outcome")}
		else{
H<-fread("~/R_Programming/Assignment3/outcome-of-care-measures.csv",select=c(2,7,11,17,23),na.strings=c("NA","Not Available"))
names(H)<-c("Hospital","State","heart_attack","heart_failure","pneumonia")
K<- H[, heart_failure:=as.numeric(heart_failure)];K<- H[, heart_attack:=as.numeric(heart_attack)];K<- H[, pneumonia:=as.numeric(pneumonia)]
# Invalid state:
	if(st %in% unique(K$State) == F) { stop("invalid state")}
		else{
setkeyv(K,out)
K<-K[State==state,mult="first"]

# final subset and writing of a control file :
K<-na.omit(subset(K,select=c("Hospital","State",paste(out))))
write.table(K,file="Kframe.txt")
o<-K[1,1,with=F]
		    }
		    }
return(o[[1]])
}


