rankhospital <- function(state, outcome, num = "best") {
  ## Read outcome data
  outcomedata<-read.csv("outcome-of-care-measures.csv", colClasses = "character", header=TRUE)
  possible_outcome<-c("heart attack", "heart failure", "pneumonia")
  outcome_col<-c(11,17,23)
  col_numero<-outcome_col[possible_outcome==outcome]
  outcomedata[,col_numero]<-as.numeric(outcomedata[,col_numero]) 
  ## Check that state and outcome are valid
  if (!(state %in% outcomedata[,7])) {stop("invalid state")}
  if (!(outcome %in% possible_outcome)){stop("invalid outcome")}
  ## Get the subset with choosen data cleared from NA
  hospital_data<-subset(outcomedata[c(2,7,col_numero)],State==state,na.rm=TRUE) 
  hospital_data<-hospital_data[complete.cases(hospital_data),]
  ## Sort this subset by hospital rank
  hospital_data_sorted<-hospital_data[order(hospital_data[,3],hospital_data[,1]),]
  ## Return hospital name in that state with the given rank
  ## 30-day death rate
  ind<-which(c("best",2:(nrow(hospital_data)-1),"worst") == as.character(num))
  if (length(ind) == 0) {NA} else {hospital_data_sorted[ind,1]}
  
}