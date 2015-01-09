best <- function(state, outcome) {
  ## Read outcome data
  outcomedata<-read.csv("outcome-of-care-measures.csv", colClasses = "character", header=TRUE)
  possible_outcome<-c("heart attack", "heart failure", "pneumonia")
  outcome_col<-c(11,17,23)
  col_numero<-outcome_col[possible_outcome==outcome]
  outcomedata[,col_numero]<-as.numeric(outcomedata[,col_numero]) 
  ## Check that state and outcome are valid

  
  if (!(state %in% outcomedata[,7])) {stop("invalid state")}
  if (!(outcome %in% possible_outcome)){stop("invalid outcome")}
  
        ## Return hospital name in that state with lowest 30-day death
        ## rate
        hospital_data<-subset(outcomedata[c(2,7,col_numero)],State==state,na.rm=TRUE) 
        
        num<-as.numeric(which(min(hospital_data[,3],na.rm=TRUE) == hospital_data[,3]))
        sort(hospital_data$Hospital.Name[num])[1]
  
}