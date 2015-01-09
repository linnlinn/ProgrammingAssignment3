rankall <- function(outcome, num = "best") {
  ## Read outcome data
  outcomedata<-read.csv("outcome-of-care-measures.csv", colClasses = "character", header=TRUE)
  possible_outcome<-c("heart attack", "heart failure", "pneumonia")
  outcome_col<-c(11,17,23)
  col_numero<-outcome_col[possible_outcome==outcome]
  outcomedata[,col_numero]<-as.numeric(outcomedata[,col_numero]) 
  
  ## Check that the outcome is valid
  if (!(outcome %in% possible_outcome)){stop("invalid outcome")}
  
  ## Remove NA
  outcomedata<-outcomedata[complete.cases(outcomedata[,col_numero]),]
  
  ## For each state, find the hospital of the given rank
  ## Return a data frame with the hospital names and the
  ## (abbreviated) state name
  res<-lapply(split(outcomedata[c(2,7,col_numero)], outcomedata$State), 
              function(hosp_data) {
              ## Sort this subset by hospital rank
              hospital_data_sorted<-hosp_data[order(hosp_data[,3],hosp_data[,1]),]
              ## Return hospital name in that state with the given rank
              ## 30-day death rate
              ind<-which(c("best",2:(nrow(hosp_data)-1),"worst") == as.character(num))
              if (length(ind) == 0) {NA} else {hospital_data_sorted[ind,1]}
              })
  
  ## Change the presentation from list to data frame
  data.frame(
    hospital = unlist(res),
    state = rep(names(res), lapply(res, length)))
}

