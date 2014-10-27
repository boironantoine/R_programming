best <- function(state, outcome) {
    #Check for invoalid inputs 
    if (!outcome %in% c("heart attack", "heart failure", "pneumonia")) {
        stop("invalid outcome")
    }
    if (!state %in% data$State) {
        stop("invalid state")
    }
  
    #Get index for our given outcome string.
    if(outcome == "heart attack"){
        index<-11
    }
    else if(outcome == "heart failure"){
        index<-17
    }
    else if(outcome == "pneumonia"){
        index<-23
    }

    
    #Read and coerce our dataset while suppressing warnings and removing NA's.
    data <- read.csv("outcome-of-care-measures.csv", colClasses="character")
    data[,index] <- suppressWarnings(as.numeric(data[,index]))
    data <- na.omit(data)
  
    slice <- subset(data, State==state)
    slice <- slice[order(slice[,index], na.last=TRUE),2]
    slice <- na.omit(slice)
  
  #Get hospital name with the lowest 30-day mortality rate.
  slice[1]
}


best("TX", "heart attack")
best("TX", "heart failure")
best("MD", "heart attack")
best("MD", "pneumonia")
best("BB", "heart attack")
best("NY", "hert attack")
