best <- function(state, outcome) {
## Read outcome data
outcomefile <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
st <- outcomefile[,"State"]
Hospital.Name = outcomefile[, "Hospital.Name"]
Heart.Attack = as.numeric(outcomefile[, 11])
Heart.Failure = as.numeric(outcomefile[, 17])
Pneumonia = as.numeric(outcomefile[, 23])
## Check that state and outcome are valid
if(length(grep(state , st,ignore.case = FALSE))<=0){stop("invalid state")}

 if(! (outcome == "heart attack" || outcome == "heart failure" || outcome == "pneumonia") ) {
    stop("invalid outcome")}
    
## Return hospital name in that state with lowest 30-day death rate
switch(outcome, `heart attack` = {
        col = 11
    }, `heart failure` = {
        col = 17
    }, pneumonia = {
        col = 23
    })
 df = outcomefile[outcomefile$State == state, c(2, col)]
 df[which.min(df[, 2]), 1]
 
## Wanted to use aggregate somehow but was getting more complex 
##aggregate(cbind(Hospital.Name,Heart.Attack,Heart.Failure,Pneumonia)  ~ State, outcomefile, function(x) min(x))

}