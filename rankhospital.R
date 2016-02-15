rankhospital <- function(state, outcome, num = "best") {
## Read outcome data
outcomefile <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
st <- outcomefile[,"State"]
## Check that state and outcome are valid
if(length(grep(state , st,ignore.case = FALSE))<=0){stop("invalid state")}

switch(outcome, `heart attack` = {
        col = 11
    }, `heart failure` = {
        col = 17
    }, pneumonia = {
        col = 23
    },stop("invalid outcome"))
outcomefile[, col] = as.numeric(outcomefile[, col])
    df = outcomefile[outcomefile[, 7] == state, c(2, col)]
    df = na.omit(df)
    noOfhospital = nrow(df)
    switch(num, best = {
        num = 1
    }, worst = {
        num = noOfhospital
    })
    if (num > noOfhospital) {
        return(NA)
    }
## Return hospital name in that state with the given rank
## 30-day death rate


ord = order(df[, 2], df[, 1])
    df[ord, ][num, 1]
}

## To Test 
## rankhospital("MD", "heart attack", "worst")