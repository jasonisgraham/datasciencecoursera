
getOutcomeData <- function(columnName) {
    outcomeData <- read.csv ("outcome-of-care-measures.csv", colClasses = "character")

    ## convert column to numeric
    outcomeData[[columnName]] <- as.numeric(outcomeData[[columnName]])

    return (outcomeData)
}

getColumnNameFromAilment <- function(ailment) {
    if (ailment == "heart attack") {
        return ("Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack")
    } else if (ailment == "heart failure") {
        return ("Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure")
    } else {
        return ("Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia")
    }
}

validateState <- function(state, outcomeData) {
    if (is.na (match (state, unique (outcomeData$State)))) {
        print(state)
        stop(paste("Error in best(", state, ", outcomeData) : invalid state"))
    }
}

validateAilment <- function(ailment) {
    if (is.na (match (ailment, c ("heart attack", "heart failure", "pneumonia")))) {
        print(ailment)
        stop(paste("Error in best(state, ", ailment, ") : invalid outcome"))
    }
}
