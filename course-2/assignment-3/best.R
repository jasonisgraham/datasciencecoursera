source("common.R")

best <- function(state, outcome) {
    ## Read outcome data
    columnName <- getColumnNameFromAilment(outcome)
    outcomeData <- getOutcomeData(columnName)

    ## Check that state and outcome are valid
    validateState(state, outcomeData)
    validateAilment(outcome)

    ## Return hospital name in that state with lowest 30-day death rate
    matchingStates <- outcomeData$State == state

    ## convert column to numeric and find minimum
    minRate <- min(outcomeData[which(matchingStates),][[columnName]], na.rm=T)

    return (outcomeData[which(outcomeData[columnName] == minRate & matchingStates), ]$Hospital.Name)
}
