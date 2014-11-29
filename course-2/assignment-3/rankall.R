## returns.  cols: Hospital.Name, State
##
##
source("rankhospital.R")
source("common.R")

library(foreach)

rankall <- function(outcome, num = "best") {
    validateAilment(outcome)

    columnName <- getColumnNameFromAilment(outcome)

    ## Read outcome data
    outcomeData <- getOutcomeData(columnName)

    states <- unique (outcomeData$State)

    hospitals <- foreach(state=states, .combine='c') %do% {
        rankhospital(state, outcome, num, outcomeData)
    }

    return (data.frame(hospital = hospitals, state = states))
}
