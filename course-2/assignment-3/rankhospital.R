## The num argument can take values “best”, “worst”, or an integer indicating the ranking
## (smaller numbers are better). If the number given by num is larger than the number of hospitals in that
## state, then the function should return NA
##
##
##
source("common.R")
rankhospital <- function(state, outcome, num = "best", outcomeData=NA) {
    ## Read outcome data
    columnName <- getColumnNameFromAilment(outcome)
    if (is.na(outcomeData)) {
        outcomeData <- getOutcomeData(columnName)
    }

    ## Check that state and outcome are valid
    validateState(state, outcomeData)
    validateAilment(outcome)

    ## filter by state, remove NA mortalityRates
    outcomeData <- outcomeData[which(!is.na(outcomeData[columnName])), ]
    outcomeData <- outcomeData[which(outcomeData$State == state), ]

    ## order by mortalityRate then hospitalName
    sorted <- outcomeData[with(outcomeData, order(outcomeData[columnName], outcomeData['Hospital.Name'])), ]

    ## Add ranking column to data frame
    summaryData <- data.frame(sorted, rank = 1:nrow(sorted))

    ## get rank number
    rankNumber <- (function() {
        if (num == "best") {
            return (1)
        } else if (num == "worst") {
            return (nrow(summaryData))
        } else {
            return (num)
        }
    })()

    ## Return hospital name in that state with the given rank
    if (rankNumber > nrow(summaryData)) {
        return (NA)
    } else {
        return (summaryData[which(summaryData$rank == rankNumber), ]$Hospital.Name)
    }
}
