corr <- function(directory, threshold = 0) {
    ## 'directory' is a character vector of length 1 indicating
    ## the location of the CSV files

    ## 'threshold' is a numeric vector of length 1 indicating the
    ## number of completely observed observations (on all
    ## variables) required to compute the correlation between
    ## nitrate and sulfate; the default is 0

    ## Return a numeric vector of correlations

    corrByFile <- function(fileName) {
        data <- read.csv(fileName)
        numberReadings <- sum(complete.cases(data))
        if (numberReadings > threshold) {
            return (cor(data$nitrate, data$sulfate, use="complete.obs"))
        }
    }

    files <- list.files (path=directory, full.names=T)

    correlations <- sapply(files, corrByFile)
    correlations <- unlist(correlations[!sapply(correlations, is.null)])
    return (correlations)
}
