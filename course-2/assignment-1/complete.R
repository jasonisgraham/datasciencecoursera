complete <- function(directory, id = 1:332) {
    nobs <- sapply(id, function(singleId) {
        fileName <- paste(formatC(singleId, width = 3, flag = "0"), ".csv", sep = "")
        filePath <- paste(directory, "/", fileName, sep = "")

        data <- read.csv(filePath)
        nrow(data[complete.cases(data),])
    })

    data.frame(id = id, nobs = nobs)
}
