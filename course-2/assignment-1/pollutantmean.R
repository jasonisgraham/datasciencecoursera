pollutantmean <- function(directory, pollutant, id = 1:332) {
    files <- list.files (path=directory, full.names=T)

    results <- lapply(files, function(file) {
        data <- read.csv(file)
        data [which (!is.na (data [[pollutant]]) &
                         data$ID <= max (id) &
                             data$ID >= min (id)), ] [[pollutant]]
    })

    round(mean(as.numeric(unlist(results[id]))), digits = 3)
}
