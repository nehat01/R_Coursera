
pollutantmean <- function(directory, pollutant = "sulfate", id = 1:332) {

    if(grep("specdata", dir) == 1) {
        directory <- ("./specdata/")
    }
    # initialize a vector to hold the pollutant data
    mean_vector <- c()
    # find all files in the specdata folder
    files <- as.character( list.files(directory) )
    paths <- paste(directory, files, sep="")
    for(i in id) {
        current_file <- read.csv(paths[i], header=T, sep=",")
        head(current_file)
        pollutant
       na_rem <- current_file[!is.na(current_file[, pollutant]), pollutant]
        mean_vector <- c(mean_vector, na_rem)

    }
    result <- mean(mean_vector)
    return(result) 
}

