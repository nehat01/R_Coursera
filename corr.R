corr <- function(directory,threshold=0)
{
if(grep("specdata", directory) == 1) {
        directory <- ("./specdata/")
    }

	 compl <- complete("specdata", 1:332)
	    nobs <- compl$nobs
	  ids <- compl$id[nobs > threshold]
	  id_len <- length(ids)
	  corr_vector <- rep(0, id_len)
 
   
  
    
    
    files <- as.character( list.files(directory) )
    paths <- paste(directory, files, sep="/")
    j <- 1
    for(i in ids) {
        current_file <- read.csv(paths[i], header=T, sep=",")
        corr_vector[j] <- cor(current_file$sulfate, current_file$nitrate, use="complete.obs")
        j <- j + 1
    }
    result <- corr_vector
    return(result)   

}


