pollutantmean <- function(directory, pollutant, id = 1:332) {
     files <- list.files(directory)
     data <- c()
     
     for (n in id) {
             path <- paste(directory, '/', files[n], sep='')
             mydata <- read.csv(path)
             mydata_cropped <- mydata[ , pollutant] 
             new_data <- mydata_cropped[!is.na(mydata_cropped)]
             data <- c(data, new_data)
     }
    mean(data)
}
