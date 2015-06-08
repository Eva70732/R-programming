complete <- function(directory, id = 1:332) {
        files <- list.files(directory)
        id_list <- c()
        nobs_list <- c()
        
        for (n in id) {
                path <- paste(directory, '/', files[n], sep='')
                mydata <- read.csv(path)
                good <- complete.cases(mydata)
                mydata_na <- mydata[good, ]
                nobs_list <- c(nobs_list, nrow(mydata_na))
                id_list <- c(id_list, n)
        }
        result <- data.frame(id=id_list, nobs=nobs_list)
        result
}
