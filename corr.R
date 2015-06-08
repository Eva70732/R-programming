corr <- function(directory, threshold = 0) {
        files <- list.files(directory)
        nobs_list <- c()
        id <- 1:332
        
        for (n in id) {
                path <- paste(directory, '/', files[n], sep='')
                mydata <- read.csv(path)
                mydata_clean <- mydata[complete.cases(mydata), ]
                nobs_list <- c(nobs_list, nrow(mydata_clean))               
        } 
        result <- data.frame(id=id, nobs=nobs_list)
        result_clean <- subset(result, nobs>threshold)
        id_clean <- c(result_clean$id)
        
        final <- c()
        
        for (i in id_clean) {
                path2 <- paste(directory, '/', files[i], sep='')
                mydata2 <- read.csv(path2)
                clean <- mydata2[complete.cases(mydata2), ] 
                cor_data <- cor(clean$sulfate, clean$nitrate)
                final <- c(final, cor_data)
        }
        final
}
