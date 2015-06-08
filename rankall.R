rankall <- function(outcome, num = "best") {
        measures <- read.csv('outcome-of-care-measures.csv')
        data <- measures[, c(2, 7, 11, 17, 23)]
        names(data) <- c('Hospital', 'State', 
                         'Heart_attack', 'Heart_failure', 
                         'Pneumonia')
        
        if (!is.element(outcome, c("heart attack", 
                                   "heart failure", 
                                   "pneumonia"))) {
                stop("invalid outcome")      
        }     
        col_outcome <- match(outcome, c('heart attack', 
                                        'heart failure',
                                        'pneumonia'))
        data_outcome <-data[ , c(1, 2, col_outcome+2)]
        data_clean <- data_outcome[which(data_outcome[3]!='Not Available'), ]
        clean <- data.frame(Hospitals = data_clean[ ,1], 
                            State = data_clean[ ,2], 
                            Rate=as.numeric(as.character(data_clean[ ,3]))) 
        split <- split(clean, clean$State, drop = FALSE)
        
        mydata <- lapply(split, function(x, num) {
                x = x[order(x$Rate, x$Hospitals), ]
                
                if (num == "best") {
                        return (x$Hospitals[1])
                } else if (num == "worst") {
                        return (x$Hospitals[nrow(x)])
                } else {
                        return (x$Hospitals[num])
                }
        }, num)
        return (data.frame(hospital=unlist(mydata), state=names(mydata)))
}