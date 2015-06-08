best <- function(state, outcome) {
        measures <- read.csv('outcome-of-care-measures.csv')
        data <- measures[, c(2, 7, 11, 17, 23)]
        names(data) <- c('Hospital', 'State', 
                         'Heart_attack', 'Heart_failure', 
                         'Pneumonia')
        
        if (!is.element(state,data$State)) {
                stop("invalid state")
                
        }  
        
        if (!is.element(outcome, c("heart attack", 
                                   "heart failure", 
                                   "pneumonia"))) {
                stop("invalid outcome")
                
        }
        
        data_state <- data[which(data$State==state), ]
        col_outcome <- match(outcome, c('heart attack', 
                                        'heart failure',
                                        'pneumonia'))
        mydata <-data_state[ , c(1, col_outcome+2)]
        mydata_na <- mydata[which(mydata[2]!='Not Available'), ]
        df <- data.frame(Hospitals= as.character(mydata_na[ ,1]), 
                         Rate=as.numeric(as.character(mydata_na[ ,2])))
       
        min <- df[which(df$Rate == min(df$Rate)), ]
        name <- c(as.character(min[1,1]))
        name
}
        