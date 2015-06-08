rankhospital <- function(state, outcome, num = "best") {
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
        df_sort <- df[order(df$Rate, df$Hospitals), ]
        ranking <- data.frame(Rank=c(1:nrow(df_sort)))
        df_final <- data.frame(Hospitals = as.character(df_sort[ ,1]), 
                               Rate = as.numeric(as.character(df_sort[ ,2])),
                               Ranking = as.numeric(as.character(ranking[ ,1])))
        
        if (num == 'best') {
                num == 1
        } else if (num == "worst") {
                num = nrow(df_final)
        } else if (num > nrow(df_final)) {
                NA
        }
        
        name <- c(as.character(df_final[num, 1]))
        name
}