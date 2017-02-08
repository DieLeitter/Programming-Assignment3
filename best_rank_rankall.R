## The function reads the outcome-of-care-measures.csv file and returns a character vector
## with the name of the hospital that has the best (i.e. lowest) 30-day mortality for the specified outcome
## in that state. The outcomes can be one of "heart attack", "heart failure", or "pneumonia"
## 
## Here is some sample output from the function.
## best("TX", "heart attack")
## [1] "CYPRESS FAIRBANKS MEDICAL CENTER"
## best("TX", "heart failure")
## [1] "FORT DUNCAN MEDICAL CENTER"

best <- function(state, outcome){
        
        ## Download data
        
        fileURL <- "https://d396qusza40orc.cloudfront.net/rprog%2Fdata%2FProgAssignment3-data.zip"
        if(!file.exists("rprog_data_ProgAssignment3-data.zip")){
                download.file(fileURL, "rprog_data_ProgAssignment3-data.zip")
                print("File downloaded")
        }
        
        if(!dir.exists("Hospital")){
                unzip("rprog_data_ProgAssignment3-data.zip", exdir = "Hospital")
                print("File unziped and folder Hospital created")
        }
        
        files_full <- list.files("Hospital", full.names = TRUE)
        data_index <- grepl("outcome", files_full)
        
        
        
        ## Read outcome data
        df <- read.csv(files_full[data_index], colClasses="character")
        
        
        ## Check that state and outcome are valid
        state_rows <- which(df$State == state)
        if(length(state_rows) == 0){
                stop("Invalid state")
        }
        
        if(outcome == "heart attack" | outcome == "heart.attack"){
                outcome <- "heart.attack"
                outcome_col = 11
        }else if(outcome == "heart failure" | outcome == "heart.failure"){
                outcome <- "heart.failure"
                outcome_col = 17
        }else if(outcome == "pneumonia"){
                outcome_col = 23
        }else{
                stop("Invalid outcome") 
        } 
        
        ## Subset hospitl name, state, and outcome
        df_sub <- df[state_rows, c(2, 7, outcome_col)]
        df_sub[,3] <- suppressWarnings(as.numeric(df_sub[,3]))
        names(df_sub) <- c("Hospital.Name", "State", outcome)
        
        ## Order according to outcome, if there is a tie order alphabetically by hospital name.
        df_sub <- df_sub[order(df_sub[,3], df_sub[,2], na.last=TRUE),]
        best_hosp <- df_sub[1,1]
        best_hosp
} 



## The function reads the outcome-of-care-measures.csv file and returns a character vector with the name
## of the hospital that has the ranking specified by the num argument. For example, the call
## rankhospital("MD", "heart failure", 5)
## would return a character vector containing the name of the hospital with the 5th lowest 30-day death rate
## for heart failure. The num argument can take values \best", \worst", or an integer indicating the ranking
## (smaller numbers are better). If the number given by num is larger than the number of hospitals in that
## state, then the function should return NA.

rankhospital <- function(state, outcome, num = "best") {
        
        ## Download data
        
        fileURL <- "https://d396qusza40orc.cloudfront.net/rprog%2Fdata%2FProgAssignment3-data.zip"
        if(!file.exists("rprog_data_ProgAssignment3-data.zip")){
                download.file(fileURL, "rprog_data_ProgAssignment3-data.zip")
                print("File downloaded")
        }
        
        if(!dir.exists("Hospital")){
                unzip("rprog_data_ProgAssignment3-data.zip", exdir = "Hospital")
                print("File unziped and folder Hospital created")
        }
        
        files_full <- list.files("Hospital", full.names = TRUE)
        data_index <- grepl("outcome", files_full)
        
        
        ## Read outcome data
        df <- read.csv(files_full[3], colClasses="character")
        
        
        ## Check that state and outcome are valid
        state_rows <- which(df$State == state)
        if(length(state_rows) == 0){
                stop("Invalid state")      
        } 
        
        if(outcome == "heart attack" | outcome == "heart.attack" ){
                outcome <- "heart.attack"
                outcome_col = 11
        }else if(outcome == "heart failure" | outcome == "heart.failure"){
                outcome <- "heart.failure"
                outcome_col = 17
        }else if(outcome == "pneumonia"){
                outcome_col = 23
        }else{
                stop("Invalid outcome")
        }
        
        
        
        ## Return hospital name in that state with the given rank
        if(num == "best") {
                return(best(state, outcome))
        }
        
        
        # Subset hospitl name, state, and outcome
        df_sub <- df[state_rows, c(2, 7, outcome_col)]
        df_sub[,3] <- suppressWarnings(as.numeric(df_sub[,3]))
        names(df_sub) <- c("Hospital.Name", "State", outcome)
        
        # Order according to outcome, if there is a tie order alphabetically by hospital name.
        df_sub <- df_sub[order(df_sub[,3], df_sub[,1], na.last=TRUE),]
        df_sub <-df_sub[!is.na(df_sub[,3]),]
        
        ## 30-day death rate
        if(num =="worst"){
                rank_hosp <- df_sub[dim(df_sub)[1],1]
                return(rank_hosp)
        }
        if(num > dim(df_sub)[1]){
                return(NA) 
        }
        # df_sub[1:num,1]
        return(df_sub[num,1])
}


## Function takes two arguments: an outcome name (outcome) and a hospital ranking (num). 
## The function reads the outcome-of-care-measures.csv file and returns a 2-column data frame
## containing the hospital in each state that has the ranking specified in num. For example the function call
## rankall("heart attack", "best") would return a data frame containing the names of the hospitals that
## are the best in their respective states for 30-day heart attack death rates. The function should return a value
## for every state (some may be NA).

rankall <- function(outcome, num="best"){  
        
        ## Download data
        
        fileURL <- "https://d396qusza40orc.cloudfront.net/rprog%2Fdata%2FProgAssignment3-data.zip"
        if(!file.exists("rprog_data_ProgAssignment3-data.zip")){ 
                download.file(fileURL, "rprog_data_ProgAssignment3-data.zip")
                print("File downloaded")
        }
        
        if(!dir.exists("Hospital")){
                unzip("rprog_data_ProgAssignment3-data.zip", exdir = "Hospital")
                print("File unziped and folder Hospital created")
        }
        
        files_full <- list.files("Hospital", full.names = TRUE)
        data_index <- grepl("outcome", files_full)
        
        
        ## Read outcome data
        df <- read.csv(files_full[3], colClasses="character")
        
        
        ## Check that state and outcome are valid
        state_rows <- which(df$State == state)
        if(length(state_rows) == 0){
                stop("Invalid state")
        }
        
        if(outcome == "heart attack" | outcome == "heart.attack"){
                outcome <- "heart.attack"
                outcome_col = 11
        }else if(outcome == "heart failure" | outcome == "heart.failure"){
                outcome <- "heart.failure"
                outcome_col = 17
        }else if(outcome == "pneumonia"){
                outcome_col = 23
        }else{
                stop("Invalid outcome")
        }
        
        
        ## For each state, find the hospital of the given rank
        states_list <- sort(unique(df$State)) 
        #states_list <- as.data.frame(states_list)
        all <- data.frame()
        for(i in seq_along(states_list)){
                temp <- rankhospital(states_list[i], outcome, num) 
                temp <- as.data.frame(cbind(temp, states_list[i]))
                all <- rbind(all, temp) 
        }
        
        
        ## Return a data frame with the hospital names and the
        ## (abbreviated) state name
        names(all) <- c("Hospital", "State")
        return(all)
}  
