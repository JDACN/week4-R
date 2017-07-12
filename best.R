## Finding the best hospital in a state

best <- function(state, outcome) {
      
      ## Read outcome data
      data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
 
      
      ## Check that state and outcome are valids
      
      if(!state %in% data[, 7]){
            stop('invalid state')
      } else if(!outcome %in% c("heart attack", "heart failure", "pneumonia")){
            stop('invalid outcome')
      } else {
            
            col <- if (outcome == "heart attack") {
                  11  # heart attack
            } else if (outcome == "heart failure") {
                  17  # heart failure
            } else {
                 23   # pneumonia
            }
            
            
            ## Retrive only columns req. for processing
            
            fd <- data [,c(2,7,col)] # all rows, col : hospital, state, selected matrix
            
            colnames(fd) <- c("hospital", "state", "outcome")
            
            fd <- fd[fd$state %in% state & fd$outcome != "Not Available",] # select only rows for specific state
            
     
            oi <- as.numeric(fd[, eval(3)])                     # get vector with outcome
            min_val <- min(oi, na.rm = TRUE)                    # find the minimum 
            result  <- fd[, "hospital"][which(oi == min_val)]   # get the hospitals with min value
            output  <- result[order(result)]                    # sort by hospital name
            
           
      }
      
      ## Return hospital name in that state with lowest 30-day death
      ## rate
      output 
}
