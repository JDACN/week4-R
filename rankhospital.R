## Finding the hospital in a state by input

rankhospital <- function(state, outcome, num = "best") {
      
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
            
     
            
            
            fd[, eval("outcome")] <- as.numeric(fd[, eval("outcome")])    # Convert the outcome col to numeric
            fd <- fd[order(fd[, eval("outcome")], fd[, "hospital"]), ]    # Sort tabel by outcome and then hospital
            
            if (num == "best") {
                  output <- fd[, "hospital"][1]                           # The first 
            }else if (num == "worst") {
                  output <- fd[, "hospital"][nrow(fd)]                    # last (= number of rows)
            } else if (is.numeric(num)) {
            output <- fd[, "hospital"][num]                               # direct number
            
            }
            else {
                  stop('invalid rank')                                    # invalid rank
            }
            
           
      }
      
      ## Return hospital name in that state with lowest 30-day death
      ## rate
      output 
}
