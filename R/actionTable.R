# Takes a directory containing audit files, and returns a dataframe named
# with SecsSinceLastAction. Id is the last 6 digits of the Interview ID.


actionTable <- function(dir = NA){
  
  #if(dir == NA) stop("Must add a directory!")


   temp <- read.delim(dir,
                      header = FALSE,
                      sep = "\t",
                      stringsAsFactors = FALSE)
   
   #- imported file in order of action, not all action contain time
   #so later sorting doesn't work.
   
   temp$actionOrder <- 1:nrow(temp)
  
  #-Test to make sure correct file
  
  #correct columns
  if(length(colnames(temp)) != 9) stop("Invalid number of columns")
  
  
  #- add column to id interview
  interviewid <- gsub(".+/paradata/","",dir)
  interviewid <- gsub(".tab","",interviewid)
  
  
  source("R/extractRight.R")
  interviewid <- extractRight(interviewid, n = 6)
  temp$id <- rep(interviewid, times = nrow(temp))
  
  #- clean column names
  colnames(temp) <- c("action",
                         "user",
                         "role",
                         "date",
                         "time",
                         "variable_name",
                         "response",
                         "rosterRowRef",
                         "actionOrder",
                         "id")
  
  #-sort in more readable way
  temp <- select(temp, 
                 id,
                 actionOrder,
                 action,
                 role,
                 user,
                 variable_name,
                 response,
                 date,
                 time,
                 rosterRowRef)
  
  #- add column for posix
  temp$posix <- paste0(temp$date, " ", temp$time)
  
  temp$posix <- as.POSIXct(temp$posix,
                          format = "%m/%d/%Y %H:%M:%S",
                          tz = "Africa/Kampala")
  
  #- sort rows, by time, and add order column
  temp <- arrange(temp, posix)
  
  
  
  #-compute item response times
  times <- c()
  
  for(i in 1:nrow(temp)){
    
    #if first iteration, just put 0 b/c first questions
    if(i == 1){
      time.temp <- NA
    } else {
      time.temp <- difftime(temp$posix[i],temp$posix[i-1])
    }
    times <- append(times, time.temp)
  }
  
  temp$SecsSinceLastAction <- times
  
  temp <- arrange(temp, id, actionOrder)
  
temp

}