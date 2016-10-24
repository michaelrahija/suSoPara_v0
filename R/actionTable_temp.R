# Takes a directory containing audit files, and returns a dataframe named
# with SecsSinceLastAction. Id is the last 6 digits of the Interview ID.


actionTable <- function(dir = NA){
  
  #stop("Issue with computing time when there are two comments set in consecutive rows!!!")
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
                 rosterRowRef,
                 date,
                 time,
                 response
                 )
  
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
    
    #if first iteration, just put 0 b/c first questions can't compute time
    if(i == 1){
      time.temp <- NA
    } else {
      time.temp <- difftime(temp$posix[i],temp$posix[i-1])
    }
    times <- append(times, time.temp)
  }
  
  temp$SecsSinceLastAction <- times
  
  temp <- arrange(temp, id, actionOrder)
  
  #########################################################
  #-SUM comment set in seconds to variable response time ##
  #########################################################
  
  # #find row ids with "Comment Set"
  # comment.df <- filter(temp, action == "CommentSet")
  # 
  # rowids <- unique(comment.df$actionOrder)
  # rowids <- which(temp$actionOrder %in% rowids)
  # 
  # #loop through rows going from row id to the beginning looking for variable
  # for(i in 1:length(rowids)){
  #   
  #   var <- temp$variable_name[rowids[1]]
  #   
  #   seq <- 1:rowids[1]
  #   seq <- rev(seq)
  #   
  #   for(i in seq){
  #     test <- 
  #     
  #     
  #   }
  # }
  # 
  # #1 Identify the actionOrder #s of Comments set
  #Start at actionOrder and go to row 1, the first row that's encountered with AnswerSet
  #and matches the variable name of the comment set. <- This is the correct comment
  # Then add time of the comment set to the variable. Proceed to next comment set.
  
  #THIS APPROACH LOOKS FOR CONSECTUTIVE ROWS WITH COMMENTSET AND ANSWER SET
  #THE ISSUE IS THAT SOMETIMES TWO CONSECUTIVE ROWS HAVE COMMENT SET
  # if("CommentSet" %in% unique(temp$action)){
  #   #create vector of ids where there is a comment set right after answer
  #   commentrows <- c()
  # 
  #   for(i in 1:nrow(temp)){
  # 
  #       if(temp$action[i] == "AnswerSet" & temp$action[i+1] == "CommentSet"){
  #           commentrows <- append(commentrows, temp$actionOrder[i])
  #       } else {
  #           commentrows <- append(commentrows, FALSE)
  #       }
  #     }
  # 
  #   commentrows <- commentrows[commentrows != 0]
  # 
  #   #sum secs since last action for id'ed rows, and put NA for comments
  #   for(i in 1:commentrows){
  # 
  #     temp$SecsSinceLastAction[commentrows[i]] <- difftime(temp$posix[commentrows[i]+2],
  #                                                          temp$posix[commentrows[i]])
  # 
  #     temp$SecsSinceLastAction[temp$action == "CommentSet"] <- NA
  #   }
  # }
##NEED ALTERNATIVE APPROACH, PERHAPS ID ROWS W/ COMMENTSET AND RECORD VARIABLE
##SEARCH ROWS AND FIND THE CLOSEST ROW W/ CORRESPONDING VARIABLE NAME.  
temp

}