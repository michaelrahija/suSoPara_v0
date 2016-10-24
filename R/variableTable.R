#This function takes a df, and 

variableTable <- function(df = df){
  
  # - Confirm that input is df
  if(!is.data.frame(df)) stop("Must be dataframe!")
  
  # - Remove without variable name
  df <- filter(df, !is.na(variable_name))
  df <- filter(df, variable_name != "")
  
  #- Remove rows w/o posix
  df <- filter(df, !is.na(posix))
  
  #-Compute item response statistics
  rstats <- df %>%
    filter(role == "Interviewer") %>%
    filter(!(nchar(variable_name) == 32)) %>%
    #filter(!is.na(posix) & !(action %in% c("CommentSet",
    #                                       "AnswerRemoved"))) %>%
    group_by(variable_name) %>%
    summarize(N = n(), 
              avgInSecs = mean(SecsSinceLastAction, na.rm = TRUE),
              medInSecs = median(SecsSinceLastAction, na.rm = TRUE),
              maxInSecs = max(SecsSinceLastAction, na.rm = TRUE),
              minInSecs = min(SecsSinceLastAction, na.rm = TRUE),
              sdInSecs = sd(SecsSinceLastAction, na.rm = TRUE))
  
  #- Count comments - Maybe makes sense to include the number of comments by enumerator!
  comm <- df %>%
          filter(action == "CommentSet") %>%
          group_by(variable_name) %>%
          summarise(comments = n())
  
  #- merge rstats and comm
  master <- merge(rstats, 
                  comm, 
                  by = "variable_name",
                  all = TRUE)
  
  master$comments[is.na(master$comments)] <- 0

  #- Count answers removed
  #length(filter(df, action == "AnswerRemoved"))
    
  
  #- AnswerRemoved status does not give a variable name, use the order
    #- to determine which variable that AnswrRemoved refers to 
  
  

 
 
 
data.frame(master)
  
}