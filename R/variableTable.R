#This function takes a df, and 

variableTable <- function(df = df){
  
  if(!is.data.frame(df)) stop("Must be dataframe!")
  
  
  # - Remove without variable name
  df <- filter(df, !is.na(variable_name))
  df <- filter(df, variable_name != "")
  
  #- Remove rows w/o posix
  df <- filter(df, !is.na(posix))
  
  #- Count comments - Maybe makes sense to include the number of comments by enumerator!
  x <- df %>%
          filter(action == "CommentSet") %>%
          group_by(variable_name) %>%
          summarise(N = n())
  
  
  
  
  
  
}