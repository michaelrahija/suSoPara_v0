# interviewer_table: organize_interviews: outputs table with id, interviewer, supervisor, HQApproved, 
#         Timesrejected, Starttime, Endtime, and Duration  
library(stringr)

interviewer_table <- function(data){
    
    #organize dataframe for subsetting
    source("R/interviewTable.R")
    interviewtable <- interviewTable(data)
    interviewtable <- interviewtable[!is.na(interviewtable$Interviewer),] # remove NAs
    
    #create Supervisor,Interviews, HQApproved, and Superapproved columns
    start = interviewtable %>%
                          group_by(Interviewer) %>%
                          summarize(Interviews = length(unique(id)),
                            SuperApproved = sum(SuperApproved))
    
    #################################################################
    ## SET WARNINGS: IF SOME INTERVIEWS HAVE > 1 INTERVIEWER, THEN  #
    ## TELL USER THOSE INTERVIEWS ARE EXCLUDED.                     #
    #################################################################
    #remove rows where > 1 interviewer
    test <- str_detect(start$Interviewer,pattern=",")  # T/F vector finding rows w/ > 1 Int
    morethanone <- sum(test)                      # # of rows w/ > 1 int
    
    #
    if(morethanone > 0){
      warning("Some interviews were assigned to > 1 interviewer. These interviews have been removed
              from mean and median time calculations")
      interviewers <- start$Interviewer[!test]
      
    }
      
    if(morethanone == 0){  
    interviewers <- unique(start$Interviewer)  
    }
    
    ##############################################################
    ##              ADD IN INTERVIEWER NAMES                     #
    ##############################################################
    #config <- read.csv("config/config.csv")
    
#     temp.list <- start$Interviewer
#     names <- c()
#     
#     for(i in 1:length(temp.list)){
#       names.temp <- as.character(config[temp.list[i] == config$interviewname,2])
#       names <- append(names,names.temp)
#     }
#     
#     start$Names <- names
# 
# start[,c(1,4,2,3)]
# 
start
}