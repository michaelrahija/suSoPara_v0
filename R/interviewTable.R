#takes dataframe directly from imported interview actions
#files, and returns dataframe sorted by interview icluding
#id, Interviewer, Supervisor, HQApproved, SuperApproved,
#Starttime, Endtime, and Duration 

interviewTable <- function(x){
  
    ##############################
    ##Checks and cleanup        ##
    ##############################
    
    coltest <- colnames(x) == c("InterviewId", "Action","Originator",
                     "Role","Date","Time")
    
    if(sum(coltest) != 6){
      stop("Column names must be InterviewId, Action,Originator,
                     Role,Date,Time")
      
    }
    
    #make sure first column is properly named
    temp <- x[1,1]
    
    if (nchar(as.character(temp[[1]])) == 32){
      colnames(x)[1] <- "id"
    } else {
      stop ("First column is not the unique id. Check dataset.")
    }
    
    #convert relevant columns to characters
    i <- sapply(x, is.factor)
    x[i] <- lapply(x[i], as.character)


    #combine date and time columns and covert posixct
    x <- mutate(x,Dateandtime = paste(Date,Time))
    #x$Dateandtime <- strptime(data$Dateandtime,format='%m/%d/%Y %H:%M:%S') #giving error??
    x <- select(x,id, Action, Originator, Role, Dateandtime)
    
    ##############################
    #Create interviewer and super#
    ##############################

    ##Create id, Headquarter, Interviewer, and Supervisor columns
    ##group by ids and define function to collapse
    collapser = 
        function(x) paste(unique(x), collapse = ",")
    
    #Cast dataframe with ID as key, and originator as column
    role.df <- cast(x,id~Role,value="Originator", fun.aggregate = collapser)
    role.df <-as.data.frame(select(role.df,id,Interviewer, Supervisor))

    ##########################################
    #Create columns for HR and SuperApproved #
    ##########################################
    approval <- x %>%
                  group_by(id) %>%
                  summarize(HQApproved=sum(Action == "ApproveByHeadquarter"),
                            SuperApproved = sum(Action == "ApproveBySupervisor"))
                
    approval <- as.data.frame(approval)

    ########################################
    #Create Starttime and Endtime columns  #
    ########################################

    takefirst = 
      function(x) x[1] #function to take first element in object
    
    #cast data frame by id and action, value is dateandtime
    times <- cast(x,id~Action,value="Dateandtime", fun.aggregate = takefirst)
    times <- as.data.frame(select(times,id,FirstAnswerSet,Completed)) #select only what we want
    colnames(times)<- c("id","Starttime","Endtime") #correct names
    
    #convert to POSIXct so we can perform computations
    temp.start <- as.POSIXct(times$Starttime,format = "%m/%d/%Y %H:%M:%S")
    temp.start<- as.data.frame(temp.start)

    temp.end <- as.POSIXct(times$Endtime,format = "%m/%d/%Y %H:%M:%S")
    temp.end<- as.data.frame(temp.end)

    times <- cbind(Starttime = temp.start, Endtime = temp.end[,1])

    #########################################
    # Calculate duration of interview       #
    #########################################
    times$diff = difftime(times[,2],times[,1],unit="secs")

    #Convert seconds to D:H:M:S
    source('R/converttime.R')
    times$diff<- sapply(times$diff,converttime)
    
    master <- cbind(role.df,approval[,2:3],times)
    colnames(master) = c("id",'Interviewer','Supervisor','HQApproved','SuperApproved',"Starttime"
                         ,"Endtime","Duration")
    
    #########################################
    # CLEAN UP TABLE!                       #
    #########################################
    test <- grepl(master$Duration, pattern = "-")  # If negative value give error
    master$Duration[test] <- "ERROR"
    test <- grepl(master$Duration, pattern = "NA") # If unknown, error
    master$Duration[test] <- "ERROR"
    
master
     
}




