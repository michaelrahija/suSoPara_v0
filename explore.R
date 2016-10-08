#script to look at SuSo paradata
library(tidyr)
library(dplyr)
library(foreign)
library(readr)
library(reshape)

sys <- Sys.info()

if(sys[5] == "x86_64"){
  wdir = "~/Dropbox/suSoPara" #Mac
  para = "~/Dropbox/suSoPara_data/uganda_para/"
} else if (sys[6]=="Rahija") {
  wdir = "C:/Users/rahija/Dropbox/suSoPara" #FAO PC
  para = "C:/Users/rahija/Dropbox/suSoPara/uganda_para/"
} else {
  stop("Implement location for current user!")
}

setwd(wdir)


#Import paradata, add column to id interview, 
#create posix column for actions
p.files <- list.files(para, pattern = ".tab")

p.files <- paste0(para,p.files)

# Create table with all actions
source("R/actionTable.R")
#para.list <- list()



system.time({
  
  #para.list <- vector(mode = "list", length = length(p.files)) 
  para.list <- list()
  
for(i in 1:length(p.files)){

  temp.df <- actionTable(dir = p.files[i])
  
  
  
  para.list[[i]] <-  temp.df
}})


para.df <- data.table::rbindlist(para.list)
para.df <- data.frame(para.df)

#-Create table which summarizes variables
source("R/variableTable.R")












##--Try out Interview Table, consider changing cast inside interview_Table
ia <- paste0(wdir,
             "/",
             "uganda_interviewActions/",
             "interview_actions.tab")

ia <- read.delim(ia,
                 stringsAsFactors = FALSE,
                 sep = "\t")

source("R/interviewTable.R")
ia <- interviewTable(ia)



table <- int.df %>%
            group_by(variable_name) %>%
            summarize(N = n(),
                      Average = mean(itemResponseTimeSecs, na.rm = TRUE),
                      Max = max(itemResponseTimeSecs, na.rm = TRUE),
                      Min = min(itemResponseTimeSecs, na.rm = TRUE))

data.frame(table)

#determine order



#identify the answerset time of the 



