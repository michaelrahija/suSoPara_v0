#script to look at SuSo paradata
library(tidyr)
library(dplyr)
library(foreign)
library(reshape)
library(XML)

sys <- Sys.info()

if(sys[5] == "x86_64"){
  wdir = "~/Dropbox/suSoPara" #Mac
  para = "~/Dropbox/suSoPara_data/uganda_para/"
  intAction = "~/Dropbox/suSoPara_data/uganda_interviewActions/" 
  ddi = "~/Dropbox/suSoPara_data/uganda_ddi/"
} else if (sys[6]=="Rahija") {
  wdir = "C:/Users/rahija/Dropbox/suSoPara" #FAO PC
  para = "C:/Users/rahija/Dropbox/suSoPara_data/uganda_para/"
#} else if (#insert CARLOTTA condition){
  #wdir = 
  #para = 
} else {
  stop("Implement location for current user!")
}

setwd(wdir)

#########################################
##-Interview Actions Files Tabulations-##
#########################################
df <- read.delim(paste0(intAction,
                        "interview_actions.tab"),
                 stringsAsFactors = FALSE)

source("R/interviewTable.R")
int <- interviewTable(df)
head(int)

source("R/interviewer_table.R")
intR <- interviewer_table(data = df)
intR

source("R/supervisor_table.R")
supR <- supervisor_table(data = df)
supR

############################
#-PARADATA - trace files  ##
############################

#Import paradata, add column to id interview, 
#create posix column for actions
p.files <- list.files(para, pattern = ".tab")

p.files <- paste0(para,p.files)

# Create table with all actions
source("R/actionTable.R")
system.time(x <- lapply(p.files, actionTable))
y <- data.table::rbindlist(x)

head(y)

#-Create table which summarizes variables
source("R/variableTable.R") #FUNCTION STARTED, NOT FINISHED
system.time(z <- variableTable(y))
options(scipen=999)
head(z)


###- DDI metadata
ddi <- paste0(ddi,"/", list.files(ddi))
xmlfile <- xmlTreeParse(ddi)

# the xml file is now saved as an object you can easily work with in R:
class(xmlfile)

# Use the xmlRoot-function to access the top node

xmltop = xmlRoot(xmlfile)

# have a look at the XML-code of the first subnodes:
print(xmltop)[1]

# To extract the XML-values from the document, use xmlSApply:

plantcat <- xmlSApply(xmltop, function(x) xmlSApply(x, xmlValue))


# Finally, get the data in a data-frame and have a look at the first rows and columns

plantcat_df <- data.frame(t(plantcat),row.names=NULL)
plantcat_df[1:5,1:4]



