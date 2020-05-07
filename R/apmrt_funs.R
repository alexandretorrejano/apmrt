if(!require(pacman)){install.packages("pacman")}
pacman::p_load(backports,bit64,cluster,data.table,DBI,DMwR,doParallel,dplyr,factoextra,foreach,ggplot2,gmodels,kableExtra,knitr,kohonen,lubridate,maditr,NbClust,odbc,openxlsx,parallel,progress,readxl,reshape,RGoogleAnalytics,rmarkdown,stringr,WriteXLS,devtools)

fstart<-function(){
  setwd(dirname(rstudioapi::getSourceEditorContext()$path)) # Sets working directory to caller script's location

  if(!require(pacman)){install.packages("pacman")} # Guarantees that pacman is installed for handling of other packages
  options(scipen=999)
  pacman::p_load(backports,bit64,cluster,data.table,DBI,DMwR,doParallel,dplyr,factoextra,foreach,ggplot2,gmodels,kableExtra,knitr,kohonen,lubridate,maditr,NbClust,odbc,openxlsx,parallel,progress,readxl,reshape,RGoogleAnalytics,rmarkdown,stringr,WriteXLS) # Installs
}

fwipe<-function(){
    closeAllConnections()
    rm(list=ls())
    tryCatch(dev.off(),error=function(e){NULL})
    gc()
    cat("\14Environment Wiped.\n")
}


fload<-function(f,...){
  ext<-f%>%strsplit("\\.")%>%unlist

  if(length(ext)>1){
    ext<-tail(ext,1)
  } else{
    ext<-"Rds"
  }

  lext<-tolower(ext)

  if(lext=="csv"){
    d<-data.table::fread(f,integer64="character",...)
  } else if(lext=="xlsx"){
    d<-openxlsx::read.xlsx(f,...)%>%data.table
  } else if(lext=="xls"){
    d<-readxl::read_xls(f,...)%>%data.table
  } else if(lext=="txt"){
    d<-utils::read.table(f,...)%>%data.table
  } else if(lext=="rds"){
    d<-readRDS(f,...)
  } else{
    stop(paste0("FILE ERROR: UNKNOWN EXTENSION FOR READING: .",ext,"\n"))
  }

  return(d)
} # Reads data from files and ensures data.table format with correct column types unless it's a RDS save in which case it reads the object as is


fsave<-function(d,f,...){
  ext<-f%>%strsplit("\\.")%>%unlist

  if(length(ext)>1){
    ext<-tail(ext,1)
  } else{
    ext<-"Rds"
    f<-paste0(f,".",ext)
  }

  lext<-tolower(ext)

  if(lext=="csv"){
    d<-data.table::fwrite(d,f,...)
  } else if(lext=="xlsx"){
    d<-openxlsx::write.xlsx(d,f,...)%>%data.table
  } else if(lext=="txt"){
    d<-utils::write.table(d,f,...)%>%data.table
  } else if(lext=="rds"){
    d<-saveRDS(d,f,...)
  } else{
    stop(paste0("FILE ERROR: UNKNOWN EXTENSION FOR WRITING: .",ext,"\n"))
  }
} # Writes data/object to file

fquery<-function(con,q){
  suf<-ifelse(grepl("\r\n",q),' (...)','')
  cat(paste0("Running Query [",gsub("\n.*",'',q)%>%gsub(pattern='\r',replacement=''),suf,"]: "))
  x<-DBI::dbGetQuery(con,q)
  cat(paste0("Success\n"))
  return(x)
}

fsub<-function(x,pattern,replacement,ignore.case = FALSE, perl = FALSE, fixed = FALSE, useBytes = FALSE){
  gsub(pattern, replacement, x, ignore.case, perl,fixed,useBytes)
}
