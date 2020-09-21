if(!require(pacman)){install.packages("pacman")}
pacman::p_load(openxlsx,data.table,utils,readxl,DBI)


# Sets working directory, disables scientific notation, installs frequently used packages
go<-function(){
  setwd(dirname(rstudioapi::getSourceEditorContext()$path))

  #if(!require(pacman)){install.packages("pacman")}
  options(scipen=999)
  pacman::p_load(backports,bit64,cluster,data.table,devtools,DBI,devtools,DMwR,doParallel,dplyr,factoextra,fastmatch,foreach,ggplot2,gmodels,kableExtra,knitr,kohonen,lubridate,maditr,NbClust,odbc,openxlsx,parallel,progress,readxl,reshape,RGoogleAnalytics,rlang,rmarkdown,stringr,WriteXLS,DBI,ROracle,chron)
  cat(paste0("Options set to the following directory:\n",getwd(),"\n"))
}


# Clears connections, objects, plots, memory and console
wipe<-function(except=c()){
  closeAllConnections()
  rm(list=ls(envir=.GlobalEnv)[!ls(envir=.GlobalEnv)%in%except],envir=.GlobalEnv)
  tryCatch(dev.off(),error=function(e){NULL})
  gc()
  cat("\14Environment Wiped.\n")
}

#wipe() then go()
wipengo<-function(except=c()){
  wipe(except=except)
  go()
}
clean<-function(except=c()){
  closeAllConnections()
  rm(list=ls(envir=.GlobalEnv)[!ls(envir=.GlobalEnv)%in%except],envir=.GlobalEnv)
  tryCatch(dev.off(),error=function(e){NULL})
  gc()
  cat("\nEnvironment Cleaned.\n")
}

cleango<-function(except=c()){
  clean(except=except)
  go()
}

# Loads data/object from file
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
}

# Saves data/object to file
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
}

# Queries an existing connection
dbquery<-function(con,q){
  suf<-ifelse(grepl("\r\n",q),' (...)','')
  cat(paste0("Running Query [",gsub("\n.*",'',q)%>%gsub(pattern='\r',replacement=''),suf,"]: "))
  x<-DBI::dbGetQuery(con,q)
  cat(paste0("Success\n"))
  return(x)
}

# Replaces a pattern in a vector of strings
psub<-function(x,pattern,replacement,ignore.case = FALSE, perl = FALSE, fixed = FALSE, useBytes = FALSE){
  gsub(pattern, replacement, x, ignore.case, perl,fixed,useBytes)
}

prog<-function(total){progress::progress_bar$new(  format = "Processing [:bar] :current/:total iterations (:percent) elapsed time: :elapsed eta: :eta",  clear = FALSE,   total = total,   width = 90)}

# Automatically processes a vector of strings that contains phone numbers. Aims to keep the last 9 digits without special characters
telproc<-function(x){
  x[is.na(x)]<-""
  x%<>%gsub(pattern="[^0-9.-]", replacement="")%>%
    gsub(pattern="\\.", replacement="")%>%
    gsub(pattern=",", replacement="")%>%
    gsub(pattern="\\+", replacement="")%>%
    gsub(pattern="\\-", replacement="")
  x[nchar(x)>9]%<>%str_sub(-9,-1)
  x[nchar(x)<9]<-NA
  x
}


prload<-function(f){
  x<-getwd()
  x1<-x%>%psub("/Worten/CVM Campaigns Management.*","/Worten/CVM Campaigns Management")
  x2<-x%>%psub(x1,"")%>%psub("/.*","/")
  x3<-"Processos Recorrentes/"
  fload(paste0(x1,x2,x3,f))
}


bdcon<-function(bd){
  if(bd=="hive"){
    return(odbc::dbConnect(odbc::odbc(),"Hive_PRD"))
  }
  if(bd=="oms"){
    return(odbc::dbConnect(odbc::odbc(), "OMS"))
  }
  if(bd=="vll"){
    return(ROracle::dbConnect(dbDriver("Oracle"),dbname="(DESCRIPTION=(ADDRESS=(PROTOCOL=tcp)(HOST=vllprd.mch.moc.sgps)(PORT=1530))(CONNECT_DATA=(SERVICE_NAME=VLL)))",username="gsoliveira",password="gsoliv3ir4"))
  }
  if(bd=="crm"){
    return(odbc::dbConnect(odbc::odbc(), "CRMx32"))
  }
}


bdquery<-function(bd,q){
  Class<-class(bd)%>%as.vector
  cat(paste0("\nRunning query on ",Class,".\n"))
  if(Class=="Microsoft SQL Server"){
    DBI::dbGetQuery(bd,q)%>%data.table
  }
  if(Class=="OraConnection"){
    ROracle::dbSendQuery(bd,q)%>%fetch
  }
  if(Class=="Hive"){
    x<-DBI::dbGetQuery(bd,q)%>%data.table
    colnames(x)%<>%gsub(pattern=".*\\.",replacement="")
    return(x)
  }
}

cquery<-function(bdname,q){
  bd<-bdcon(bdname)
  bdquery(bd,q)
}
