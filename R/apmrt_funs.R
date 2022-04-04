if(!require(pacman)){install.packages("pacman")}
pacman::p_load(openxlsx,data.table,utils,DBI)



prsource<-function(f){
  x<-getwd()
  x1<-x%>%psub("/Worten/CVM Campaigns Management.*","/Worten/CVM Campaigns Management")
  x2<-x%>%psub(x1,"")%>%psub("/.*","/")
  x3<-"Processos Recorrentes/"
  source(paste0(x1,x2,x3,f))
}


# Sets working directory, disables scientific notation, installs frequently used packages
go<-function(short=FALSE){
  setwd(dirname(rstudioapi::getSourceEditorContext()$path))

  #if(!require(pacman)){install.packages("pacman")}
  options(scipen=999)
  if(short==FALSE){
    pacman::p_load(backports,bit64,cluster,data.table,devtools,DBI,devtools,DMwR,doParallel,dplyr,factoextra,fastmatch,foreach,ggplot2,gmodels,kableExtra,knitr,kohonen,lubridate,maditr,NbClust,odbc,openxlsx,parallel,progress,readxl,reshape,RGoogleAnalytics,rlang,rmarkdown,stringr,WriteXLS,DBI,ROracle,chron)
  } else{
    pacman::p_load(DBI,data.table,magrittr,stringr,openxlsx,lubridate)
  }

  if(getwd()%>%like("CVM Campaigns Management"))
    prsource("wrtcon.R")
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
wipengo<-function(except=c(),short=FALSE){
  wipe(except=except)
  go(short=short)
}



clean<-function(except=c()){
  closeAllConnections()
  rm(list=ls(envir=.GlobalEnv)[!ls(envir=.GlobalEnv)%in%except],envir=.GlobalEnv)
  tryCatch(dev.off(),error=function(e){NULL})
  gc()
  cat("\nEnvironment Cleaned.\n")
}

wash<-function(except=c()){
  rm(list=ls(envir=.GlobalEnv)[!ls(envir=.GlobalEnv)%in%except],envir=.GlobalEnv)
  tryCatch(dev.off(),error=function(e){NULL})
  gc()
  cat("\nEnvironment Washed.\n")
}

cleango<-function(except=c(),short=FALSE){
  clean(except=except)
  go(short=short)
}

wgo<-function(){
  wipe()
  go(short=TRUE)
}
cgo<-function(){
  clean()
  go(short=TRUE)
}

# Loads data/object from file
fload<-function(f,septxt=TRUE,...){
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
  } else if(lext=="txt" & septxt==TRUE){
    d<-utils::read.table(f,...)%>%data.table
  } else if(lext=="txt" & septxt==FALSE){
    d<-base::readLines(f,...)
  } else if(lext=="rds"){
    d<-base::readRDS(f,...)
  } else{
    stop(paste0("FILE ERROR: UNKNOWN EXTENSION FOR READING: .",ext,"\n"))
  }

  return(d)
}

# Saves data/object to file
fsave<-function(d,f,septxt=TRUE,...){
  ext<-f%>%strsplit("\\.")%>%unlist

  if(length(ext)>1){
    ext<-tail(ext,1)
  } else{
    ext<-"Rds"
    f<-paste0(f,".",ext)
  }

  lext<-tolower(ext)

  if(lext=="csv"){
    data.table::fwrite(d,f,...)
  } else if(lext=="xlsx"){
    openxlsx::write.xlsx(d,f,...)
  } else if(lext=="txt" & septxt==TRUE){
    utils::write.table(d,f,...)
  } else if(lext=="txt" & septxt==FALSE){
    base::writeLines(d,f,...)
  } else if(lext=="rds"){
    base::saveRDS(d,f,...)
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
psub<-function(x,pattern,replacement="",ignore.case = FALSE, perl = FALSE, fixed = FALSE, useBytes = FALSE){
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


prload<-function(f,...){
  x<-getwd()
  x1<-x%>%psub("/Worten/CVM Campaigns Management.*","/Worten/CVM Campaigns Management")
  x2<-x%>%psub(x1,"")%>%psub("/.*","/")
  x3<-"Processos Recorrentes/"
  fload(paste0(x1,x2,x3,f),...)
}

prsave<-function(o,f,...){
  x<-getwd()
  x1<-x%>%psub("/Worten/CVM Campaigns Management.*","/Worten/CVM Campaigns Management")
  x2<-x%>%psub(x1,"")%>%psub("/.*","/")
  x3<-"Processos Recorrentes/"
  fsave(o,paste0(x1,x2,x3,f),...)
}





bdquery<-function(bd,q){
  Class<-class(bd)%>%as.vector
  cat(paste0("\nRunning query on ",Class,".\n"))
  if(Class=="Microsoft SQL Server"){
    return(DBI::dbGetQuery(bd,q)%>%data.table)
  }
  if(Class=="OraConnection"){
    return(ROracle::dbSendQuery(bd,q)%>%fetch%>%as.data.table)
  }
  if(Class=="Hive"){
    x<-DBI::dbGetQuery(bd,q)%>%data.table
    colnames(x)%<>%gsub(pattern=".*\\.",replacement="")
    return(x)
  }
  if(Class=="Impala"){
    x<-DBI::dbGetQuery(bd,q)%>%data.table
    colnames(x)%<>%gsub(pattern=".*\\.",replacement="")
    return(x)
  }
  cat("\nThis function doesn't know how to handle this DB type...")
}

chunk_cycle<-function(data_size,chunk_size){
  i<-1:ceiling(data_size/chunk_size)
  data.frame(
    i=i,
    chunk_begin=(i-1)*chunk_size+1,
    chunk_end=ifelse(i*chunk_size<data_size,i*chunk_size,data_size)
  )
}


wxl<-function(tablist,path,overwrite=TRUE){
  wb<-openxlsx::createWorkbook()
  for(i in 1:length(tablist)){
    openxlsx::addWorksheet(wb,names(tablist)[i])
    openxlsx::writeDataTable(wb,names(tablist)[i],tablist[[i]])
    widths<-tablist[[i]]%>%as.matrix%>%rbind(colnames(tablist[[i]]))%>%nchar%>%apply(2,max)+4
    openxlsx::setColWidths(wb, sheet = names(tablist)[i], cols = 1:ncol(tablist[[i]]), widths = widths)
  }
  openxlsx::saveWorkbook(wb,path,overwrite=overwrite)
}
