{
  if(require("apmrt")){
    ver1<-packageVersion("apmrt")
    remove.packages("apmrt")
    }
  devtools::install_github("alexandretorrejano/apmrt")
  library(apmrt)

  cat(paste("\nVersion as of Script Call:",ver1))
  cat(paste("\nNewly Installed Version:  ",packageVersion("apmrt")))
  rm(ver1)
}
