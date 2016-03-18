readData3 <- function(blld,qpath,vname,vfname=NULL,tzab="UCT"){
  # This qdata was created in 3-advmodel\aggdataNN.Rmd
  if (is.null(vfname)){
    vfname <- vname
  }
  fname <- sprintf("%s/%s-%s.csv",qpath,blld,vfname)
  df <- read.csv(fname)
  dbdf <<- df
  df$time <- ymd_hms(df$time,tz=tzab)
  print(sprintf("Read %d rows from %s",nrow(df),fname))
  sapply(df,class)
  return(df)
}