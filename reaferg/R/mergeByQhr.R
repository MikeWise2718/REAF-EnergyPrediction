mergeByQhr<- function(cname,df,df2){
  ddf2 <-  df2[,match(c("qhr",cname),names(df2))]
  rdf <- merge(df,ddf2,by="qhr")
  return(rdf)
}

mergeDfsByQhr <- function(df1,df2,df3){
  df <- df1
  sapply( names(df2), function(x){ if( is.null(df[[x]])){ df <<- mergeByQhr(x,df,df2) } } )
  sapply( names(df3), function(x){ if( is.null(df[[x]])){ df <<- mergeByQhr(x,df,df3) } } )
  return(df)
} 