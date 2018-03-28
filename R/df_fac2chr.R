#funtion of convert type of data frame column.
#this make df to character
#use coerce factoer to character

df_fac2chr <- function(df){
  df <- as.data.frame(df)
  for(i in c(1:ncol(df))){
    df[,i] <- as.character(df[,i])
  }
  return(df)
}
