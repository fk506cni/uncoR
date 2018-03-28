#funtion of referential sort
#this function return index vector of re sorted input vector
#sorting will be done by reference vector
#
#use this when some matrix sort as same order to referential matrix
#m <- m[ref_sort(sort_vector =  rownames(m),ref_vector = rownames(r)),]
#

ref_sort <- function(sort_vector, ref_vector){
  result_vector <- c()
  for(i in c(1:length(ref_vector))){
    index_i <- which(sort_vector==ref_vector[i])
    result_vector <- c(result_vector, index_i)
  }
  return(result_vector)
}
