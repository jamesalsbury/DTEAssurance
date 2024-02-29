#' Function to censor a data set
#'
#' @param dataCombined a data frame
#' @param censTime Time at whcih you wish to perform the censoring
#' @return A dataframe
#' @export


CensFunc <- function(dataCombined, censTime){
  dataCombined$status <- dataCombined$pseudoTime < censTime
  dataCombined$status <- dataCombined$status * 1
  dataCombined$enrolled <- dataCombined$recTime < censTime
  dataCombined <- dataCombined[dataCombined$enrolled, ]
  dataCombined$survival_time <- ifelse(dataCombined$pseudoTime > censTime,
                                       censTime - dataCombined$recTime,
                                       dataCombined$time)

  return(dataCombined)
}
