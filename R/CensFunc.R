#' Function to censor a data set
#'
#' @param dataCombined An uncensored dataframe
#' @param censEvents Number of events at which you wish to perform the censoring
#' @param censTime Time at which you wish to perform the censoring
#' @return A censored dataframe
#' @export


CensFunc <- function(dataCombined, censEvents = NULL, censTime = NULL){

  if (is.null(censEvents) && is.null(censTime)) {
    stop("Either censEvents or censTime must be specified")
  }

  if (!is.null(censEvents)){
    dataCombined <- dataCombined[order(dataCombined$pseudoTime),]
    censTime <- dataCombined$pseudoTime[censEvents]
  }



  dataCombined$status <- dataCombined$pseudoTime <= censTime
  dataCombined$status <- dataCombined$status * 1
  dataCombined$enrolled <- dataCombined$recTime < censTime
  dataCombined <- dataCombined[dataCombined$enrolled, ]
  dataCombined$survival_time <- ifelse(dataCombined$pseudoTime > censTime,
                                       censTime - dataCombined$recTime,
                                       dataCombined$time)

  return(list(dataCombined = dataCombined, censEvents = censEvents, censTime = censTime))
}
