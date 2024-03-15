calculateAssurance <- function(n_C, n_E, lambda_C, gammac, P_E, P_DTE, delayT, HRStar, nEvents,
                               rec_method, analysis_method, rho = 0, gamma = 0, nSims=1e5){

  control_n <- length(lambdac)

  for (i in 1:nSims){

    #Sample the control parameters
    u <- sample(1:control_n, size = 1)
    sampled_lambdac <- lambdac[u]
    sampled_gammac <- gammac[u]

    #Sample the treatment effect parameters
    u <- sample(1:delayT, size = 1)
    sampled_delayT <- delayT[u]
    sampled_HRStar <- HRStar[u]

    #Make the simplifications
    sampled_lambdae <- sampled_HRStar*sampled_lambdac
    sampled_gammae <- sampled_gammac

    if (rec_method=="Power"){
      dataCombined <- SimDTEDataSetPower(n_C, n_E, sampled_gammae, sampled_gammac, sampled_lambdae,
                                         sampled_lambdac, sampled_delayT, rec_period, rec_power)
    }

    if (rec_method=="PWC"){
      dataCombined <- SimDTEDataSetPWC(n_C, n_E, sampled_gammae, sampled_gammac, sampled_lambdae,
                                       sampled_lambdac, sampled_delayT, rec_rate, rec_duration)
    }


    dataCombined <-
      CensFunc <- function(dataCombined, censTime){





      }


  }

}
