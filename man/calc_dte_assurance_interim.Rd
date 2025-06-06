% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/main.R
\name{calc_dte_assurance_interim}
\alias{calc_dte_assurance_interim}
\title{Simulates a Group Sequential Trial for a trial with a Delayed Treatment Effect, with elicited priors}
\usage{
calc_dte_assurance_interim(
  n_c,
  n_t,
  control_dist = "Exponential",
  control_parameters = "Fixed",
  fixed_parameters_type = "Parameter",
  lambda_c = NULL,
  gamma_c = NULL,
  t1 = NULL,
  t2 = NULL,
  surv_t1 = NULL,
  surv_t2 = NULL,
  t1_Beta_a = NULL,
  t1_Beta_b = NULL,
  diff_Beta_a = NULL,
  diff_Beta_b = NULL,
  delay_time_SHELF,
  delay_time_dist = "hist",
  post_delay_HR_SHELF,
  post_delay_HR_dist = "hist",
  P_S = 1,
  P_DTE = 0,
  cens_events = NULL,
  rec_method,
  rec_period = NULL,
  rec_power = NULL,
  rec_rate = NULL,
  rec_duration = NULL,
  alpha_spending = NULL,
  beta_spending = NULL,
  IF_list = NULL,
  k = 2,
  type_one_error = NULL,
  nSims = 1000
)
}
\arguments{
\item{n_c}{Number of patients in the control group}

\item{n_t}{Number of patients in the treatment group}

\item{control_dist}{Distribution of control group, must be one of "Exponential" (default) or "weibull"}

\item{control_parameters}{The parameters for the control group are either "Fixed" (default) or "Distribution"}

\item{fixed_parameters_type}{The fixed parameters are either a "Parameters" (default) or "Landmark"}

\item{lambda_c}{Control group parameter}

\item{gamma_c}{Control group parameter}

\item{t1}{Time 1}

\item{t2}{Time 2}

\item{surv_t1}{Survival probability at time 1}

\item{surv_t2}{Survival probability at time 2}

\item{t1_Beta_a}{Hyperparameter a for the Beta distribution for the survival probability at time 1}

\item{t1_Beta_b}{Hyperparameter a for the Beta distribution for the survival probability at time 1}

\item{diff_Beta_a}{Hyperparameter a for the Beta distribution for the difference in survival probabilities (t2-t1)}

\item{diff_Beta_b}{Hyperparameter b for the Beta distribution for the difference in survival probabilities (t2-t1)}

\item{delay_time_SHELF}{A SHELF object, beliefs about the delay time}

\item{delay_time_dist}{Distribution of the delay time, "hist" is default. See SHELF help for more details}

\item{post_delay_HR_SHELF}{A SHELF object, beliefs about the post-delay hazard ratio}

\item{post_delay_HR_dist}{Distribution of the post-delay hazard ratio, "hist" is default. See SHELF help for more details}

\item{P_S}{Probability of the survival curves separating}

\item{P_DTE}{Probability of the survival curves being subject to a DTE, given they separate}

\item{rec_method}{Recruitment method, must be one of "power" or "PWC" (piecewise constant)}

\item{rec_period}{Parameter used to model recruitment according to power model}

\item{rec_power}{Parameter used to model recruitment according to power model}

\item{rec_rate}{Parameter used to model recruitment according to piecewise constant model}

\item{rec_duration}{Parameter used to model recruitment according to piecewise constant model}

\item{alpha_spending}{Cumulative alpha spending}

\item{beta_spending}{Cumulative beta spending}

\item{nSims}{Number of simulations, default is 1000}

\item{cens_IF}{Number of events at which you wish to perform the censoring (must be less than n_c + n_t)}

\item{IF_vec}{Vector of information fraction's}
}
\description{
Simulates a Group Sequential Trial for a trial with a Delayed Treatment Effect, with elicited priors
}
