library(shiny)
library(shinyMatrix)
library(tidyverse)
library(GGally)
library(gridExtra)

################################################################################

# Shiny Dashboard helper functions

################################################################################

simulate_gcglm_data = function(sample_size,
                               n_covariates, covar_params,
                               n_outcomes, outcome_params,
                               coefmat, scale) {

  # Construct the covariates based on the inputs parameters
  covariates = gc.sample.x(n_covariates = n_covariates,
                           params = covar_params,
                           N = sample_size,
                           scale = scale)

  # START ON GENERATING THE OUTCOMES NEXT
  outcomes = gc.sample.y(n_outcomes = n_outcomes,
                         params = outcome_params,
                         data = covariates,
                         coefmat = coefmat)

  cbind(covariates, outcomes)
}


################################################################################

# FROM ALT gcglmpos REPO

################################################################################

# Taken from Prof. Alt's gcglmpos
# modified to work with the dashboard inputs
gc.sample.x = function(n_covariates, params, N, scale) {

  ## Extract correlation from parameters
  Sigma = params[["covariance"]]

  ## Draw sample from Gaussian Copula
  X = pnorm(mvtnorm::rmvnorm(N, sigma = Sigma))
  colnames(X) = paste0("X", 1:n_covariates)

  ## Loop through X's and perform inverse-CDF
  for( p in 1:n_covariates ) {

    ## Get name of covariate, extract parameters, and distribution name
    covar = paste0("X", p)
    mu = params[[covar]][["mu"]]
    sigma = params[[covar]][["sigma"]]
    dist = params[[covar]][["dist"]]

    ## Generate covarate from GC based on proper quantile function
    ## depending on distribution
    if ( dist == 'Lognormal' )
      X[,p] = qlnorm(X[,p], meanlog = mu, sdlog = sigma)

    if ( dist == 'Normal' )
      X[,p] = qnorm(X[,p], mean = mu, sd = sigma)
  }
  ## None of the variables should be negative nor exceed 100
  X = ifelse(X < 0, 0, X)
  X = ifelse(X > 100, 100, X)
  ## Center/scale if requested (Default)
  if(scale)
    X = as.data.frame(scale(X))

  X
}

# Derive the outcomes based on their correlation matrix + marginals + covariates
gc.sample.y = function(n_outcomes, params, data, coefmat) {

  N = nrow(data)
  Sigma = params[["covariance"]]

  ## Sample outcomes from Gaussian copula
  Y = pnorm( mvtnorm::rmvnorm(N, sigma = Sigma) )
  colnames(Y) = paste0("Y", 1:n_outcomes)

  ## Loop through outcomes; extract right parameters; generate outcome
  for ( j in 1:n_outcomes ){

    betas = coefmat[j,] # 1 x p
    XB = data %*% betas # (n x p) x (p x 1) = (n x 1)

    Yj = colnames(Y)[j]
    family_j = params[[Yj]][["family"]]

    if (family_j == 'Normal'){

      mu_j = params[[Yj]][["linkinv"]](XB)
      sigma_j = params[[Yj]][["sigma"]]
      Y[,j] = qnorm(Y[,j], mean = mu_j, sd = sigma_j)

    } else if (family_j == 'Bernoulli') {

      prob = params[[Yj]][["linkinv"]](XB)
      Y[,j] = qbinom(Y[,j], size = 1, prob = prob)

    } else if (family_j == 'Poisson') {

      lambda = params[[Yj]][["linkinv"]](XB)
      Y[,j] = qpois(Y[,j], lambda = lambda)

    }
  }

  data.frame(Y)
}

identity = function(x) { x }
invlogit = function(x) { exp(x)/(1 + exp(x)) }
