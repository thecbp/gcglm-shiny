library(shiny)
library(shinyMatrix)
library(tidyverse)
library(GGally)
library(gridExtra)
library(cmdstanr)
library(posterior)

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

  # Construct the outcomes based on the output parameters
  outcomes = gc.sample.y(n_outcomes = n_outcomes,
                         params = outcome_params,
                         data = covariates,
                         coefmat = coefmat)

  d = cbind(covariates, outcomes)

  d
}

gcglm = function(data, outcomes, formula.list, mu0 = NULL, lambda = .01, init = 'mle', ...) {

  # Prof Alt's Stan file for the GCGLM
  mod = cmdstan_model('glm_copula_cmdstanr.stan')

  N  = nrow(data)
  J  = length(formula.list)

  # Reorder variables if necessary
  family.list = lapply(1:J, function(j) outcomes[[j]][["family"]])
  n.indx = which(family.list == 'Gaussian')
  b.indx = which(family.list == 'Binomial')
  p.indx = which(family.list == 'Poisson')
  new.order = c(n.indx, b.indx, p.indx)

  ## Count the number of outcomes of each type
  Jn = length(n.indx)
  Jb = length(b.indx)
  Jp = length(p.indx)

  # Isolate the data columns for each outcome type and make Stan friendly
  Y = data %>% select(contains("Y"))
  Yn = Y %>% select(contains("Y")) %>% select(n.indx) %>% as.matrix
  Yb = Y %>% select(contains("Y")) %>% select(b.indx) %>% as.matrix
  Yp = Y %>% select(contains("Y")) %>% select(p.indx) %>% as.matrix

  # Organize design matrices and number of covariates per regression
  Xlist = lapply(formula.list, model.matrix, data = data)
  Xbig  = do.call(cbind, Xlist)

  # Count the total number of covariates from design matrices + for each outcome
  K = ncol(Xbig)
  Kj = sapply(Xlist, ncol)

  # Define endpoints to delineate correct design matrix to use in Stan
  Xindx  = matrix(nrow = J, ncol = 2)
  xstart = 1
  for ( j in 1:J ) {
    xend = xstart + Kj[j] - 1
    Xindx[j, ] = c(xstart, xend)
    xstart = xend + 1
  }

  args = list(...)
  chains = args$chains
  if (is.null(chains) )
    chains = 4  ## stan default

  if ( !all( new.order == 1:J ) ) {
    message("Rearranging order of dependent variables to normal, binomial, poisson")
    formula.list = formula.list[new.order]
    family.list  = family.list[new.order]
  }

  # Change family names to lower case for glm() 'family' argument
  family.list = lapply(family.list, function(x) { tolower(x) })

  # Conduct frequentist analysis to get good starting values
  if ( init == 'mle' ) {

    fitlist = mapply(function(f, fam) glm(formula = f, family = fam, data = data), f = formula.list, fam = family.list, SIMPLIFY = F)
    beta.start = sapply(fitlist, coef)
    sigmasq.start = sapply( fitlist[sapply(family.list, function(fam) fam == 'gaussian')], function(x) summary(x)$dispersion )
    sigmasq.start = as.array(sigmasq.start)

    # Approximate MLE of correlation matrix
    betalist = lapply(fitlist, function(f) coef(f))
    Un = matrix(nrow = N, ncol = Jn)
    Ub = matrix(nrow = N, ncol = Jb)
    Up = matrix(nrow = N, ncol = Jp)
    j = 1
    jn = 1
    jb = 1
    jp = 1

    # In order, get the u-values for Gaussian, Binomial, then Poisson outcomes
    while ( jn <= Jn ) {
      Un[, jn] = pnorm(Yn[, j],
                       mean = outcomes[[j]][["linkinv"]](Xlist[[j]] %*% betalist[[j]]),
                       sd = sqrt(summary(fitlist[[j]])$dispersion) )
      jn = jn + 1
      j  = j  + 1
    }

    while ( jb <= Jb ) {
      mu = outcomes[[j]][["linkinv"]](Xlist[[j]] %*% betalist[[j]])
      Ub[, jb] = rowMeans(
        cbind(
          min = pbinom(Yb[, jb] - 1, size = 1, prob = mu),
          max = pbinom(Yb[, jb], size = 1, prob = mu)
        )
      )
      jb = jb + 1
      j  = j  + 1
    }

    while ( jp <= Jp ) {
      mu = outcomes[[j]][["linkinv"]](Xlist[[j]] %*% betalist[[j]])
      Up[, jp] = rowMeans(
        cbind(
          min = ppois(Yp[, jp] - 1, lambda = mu),
          max = ppois(Yp[, jp], lambda = mu)
        )
      )
      jp = jp + 1
      j  = j  + 1
    }

    Gamma.start = cor(cbind(Un, Ub, Up))
    L = t(chol(Gamma.start))

    # Set initial values to the MLE; each MCMC chain needs a copy
    init = lapply(1:chains, function(i)
      list(
        'beta' = unlist(beta.start),
        'sigmasq' = sigmasq.start,
        'L' = L,
        'uraw_b' = Ub,
        'uraw_p' = Up
        )
      )

  }

  # Set initial mu to 0 for Gaussian families and adjust for others
  if ( is.null(mu0) ) {

    mu0 = matrix(0, nrow = N, ncol = J)

    for ( j in 1:J ) {
      if(family.list[[j]] == 'binomial')
        mu0[, j] = 0.5
      if(family.list[[j]] == 'poisson')
        mu0[, j] = 1
    }
  }

  ## Set default value of lambda for GLM prior
  if ( is.null(lambda) )
    lambda = rep(0.01, J)
  if ( length(lambda) == 1 )
    lambda = rep(lambda, J)
  if ( length(lambda) != J )
    stop("lambda0 must be a scalar or vector of dimension length(formula.list)")

  ## construct Stan data according to the model
  standat = list(
    'N'      = N,         # number of observations
    'J'      = J,         # total number of outcomes
    'Jn'     = Jn,        # number of normal outcomes
    'Jb'     = Jb,        # number of binomial outcomes
    'Jp'     = Jp,        # number of poisson outcomes
    'K'      = K,         # number of covariates for concatenated design matrices (X1, .., XK)
    'Yn'     = Yn,        # normal outcomes
    'Yb'     = Yb,        # bernoulli outcomes
    'Yp'     = Yp,        # poisson outcomes
    'X'      = Xbig,      # concatenated design matrices (X1, ..., XK)
    'Xindx'  = Xindx,     # Jx2 integer array giving the start and end indexes of X matrix for each outcome
    'Kj'     = Kj,        # J-dim integer array giving how many covariates per outcome
    'mu0'    = mu0,       # matrix giving prior prediction for each response (for Chen-Ibrahim (2003) conjugate prior of GLM)
    'lambda' = lambda     # precision parameters for conjugate prior of Chen and Ibrhaim (2003)
  )

  # Fit Stan model
  fit = mod$sample(data = standat, init = init, ...)

  # ADjust the shiny inputs to adjust the relevant MCMC parameters

  draws = as_draws_df(fit$draws()) %>%
    select(contains("beta"), contains("sigmasq"), contains("Gamma"))

  # Rename betas to align with indexing by outcome + predictor
  # Ex: beta[j][i]: is the i-th coef of j-th outcome/model
  adj.beta.names = c()
  start = 1
  for (j in 1:J) {
    numCoefs = length(betalist[[j]])
    adj.beta.names = c(adj.beta.names,
                       paste0("beta[", j, "][", 1:numCoefs  ,"]"))
    # Adjust starting point
    start = start + numCoefs
  }
  colnames(draws)[1:length(adj.beta.names)] = adj.beta.names

  list(
    'samples'      = draws,
    'formula.list' = formula.list,
    'family.list'  = family.list,
    'n.gaussian'   = Jn,
    'n.bernoulli'  = Jb,
    'n.poisson'    = Jp
  )
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

    if ( dist == 'Gaussian' )
      X[,p] = qnorm(X[,p], mean = mu, sd = sigma)
  }
  ## None of the variables should be negative nor exceed 100
  X = ifelse(X < 0, 0, X)
  X = ifelse(X > 100, 100, X)
  ## Center/scale if requested (Default)
  if(scale)
    X = as.matrix(scale(X))

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

    betas = coefmat[j, , drop = F] # 1 x p
    XB = data %*% t(betas) # (n x p) x (p x 1) = (n x 1)

    Yj = colnames(Y)[j]
    family_j = params[[Yj]][["family"]]

    if (family_j == 'Gaussian'){

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

coefmat2plotnames = function(coefmat) {

  J = nrow(coefmat)
  df = tibble()

  for (j in 1:J) {

    row = coefmat[j,]
    nonzero_coefs = row(which(row != 0)) # additional +1 is for intercept

    d = tibble(
      coefficient = paste0("beta[", j, "][", 2:length(nonzero_coefs), "]"),
      truth = row)

    df = bind_rows(df, d)
  }

  df
}
