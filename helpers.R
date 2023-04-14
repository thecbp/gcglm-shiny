################################################################################

# Shiny Dashboard helper functions

################################################################################

simulate_gcglm_data = function(sample_size, n_covariates, covar_params, n_outputs, outcome_params, scale) {


  # Construct the covariates based on the inputs parameters
  covariates = gc.sample.x(n_covariates = n_covariates,
                           params = covar_params,
                           N = sample_size,
                           scale = scale)

  # START ON GENERATING THE OUTCOMES NEXT
  outcomes = gc.sample.y()

  covariates
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


gc.sample.y = function(parms, ynames, formula.list, family.list, data, corr = NULL) {
  J = length(formula.list)
  N = nrow(data)
  ## Extract correlation (or generate if applicable)
  if(is.null(corr)) {
    corr = parms[grepl('corr', names(parms))]
    corr = corrvec2corrmat(corr)
  } else {
    corr = matrix(corr, J, J) + diag(1 - corr, J)
  }
  ## Sample from Gaussian copula
  Y = pnorm( mvtnorm::rmvnorm(N, sigma = corr) )
  colnames(Y) = ynames
  ## Loop through outcomes; extract right parameters; generate outcome
  for ( j in 1:J ){
    yname_j       = ynames[j]
    formula_j     = formula.list[[j]]
    family_j      = family.list[[j]]
    rhs.formula_j = formula_j[-2]
    X_j           = model.matrix(rhs.formula_j, data)
    parms_j       = parms[grepl( paste0(yname_j, '\\['), names(parms))]
    if (family_j$family == 'gaussian') {
      indx.sigmasq = which(grepl('sigmasq', names(parms_j)))
      sigmasq_j    = parms_j[indx.sigmasq]   ## extract sigmasq
      parms_j      = parms_j[-indx.sigmasq]  ## now only regression coeffs
    }
    parms_j = parms_j[paste0(yname_j, '[', colnames(X_j), ']')]  ## convert to beta; make sure order is correct based on X_j
    mean_j  = family_j$linkinv(X_j %*% parms_j)
    if (family_j$family == 'gaussian'){
      Y[,j]        = qnorm(Y[,j], mean = mean_j, sd = sqrt(sigmasq_j))
    } else if (family_j$family == 'binomial') {
      Y[,j] = qbinom(Y[,j], size = 1, prob = family_j$linkinv(X_j %*% parms_j))
    } else {
      Y[,j] = qpois(Y[,j], lambda = family_j$linkinv(X_j %*% parms_j))
    }
  }
  data.frame(Y)
}
