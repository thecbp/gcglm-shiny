source("helpers.R")

server <- function(input, output, session) {

  # Create a reactive value to store simulated data
  simulated_data <- reactiveVal()

  # Compile all of the
  covar_params = reactive({

    n_covariates = input$numSelectCovariates
    params = list()
    sigmas = c()

    for (i in 1:n_covariates) {
      params[[paste0("X", i)]] = list()
      params[[paste0("X", i)]][["dist"]] = input[[paste0("covar_dist_", i)]]
      params[[paste0("X", i)]][["mu"]] = input[[paste0("covar_mu_", i)]]
      params[[paste0("X", i)]][["sigma"]] = input[[paste0("covar_sigma_", i)]]
      sigmas = c(sigmas, params[[paste0("X", i)]][["sigma"]])
    }

    params[["covariance"]] = diag(sigmas) %*% input$covariate_corr %*% diag(sigmas)

    params

  })

  # Collect all of the information on the outcome parameters
  outcome_params = reactive({

    n_outcomes = input$numSelectOutcomes
    params = list()

    for (i in 1:n_outcomes) {
      params[[paste0("Y", i)]] = list()
      params[[paste0("Y", i)]][["family"]] = input[[paste0("outcome_dist_", i)]]

      if (params[[paste0("Y", i)]][["family"]] == 'Normal'){

        params[[paste0("Y", i)]][["sigma"]] = input[[paste0("outcome_sigma_", i)]]
        params[[paste0("Y", i)]][["linkinv"]] = identity

      } else if (params[[paste0("Y", i)]][["family"]] == 'Bernoulli') {

        params[[paste0("Y", i)]][["linkinv"]] = invlogit

      } else if (params[[paste0("Y", i)]][["family"]] == 'Poisson') {

        params[[paste0("Y", i)]][["linkinv"]] = exp

      }

    }

    params[["covariance"]] = input$outcome_corr

    params

  })

  output$dynamicCovariates <- renderUI({

    lapply(1:input$numSelectCovariates, function(i) {
      tagList(
        fluidRow(
          column(6, selectInput(paste0("covar_dist_", i), paste0("Distribution ", i, ":"),
                                choices = c("Normal", "Lognormal"))),
          column(3, numericInput(paste0("covar_mu_", i), paste0("Mean ", i, ":"), value = 0)),
          column(3, numericInput(paste0("covar_sigma_", i), paste0("Sigma ", i, ":"), value = 1))
        )
      )
    })

  })

  output$covariateCorrelationMatrix <- renderUI({

    xnames = paste0("X", 1:input$numSelectCovariates)
    xcorr = diag(input$numSelectCovariates)
    colnames(xcorr) = rownames(xcorr) = xnames

    matrixInput("covariate_corr",
                "Covariate Correlation matrix",
                value = xcorr,
                class = "numeric")
  })

  output$outcomeCorrelationMatrix <- renderUI({

    ynames = paste0("Y", 1:input$numSelectOutcomes)
    ycorr = diag(input$numSelectOutcomes)
    colnames(ycorr) = rownames(ycorr) = ynames

    matrixInput("outcome_corr",
                "Outcome Correlation matrix",
                value = ycorr,
                class = "numeric")
  })

  output$glmParameterMatrix <- renderUI({

    mat = matrix(0, nrow = input$numSelectOutcomes, ncol = input$numSelectCovariates)
    xnames = paste0("X", 1:input$numSelectCovariates)
    ynames = paste0("Y", 1:input$numSelectOutcomes)
    ycorr = diag(input$numSelectOutcomes)
    rownames(mat) = ynames
    colnames(mat) = xnames

    matrixInput("glm_coefs",
                "GLM regression coefficients",
                value = mat,
                class = "numeric")
  })

  observeEvent(input$covariate_corr, {
    # Read the updated matrix input
    mat <- input$covariate_corr

    # Check if the current correlation matrix is not symmetric
    asymm = nrow(which(t(mat) != mat, arr.ind = T)) > 0

    if (!is.null(mat) & asymm) {
      changed = which(t(mat) != mat, arr.ind = T)
      i = changed[1,1]
      j = changed[1,2]
      mat[i, j] = mat[j, i]
    }

    diag(mat) = 1

    # Update the matrix input with the symmetric value and set diagonals to 1
    updateMatrixInput(session, "covariate_corr", value = mat)

  })

  observeEvent(input$outcome_corr, {
    # Read the updated matrix input
    mat <- input$outcome_corr

    # Check if the current correlation matrix is not symmetric
    asymm = nrow(which(t(mat) != mat, arr.ind = T)) > 0

    if (!is.null(mat) & asymm) {
      changed = which(t(mat) != mat, arr.ind = T)
      i = changed[1,1]
      j = changed[1,2]
      mat[i, j] = mat[j, i]
    }

    diag(mat) = 1

    # Update the matrix input with the symmetric value and set diagonals to 1
    updateMatrixInput(session, "outcome_corr", value = mat)

  })

  # Create dynamic select inputs based on the numeric input value
  output$dynamicOutcomes <- renderUI({
    numOutputs <- input$numSelectOutcomes
    lapply(1:numOutputs, function(i) {
      selectInput(paste0("outcome_dist_", i), paste("Outcome Distribution", i, ":"),
                  choices = c("Normal", "Bernoulli", "Poisson"))
    })
  })

  # Adding extra parameters for the Normal family
  output$dynamicExtraParameters <- renderUI({
    numOutputs <- input$numSelectOutcomes
    lapply(1:numOutputs, function(i) {
      distribution <- input[[paste0("outcome_dist_", i)]]
      if (distribution == "Normal") {
        numericInput(paste0("outcome_sigma_", i), paste("Sigma for outcome", i, ":"), value = 1)
      } else {
        NULL
      }
    })
  })

  # Simulate data when the simulate button is pressed
  observeEvent(input$simulate, {

    n_outputs = input$numSelectOutputs

    data = simulate_gcglm_data(sample_size = input$sampleSize,
                               n_covariates = input$numSelectCovariates,
                               covar_params = covar_params(),
                               n_outcomes = input$numSelectOutcomes,
                               outcome_params = outcome_params(),
                               coefmat = input$glm_coefs,
                               scale = input$scale)

    simulated_data(data)

  })

  output$table <- renderTable({
    # to use the dataset elsewhere from reactiveVal
    data = simulated_data()
  })

  output$statusBar <- renderText({
    if (is.null(simulated_data())) {
      return("No data has been simulated yet")
    } else {
      return(NULL)
    }
  })

  ##############################################################################

  # Plots for the Simulation tab

  ##############################################################################

  output$outcomeScatter = renderPlot({

    data = simulated_data()

    if (!is.null(data)) {

      outcomes = data %>% select(contains("Y"))
      ggpairs(outcomes) +
        theme_bw() +
        ggtitle("Outcome Pairplots") +
        theme(plot.title = element_text(hjust = 0.5))

    } else {

      ggplot() +
        geom_blank() +
        theme_bw() +
        ggtitle("No data has been simulated yet") +
        theme(plot.title = element_text(hjust = 0.5))

    }



  })

  output$covariateScatter = renderPlot({

    data = simulated_data()

    if (!is.null(data)) {

      outcomes = data %>% select(contains("X"))
      ggpairs(outcomes) +
        theme_bw() +
        ggtitle("Covariate Pairplots") +
        theme(plot.title = element_text(hjust = 0.5))

    } else {

      ggplot() +
        geom_blank() +
        theme_bw() +
        ggtitle("No data has been simulated yet") +
        theme(plot.title = element_text(hjust = 0.5))

    }



  })

}
