library(shiny)
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

  output$dynamicCovariates <- renderUI({

    lapply(1:input$numSelectCovariates, function(i) {
      tagList(
        fluidRow(
          column(4, selectInput(paste0("covar_dist_", i), paste0("Distribution ", i, ":"),
                                choices = c("Normal", "Lognormal"))),
          column(4, numericInput(paste0("covar_mu_", i), paste0("Mean ", i, ":"), value = 0)),
          column(4, numericInput(paste0("covar_sigma_", i), paste0("Sigma ", i, ":"), value = 1))
        )
      )
    })

  })

  output$outcomeCorrelationMatrix <- renderUI({

    ynames = paste0("Y", 1:input$numSelectOutcomes)
    ycorr = diag(input$numSelectOutcomes)
    colnames(ycorr) = rownames(ycorr) = ynames

    matrixInput("covariate_corr",
                "Correlation matrix",
                value = ycorr,
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

  # Create dynamic select inputs based on the numeric input value
  output$dynamicOutcomes <- renderUI({
    numOutputs <- input$numSelectOutcomes
    lapply(1:numOutputs, function(i) {
      selectInput(paste0("distribution_", i), paste("Outcome distribution", i, ":"),
                  choices = c("Normal", "Bernoulli", "Poisson"))
    })
  })

  # Simulate data when the simulate button is pressed
  observeEvent(input$simulate, {

    n_outputs <- input$numSelectOutputs

    # Call the simulate_gcglm_data function with the provided inputs
    data <- simulate_gcglm_data(sample_size = input$sampleSize,
                                     n_covariates = input$numSelectCovariates,
                                     covar_params = covar_params(),
                                     outcome_params = outcome_params(),
                                     scale = input$scale)

    simulated_data(data)


  })


  # Create a reactive text output based on the inputs
  output$text <- renderText({
    numInputs <- input$numSelectCovariates
    selections <- lapply(1:numInputs, function(i) {
      input[[paste0("distribution_", i)]]
    })
    paste("Selected distributions:", paste(unlist(selections), collapse = ", "))
  })

  output$table <- renderTable({
    # to use the dataset elsewhere from reactiveVal
    data <- simulated_data()
  })

  output$statusBar <- renderText({
    if (is.null(simulated_data())) {
      return("No data has been simulated yet")
    } else {
      return("Data has been simulated")
    }
  })

}
