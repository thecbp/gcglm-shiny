library(shiny)

server <- function(input, output) {

  # Create a reactive value to store simulated data
  simulated_data <- reactiveVal()

  output$dynamicInputs <- renderUI({
    numInputs <- input$numSelectCovariates
    lapply(1:numInputs, function(i) {
      selectInput(paste0("distribution_", i), paste("Select distribution", i, ":"),
                  choices = c("Normal", "Lognormal"))
    })
  })

  # Create dynamic select inputs based on the numeric input value
  output$dynamicOutcomes <- renderUI({
    numOutputs <- input$numSelectOutcomes
    lapply(1:numOutputs, function(i) {
      selectInput(paste0("distribution_", i), paste("Select outcome distribution", i, ":"),
                  choices = c("Normal", "Bernoulli", "Poisson"))
    })
  })

  # Simulate data when the simulate button is pressed
  observeEvent(input$simulate, {
    num_covariates <- input$numSelectCovariates
    num_outputs <- input$numSelectOutputs
    sample_size <- input$sampleSize
    distributions <- unlist(lapply(1:num_covariates, function(i) {
      input[[paste0("distribution_", i)]]
    }))

    # Call the simulate_gcglm_data function with the provided inputs
    # Make sure to define or import the function
    simulated_data <- simulate_gcglm_data(num_covariates, num_outputs, sample_size, distributions)

    # Store the generated data in the reactive value
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
