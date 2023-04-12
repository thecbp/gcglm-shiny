library(shiny)

ui <- navbarPage("GCGLM Study Planner App",
                 tabPanel("Simulation",
                          # Create a sidebar layout with side panel and main panel
                          sidebarLayout(

                            # Side panel
                            sidebarPanel(
                              # Add a numeric input for the number of covariates
                              numericInput("numSelectCovariates", "Number of covariates:", value = 1, min = 1, max = 10),
                              numericInput("numSelectOutcomes", "Number of outputs:", value = 1, min = 1, max = 10),
                              numericInput("sampleSize", "Sample Size:", value = 50, min = 1, max = 1000),
                              selectInput("copula", "Copula:", choices = c("Gaussian")),

                              # Add a well panel with inputs
                              wellPanel(
                                uiOutput("dynamicInputs"), # Placeholder for dynamic inputs
                                uiOutput("dynamicOutcomes") # Placeholder for dynamic inputs
                              ),
                              actionButton("simulate", "Simulate")
                            ),

                            # Main panel
                            mainPanel(
                              # Add a text output to the main panel
                              textOutput("text"),
                              textOutput("statusBar")
                            )
                          )
                 ),
                 tabPanel("Analysis"),
                 tabPanel("Help")
)
