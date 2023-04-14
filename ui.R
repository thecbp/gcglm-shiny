library(shiny)
library(shinyMatrix)

ui <- navbarPage("GCGLM Study Planner App",
                 tabPanel("Simulation",
                          # Create a sidebar layout with side panel and main panel
                          sidebarLayout(

                            # Side panel
                            sidebarPanel(

                              h3("Generate GCGLM Data"),

                              wellPanel(
                                h4("Dataset Options"),
                                numericInput("sampleSize", "Sample Size:", value = 50, min = 1, max = 1000)
                              ),

                              wellPanel(
                                h4("Covariate Options"),
                                numericInput("numSelectCovariates", "Number of covariates:", value = 2, min = 1, max = 10),
                                uiOutput("dynamicCovariates"),
                                checkboxInput("scale", "Scale covariates?")
                              ),

                              wellPanel(
                                h4("Output Options"),
                                numericInput("numSelectOutcomes", "Number of outputs:", value = 1, min = 1, max = 10),
                                uiOutput("dynamicOutcomes"),
                                uiOutput("outcomeCorrelationMatrix")
                              ),

                              # Add a well panel with inputs
                              actionButton("simulate", "Simulate", width = "100%")
                            ),

                            # Main panel
                            mainPanel(
                              textOutput("statusBar"),
                              tableOutput("table")
                            )
                          )
                 ),
                 tabPanel("Analysis"),
                 tabPanel("Help")
)
