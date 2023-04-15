ui <- navbarPage("GCGLM Study Planner App",
                 tabPanel("Simulation",
                          # Create a sidebar layout with side panel and main panel
                          sidebarLayout(

                            # Side panel
                            sidebarPanel(

                              h3("Generate GCGLM Data"),

                              wellPanel(
                                h4("Dataset Options"),
                                numericInput("sampleSize", "Sample Size:", value = 50, min = 1, max = 1000),
                                numericInput("numSelectCovariates", "Number of covariates:", value = 2, min = 1, max = 10),
                                numericInput("numSelectOutcomes", "Number of outputs:", value = 2, min = 1, max = 10)
                              ),

                              wellPanel(
                                h4("Covariate Distributions"),
                                uiOutput("dynamicCovariates"),
                                checkboxInput("scale", "Scale covariates?"),
                                uiOutput("covariateCorrelationMatrix")
                              ),

                              wellPanel(
                                h4("Outcome Distributions"),
                                uiOutput("dynamicOutcomes"),
                                uiOutput("dynamicExtraParameters"),
                                uiOutput("outcomeCorrelationMatrix")
                              ),

                              wellPanel(
                                h4("Regression Parameters"),
                                uiOutput("glmParameterMatrix")
                              ),

                              # Add a well panel with inputs
                              actionButton("simulate", "Simulate", width = "100%")
                            ),

                            # Main panel
                            mainPanel(
                              plotOutput("outcomeScatter"),
                              plotOutput("covariateScatter")
                            )
                          )
                 ),
                 tabPanel("Analysis",
                          sidebarLayout(
                            sidebarPanel(),
                            mainPanel(
                              textOutput("statusBar")
                            )
                          )),
                 tabPanel("Help")
)
