ui <- navbarPage("GCGLM Study Planner App",
                 tabPanel("Simulation",
                          # Create a sidebar layout with side panel and main panel
                          sidebarLayout(

                            # Side panel
                            sidebarPanel(

                              h3("Generate GCGLM Data"),

                              wellPanel(
                                h4("Dataset Options"),
                                numericInput("sampleSize", "Sample Size:", value = 100, min = 1, max = 1000),
                                numericInput("numSelectCovariates", "Number of covariates:", value = 3, min = 1, max = 10),
                                numericInput("numSelectOutcomes", "Number of outputs:", value = 3, min = 1, max = 10)
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
                            sidebarPanel(
                              wellPanel(
                                h4("MCMC Options"),
                                numericInput("numChains", "Number of chains:", value = 4, min = 1, max = 4),
                                numericInput("burnin", "Burn-in:", value = 1000, min = 1, max = 1000),
                                numericInput("iters", "Iterations:", value = 1000, min = 1, max = 10000)
                              ),
                              actionButton("analyze", "Analyze Data", width = "100%")
                            ),
                            mainPanel(
                              textOutput("statusBar"),
                              plotOutput("betasHistograms")
                              # verbatimTextOutput("test")
                            )
                          ))
)
