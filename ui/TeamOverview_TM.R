tabPanel("Team Overview",
         h1("Team Overview"),
         br(),
         
         
         fluidRow(
           tags$head(tags$style("#optionsPanel{background-color: #e6e6e6; padding:15px; vertical-align: middle}")),
           box( width=12,
                div(id="optionsPanel",
                    fluidRow(
                      column(1,
                             uiOutput("teamsSelector")     
                      ),
                      column(2,
                             uiOutput("completedSelector")     
                      ),
                      conditionalPanel("input.completed != 'Live Projects'",
                                       column(2,
                                              selectInput("sinceCompletedTeamOverview", "Completed projects in the: ", c("Last Month", "Last 6 Months", "Last year", "Last 3 years", "All"))
                                              )
                                       ),
                      conditionalPanel("input.completed == 'Live Projects'",
                                       column(1,
                                              br()
                                       )
                      ),
                      column(6,
                             p("All team members in the selected teams:" , style="font-weight: bold; align: center"),
                             p(textOutput("peopleInTeam"), align= 'center')
                             
                      )
                    )
                )
                
           )           
           
           
         ),
         
         # Live Projects -----------------------------------------------------------
         
         conditionalPanel("input.completed == 'Live Projects'",
                          h2("Live Projects", style="text-decoration: underline;"),
                          box( width=12,
                               fluidRow(
                                 column(6,
                                        h3("Number of Projects"),
                                        fluidRow(
                                          uiOutput("liveNumberOfProjects", style="background-color: #e6e6e6;")
                                        )
                                 ),
                                 column(6,
                                        h3("KPIs"),
                                        fluidRow(
                                          column(6,
                                                 conditionalPanel(
                                                   condition = "output.liveQALogKPIcolour == 'FALSE'",
                                                   uiOutput("liveQALogKPI", style="Background-color: #f7e1e1;")
                                                 ),
                                                 conditionalPanel(
                                                   condition = "output.liveQALogKPIcolour != 'FALSE'",
                                                   uiOutput("liveQALogKPI2", style="Background-color: #d4f7d2;")
                                                 )
                                          ),
                                          column(6,
                                                 conditionalPanel(
                                                   condition = "output.liveProjectBriefKPIcolour == 'FALSE'",
                                                   uiOutput("liveProjectBriefKPI", style="Background-color: #f7e1e1;")
                                                 ),
                                                 conditionalPanel(
                                                   condition = "output.liveProjectBriefKPIcolour != 'FALSE'",
                                                   uiOutput("liveProjectBriefKPI2", style="Background-color: #d4f7d2;")
                                                 )
                                          )
                                        )
                                 ))),
                          
                          fluidRow(
                            
                            column(6,
                                   tags$head(tags$style("#blackBorder{padding: 0.5%; border: solid; align=center}")),
                                   fluidRow(
                                     plotlyOutput("RAGPie", width="80%", inline = F)
                                   ),
                                   fluidRow(
                                     uiOutput("RAG_Status")
                                   )
                            ),
                            column(6,
                                   plotlyOutput("noProjectPerPerson"),
                                   uiOutput("teamOverviewPersonSelector"),
                                   DT::dataTableOutput("teamOverviewPerson"),
                                   br(),
                                   conditionalPanel(
                                     "input.teamOverviewPerson_rows_selected !=  0",
                                     actionButton("projectDetailsTeamOverview", "Details of Project")  
                                   ),
                                   br()
                                   
                            )
                            
                          )
         ),
         
         
         
         
         # Completed Projects ------------------------------------------------------
         
         
         conditionalPanel("input.completed != 'Live Projects'",
                          h2("Completed Projects", style="text-decoration: underline;"),
                          box(width=12,
                            fluidRow(
                              column(3,
                                     h3("Number of Projects"),
                                     fluidRow(
                                       uiOutput("completedNumberOfProjects", style="background-color: #e6e6e6;")
                                     )
                              ),
                              column(9,
                                     h3("KPIs"),
                                     fluidRow(
                                       column(4,
                                              conditionalPanel(
                                                condition = "output.completedQALogKPIcolour == 'FALSE'",
                                                uiOutput("completedQALogKPI", style="Background-color: #f7e1e1;")
                                              ),
                                              conditionalPanel(
                                                condition = "output.completedQALogKPIcolour != 'FALSE'",
                                                uiOutput("completedQALogKPI2", style="Background-color: #d4f7d2;")
                                              )
                                       ),
                                       column(4,
                                              conditionalPanel(
                                                condition = "output.completedProjectBriefKPIcolour == 'FALSE'",
                                                uiOutput("completedProjectBriefKPI", style="Background-color: #f7e1e1;")
                                              ),
                                              conditionalPanel(
                                                condition = "output.completedProjectBriefKPIcolour != 'FALSE'",
                                                uiOutput("completedProjectBriefKPI2", style="Background-color: #d4f7d2;")
                                              )
                                       ),
                                       column(4,
                                              conditionalPanel(
                                                condition = "output.deadlineMetKPIcolour == 'FALSE'",
                                                uiOutput("deadlineMetKPI", style="Background-color: #f7e1e1;")
                                              ),
                                              conditionalPanel(
                                                condition = "output.deadlineMetKPIcolour != 'FALSE'",
                                                uiOutput("deadlineMetKPI2", style="Background-color: #d4f7d2;")
                                              )
                                       )
                                     )
                                     )
                              ),
                            fluidRow(
                              column(6,
                                     plotlyOutput("customerTreeMap")
                              )
                            ),
                            fluidRow(
                              column(12,
                                     uiOutput("customers")
                                     )
                              )
                          )
                          )
         )


