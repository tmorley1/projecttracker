tabPanel("Person Overview",
         h1("Person Overview"),
         
         fluidRow(
           tags$head(tags$style("#optionsPanel{background-color: #e6e6e6; padding:15px; vertical-align: middle}")),
           box( width=12,
                div(id="optionsPanel",
                    fluidRow(
                      column(2,
                             uiOutput("teamMemberSelector")     
                      ),
                      column(2,
                             uiOutput("completedSelector2" )     
                      ),
                      column(2,
                             uiOutput("TeamMembersLiveProjects")
                      ),
                      column(2, 
                             uiOutput("TeamMembersCompletedProjects")
                             )
                    )
                )
           )
         ),
         # Place to select the person you want an overview of
         fluidRow(
           column(12,
                  br(),
                  conditionalPanel(
                    "output.completedInThePersonOverview == 'TRUE'",
                    selectInput("sinceCompletedPersonOverview", "Completed projects in the: ", c("Last Month", "Last 6 Months", "Last year", "Last 3 years", "All")),
                  ),
                  br()
           )
           ),
         fluidRow(
           column(9,
                  DT::dataTableOutput("personProject")
                  ),
           column(3,
                  conditionalPanel(condition = "input.personProject_rows_selected !=  0",
                                   actionButton("projectDetailsPersonOverview", "Details of Project")
                  )
                  )
         )
           
                  
                  
                  )

