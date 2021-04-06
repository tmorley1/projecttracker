tabPanel("Timeline",
         h1("Timeline"),
         
         fluidRow(
           tags$head(tags$style("#optionsPanel{background-color: #e6e6e6; padding:15px; vertical-align: middle}")),
           box( width=12,
                div(id="optionsPanel",
                    fluidRow(
                      column(2,
                             uiOutput("TimelineTeamSelector")
                      ),
                      column(2,
                             uiOutput("TimelineTeamMemberSelector")
                      ),
                      column(2,
                             uiOutput("TimelineCompletedProjects"),
                             
                      )
                    )
                )
           )
         ),
         fluidRow(
           column(12,
                  conditionalPanel(
                    "input.completed3 == 'Completed Projects'",
                    br(),
                    selectInput("sinceCompletedTimeline", "Completed projects in the: ", c("Last Month", "Last 6 Months", "Last year", "Last 3 years", "All")))
                    ,
                  )
                           ),
         # Place to select the person you want an overview of
         fluidRow(
           column(12,
                  br(),
                  plotOutput("ganntChart"),
                  br(),
                  DT::dataTableOutput("TimeLineTable"),
                  conditionalPanel(condition = "input.TimeLineTable_rows_selected !=  0",
                                   actionButton("projectDetailsTimeline", "Details of Project")
                                   )
                  # table of projects that the selected person is involved in
                  #h2("Projects", style="text-decoration: underline;"),
           )
         )

#                   
#            )
#            
#          )
)
