##TIMELINE UI PAGE OF PROJECT TRACKER

tabPanel("New Timeline",
         h1("New Timeline"),
         
         fluidRow(
           tags$head(tags$style("#optionsPanel{background-color: #e6e6e6; padding:15px; vertical-align: middle}")),
           box( width=12,
                div(id="optionsPanel",
                    fluidRow(
                      column(2,
                             uiOutput("newTimelineTeamSelector")
                      ),
                      column(2,
                             uiOutput("newTimelineTeamMemberSelector")
                      ),
                      column(2,
                             uiOutput("newTimelineCompletedProjects")
                      )
                    )
                )
           )
         ),
         
         fluidRow(
           conditionalPanel("output.projectsNumberjustnumber == 'FALSE'",(column(12,
                  plotOutput("newganttChart")
         ))
         )),
         
         fluidRow(
           column(12,
                  DT::dataTableOutput("newTimeLineTable"),
                  conditionalPanel(condition = "input.newTimeLineTable_rows_selected !=  0",
                                   actionButton("newprojectDetailsTimeline", "Details of Project"))
           )
         )
)
         