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
           conditionalPanel("output.projectsNumberjustnumber == 'FALSE'",
                            conditionalPanel("input.newcompleted3 == 'Live Projects'",
                                             (column(12,plotOutput("newganttChartlive")
         ))
                            ),
         conditionalPanel("input.newcompleted3 != 'Live Projects'",
                          (column(12,plotOutput("newganttChartcompleted")
                          ))
                          )
         )),
         
         fluidRow(
           conditionalPanel("input.newcompleted3 == 'Live Projects'",
             column(12,
                  DT::dataTableOutput("newTimeLineTablelive"),
                  conditionalPanel(condition = "input.newTimeLineTablelive_rows_selected !=  0",
                                   actionButton("newprojectDetailsTimelinelive", "Details of Project"))
           )
         )
),

          fluidRow(
              conditionalPanel("input.newcompleted3 != 'Live Projects'",
                   column(12,
                          DT::dataTableOutput("newTimeLineTablecompleted"),
                          conditionalPanel(condition = "input.newTimeLineTablecompleted_rows_selected !=  0",
                                           actionButton("newprojectDetailsTimelinecompleted", "Details of Project"))
                   )
  )
),
)
         