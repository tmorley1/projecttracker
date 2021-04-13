##TIMELINE UI PAGE OF PROJECT TRACKER

tabPanel("New Timeline",
         h1("New Timeline"),
         fluidRow(
           column(12,
                  plotOutput("newganttChart")
         )
         ),
         
         fluidRow(
           column(12,
                  DT::dataTableOutput("newTimeLineTable"),
           )
         ),
)
         