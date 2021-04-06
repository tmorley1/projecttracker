tabPanel("Edit info", #button in navigation panel
         h1(strong("Project Tracker"), align="center"),
         fluidRow(
           column(12,
                  h2("Team Members")
           )
         ),
         fluidRow(
           column(4,
                  DT::dataTableOutput("people"),
                  br(),
                  actionButton("addPerson", "Add New Team Member") #saves changes made to the editable table
           ),
           column(2,
                  conditionalPanel(
                    condition = "input.people_rows_selected !=  0",
                    br(),
                    br(),
                    actionButton("editPerson", "Edit details for selected team member"), # pop up form opens
                    br(),
                    br(),
                    actionButton("removePerson", "Remove Team Member") #pop up form opens 
                  ) 
                  
           )),
         fluidRow(
           column(12,
                  h2("Projects")  
           )
         ),
         fluidRow(
           column(10,
                  dataTableOutput("project"),
                  br(),
                  actionButton("addProject", "Insert New Project"),
                  br(),
                  br()
           ),
           column(2,
                  conditionalPanel(
                    condition = "input.project_rows_selected !=  0",
                    br(),
                    br(),
                    actionButton("editProject", "Edit details for selected project"), # pop up form opens
                    br(),
                    br(),
                    actionButton("removeProject", "Delete Project"), #pop up form opens
                    br(),
                    br(),
                    actionButton("projectDetails", "Details of Project")
                  )
           )
         )
)
