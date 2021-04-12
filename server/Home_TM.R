# Setting up section ------------------------------------------------------

# Rendering the 2 tables and re-establishing the functions
output$people <- DT::renderDataTable(peopleData() %>% filter(CurrentlyInTeam == "TRUE") %>% select(-CurrentlyInTeam), 
                                     server = FALSE, selection='single',
                                     extensions = c('Responsive'))

output$project<- DT::renderDataTable(
  dat1 <-datatable(projectData()%>%
                     select(-Comments, -Documentation)%>%
                     mutate(in_team = ifelse(TeamMembers %in% list_current(), T, F))%>%
                     mutate(incomplete = ifelse(Completed == F & in_team == F, T, F))%>%
                     select(-in_team), options=list(columnDefs = list(list(visible=FALSE, targets=c(12)))),
 selection='single')%>%
  formatStyle('incomplete', target='row', backgroundColor=styleEqual(c(T,F), c('red', ''))))

peopleData <- function() {
  peopleData <- as.data.frame(read.csv(paste(dataPathway, "People.csv", sep="")))
  peopleData
}

people_current <- function() {
  people_current<- peopleData()%>%
    filter(CurrentlyInTeam == TRUE)%>%
    select(-CurrentlyInTeam,-Team)
  people_current
}

list_current <- function() {
  list_current <- list(people_current()$Name)[[1]]
  list_current
}

projectData <- function() {
  projectData <- as.data.frame(read.csv(paste(dataPathway, "Projects.csv", sep="")))
  projectData
}
 

# Reactive value that changes whenever a confirm button is clicked (so whenever someone edits the data files)
# This is called when creating the datasets for the overview pages so they are updated whenever edits are made to the files

confirmingButtons <- reactive({
  input$ConfirmNewPerson + input$ConfirmPerson + input$ConfirmRemovalOfPerson + input$ConfirmRemovalOfProject + input$ConfirmProject + input$ConfirmNewProject + input$amendDatesConfirm
})



# functions to save changed data to file from the home page
# removed comments from visibility for now
saveChangesToPeopleFile <- function(data) {
  if (file.exists(paste(dataPathway, "People.csv", sep=""))){
    file.remove(paste(dataPathway, "People.csv", sep=""))
    write.csv(data, paste(dataPathway, "People.csv", sep=""), row.names=FALSE)
    output$people <-  DT::renderDataTable(peopleData() %>% filter(CurrentlyInTeam == "TRUE") %>% select(-CurrentlyInTeam), server = FALSE, selection='single')
    peopleData <<- peopleData()
  }
}
saveChangesToProjectFile <- function(data) {
  if (file.exists(paste(dataPathway, "Projects.csv", sep=""))){
    file.remove(paste(dataPathway, "Projects.csv", sep=""))
    write.csv(data, paste(dataPathway, "Projects.csv", sep=""), row.names=FALSE)
    output$project <- DT::renderDataTable(data %>% select(-Comments, -Documentation), server = FALSE, selection='single')
    projectData <<- projectData()
  }
}

# Adding a new team member ------------------------------------------------

# booleans for error messages
newNameExists <- reactive(tolower(input$addPersonForm) %in% tolower(peopleData()$Name))

output$newNameHasBeenRemoved <- renderText({
  if (newNameExists()) {
    if(nrow((peopleData() %>% filter(tolower(Name) == tolower(input$addPersonForm) & !CurrentlyInTeam)))>0) {
      TRUE
    } else{
      FALSE
    }
  } else { 
    FALSE
  }
})

outputOptions(output, "newNameHasBeenRemoved", suspendWhenHidden=FALSE)

output$newNameExists <- renderText({ tolower(input$addPersonForm) %in% tolower(peopleData()$Name) }) # Checking new name isn't already in table
outputOptions(output, "newNameExists", suspendWhenHidden=FALSE) # keeps value, even if values isn't outputted in the table

output$newTeamNameBlank <- renderText(input$addPersonForm == "") 
outputOptions(output, "newTeamNameBlank", suspendWhenHidden=FALSE)

#Adding person form
observeEvent(input$addPerson, {
  showModal(modalDialog(
    title = "Add a team member",
    textInput("addPersonForm", "Name: "),
    conditionalPanel(
      condition = "output.newNameExists == 'TRUE' & output.newNameHasBeenRemoved != 'TRUE'",
      h5("Team member is already in list")
    ),
    conditionalPanel(
      condition = "output.newNameHasBeenRemoved == 'TRUE'",
      h5("This Team Member was previously deleted, to reinstate them click confirm"),
      h5("Otherwise to avoid merging the 2 different team members' projects together, please choose a different name")
    ),
    conditionalPanel(
      condition = "output.newTeamNameBlank == 'TRUE'",
      h5("Add a name!")
    ),
    textInput("addPersonForm2", "Team:"),
    h5("If you are a member of multiple teams, please separate each team name with a ',' and no spaces"),
    textInput("addPersonForm3", "Email:"),
    textInput("addPersonForm4", "Location:"),
    textInput("addPersonForm4", "Grade"),
    conditionalPanel(
      condition = "( (output.newNameExists != 'TRUE') | (output.newNameHasBeenRemoved == 'TRUE' & output.newNameExists == 'TRUE')) & output.newTeamNameBlank != 'TRUE' ",
      actionButton("ConfirmNewPerson", "Confirm")
    )
    
  ))
})

#code to add new person to list in response to clicking confirm on form
observeEvent(input$ConfirmNewPerson, {
  if(!newNameExists()) {
    peopleData <- peopleData() %>%
      rbind(data.frame(Name = c(input$addPersonForm), 
                       Team=c(input$addPersonForm2), 
                       CurrentlyInTeam=c("TRUE"), 
                       Email=c(input$addPersonForm3), 
                       Location=c(input$addPersonForm4), 
                       Grade=c(input$addPersonForm5)))
    #saves changes to csv file and re-renders table
  }
  else { 
    # if reinstating a team member deleting the original row and adding a new row where they are currently in the team
    peopleData <- peopleData() %>% 
      filter(tolower(Name)!=tolower(input$addPersonForm)) %>%
      rbind(data.frame(Name = c(input$addPersonForm), 
                       Team=c(input$addPersonForm2), 
                       CurrentlyInTeam=c("TRUE"), 
                       Email=c(input$addPersonForm3),
                       Location=c(input$addPersonForm4),
                       Grade=c(inputer$addPersonForm5)))
  }
  #reads data from csv file (peopleData() function), then attatches new person to table
  saveChangesToPeopleFile(peopleData)
  removeModal()
})

# Editing a team members details ------------------------------------------


# booleans for error messages
output$editedNameExists <- renderText(tolower(input$editPersonForm) %in% tolower((peopleData()%>% filter(CurrentlyInTeam))[-input$people_rows_selected,]$Name)) # same as above ( but not including the person you are editing)
outputOptions(output, "editedNameExists", suspendWhenHidden=FALSE)

output$editedTeamNameBlank <- renderText(input$editPersonForm == "") 
outputOptions(output, "editedTeamNameBlank", suspendWhenHidden=FALSE)

editedNameExists <- reactive(tolower(input$editPersonForm) %in% tolower(c(as.character((peopleData()%>% filter(CurrentlyInTeam))[-input$people_rows_selected,]$Name),as.character((peopleData()%>% filter(!CurrentlyInTeam))$Name))))
output$editedNameHasBeenRemoved <- renderText({
  if (editedNameExists()) {
    if(input$editPersonForm %in% as.character((peopleData()%>% filter(!CurrentlyInTeam))$Name)) {
      TRUE
    } else {FALSE}
  } else {FALSE}
})
outputOptions(output, "editedNameHasBeenRemoved", suspendWhenHidden=FALSE)

#edit person form
observeEvent(input$editPerson, {
  peopleData <- peopleData() %>% filter(CurrentlyInTeam)
  showModal(modalDialog(
    title = "Edit a team member",
    textInput("editPersonForm", "Name: ", value = peopleData[input$people_rows_selected,1]),
    
    # error message
    conditionalPanel(
      condition = "output.editedNameExists == 'TRUE' & output.editedNameHasBeenRemoved != 'TRUE'",
      h5("Team member is already in list")
    ),
    conditionalPanel(
      condition = "output.editedTeamNameBlank == 'TRUE'",
      h5("Add a name!")
    ),
    conditionalPanel(
      condition = "output.editedNameHasBeenRemoved == 'TRUE'",
      h5("A previous team member had the same name as the one you have entered"),
      h5("Please enter a different name to avoid merging 2 different people's projects being merged together")
    ),
    textInput("editPersonForm2", "Team:", value = peopleData[input$people_rows_selected,2]),
    textInput("editPersonForm3", "Email:", value = peopleData[input$people_rows_selected,4]),
    textInput("editPersonForm4", "Location:", value=peopleData[input$people_rows_selected,5]),
    textInput("editPersonForm5", "Grade:", value=peopleData[input$people_rows_selected,6]),
    #error message
    conditionalPanel(
      condition = "output.editedNameExists != 'TRUE' & output.editedTeamNameBlank != 'TRUE'", #  & output.editedNameHasBeenRemoved != 'TRUE'", - this isn't needed?????
      actionButton("ConfirmPerson", "Confirm")
    )
  ))
})

#code to edit person in list in response to clicking confirm on form
observeEvent(input$ConfirmPerson, {
  newPeopleData <- peopleData() %>% 
    filter(CurrentlyInTeam) %>%
    mutate_all(as.character)
  newPeopleData[input$people_rows_selected, 1] <- as.character(input$editPersonForm)
  newPeopleData[input$people_rows_selected, 2] <- as.character(input$editPersonForm2)
  newPeopleData[input$people_rows_selected, 3] <- "TRUE"
  newPeopleData[input$people_rows_selected, 4] <- as.character(input$editPersonForm3)
  newPeopleData[input$people_rows_selected, 5] <- as.character(input$editPersonForm4)
  newPeopleData[input$people_rows_selected, 6] <- as.character(input$editPersonForm5)
  
  newPeopleData <- newPeopleData %>% rbind(peopleData() %>% filter(!CurrentlyInTeam))
  projectData <- projectData() %>%
    mutate_all(as.character) %>%
    mutate(
      TeamMembers = str_replace_all(TeamMembers, as.character(peopleData()[input$people_rows_selected,1]), input$editPersonForm)
    )
  saveChangesToPeopleFile(newPeopleData)
  saveChangesToProjectFile(projectData)
  removeModal()
})

# Removing a team member --------------------------------------------------

#removing person form
observeEvent(input$removePerson, {
  showModal(
    modalDialog(
      h4("Are you sure you want to remove this team member?"),
      actionButton("ConfirmRemovalOfPerson", "Confirm")
    )
  )
})

#code to remove person from list in response to clicking confirm on form
observeEvent(input$ConfirmRemovalOfPerson, {
  #remove person from people table
  newPeopleData <- peopleData() %>% filter(CurrentlyInTeam)
  personToRemove <-newPeopleData %>% filter(tolower(Name) == tolower(newPeopleData[input$people_rows_selected,1]))
  personToRemove[1,3] <- "FALSE"
  
  newPeopleData <- newPeopleData %>% filter(tolower(Name) != tolower(newPeopleData[input$people_rows_selected,1])) %>%
    rbind(personToRemove) %>%
    rbind(peopleData() %>% filter(!CurrentlyInTeam))
  
  
  saveChangesToPeopleFile(newPeopleData)
  removeModal()
  
  
})


# Adding a new project ----------------------------------------------------

# boolean as to whether error message is needed - project name already exists
output$newProjectNameExists <- renderText(tolower(input$addProjectForm) %in% tolower(projectData()$Name))
outputOptions(output, "newProjectNameExists", suspendWhenHidden=FALSE)

# boolean as to whether error message is needed - team members do not exist
output$newTeamMembersDoNotExist <- renderText(
  !(((length(unlist(str_split(input$addProjectForm2, ", "))[!(unlist(str_split(input$addProjectForm2, ", ")) %in% peopleData()$Name)]) == 0))||(input$addProjectForm2==""))
)
outputOptions(output, "newTeamMembersDoNotExist", suspendWhenHidden=FALSE)

output$newProjectNameBlank <- renderText(input$addProjectForm == "") 
outputOptions(output, "newProjectNameBlank", suspendWhenHidden=FALSE)

#Form for adding a project
observeEvent(input$addProject, {
  showModal(
    modalDialog(
      textInput("addProjectForm", "Project Name: "),
      
      #error message
      conditionalPanel(
        condition = "output.newProjectNameExists == 'TRUE'",
        h5("This name is already taken, please choose another one")
      ),
      conditionalPanel(
        condition = "output.newProjectNameBlank == 'TRUE'",
        h5("Add a name!")
      ),
      textInput("addProjectForm2", "Names of the people working on the project (separated by a comma): "),
      
      #error message
      conditionalPanel(
        condition = "output.newTeamMembersDoNotExist == 'TRUE'",
        h5("Team member(s) listed above does not exist, please try again. ")
      ),
      selectInput("addProjectForm10", "Customer group: ", c("Tech", "AC", "HR", "Other"), selected=NULL),
      textInput("addProjectForm3", "Customer: "),
      checkboxInput("addProjectForm4", "Tick if this project has a brief"),
      checkboxInput("addProjectForm5", "Tick if the there is a QA log for this project" ),
      dateInput("addProjectForm6", "Date of deadline: "),
      selectInput("addProjectForm7", "RAG Rating: ", c("R", "R-A", "A", "A-G", "G"), selected=NULL),
      textInput("addProjectForm9", "Link to where the documentation for this project is stored: "),
      checkboxInput("addProjectForm8", "Tick if the Project has been completed ", value=FALSE),
      
      # Do not allow the confirm button to be shown unless there are no error messages 
      conditionalPanel(
        condition = "output.newTeamMembersDoNotExist != 'TRUE' & output.newProjectNameExists != 'TRUE' & output.newProjectNameBlank != 'TRUE' & output.newProjectNameBlank != 'TRUE'",
        actionButton("ConfirmNewProject", "Confirm")
      )
      
    )
  )
})

# code for adding new project
observeEvent(input$ConfirmNewProject, {
  projectData <- rbind(projectData() %>% mutate_all(as.character), data.frame(Name = c(input$addProjectForm), TeamMembers=c(input$addProjectForm2), Customer=c(input$addProjectForm3), ProjectBrief=c(input$addProjectForm4), QALog=c(input$addProjectForm5), Deadline=c(format(input$addProjectForm6, "%d/%m/%Y")), RAG=c(input$addProjectForm7), Completed=c(input$addProjectForm8), Comments= c(""), Documentation=c(input$addProjectForm9), DateCompleted = c(""),StartDate=c(format(Sys.Date(), "%d/%m/%Y")), CustomerGroup = c(input$addProjectForm10)))
  saveChangesToProjectFile(projectData)
  removeModal()
})

# Editing a project -------------------------------------------------------

#boolean: name already exists (- project being edited)
output$editedProjectNameExists <- renderText(tolower(input$editProjectForm) %in% tolower(projectData()[-input$project_rows_selected,]$Name)) 
outputOptions(output, "editedProjectNameExists", suspendWhenHidden=FALSE)

# boolean: team members do not exist
output$editedProjectTeamMembersDoNotExist <- renderText(
  !(((length(unlist(str_split(input$editProjectForm2, ", "))[!(unlist(str_split(input$editProjectForm2, ", ")) %in% peopleData()$Name)]) == 0))||(input$editProjectForm2==""))
)
outputOptions(output, "editedProjectTeamMembersDoNotExist", suspendWhenHidden=FALSE)

output$editedProjectNameBlank <- renderText(input$editProjectForm == "") 
outputOptions(output, "editedProjectNameBlank", suspendWhenHidden=FALSE)

# Form for editing project
observeEvent(input$editProject, {
  project <<- projectData() %>% filter(Name == projectData()[input$project_rows_selected,1])
  projectData <<- projectData() # using right edit values
  showModal(
    modalDialog(
      h2("Edit: ", span(project[1,1], style="font-weight:bold")),
      textInput("editProjectForm", "Project Name: ", value=projectData[input$project_rows_selected,1]),

      # error message
      conditionalPanel(
        condition = "output.editedProjectNameExists == 'TRUE'",
        h5("This name is already taken, please choose another one")
      ),
      conditionalPanel(
        condition = "output.editedProjectNameBlank == 'TRUE'",
        h5("Add a name!")
      ),
      
      textInput("editProjectForm2", "Names of the people working on the project (separated by a comma): ", value=projectData[input$project_rows_selected,2]),

      # error message
      conditionalPanel(
        condition = "output.editedProjectTeamMembersDoNotExist == 'TRUE'",
        h5("Team member(s) listed above does not exist, please try again. ")
      ),
      selectInput("editProjectForm7", "RAG Rating: ", c("R", "R-A", "A", "A-G", "G"), selected=projectData[input$project_rows_selected,7]),
      
      h4("Customer:", style="background-color: #e95420; color:white"),
      selectInput("editProjectForm10", "Customer group: ", c("Tech", "AC", "HR", "Other"), selected=projectData[input$project_rows_selected,13]),
      textInput("editProjectForm3", "Customer: ", value=projectData[input$project_rows_selected,3]),
      
      h4("Documentation:", style="background-color: #e95420; color:white"),
      checkboxInput("editProjectForm4", "Tick if this project has a brief", value=projectData[input$project_rows_selected,4]),
      checkboxInput("editProjectForm5", "Tick if the there is a QA log for this project" , value=projectData[input$project_rows_selected,5]),
      textInput("editProjectForm9", "Link to where the documentation for this project is stored: ", value = projectData[input$project_rows_selected,10]),
      
      h4("Completing the project:", style="background-color: #e95420; color:white"),
      dateInput("editProjectForm6", "Date of deadline: ", value=as.Date(projectData[input$project_rows_selected,6], format="%d/%m/%Y")),
      checkboxInput("editProjectForm8", "Tick if the Project has been completed ", value=projectData[input$project_rows_selected,8]),
      # Only show confirm button if there are no errors
      conditionalPanel(
        condition = "output.editedProjectTeamMembersDoNotExist != 'TRUE' & output.editedProjectNameExists != 'TRUE' & output.editedProjectNameBlank != 'TRUE'",
        actionButton("ConfirmProject", "Confirm")
      )
    )
  )

})

observeEvent(input$ConfirmProject, {
  projectData <- projectData() %>%
    mutate_all(as.character)
  if(projectData[input$project_rows_selected,8] == FALSE && input$editProjectForm8==TRUE) {
    projectData[input$project_rows_selected,11] <- format(Sys.Date(), "%d/%m/%Y")
  }
  if(input$editProjectForm8==FALSE) {
    projectData[input$project_rows_selected,11] <- ""
  }
  projectData[input$project_rows_selected,1] <- as.character(input$editProjectForm)
  projectData[input$project_rows_selected,2] <- as.character(input$editProjectForm2)
  projectData[input$project_rows_selected,3] <- as.character(input$editProjectForm3)
  projectData[input$project_rows_selected,4] <- as.character(input$editProjectForm4)
  projectData[input$project_rows_selected,5] <- as.character(input$editProjectForm5)
  projectData[input$project_rows_selected,6] <- format(input$editProjectForm6, "%d/%m/%Y") # saving data input as character
  projectData[input$project_rows_selected,7] <- as.character(input$editProjectForm7)
  projectData[input$project_rows_selected,8] <- as.character(input$editProjectForm8)
  projectData[input$project_rows_selected,10] <- as.character(input$editProjectForm9)
  projectData[input$project_rows_selected,13] <- as.character(input$editProjectForm10)
  
  saveChangesToProjectFile(projectData)
  removeModal()
})

# Removing a project ------------------------------------------------------

#form for removing projects
observeEvent(input$removeProject, {
  showModal(
    modalDialog(
      h4("Are you sure you want to remove this project?"),
      actionButton("ConfirmRemovalOfProject", "Confirm")
    )
  )
})

# code for removing a project
observeEvent(input$ConfirmRemovalOfProject, {
  projectData <- projectData()[-input$project_rows_selected,]
  saveChangesToProjectFile(projectData)
  removeModal()
})


# Project Details ---------------------------------------------------------


observeEvent(input$projectDetails, {
  project <<- projectData() %>% filter(Name == projectData()[input$project_rows_selected,1])
  projectData <- projectData() %>% mutate_all(as.character)# using right edit values
  teamMembers <- data.frame(team_members = unlist(strsplit(projectData[input$project_rows_selected,2], ", |,")))
  comments <<- data.frame(Comments = unlist(strsplit(as.character(projectData[input$project_rows_selected,9]),";"))) %>% mutate_all(as.character)
  
  output$projectComments <- renderUI({
    apply(comments, 1, function(x) tags$li(x['Comments']))
  })
  
  output$list <- renderUI({
    apply(teamMembers, 1, function(x) tags$li(x['team_members']))
  })
  
  documentationText <- if(as.logical(projectData[input$project_rows_selected,4]) & as.logical(projectData[input$project_rows_selected,5])) {
    c("This project has both a project brief and a QA Log", "")
  } else {
    if(!as.logical(projectData[input$project_rows_selected,4]) & !as.logical(projectData[input$project_rows_selected,5])){
      c("This project currently has NO documentation associated with it", "color: red; font-weight: bold")
    } else {
      if (as.logical(projectData[input$project_rows_selected,4])) {
        c("This project currently has a project brief but no QA log", "color: red")
      } else {
        c("This project has a QA log but no project brief", "color: red")
      }
    }
  }
  
  
  daysToDeadline <- 
    if(as.Date(projectData[input$project_rows_selected,6], "%d/%m/%Y")< Sys.Date()){ 
      -sum(!weekdays(seq(as.Date(projectData[input$project_rows_selected,6], "%d/%m/%Y"), Sys.Date(), "days")) %in% c("Saturday", "Sunday"))
    } else {
      sum(!weekdays(seq(Sys.Date(), as.Date(projectData[input$project_rows_selected,6], "%d/%m/%Y"), "days")) %in% c("Saturday", "Sunday"))
    }
  
  deadlineText <- 
    if(!as.logical(projectData[input$project_rows_selected,8])){
      if (as.Date(projectData[input$project_rows_selected,6], "%d/%m/%Y") == Sys.Date()){
        c("This project is due today!", "color:red")
      } else {
        if (as.integer(daysToDeadline) < 0){
          c(paste("This project is ", as.integer(daysToDeadline)*-1, " working days overdue!"), "color:red")
        } else {
          paste("There are ", as.integer(daysToDeadline), " days till the deadline for this project")
        }
      }
    } else {
      if(as.Date(projectData[input$project_rows_selected,11], "%d/%m/%Y") <= as.Date(projectData[input$project_rows_selected,6], "%d/%m/%Y")) {
        c("This project was completed on time","")
      } else{
        c(paste("This Project was completed",
                as.integer(sum(!weekdays(seq(as.Date(projectData[input$project_rows_selected,6], "%d/%m/%Y"), as.Date(projectData[input$project_rows_selected,11], "%d/%m/%Y"), "days")) %in% c("Saturday", "Sunday"))),
                " working days after the specified deadline"
        ),
        "color:red")
      }
    } 
    
  
  output$noDocumentation <- renderText(is.na(projectData()[input$project_rows_selected,10]))
  outputOptions(output, "noDocumentation", suspendWhenHidden=FALSE)
  
  showModal(
    modalDialog(
      h2(projectData[input$project_rows_selected,1]),
      if(projectData[input$project_rows_selected,8] == "TRUE"){
        h4(span("Date Completed: ", style="font-weight:bold"), projectData[input$project_rows_selected,11], style="background-color: #e95420; color:white")
      } else {
        h4("This is a LIVE project", style="font-weight:bold; background-color: #e95420; color:white")
      },
      
      p("The team member(s) involved in this project are: "),
      tags$ul(
        uiOutput('list')
      ),
      p(span("Customer Group: ", style="font-weight:bold"), projectData[input$project_rows_selected,13]),
      p(span("Customer: ", style="font-weight:bold"), projectData[input$project_rows_selected,3]),
      p(span("Documentation: ", style="color: black; font-weight:bold"), documentationText[1], style= documentationText[2]),
      p(span("Start Date: ", style="font-weight:bold"), projectData[input$project_rows_selected,12]),
      p(span("Deadline: ", style="color: black; font-weight: bold"), projectData[input$project_rows_selected,6], " - ", deadlineText[1], style=deadlineText[2]),
      
      
    
      p(span("RAG Status: ", style="font-weight: bold"), projectData[input$project_rows_selected,7]),
      conditionalPanel(
        condition = "output.noDocumentation != 'TRUE'",
        p(span("Find documentation here: ", style="font-weight: bold"), tags$a(href=as.character(projectData[input$project_rows_selected,10]), "Click here!"))
      ),
      conditionalPanel(
        condition = "output.noDocumentation == 'TRUE'",
        p(span("Find documentation here: ", style="font-weight: bold"), "No link to the documentation for this project has been given")
      ),
      br(),
      p("Comments", style="font-weight:bold"),
      tags$ul(
        uiOutput('projectComments')
      ),
      textInput("commentadded", "Add a comment:"),
      actionButton("submitComment", "Submit"),
      br(),
      br(),
      actionLink("amendDates", "(Amend any dates here!)")
    )
  )
})

## Adding comment to comments for project and updating the modal
observeEvent(input$submitComment, {
  comments <<- comments %>%
    rbind(data.frame(Comments = c(input$commentadded)))
  
  output$projectComments <- renderUI({
    apply(comments, 1, function(x) tags$li(x['Comments']))
  })
  
  newProjectData <- projectData() %>% mutate_all(as.character)
  
  newProjectData[input$project_rows_selected,9] <- as.character(paste(comments$Comments, collapse=";"))
  saveChangesToProjectFile(newProjectData)
  projectData <- projectData() %>% mutate_all(as.character)
})

# When the action link is pressed, the project details modal will be closed and a new modal will appear
# with date inputs to change the start date, deadline and if the project is completed the deadline as well.

observeEvent(input$amendDates, {
  removeModal()
  showModal(
    modalDialog(
      h2("Amend the dates for the project: ", project[1,1]),
      dateInput("amendDatesinput1", "Start Date: ", value=as.Date(project[1,12], format="%d/%m/%Y")),
      dateInput("amendDatesinput2", "Date of deadline: ", value=as.Date(project[1,6], format="%d/%m/%Y")),
      if (as.logical(project[1,8])){
        dateInput("amendDatesinput3", "Date project was completed: ", value=as.Date(project[1,11], format="%d/%m/%Y"))
      },
      actionButton("amendDatesConfirm", "Confirm")
    )
  )
  
})

## The code to save the date inputs from the amend dates modal.
observeEvent(input$amendDatesConfirm, {
  removeModal()
  project <- project %>% mutate_all(as.character)
  project[1,12] <- format(input$amendDatesinput1, "%d/%m/%Y") 
  project[1,6] <- format(input$amendDatesinput2, "%d/%m/%Y") # saving data input as character
  if (as.logical(project[1,8])){
    project[1,11] <- format(input$amendDatesinput3, "%d/%m/%Y")
  }
  newProjectData <- projectData() %>% mutate_all(as.character) %>% filter(Name != project[1,1]) %>% rbind(project)
  saveChangesToProjectFile(newProjectData)

})
