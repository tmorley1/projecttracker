## Loading Libraries Needed
library(shiny)
library(tidyverse)
library(shinythemes) #theme -> css
library(plotly) # plotlyOutputs - graphs
library(DT)
library(stringr)
library(shinydashboard)
library(treemap)
library(lubridate)

username <- "tmorley"#change to be your username
dataPathway <- paste("C:\\Users\\", username, "\\OneDrive - Department for Education\\Documents\\Projects\\Project Tracker\\project_tracker\\Inputs\\", sep="")


# Functions ---------------------------------------------------------------


peopleData <- function() {
  peopleData <- as.data.frame(read.csv(paste(dataPathway, "People.csv", sep="")))
  peopleData
}

projectData <- function() {
  projectData <- as.data.frame(read.csv(paste(dataPathway, "Projects.csv", sep="")))
  projectData
}

projectDetails <- function(dataSource, inputRow) {
  project <<- projectData() %>% mutate_all(as.character) %>% filter(Name == dataSource[inputRow,1])# using right edit values
  teamMembers <- data.frame(team_members = unlist(strsplit(project[1,2], ", |,")))
  comments <<- data.frame(Comments = unlist(strsplit(as.character(project[1,9]),";"))) %>% mutate_all(as.character)
  
  documentationText <- if(as.logical(project[1,4]) & as.logical(project[1,5])) {
    c("This project has both a project brief and a QA Log", "")
  } else {
    if(!as.logical(project[1,4]) & !as.logical(project[1,5])){
      c("This project currently has NO documentation associated with it", "color: red; font-weight: bold")
    } else {
      if (as.logical(project[1,4])) {
        c("This project currently has a project brief but no QA log", "color: red")
      } else {
        c("This project has a QA log but no project brief", "color: red")
      }
    }
  }
  
  
  daysToDeadline <-
    if(as.Date(project[1,6], "%d/%m/%Y")< Sys.Date()){
      -sum(!weekdays(seq(as.Date(project[1,6], "%d/%m/%Y"), Sys.Date(), "days")) %in% c("Saturday", "Sunday"))
    } else {
      sum(!weekdays(seq(Sys.Date(), as.Date(project[1,6], "%d/%m/%Y"), "days")) %in% c("Saturday", "Sunday"))
    }
  
  deadlineText <-
    if(!as.logical(project[1,8])){
      if (as.Date(project[1,6], "%d/%m/%Y") == Sys.Date()){
        c("This project is due today!", "color:red")
      } else {
        if (as.integer(daysToDeadline) < 0){
          c(paste("This project is ", as.integer(daysToDeadline)*-1, " working days overdue!"), "color:red")
        } else {
          paste("There are ", as.integer(daysToDeadline), " days till the deadline for this project")
        }
      }
    } else {
      if(as.Date(project[1,11], "%d/%m/%Y") <= as.Date(project[1,6], "%d/%m/%Y")) {
        c("This project was completed on time","")
      } else{
        c(paste("This Project was completed",
                as.integer(sum(!weekdays(seq(as.Date(project[1,6], "%d/%m/%Y"), as.Date(project[1,11], "%d/%m/%Y"), "days")) %in% c("Saturday", "Sunday"))),
                " working days after the specified deadline"
        ),
        "color:red")
      }
    }
  
  showModal(
    modalDialog(
      h2(project[1,1]),
      
      if(project[1,8] == "TRUE"){
        h4(span("Date Completed: ", style="font-weight:bold"), project[1,11], style="background-color: #e95420; color:white")
      } else {
        h4("This is a LIVE project", style="font-weight:bold; background-color: #e95420; color:white")
      },
      
      p("The team member(s) involved in this project are: "),
      tags$ul(
        apply(teamMembers, 1, function(x) tags$li(x['team_members']))
      ),
      p(span("Customer Group: ", style="font-weight:bold"), project[1,13]),
      p(span("Customer: ", style="font-weight:bold"), project[1,3]),
      p(span("Documentation: ", style="color: black; font-weight:bold"), documentationText[1], style= documentationText[2]),
      p(span("Deadline: ", style="color: black; font-weight: bold"), project[1,6], " - ", deadlineText[1], style=deadlineText[2]),
      p(span("RAG Status: ", style="font-weight: bold"), project[1,7]),
      
      if(is.na(project[1,10])){
        p(span("Find documentation here: ", style="font-weight: bold"), "No link to the documentation for this project has been given")
      } else {
        p(span("Find documentation here: ", style="font-weight: bold"), tags$a(href=as.character(project[1,10]), "Click here!"))
      },
      
      br(),
      p("Comments", style="font-weight:bold"),
      tags$ul(apply(comments, 1, function(x) tags$li(x['Comments']))),
      textInput("commentadded", "Add a comment:"),
      actionButton("submitCommentFunction", "Submit"),
      br(),
      br(),
      actionLink("amendDates", "(Amend any dates here!)")
      
    )
  )
  
  
}

# App ---------------------------------------------------------------------

ui <- navbarPage("Project Management",
                 theme = shinytheme("united"),
                 source(paste("C:\\Users\\", username, "\\OneDrive - Department for Education\\Documents\\Projects\\Project Tracker\\project_tracker\\ui\\TeamOverview_TM.R", sep=""), local=TRUE)$value,
                 source(paste("C:\\Users\\", username, "\\OneDrive - Department for Education\\Documents\\Projects\\Project Tracker\\project_tracker\\ui\\PersonOverview_TM.R", sep=""), local=TRUE)$value,
                 source(paste("C:\\Users\\", username, "\\OneDrive - Department for Education\\Documents\\Projects\\Project Tracker\\project_tracker\\ui\\Timeline_TM.R", sep=""), local=TRUE)$value,
                 source(paste("C:\\Users\\", username, "\\OneDrive - Department for Education\\Documents\\Projects\\Project Tracker\\project_tracker\\ui\\Home_TM.R", sep=""), local=TRUE)$value,            
                 tags$head(tags$style("h5{color: red;}")) # making error messages red
)

server <- function(input, output, session) {
  source(paste("C:\\Users\\", username, "\\OneDrive - Department for Education\\Documents\\Projects\\Project Tracker\\project_tracker\\server\\Home_TM.R", sep=""), local=TRUE)$value
  source(paste("C:\\Users\\", username, "\\OneDrive - Department for Education\\Documents\\Projects\\Project Tracker\\project_tracker\\server\\TeamOverview_TM.R", sep=""), local=TRUE)$value
  source(paste("C:\\Users\\", username, "\\OneDrive - Department for Education\\Documents\\Projects\\Project Tracker\\project_tracker\\server\\PersonOverview_TM.R", sep=""), local=TRUE)$value
  source(paste("C:\\Users\\", username, "\\OneDrive - Department for Education\\Documents\\Projects\\Project Tracker\\project_tracker\\server\\Timeline_TM.R", sep=""), local=TRUE)$value
  
}

shinyApp(ui,server)