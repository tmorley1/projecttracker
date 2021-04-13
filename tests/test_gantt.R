library(shiny)
library(tidyverse)
library(shinythemes) #theme -> css
library(plotly) # plotlyOutputs - graphs
library(DT)
library(stringr)
library(shinydashboard)
library(treemap)
library(lubridate)
library(ganttrify)

username <- "tmorley"#change to be your username
dataPathway <- paste("C:\\Users\\", username, "\\OneDrive - Department for Education\\Documents - Strategic Operations Analysis Division\\General\\Project Tracker_TM\\projecttracker\\Inputs\\", sep="")

projectData <- projectData <- as.data.frame(read.csv(paste(dataPathway, "Projects.csv", sep="")))
peopleData <- peopleData <- as.data.frame(read.csv(paste(dataPathway, "People.csv", sep="")))

newTimelineData <- left_join(
  tidyr::separate_rows(projectData, TeamMembers, sep=" ,|, |,") %>%
    rename(TeamMember = TeamMembers),
  projectData %>%
    select(Name, TeamMembers)) %>%
  left_join(peopleData, by=c("TeamMember"="Name")) %>%
  filter(!duplicated(Name))%>%
  mutate(Deadline = as.Date(Deadline, "%d/%m/%Y"),
         StartDate = as.Date(StartDate, "%d/%m/%Y"),
         DateCompleted = as.Date(DateCompleted, "%d/%m/%Y"),
         deadlinePassed = ifelse(Completed & (Deadline < DateCompleted), "Deadline Not Met",
                                 ifelse(Completed, "Deadline Met",
                                        ifelse(Deadline < Sys.Date(), "Passed Deadline","On Track")
                                 )
         ))%>%
  select(Name,TeamMembers, Customer, StartDate, Deadline, DateCompleted)%>%
  rename(activity = Name, start_date=StartDate, end_date=DateCompleted, spot_date=Deadline)%>%
  drop_na("start_date", "end_date")%>%
  mutate(wp="", spot_type="D")

spots_table <- newTimelineData%>% select(activity, spot_type, spot_date)

ganttchart <- ganttrify(project = newTimelineData,
          spots = newTimelineData,
          by_date = TRUE,
          exact_date = TRUE,
          month_number_label = FALSE,
          font_family = "Roboto Condensed")
typeof(ganttchart)

print (ganttchart)
