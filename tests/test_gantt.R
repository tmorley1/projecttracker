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
  select(Name,TeamMembers, Customer, StartDate, Deadline, DateCompleted, deadlinePassed)%>%
  rename(activity = Name, wp=deadlinePassed, start_date=StartDate, end_date=DateCompleted, spot_date=Deadline)%>%
  drop_na("start_date")%>%
  mutate(spot_type="D")%>%
  mutate(end_date = ifelse(is.na(end_date),format(Sys.Date(), "%d/%m/%Y"),format(end_date, "%d/%m/%Y"))) %>%
  mutate(end_date = as.Date(end_date, "%d/%m/%Y"))

source("C:\\Users\\tmorley\\OneDrive - Department for Education\\Documents - Strategic Operations Analysis Division\\General\\Project Tracker_TM\\projecttracker\\tests\\ganttrifyy.R")


cols <- c("Deadline Not Met" = "red", "Deadline Met" = "green", "Passed Deadline" = "red", "On Track" = "orange")

ganttchart <- ganttrifyy(project = newTimelineData,
          spots = newTimelineData,
          by_date = TRUE,
          exact_date = TRUE,
          month_number_label = FALSE,
          colour_palette = cols,
          hide_wp = TRUE,
          font_family = "Roboto Condensed")

print (ganttchart)
