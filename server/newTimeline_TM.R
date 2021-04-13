## TIMELINE SERVER PAGE OF PROJECT TRACKER
## SPECIFICALLY LOOKING AT THE DATES OF PROJECTS

## Different options for options bar

output$newTimelineTeamSelector <- renderUI({ 
  confirmingButtons()
  Teams = sort(unique(unlist(strsplit((peopleData() %>% filter(CurrentlyInTeam))$Team,','))))
  checkboxGroupInput("newteam2", "Teams:", Teams, selected=Teams) 
})

output$newTimelineTeamMemberSelector <- renderUI({ 
  confirmingButtons()
  Names = append("All", as.character(sort(unique((peopleData() %>% filter(CurrentlyInTeam) %>% filter(grepl(paste(input$newteam2, collapse='|'), Team)))$Name))))
  selectInput("newname2", "Name:", Names, Names[1]) 
})

output$newTimelineCompletedProjects <- renderUI({ 
  confirmingButtons()
  selectInput("newcompleted3", "Completed Projects or Live Projects", c("Completed Projects", "Live Projects"), selected = "Live Projects") 
})

## Reading in data

newTimelineData <- reactive({
  data <- if(input$newname2 == "All"){left_join(
  tidyr::separate_rows(projectData(), TeamMembers, sep=" ,|, |,") %>%
    rename(TeamMember = TeamMembers),
  projectData() %>%
  select(Name, TeamMembers)) %>%
  left_join(peopleData(), by=c("TeamMember"="Name")) %>%
  filter(grepl(paste(input$newteam2, collapse='|'), Team)) %>%
  filter(Completed == ifelse(input$newcompleted3=="Live Projects", FALSE, TRUE)) %>%
  filter(!duplicated(Name))%>%
  mutate(Deadline = as.Date(Deadline, "%d/%m/%Y"),
         StartDate = as.Date(StartDate, "%d/%m/%Y"),
         DateCompleted = as.Date(DateCompleted, "%d/%m/%Y"),
         deadlinePassed = ifelse(Completed & (Deadline < DateCompleted), "Deadline Not Met",
                                 ifelse(Completed, "Deadline Met",
                                        ifelse(Deadline < Sys.Date(), "Passed Deadline","On Track")
                                 )
         ))%>%
  select(Name,TeamMembers, Customer, StartDate, Deadline, DateCompleted, Completed)%>%
  rename(wp = Name, start_date=StartDate, end_date=DateCompleted, spot_date=Deadline)%>%
  drop_na("start_date")%>%
  mutate(activity=wp, spot_type="D")%>%
  mutate(end_date = ifelse(is.na(end_date),format(Sys.Date(), "%d/%m/%Y"),format(end_date, "%d/%m/%Y"))) %>%
  mutate(end_date = as.Date(end_date, "%d/%m/%Y"))
  }
  else{
    left_join(
      tidyr::separate_rows(projectData(), TeamMembers, sep=" ,|, |,") %>%
        rename(TeamMember = TeamMembers),
      projectData() %>%
        select(Name, TeamMembers)) %>%
      left_join(peopleData(), by=c("TeamMember"="Name")) %>%
      filter(grepl(paste(input$newteam2, collapse='|'), Team)) %>%
      filter(Completed == ifelse(input$newcompleted3=="Live Projects", FALSE, TRUE)) %>%
      filter(TeamMember == input$newname2)%>%
      mutate(Deadline = as.Date(Deadline, "%d/%m/%Y"),
             StartDate = as.Date(StartDate, "%d/%m/%Y"),
             DateCompleted = as.Date(DateCompleted, "%d/%m/%Y"),
             deadlinePassed = ifelse(Completed & (Deadline < DateCompleted), "Deadline Not Met",
                                     ifelse(Completed, "Deadline Met",
                                            ifelse(Deadline < Sys.Date(), "Passed Deadline","On Track")
                                     )
             ))%>%
      select(Name,TeamMembers, Customer, StartDate, Deadline, DateCompleted, Completed)%>%
      rename(wp = Name, start_date=StartDate, end_date=DateCompleted, spot_date=Deadline)%>%
      drop_na("start_date")%>%
      mutate(activity=wp, spot_type="D")%>%
      mutate(end_date = ifelse(is.na(end_date),format(Sys.Date(), "%d/%m/%Y"),format(end_date, "%d/%m/%Y"))) %>%
      mutate(end_date = as.Date(end_date, "%d/%m/%Y"))
  }
  newTimelineData <- data
})

## Creating table

output$newTimeLineTable <- DT::renderDataTable(newTimelineData()%>%rename(Name = wp, StartDate = start_date, DateCompleted=end_date, Deadline = spot_date)%>%select(-activity,-spot_type), selection="single")

## Creating gantt chart

output$newganttChart <- renderPlot(
  ganttrify(project = newTimelineData(),
            spots = newTimelineData(),
            by_date = TRUE,
            exact_date = TRUE,
            hide_wp = TRUE,
            month_number_label = FALSE,
            font_family = "Roboto Condensed"))


## PROJECT DETAILS
observeEvent(input$newprojectDetailsTimeline, {
  projectDetails(
    as.data.frame(
      newTimelineData()
    ),
    input$newTimeLineTable_rows_selected
  )
})

## Testing whether there are any entries in the projects table
output$projectsNumberjustnumber <- renderText({
  if(nrow(newTimelineData())==0){
    "TRUE"
  } 
  else {
    "FALSE"
  }
})
outputOptions(output, "projectsNumberjustnumber", suspendWhenHidden=FALSE)
