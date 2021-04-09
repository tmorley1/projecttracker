## TIMELINE PAGE OF PROJECT TRACKER
## SPECIFICALLY LOOKING AT THE DATES OF PROJECTS

# Creating the gannt chart on the page
output$ganntChart <- renderPlot(
  ggplot(TimelineData() %>%
           mutate(Deadline = as.Date(Deadline, "%d/%m/%Y"),
                  StartDate = as.Date(StartDate, "%d/%m/%Y"),
                  DateCompleted = as.Date(DateCompleted, "%d/%m/%Y"),
                  deadlinePassed = ifelse(Completed & (Deadline < DateCompleted), "Deadline Not Met",
                                          ifelse(Completed, "Deadline Met",
                                                 ifelse(Deadline < Sys.Date(), "Passed Deadline",
                                                        "On Track")))),
         aes(x=StartDate, xend=Deadline, y=Name, yend=Name, color=deadlinePassed)) +
    geom_segment() + geom_vline(aes(xintercept=Sys.Date()), size=3) +
    theme_bw()+ #use ggplot theme with black gridlines and white background
    # geom_segment(size=15) + #increase line width of segments in the chart
    labs(x='Timescale', y='Project', color="Deadline State")
   
)

output$TimeLineTable <- DT::renderDataTable(TimelineData() %>% select(Name, TeamMembers, Customer, StartDate, Deadline, ProjectBrief, QALog), selection="single")

output$TimelineTeamSelector <- renderUI({ 
  confirmingButtons()
  Teams = sort(unique(unlist(strsplit((peopleData() %>% filter(CurrentlyInTeam))$Team,','))))
  checkboxGroupInput("team2", "Teams:", Teams, selected=Teams) 
})

output$TimelineTeamMemberSelector <- renderUI({ 
  confirmingButtons()
  Names = append("All", as.character(sort(unique((peopleData() %>% filter(CurrentlyInTeam) %>% filter(grepl(paste(input$team2, collapse='|'), Team)))$Name))))
  selectInput("name2", "Name:", Names, Names[1]) 
})

output$TimelineCompletedProjects <- renderUI({ 
  confirmingButtons()
  selectInput("completed3", "Completed Projects or Live Projects", c("Completed Projects", "Live Projects"), selected = "Live Projects") 
})

TimelineData <- reactive({
  confirmingButtons()
  date <- if(input$completed3 == "Completed Projects") {
    ifelse(input$sinceCompletedTimeline == "Last Month",
           Sys.Date()%m-% months(1),
           ifelse(input$sinceCompletedTimeline == "Last 6 Months",
                  Sys.Date()%m-% months(1),
                  ifelse(input$sinceCompletedTimeline == "Last year",
                         Sys.Date()%m-% months(12),
                         ifelse(input$sinceCompletedTimeline == "Last 3 years",
                                Sys.Date()%m-% months(36),
                                as.Date("01/01/2000", "%d/%m/%Y")
           ))))
  } else{
    as.Date("01/01/2000", "%d/%m/%Y")
  }
  
  data <- if(input$name2 == "All"){
    left_join(
      tidyr::separate_rows(projectData(), TeamMembers, sep=" ,|, |,") %>%
        rename(TeamMember = TeamMembers),
      projectData() %>%
        select(Name, TeamMembers)) %>%
      left_join(peopleData(), by=c("TeamMember"="Name")) %>%
      filter(Team %in% input$team2) %>%
      filter(Completed == ifelse(input$completed3=="Live Projects", FALSE, TRUE)) %>%
      filter(!duplicated(Name)) %>%
      mutate(DateCompleted = as.Date(DateCompleted, "%d/%m/%Y"))
  } else{
    left_join(
      tidyr::separate_rows(projectData(), TeamMembers, sep=" ,|, |,") %>%
        rename(TeamMember = TeamMembers),
      projectData() %>%
        select(Name, TeamMembers)) %>%
      left_join(peopleData(), by=c("TeamMember"="Name")) %>%
      filter(Team %in% input$team2) %>%
      filter(Completed == ifelse(input$completed3=="Live Projects", FALSE, TRUE)) %>%
      filter(TeamMember == input$name2)
  }
  
  if(input$completed3 == "Completed Projects") {
      TimelineData <- data %>% filter(DateCompleted > date)
  } else{
    TimelineData <- data
  }
  
})



# ProjectDetails ----------------------------------------------------------

# PROJECT DETAILS
observeEvent(input$projectDetailsTimeline, {
  projectDetails(
    as.data.frame(
      TimelineData()
    ),
    input$TimeLineTable_rows_selected
  )
})
  