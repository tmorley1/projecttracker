## TIMELINE SERVER PAGE OF PROJECT TRACKER
## SPECIFICALLY LOOKING AT THE DATES OF PROJECTS

## Different options for options bar

#Options to select which team is viewed
output$newTimelineTeamSelector <- renderUI({ 
  confirmingButtons()
  Teams = sort(unique(unlist(strsplit((peopleData() %>% filter(CurrentlyInTeam))$Team,','))))
  checkboxGroupInput("newteam2", "Teams:", Teams, selected=Teams) 
})

#Options to select which members are viewed
output$newTimelineTeamMemberSelector <- renderUI({ 
  confirmingButtons()
  Names = append("All", as.character(sort(unique((peopleData() %>% filter(CurrentlyInTeam) %>% filter(grepl(paste(input$newteam2, collapse='|'), Team)))$Name))))
  selectInput("newname2", "Name:", Names, Names[1]) 
})

#Options to select whether Completed or Live projects are viewed
output$newTimelineCompletedProjects <- renderUI({ 
  confirmingButtons()
  selectInput("newcompleted3", "Completed Projects or Live Projects", c("Live Projects","Completed Projects"), selected = "Live Projects") 
})

## Reading in data

newTimelineData <- reactive({
  confirmingButtons()
  
  date <- if(input$newcompleted3 == "Completed Projects") {
    ifelse(input$newsinceCompletedTimeline == "Last Month",
           Sys.Date()%m-% months(1),
           ifelse(input$newsinceCompletedTimeline == "Last 6 Months",
                  Sys.Date()%m-% months(6),
                  ifelse(input$newsinceCompletedTimeline == "Last year",
                         Sys.Date()%m-% months(12),
                         ifelse(input$newsinceCompletedTimeline == "Last 3 years",
                                Sys.Date()%m-% months(36),
                                as.Date("01/01/2000", "%d/%m/%Y")
                         ))))
  } else{
    as.Date("01/01/2000", "%d/%m/%Y")
  }
  
  data <- if(input$newname2 == "All"){left_join(                                #if all members are selected
  tidyr::separate_rows(projectData(), TeamMembers, sep=" ,|, |,") %>%           #reads in project data
    rename(TeamMember = TeamMembers),                                           
  projectData() %>%
  select(Name, TeamMembers)) %>%                                                
  left_join(peopleData(), by=c("TeamMember"="Name")) %>%                        #joins with people data
  filter(grepl(paste(input$newteam2, collapse='|'), Team)) %>%                  #selects members associated with chosen team
  filter(Completed == ifelse(input$newcompleted3=="Live Projects", FALSE, TRUE)) %>% #selects Live projects
  filter(!duplicated(Name))%>%                                                  #removes duplicated names
  mutate(Deadline = as.Date(Deadline, "%d/%m/%Y"),                              #reads in deadline as date
         StartDate = as.Date(StartDate, "%d/%m/%Y"),                            #reads in start date as date
         DateCompleted = as.Date(DateCompleted, "%d/%m/%Y"),                    #reads in completed date as date
         deadlinePassed = ifelse(Completed & (Deadline < DateCompleted), "Deadline Not Met",
                                 ifelse(Completed, "Deadline Met",
                                        ifelse(Deadline < Sys.Date(), "Passed Deadline","On Track")
                                 )                                              #creates new column deadlinePassed
         ))%>%
  select(Name,TeamMembers, Customer, StartDate, Deadline, DateCompleted, deadlinePassed, Completed, ProjectBrief, QALog)%>% #selects only relevant columns
  rename(activity = Name, wp=deadlinePassed, start_date=StartDate, end_date=DateCompleted, spot_date=Deadline)%>% #renames columns for use in ganttrifyy function
  drop_na("start_date")%>%                                                      #removes projects with no start date
  mutate(spot_type="D")%>%                                                      #displays a D for deadline
  mutate(end_date = ifelse(is.na(end_date),format(Sys.Date(), "%d/%m/%Y"),format(end_date, "%d/%m/%Y"))) %>% #if project is still live, then the end date is given as today's date for purposes of gantt chart
  mutate(end_date = as.Date(end_date, "%d/%m/%Y"))                              #reads in end date as date
  }
  else{                                                                         #if a specific team member is selected
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
      select(Name,TeamMembers, Customer, StartDate, Deadline, deadlinePassed, DateCompleted, Completed, ProjectBrief, QALog)%>%
      rename(activity=Name, wp=deadlinePassed, start_date=StartDate, end_date=DateCompleted, spot_date=Deadline)%>%
      drop_na("start_date")%>%
      mutate(spot_type="D")%>%
      mutate(end_date = ifelse(is.na(end_date),format(Sys.Date(), "%d/%m/%Y"),format(end_date, "%d/%m/%Y"))) %>%
      mutate(end_date = as.Date(end_date, "%d/%m/%Y"))
  }
  
  if(input$newcompleted3 == "Completed Projects") {
    newTimelineData <- data %>% filter(end_date > date)
  } else{
    newTimelineData <- data
  }
  
  
})

## Creating table

output$newTimeLineTablelive <- DT::renderDataTable(newTimelineData()%>%rename(Name = activity, StartDate = start_date, DateCompleted=end_date, Deadline = spot_date)%>%select(-spot_type, -wp, -Completed, -DateCompleted), selection="single")
output$newTimeLineTablecompleted <- DT::renderDataTable(newTimelineData()%>%rename(Name = activity, StartDate = start_date, DateCompleted=end_date, Deadline = spot_date)%>%select(-spot_type, -wp, -Completed), selection="single")

## Creating gantt chart

source("C:\\Users\\tmorley\\OneDrive - Department for Education\\Documents - Strategic Operations Analysis Division\\General\\Project Tracker_TM\\projecttracker\\tests\\ganttrifyy.R")

cols <- c("Deadline Not Met" = "red", "Deadline Met" = "green", "Passed Deadline" = "red", "On Track" = "green")

output$newganttChartlive <- renderPlot(
  ganttrifyy(project = newTimelineData(),
            spots = newTimelineData(),
              #mutate(spot_date = ifelse(Sys.Date()<spot_date, "", format(spot_date, "%d/%m/%Y")))%>%
             # mutate(spot_date = as.Date(spot_date, "%d/%m/%Y")),
            by_date = TRUE,
            exact_date = TRUE,
            hide_wp = TRUE,
            colour_palette = cols,
            month_number_label = FALSE,
            font_family = "Roboto Condensed")+ geom_vline(aes(xintercept=Sys.Date()), size=2))

output$newganttChartcompleted <- renderPlot( #no line for today when looking at completed projects
  ganttrifyy(project = newTimelineData(),
             spots = newTimelineData(),
             #mutate(spot_date = ifelse(Sys.Date()<spot_date, "", format(spot_date, "%d/%m/%Y")))%>%
             # mutate(spot_date = as.Date(spot_date, "%d/%m/%Y")),
             by_date = TRUE,
             exact_date = TRUE,
             hide_wp = TRUE,
             colour_palette = cols,
             month_number_label = FALSE,
             font_family = "Roboto Condensed"))

## PROJECT DETAILS
observeEvent(input$newprojectDetailsTimelinelive, {
  projectDetails(
    as.data.frame(
      newTimelineData()
    ),
    input$newTimeLineTablelive_rows_selected
  )
})

observeEvent(input$newprojectDetailsTimelinecompleted, {
  projectDetails(
    as.data.frame(
      newTimelineData()
    ),
    input$newTimeLineTablecompleted_rows_selected
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
