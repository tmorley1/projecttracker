## TIMELINE SERVER PAGE OF PROJECT TRACKER
## SPECIFICALLY LOOKING AT THE DATES OF PROJECTS

newTimelineData <- left_join(
  tidyr::separate_rows(projectData(), TeamMembers, sep=" ,|, |,") %>%
    rename(TeamMember = TeamMembers),
  projectData() %>%
    select(Name, TeamMembers)) %>%
  left_join(peopleData(), by=c("TeamMember"="Name")) %>%
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

output$newTimeLineTable <- DT::renderDataTable(newTimelineData, selection="single")

output$newganttChart <- renderPlot(
  ganttrify(project = newTimelineData,
            spots = newTimelineData,
            by_date = TRUE,
            exact_date = TRUE,
            month_number_label = FALSE,
            font_family = "Roboto Condensed"))