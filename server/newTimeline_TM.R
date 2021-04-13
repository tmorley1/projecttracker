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
                                ),
        duration = difftime(Deadline, StartDate, units = "days")
          )

output$newTimeLineTable <- DT::renderDataTable(newTimelineData%>%select(Name, TeamMembers, Customer, StartDate, Deadline, duration), selection="single")

  fig <- ganttrify(project = ganttrify::newTimelineData,
                   project_start_date = "2020-01",
                   font_family = "Roboto Condensed")

  output$newganttChart <- renderPlot(fig)
