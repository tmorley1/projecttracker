## PERSON OVERVIEW TAB OF PROJECT TRACKER



# Grey banner -------------------------------------------------------------


# Allows a user to select a team member to look at their live and completed projects
output$teamMemberSelector <- renderUI({ 
  confirmingButtons()
  Names = sort(unique((peopleData() %>% filter(CurrentlyInTeam))$Name))
  selectInput("name", "Name:", Names, Names[1]) 
})

# Allows a user to select whether to see the chosen team member's live or completed projects
output$completedSelector2 <- renderUI({ 
  confirmingButtons()
  selectInput("completed2", "Completed Projects or Live Projects", c("Completed Projects", "Live Projects"), selected = "Live Projects") 
})

# Shows total number of projects the chosen team member has completed
output$TeamMembersCompletedProjects <- renderValueBox(valueBox(
  nrow(
    left_join(
      tidyr::separate_rows(projectData(), TeamMembers, sep=" ,|, |,") %>%
        rename(TeamMember = TeamMembers),
      projectData() %>%
        select(Name, TeamMembers)
    ) %>%
      filter(TeamMember == input$name) %>%
      filter(Completed=="TRUE")),
  subtitle="Completed Projects"))

# Shows number of live projects the chosen team member has
output$TeamMembersLiveProjects <- renderValueBox(valueBox(
  nrow(
    left_join(
      tidyr::separate_rows(projectData(), TeamMembers, sep=" ,|, |,") %>%
        rename(TeamMember = TeamMembers),
      projectData() %>%
        select(Name, TeamMembers)
    ) %>%
      filter(TeamMember == input$name) %>%
      filter(Completed=="FALSE")),
  subtitle="Live Projects"))




# Main body of page -------------------------------------------------------

#Only show the drop down box with different periods of time if this is "TRUE"
output$completedInThePersonOverview <- renderText({
  ifelse(input$completed2 == "Completed Projects", "TRUE", "FALSE")
})
outputOptions(output, "completedInThePersonOverview", suspendWhenHidden=FALSE)


# Creating data to be shown in data table
personData <- reactive ({
  confirmingButtons()
  date <- ifelse(input$sinceCompletedPersonOverview == "Last Month",
           Sys.Date()%m-% months(1),
           ifelse(input$sinceCompletedPersonOverview == "Last 6 Months",
                  Sys.Date()%m-% months(1),
                  ifelse(input$sinceCompletedPersonOverview == "Last year",
                         Sys.Date()%m-% months(12),
                         ifelse(input$sinceCompletedPersonOverview == "Last 3 years",
                                Sys.Date()%m-% months(36),
                                as.Date("01/01/2000", "%d/%m/%Y")
                         ))))
    
  data <-
      left_join(
        tidyr::separate_rows(projectData(), TeamMembers, sep=" ,|, |,") %>%
          rename(TeamMember = TeamMembers),
        projectData() %>%
          select(Name, TeamMembers)) %>%
      filter(TeamMember == input$name) %>%
      mutate(DateCompleted = as.Date(DateCompleted, "%d/%m/%Y"))
  
  if(input$completed2 == "Completed Projects"){
    personData <- data %>% filter(DateCompleted > date) %>% mutate_all(as.character)
  } else {
    personData <- data
  }
})

output$personProject <- DT::renderDataTable(personData() %>% filter(Completed == ifelse(input$completed2 == "Completed Projects", TRUE, FALSE)) %>% select(-Comments, -Documentation, -TeamMember, -Completed), server=FALSE)



# Project details modal ---------------------------------------------------


observeEvent(input$projectDetailsPersonOverview, {
  projectDetails(
    as.data.frame(
      personData() %>% filter(Completed == ifelse(input$completed2 == "Completed Projects", TRUE, FALSE))
    ),
    input$personProject_rows_selected
  )
})