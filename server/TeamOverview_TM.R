# Gray banner -------------------------------------------------------------

# Creates the checkbox in the gray banner t select teams which you want to see
output$teamsSelector <- renderUI({
  confirmingButtons()
  Teams = sort(unique(unlist(strsplit((peopleData() %>% filter(CurrentlyInTeam))$Team,','))))
  checkboxGroupInput("team", "Teams:", Teams, selected=Teams) 
})

# Creates the drop down box to select either live projects or completed projects
output$completedSelector <- renderUI({ 
  confirmingButtons()
  Teams = sort(unique((peopleData() %>% filter(CurrentlyInTeam))$Team))
  selectInput("completed", "Completed Projects or Live Projects", c("Completed Projects", "Live Projects"), selected = "Live Projects") 
})

# Prints the list of team members in the teams selected 
output$peopleInTeam <- renderText({ paste((peopleData() %>% filter(grepl(paste(input$team, collapse='|'), Team))%>%filter(CurrentlyInTeam))$Name, collapse= ', ')})



# Manipulating data files into useful dataframes for this page ------------

# Creating team column using the people table
TeamData <- reactive({ 
  confirmingButtons()
  left_join(
    left_join(
      tidyr::separate_rows(projectData(), TeamMembers, sep=" ,|, |,"),
      peopleData()%>%filter(CurrentlyInTeam),
      by=c("TeamMembers"="Name")) %>%
      filter(!is.na(Team)) %>%
      group_by(Name) %>%
      summarise(Team = toString(unique(Team))
      ),
    projectData()) %>%
    mutate(TeamsInvolved = Team) %>%
    tidyr::separate_rows(Team, sep=" ,|, |,")
})

# Filtering above data frame using inputs from the gray banner
specificTeamData <- reactive({
  date <- ifelse(input$sinceCompletedTeamOverview == "Last Month",
                 Sys.Date()%m-% months(1),
                 ifelse(input$sinceCompletedTeamOverview == "Last 6 Months",
                        Sys.Date()%m-% months(1),
                        ifelse(input$sinceCompletedTeamOverview == "Last year",
                               Sys.Date()%m-% months(12),
                               ifelse(input$sinceCompletedTeamOverview == "Last 3 years",
                                      Sys.Date()%m-% months(36),
                                      as.Date("01/01/2000", "%d/%m/%Y")
                               ))))
  data <- TeamData() %>%
    filter(Team %in% input$team) %>%
    mutate(DateCompleted = as.Date(DateCompleted, "%d/%m/%Y")) %>%
    filter(Completed == ifelse(input$completed=="Live Projects", FALSE, TRUE)) %>%
    distinct(Name, .keep_all = TRUE)
  
  if(input$completed=="Completed Projects"){
    specificTeamData <- data %>% filter(DateCompleted > date)
  } else{
    specificTeamData <- data
  }
  
})




# Live Projects KPIs ------------------------------------------------------


output$liveNumberOfProjects <- renderValueBox(valueBox(nrow(specificTeamData()), subtitle="No. of Projects", color="aqua"))
output$liveQALogKPI <- renderValueBox(valueBox(paste(round((nrow(specificTeamData()%>% filter(QALog == TRUE))/nrow(specificTeamData() ))*100, digits=0), "%"), subtitle="Projects with QA logs (90%)"))
output$liveQALogKPI2 <- renderValueBox(valueBox(paste(round((nrow(specificTeamData()%>% filter(QALog == TRUE))/nrow(specificTeamData() ))*100, digits=0), "%"), subtitle="Projects with QA logs (90%)"))
output$liveProjectBriefKPI <- renderValueBox(valueBox(paste(round((nrow(specificTeamData()%>% filter(ProjectBrief == TRUE))/nrow(specificTeamData() ))*100, digits=0), "%"), subtitle="Projects with a project brief (90%)"))
output$liveProjectBriefKPI2 <- renderValueBox(valueBox(paste(round((nrow(specificTeamData()%>% filter(ProjectBrief == TRUE))/nrow(specificTeamData() ))*100, digits=0), "%"), subtitle="Projects with a project brief (90%)"))


output$liveQALogKPIcolour <- renderText({
  QAlogpercentage <- ((nrow(specificTeamData()%>% filter(QALog == "TRUE")))/nrow(specificTeamData()))*100
  if(nrow(specificTeamData())!= 0) {
    if( QAlogpercentage >= 90) {
      "TRUE"
    } else if ( QAlogpercentage < 90){
      "FALSE"
    }
  }else {
    "FALSE"
  }
})
outputOptions(output, "liveQALogKPIcolour", suspendWhenHidden=FALSE)

output$liveProjectBriefKPIcolour <- renderText({
  projectBriefpercentage <- ((nrow(specificTeamData()%>% filter(ProjectBrief == "TRUE")))/nrow(specificTeamData()))*100
  if(nrow(specificTeamData())!= 0) {
    if( projectBriefpercentage >= 90) {
      "TRUE"
    } else if ( projectBriefpercentage < 90){
      "FALSE"
    }
  }else {
    "FALSE"
  }
})
outputOptions(output, "liveProjectBriefKPIcolour", suspendWhenHidden=FALSE)





# Number of projects per person graph -------------------------------------
output$noProjectPerPerson <- renderPlotly({
  confirmingButtons()
  ggplotly(
    left_join(
      tidyr::separate_rows(projectData() %>% filter(Completed == FALSE), TeamMembers, sep=" ,|, |,"),
      peopleData(),
      by=c("TeamMembers"="Name")) %>%
      group_by(TeamMembers) %>%
      filter(CurrentlyInTeam)%>%
      summarise(NoProjects = n()) %>%
      right_join(peopleData()%>%filter(CurrentlyInTeam), by=c("TeamMembers" = "Name")) %>%
      filter(grepl(paste(input$team, collapse='|'), Team)) %>%
      ggplot(aes(x=TeamMembers, y=NoProjects)) +
      geom_bar(stat= "identity", fill='#EA5421' )+
      labs(title="Number of projects by team member") +
      xlab("Team Member") +
      ylab("Number of Projects"))
  
})

# Person selector: choose a person to see all the projects which are contributing to the chart 
# for each person

output$teamOverviewPersonSelector <- renderUI({
  NamesOfPeopleInSelectedTeams <- sort(unique(tidyr::separate_rows(specificTeamData(), TeamMembers, sep=" ,|, |,")$TeamMembers))
  selectInput("personSelector", "Select a team member to see their projects", NamesOfPeopleInSelectedTeams)
})

# see a table of all the projects of the person selected in the drop down box above
output$teamOverviewPerson <- DT::renderDataTable(
  tidyr::separate_rows(specificTeamData(), TeamMembers, sep=" ,|, |,") %>% 
    filter(TeamMembers == input$personSelector) %>%
    select(-TeamsInvolved, -Documentation, -Comments, -Completed, -Team, -TeamMembers, -StartDate, -DateCompleted) %>%
    distinct(Name, .keep_all = TRUE),  
  server = FALSE, 
  selection='single'
  )

# Project details button for the above table
observeEvent(input$projectDetailsTeamOverview, {
  projectDetails(
    as.data.frame(tidyr::separate_rows(specificTeamData(), TeamMembers, sep=" ,|, |,") %>% 
                    filter(TeamMembers == input$personSelector) %>%
                    select(-TeamsInvolved, -Documentation, -Comments, -Completed, -Team, -TeamMembers, -StartDate, -DateCompleted) %>%
                    distinct(Name, .keep_all = TRUE)
    ),
    input$teamOverviewPerson_rows_selected
  )
})

# No. Projects at each RAG rating stacked bar chart -------------------------------
output$RAGPie <- renderPlotly({
  RAGPieData <- specificTeamData() %>%
    tidyr::separate_rows(TeamMembers, sep=" ,|, |,") %>%
    distinct(Name, .keep_all = TRUE) %>%
    mutate(RAG = factor(RAG,c("R", "R-A", "A", "A-G", "G"))) %>%
    group_by(RAG) %>%
    summarise(NoProjects = n()) %>%
    mutate(number="Projects",
           RAGColor = ifelse(RAG == "R","#fc5a03",
                             ifelse(RAG == "R-A", "#de8116",
                                    ifelse(RAG == "A", "#fcd362",
                                           ifelse(RAG == "A-G", "#e9fa9d",
                                                  "#7eb80b" 
                                           ))))) %>%
    arrange(RAG)
  
  RAGPie <- ggplot(RAGPieData,
                   aes(x=number, y = NoProjects, fill = RAG)) + 
    geom_bar(position = "fill",stat = "identity") + 
    scale_fill_manual(values= unique(RAGPieData$RAGColor)) +
    coord_flip() + 
    labs(y="Percentage of projects", x=" ", title="RAG statuses of live projects")
})

# Tables to output in each tab underneath the above chart
output$redProjects <- DT::renderDataTable(specificTeamData() %>% filter(RAG == 'R') %>% select(-TeamsInvolved, -Documentation, -Comments, -RAG, -Completed, -Team, -DateCompleted, -StartDate), server = FALSE, selection='single')
output$redAmberProjects <- DT::renderDataTable(specificTeamData() %>% filter(RAG == 'R-A') %>% select(-TeamsInvolved, -Documentation, -Comments, -RAG, -Completed, -Team, -DateCompleted, -StartDate), server = FALSE, selection='single')
output$amberProjects <- DT::renderDataTable(specificTeamData() %>% filter(RAG == 'A') %>% select(-TeamsInvolved, -Documentation, -Comments, -RAG, -Completed, -Team, -DateCompleted, -StartDate), server = FALSE, selection='single')
output$amberGreenProjects <- DT::renderDataTable(specificTeamData() %>% filter(RAG == 'A-G') %>% select(-TeamsInvolved, -Documentation, -Comments, -RAG, -Completed, -Team, -DateCompleted, -StartDate), server = FALSE, selection='single')
output$greenProjects <- DT::renderDataTable(specificTeamData() %>% filter(RAG == 'G') %>% select(-TeamsInvolved, -Documentation, -Comments, -RAG, -Completed, -Team, -DateCompleted, -StartDate), server = FALSE, selection='single')
output$allProjects <- DT::renderDataTable(specificTeamData() %>% select(-TeamsInvolved, -Documentation, -Comments, -RAG, -Completed, -Team, -DateCompleted, -StartDate), server = FALSE, selection='single')

# Creating the tab box underneath the stacked bar chart showing RAG statuses
output$RAG_Status <- renderUI({
    tabBox(
      id = "box_pat",
      tabPanel(
        title = "All",
        DT::dataTableOutput("allProjects"),
        conditionalPanel(
          "input.allProjects_rows_selected !=  0",
          actionButton("projectDetailsAll", "Details of Project")
        )
        
        
      ),
      tabPanel(
        title = "R",
        DT::dataTableOutput("redProjects"),
        conditionalPanel(
          "input.redProjects_rows_selected !=  0",
          actionButton("projectDetailsRed", "Details of Project")
        )
        

      ),
      tabPanel(
        title = "R-A",
        DT::dataTableOutput("redAmberProjects"),
        conditionalPanel(
          "input.redAmberProjects_rows_selected !=  0",
          actionButton("projectDetailsRedAmber", "Details of Project")         
        )

      ),
      tabPanel(
        title = "A",
        DT::dataTableOutput("amberProjects"),
        conditionalPanel(
          "input.amberProjects_rows_selected !=  0",
          actionButton("projectDetailsAmber", "Details of Project")  
        )
        
      ),
      tabPanel(
        title = "A-G",
        DT::dataTableOutput("amberGreenProjects"),
        conditionalPanel(
          "input.amberGreenProjects_rows_selected !=  0",
          actionButton("projectDetailsAmberGreen", "Details of Project")  
        )
        
      ),
      tabPanel(
        title = "G",
        DT::dataTableOutput("greenProjects"),
        conditionalPanel(
          "input.greenProjects_rows_selected !=  0",
          actionButton("projectDetailsGreen", "Details of Project")  
        )
        
      )
  )
})

# The code which produces the modal showing the project details of the selected project
# one for each tab in the tab box

observeEvent(input$projectDetailsAll, {
  projectDetails(
    as.data.frame(
      specificTeamData()
    ),
    input$allProjects_rows_selected
  )
})

observeEvent(input$projectDetailsRed, {
  projectDetails(
    as.data.frame(
      specificTeamData() %>% filter(RAG == 'R')
    ),
    input$redProjects_rows_selected
  )
})

observeEvent(input$projectDetailsRedAmber, {
  projectDetails(
    as.data.frame(
      specificTeamData() %>% filter(RAG == 'R-A')
    ),
    input$redAmberProjects_rows_selected
  )
})

observeEvent(input$projectDetailsAmber, {
  projectDetails(
    as.data.frame(
      specificTeamData() %>% filter(RAG == 'A')
    ),
    input$amberProjects_rows_selected
  )
})

observeEvent(input$projectDetailsAmberGreen, {
  projectDetails(
    as.data.frame(
      specificTeamData() %>% filter(RAG == 'A-G')
    ),
    input$amberGreenProjects_rows_selected
  )
})

observeEvent(input$projectDetailsGreen, {
  projectDetails(
    as.data.frame(
      specificTeamData() %>% filter(RAG == 'G')
    ),
    input$greenProjects_rows_selected
  )
})
# Completed projects KPIs -------------------------------------------------

## KPIs 
output$completedNumberOfProjects <- renderValueBox(valueBox(nrow(specificTeamData()), subtitle="Projects' selected", color="aqua"))
output$completedQALogKPI <- renderValueBox(valueBox(paste(round((nrow(specificTeamData()%>% filter(QALog == TRUE))/nrow(specificTeamData() ))*100, digits=0), "%"), subtitle="Projects with QA logs (100%)"))
output$completedQALogKPI2 <- renderValueBox(valueBox(paste(round((nrow(specificTeamData()%>% filter(QALog == TRUE))/nrow(specificTeamData() ))*100, digits=0), "%"), subtitle="Projects with QA logs (100%)"))
output$completedProjectBriefKPI <- renderValueBox(valueBox(paste(round((nrow(specificTeamData()%>% filter(ProjectBrief == TRUE))/nrow(specificTeamData() ))*100, digits=0), "%"), subtitle="Projects with a project brief (90%)"))
output$completedProjectBriefKPI2 <- renderValueBox(valueBox(paste(round((nrow(specificTeamData()%>% filter(ProjectBrief == TRUE))/nrow(specificTeamData() ))*100, digits=0), "%"), subtitle="Projects with a project brief (90%)"))
output$deadlineMetKPI<- renderValueBox(valueBox(paste(round((nrow(specificTeamData()%>% filter(as.Date(DateCompleted, "%d/%m/%Y") < as.Date(Deadline, "%d/%m/%Y") ))/nrow(specificTeamData() ))*100, digits=0), "%"), subtitle="Projects which met their deadline (80%)"))
output$deadlineMetKPI2<- renderValueBox(valueBox(paste(round((nrow(specificTeamData()%>% filter(as.Date(DateCompleted, "%d/%m/%Y") < as.Date(Deadline, "%d/%m/%Y") ))/nrow(specificTeamData() ))*100, digits=0), "%"), subtitle="Projects which met their deadline (80%)"))


# Condition to make the KPI appear green if the KPI is met and red if otherwise
output$deadlineMetKPIcolour <- renderText({
  deadlineMetpercentage <- ((nrow(specificTeamData()%>% filter(as.Date(DateCompleted, "%d/%m/%Y") < as.Date(Deadline, "%d/%m/%Y") )))/nrow(specificTeamData()))*100
  if(nrow(specificTeamData())!= 0) {
    if( deadlineMetpercentage >= 80) {
      "TRUE"
    } else if ( deadlineMetpercentage < 80){
      "FALSE"
    }
  }else {
    "FALSE"
  }
})
outputOptions(output, "deadlineMetKPIcolour", suspendWhenHidden=FALSE)
output$completedQALogKPIcolour <- renderText({
  QAlogpercentage <- ((nrow(specificTeamData()%>% filter(QALog == "TRUE")))/nrow(specificTeamData()))*100
  if(nrow(specificTeamData())!= 0) {
    if( QAlogpercentage >= 100) {
      "TRUE"
    } else if ( QAlogpercentage < 100){
      "FALSE"
    }
  }else {
    "FALSE"
  }
})
outputOptions(output, "completedQALogKPIcolour", suspendWhenHidden=FALSE)

output$completedProjectBriefKPIcolour <- renderText({
  projectBriefpercentage <- ((nrow(specificTeamData()%>% filter(ProjectBrief == "TRUE")))/nrow(specificTeamData()))*100
  if(nrow(specificTeamData())!= 0) {
    if( projectBriefpercentage >= 90) {
      "TRUE"
    } else if ( projectBriefpercentage < 90){
      "FALSE"
    }
  }else {
    "FALSE"
  }
})
outputOptions(output, "completedProjectBriefKPIcolour", suspendWhenHidden=FALSE)


# Customers tab -----------------------------------------------------------


## Creating the individual tables for the customer tab
output$techTable <- DT::renderDataTable(specificTeamData() %>% filter(CustomerGroup == 'Tech') %>% distinct(Name, .keep_all = TRUE) %>% select( -TeamsInvolved, -Documentation, -Comments, -RAG, -Completed, -Team, -DateCompleted, -StartDate), server = FALSE, selection='single')
output$HRTable <- DT::renderDataTable(specificTeamData() %>% filter(CustomerGroup == 'HR') %>% distinct(Name, .keep_all = TRUE) %>% select(-TeamsInvolved, -Documentation, -Comments, -RAG, -Completed, -Team, -DateCompleted, -StartDate), server = FALSE, selection='single')
output$ACTable <- DT::renderDataTable(specificTeamData() %>% filter(CustomerGroup == 'AC') %>% distinct(Name, .keep_all = TRUE) %>% select(-TeamsInvolved, -Documentation, -Comments, -RAG, -Completed, -Team, -DateCompleted, -StartDate), server = FALSE, selection='single')
output$otherTable <- DT::renderDataTable(specificTeamData() %>% filter(CustomerGroup == 'Other') %>% distinct(Name, .keep_all = TRUE) %>% select( -TeamsInvolved, -Documentation, -Comments, -RAG, -Completed, -Team, -DateCompleted, -StartDate), server = FALSE, selection='single')

## Treemap for completed projects section
output$customerTreeMap <- renderPlotly({
  projects <- specificTeamData() %>% 
    group_by(CustomerGroup, Customer) %>% 
    summarise(count= n()) %>%
    mutate(Customer = ifelse(as.character(CustomerGroup) == as.character(Customer), paste("Unknown ", CustomerGroup, " customer"), ifelse(Customer=="", paste("Unknown ", CustomerGroup, " customer"), as.character(Customer)))) %>%
    rbind(data.frame(CustomerGroup =c("","","",""),Customer=c("AC", "Tech", "HR", "Other"), count=c(0,0,0,0)))
  
  plot_ly(
    type="treemap",
    labels=as.character(projects$Customer),
    parents=as.character(projects$CustomerGroup),
    values=projects$count
  )
  #   group_by(CustomerGroup, Customer) %>% 
  #   summarise(count= n()) %>%
  #   mutate(Customer = ifelse(as.character(CustomerGroup) == as.character(Customer), "Unknown", as.character(Customer)))
  # labels = c("AC", "Tech", "HR", "Other", as.character(projects$Customer))
  # parents=c("", "", "","", as.character(projects$CustomerGroup))
  # 
  # plot_ly(
  #   type="treemap",
  #   labels=labels,
  #   parents=parents
  # )
  })

## ui for the customer section
output$customers <- renderUI({
  tabBox(
    id = "customers_tab",
    tabPanel(
      br(),
      title = "Tech",
      DT::dataTableOutput("techTable"),
      conditionalPanel(
        "input.techTable_rows_selected != 0",
        actionButton("projectDetailsTech", "Details of Project")
      ),
      br()
      
    ),
    tabPanel(
      br(),
      title = "AC",
      DT::dataTableOutput("ACTable"),
      conditionalPanel(
        "input.ACTable_rows_selected != 0",
        actionButton("projectDetailsAC", "Details of Project")
      ),
      br()
    ),
    tabPanel(
      br(),
      title = "HR",
      DT::dataTableOutput("HRTable"),
      conditionalPanel(
        "input.HRTable_rows_selected != 0",
        actionButton("projectDetailsHR", "Details of Project")
      ),
      br()
    ),
    tabPanel(
      br(),
      title = "Other",
      DT::dataTableOutput("otherTable"),
      conditionalPanel(
        "input.otherTable_rows_selected != 0",
        actionButton("projectDetailsOther", "Details of Project")
      ),
      br()
    )
  )
})

observeEvent(input$projectDetailsTech, {
  projectDetails(
    as.data.frame(
      specificTeamData() %>% filter(CustomerGroup == 'Tech')
    ),
    input$techTable_rows_selected
  )
})

observeEvent(input$projectDetailsAC, {
  projectDetails(
    as.data.frame(
      specificTeamData() %>% filter(CustomerGroup == 'AC')
    ),
    input$ACTable_rows_selected
  )
})

observeEvent(input$projectDetailsHR, {
  projectDetails(
    as.data.frame(
      specificTeamData() %>% filter(CustomerGroup == 'HR')
    ),
    input$HRTable_rows_selected
  )
})

observeEvent(input$projectDetailsOther, {
  projectDetails(
    as.data.frame(
      specificTeamData() %>% filter(CustomerGroup == 'Other')
    ),
    input$otherTable_rows_selected
  )
})

observeEvent(input$submitCommentFunction, {
  comments <<- comments %>%
    rbind(data.frame(Comments = c(input$commentadded)))
  
  output$projectComments <- renderUI({
    apply(comments, 1, function(x) tags$li(x['Comments']))
  })
  
  project[1,9] <- as.character(paste(comments$Comments, collapse=";"))
  newProjectData <- projectData() %>% mutate_all(as.character) %>%
    filter(Name != project[1,1]) %>%
    rbind(project)
  
  
  saveChangesToProjectFile(newProjectData)
  projectData <- projectData() %>% mutate_all(as.character)
  removeModal()
})