library("patentsview")
library("tidyverse")
library("shiny")

#### Pull, Backup, and unnest Patent data ####
#Set the working directory and  verify
setwd("/Users/joeybodeep/Google Drive/School/Unstructured Data Managment/Squad Fleek Team Project/Working Directory")
getwd()


#Find everything between these dates
my_query <- qry_funs$and(
  qry_funs$gte(patent_date = "2018-01-01"),
  qry_funs$lte(patent_date = "2018-03-31")
  )

#Pull BIG LIST of fields using query and write to "localdata"
localdata = search_pv(
  query = my_query,
  fields = c("patent_number", "inventor_id", "assignee_id","patent_date","inventor_last_name","inventor_city", "inventor_country", 
             "assignee_organization", "assignee_lastknown_state", "assignee_total_num_patents"),
  all_pages = TRUE
)

#Write above query into rds file
write_rds(localdata, "localdata_patentsview.rds")   #Create file containing above query
list.files()                                        #Confirm
localdata <- read_rds("localdata_patentsview.rds")  #Overwrite "localdata" with rds (of itself|redundant)

#Return the patent fields for patent#, inventor id, and assignee id
#Unnested the localdata from above
unnested_localdata = localdata$data$patents %>% 
  unnest(inventors, .drop = FALSE) %>%
  unnest(assignees)
unnested_localdata

#### Non-Shiny Summations ####

### Core Objective 1
#Get data summary of patents, inventors and assignees
unique_patentnum <- unique(na.omit(unnested_localdata$patent_number)) 
unique_assigneeid <- unique(na.omit(unnested_localdata$assignee_id))
unique_inventorid <- unique(na.omit(unnested_localdata$inventor_id))

### Core Objective 3
five_organization = rev(sort(table(unnested_localdata$assignee_organization)))[1:5]
five_organization

###Find average length between patent_date and app_date
dateDiffs =  difftime(unnested_resultDate$patent_date, unnested_resultDate$app_date , units = c("days"))
avgDateDiffs = mean(dateDiffs)
avgDateDiffs


#### Shiny App ####

ui <- fluidPage(
  titlePanel(
    h1("Patentsview Project",
       style = "text-align:center; line-height: 3.0; color: #DC143C;")),
  sidebarLayout(
    sidebarPanel(
      
      ##Core Obj 4##
      #Allow user to select states from dropdown
      selectInput(inputId = "state",
                  label = "Assignee State",
                  choices = c("All", 
                              unique(as.character(unnested_localdata$assignee_lastknown_state)))), 
      
      ##Core Obj 5##
      #Allow user to find inventor last name by typing
      textInput(inputId = "name",
                label = "Inventor Last Name",
                value = "All")),
    
    mainPanel(
      
      ##Core Obj 1##
      #Summary for patents, inventors and assignees
      htmlOutput("sum"),
      hr(),
      
      ##Menu Obj 5##
      #Average length between patent_date and app_date
      htmlOutput("average"),
      hr(),
      
      #Tabs for each table and plot
      tabsetPanel(type = "tabs",
                  tabPanel("Main Table", dataTableOutput("main_table")),
                  tabPanel("Top 5 Assignee Organization", plotOutput("main_plot"), 
                           tableOutput("assignee_table")),
                  tabPanel("Top 5 Prolific Inventor Table", tableOutput("inventor_table")),
                  tabPanel("Top 5 Inventor Country Table", tableOutput("country_table"))),
      
      #Background image
      tags$style("body{background:url(http://static.tumblr.com/a737e22f0b9433baae031c565ed58271/boswqy8/e9pmykmme/tumblr_static_12.png)}")
    )
  )
)

server <- function(input, output) {
  
  ##Core Obj 1##
  #Patents, inventors and assignees output
  output$sum = renderText({
    paste("<strong>Patents:</strong>", length(unique_patentnum), 
          "<strong>&emsp;&emsp;&emsp;&emsp;Inventors:</strong>", length(unique_inventorid), 
          "<strong>&emsp;&emsp;&emsp;&emsp;Assignees:</strong>", length(unique_assigneeid))
  })
  
  ##Menu Obj 4##
  #Table shows length patent_date and app_date 
  output$average = renderText({
    paste("<strong>Difference in Filed and Granted Date:</strong>", avgDateDiffs)
  })
  
  ##Core Obj 2##
  #Patentsview table output
  output$main_table <- renderDataTable({
    df_sub = unnested_localdata %>%
      select(patent_number, patent_date, inventor_last_name, inventor_city,
             assignee_organization, assignee_lastknown_state)
    if(input$name != "All"){
      df_sub = df_sub %>%
        filter(df_sub$inventor_last_name == input$name)
    }
    if(input$state != "All"){
      df_sub = df_sub %>%
        filter(df_sub$assignee_lastknown_state == input$state)
    }
    return(df_sub)
  })
  
  ##Core Obj 3##
  #Boxplot shows top 5 assignee organizations output
  output$main_plot <- renderPlot({
    barplot(five_organization, 
            col = "tomato",
            main = "Top 5 assignee organizations",
            ylab = "Number of patents",
            xlab = "Assignee Organizations")
  })
  
  ##Core Obj 3##
  #Table shows top 5 assignee organizations
  output$assignee_table <- renderTable({
    head(five_organization)
  })
  
  ##Menu Obj 1##
  #Table shows top 5 prolific inventors
  output$inventor_table <- renderTable({
    head(five_inventor)
  })  
  
  ##Menu Obj 2##
  #Table shows top 5 inventor countries
  output$country_table <- renderTable({
    head(five_country)
  })
}

shinyApp(ui, server)