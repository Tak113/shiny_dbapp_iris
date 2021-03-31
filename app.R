library(shiny)
# library(tidyverse)
library(DBI)
library(odbc)
library(RPostgres)
library(config)
  
# connect to db
dw <- config::get('dataconnection')
Sys.setenv(R_CONFIG_ACTIVE = "shinyapps")
con <- dbConnect(odbc::odbc(),
                Driver = dw$driver,
                Database = dw$database,
                UID = dw$uid,
                PWD = dw$pwd,
                Server = dw$server,
                Port = dw$port
                )

#below is working
# con <- DBI::dbConnect(odbc::odbc(),
#                     driver = "PostgreSQL",
#                     # dbname = "taktestdbinstance",
#                     Database = "postgres",
#                     UID = "postgres",
#                     PWD = "takawspostgres",
#                     Server = "taktestdbinstance.ctu5c4ueoiyt.us-west-2.rds.amazonaws.com",
#                     port = 5432)
# con <- dbConnect(
#                  RPostgres::Postgres(),
#                  # driver = "PostgreSQL",
#                  host = "taktestdbinstance.ctu5c4ueoiyt.us-west-2.rds.amazonaws.com",
#                  # dbname = "taktestdbinstance",
#                  # dbname = "postgres",
#                  user = "postgres",
#                  password = "takawspostgres",
#                  port = 5432)



# load table
dat <- dbReadTable(con,'iris_test')
  
# get column name except for 'species'
# names() provides all column name
# setdiff(x, y) with names () provides specific column name x, excluding y
vars <- setdiff(names(dat), "Species")
  

shinyApp(

# ui ----------------------------------------------------------------------
  ui = fluidPage(
      pageWithSidebar(
      headerPanel('Iris k-means clustering'),
      sidebarPanel(
        selectInput('xcol', 'X Variable', vars),
        selectInput('ycol', 'Y Variable', vars, selected = vars[[2]]),
        numericInput('clusters', 'Cluster count', 3, min = 1, max = 9)
      ),
      mainPanel(
        plotOutput('plot1'),
        DT::dataTableOutput('table1')
      )
    )
  ),


# server ------------------------------------------------------------------
  server = function(input, session, output) {
    # Combine the selected variables into a new data frame
    selectedData <- reactive({
      dat[, c(input$xcol, input$ycol)]
    })
    
    clusters <- reactive({
      kmeans(selectedData(), input$clusters)
    })
    
    output$plot1 <- renderPlot({
      palette(c("#E41A1C", "#377EB8", "#4DAF4A", "#984EA3",
                "#FF7F00", "#FFFF33", "#A65628", "#F781BF", "#999999"))
      
      par(mar = c(5.1, 4.1, 0, 1))
      plot(selectedData(),
           col = clusters()$cluster,
           pch = 20, cex = 3)
      points(clusters()$centers, pch = 4, cex = 4, lwd = 4)
    })
    
    output$table1 <- DT::renderDataTable(DT::datatable({
      dat
    }))
  }
)