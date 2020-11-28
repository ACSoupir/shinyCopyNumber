#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(DT)

# Define UI for application that draws a histogram
ui <- navbarPage("Shiny Copy Numbers!",

    # Sidebar with a slider input for number of bins 
    tabPanel("Upload Data",
             sidebarPanel(
                 fileInput("file1", "Choose CSV File 1",
                           multiple = TRUE,
                           accept = c("text/csv/tsv",
                                      "text/comma-separated-values/tab-separated",
                                      c(".csv",".tsv"))),
                 fileInput("file2", "Choose CSV File 2",
                           multiple = TRUE,
                           accept = c("text/csv/tsv",
                                      "text/comma-separated-values/tab-separated",
                                      c(".csv",".tsv"))),
        
                 tags$hr(),
         
                 checkboxInput("header", "Header", TRUE),
        
                  radioButtons("sep", "Separator",
                               choices = c(Comma = ",",
                                           Semicolon = ";",
                                           Tab = "\t"),
                               selected = ","),
                 tags$hr(),
        
             radioButtons("quote", "Quote",
                     choices = c(None = "",
                                 "Double Quote" = '"',
                                 "Single Quote" = "'"),
                     selected = '"'),
             tags$hr(),
             width = 3
        ),
        mainPanel(type="tabs",
                  tabPanel("Upload Data", DT::dataTableOutput("rawTable1"), DT::dataTableOutput("rawTable2"))
                  )
        ),
    
    tabPanel("Merge Tables",
             mainPanel(type="tabs",
                       tabPanel("Merge Tables",  DT::dataTableOutput("mergedTable"))
                       )
             )
    )

# Define server logic required to draw a histogram
server <- function(input, output) {

    output$rawTable1 = DT::renderDataTable({
        req(input$file1)
        
        df_file1 = read.csv(input$file1$datapath,
                      header = input$header,
                      sep = input$sep,
                      quote = input$quote)
        DT::datatable(df_file1, width = 800)
    })
    
    output$rawTable2 = DT::renderDataTable({
        req(input$file2)
        
        df_file2 = read.csv(input$file2$datapath,
                      header = input$header,
                      sep = input$sep,
                      quote = input$quote)
        DT::datatable(df_file2, width = 800)
    })
    
    output$mergedTable = DT::renderDataTable({
        req(input$file1)
        req(input$file2)
        
        df_file1 = read.csv(input$file1$datapath,
                            header = input$header,
                            sep = input$sep,
                            quote = input$quote)
        
        df_file2 = read.csv(input$file2$datapath,
                            header = input$header,
                            sep = input$sep,
                            quote = input$quote)
        
        df_merged = merge(df_file1, df_file2, by=intersect(names(df_file1), names(df_file2)), all=TRUE)
        DT::datatable(df_merged, width=100)
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
