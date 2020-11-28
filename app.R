#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

# Define UI for application that draws a histogram
ui <- navbarPage("Shiny Copy Numbers!",

    # Sidebar with a slider input for number of bins 
    tabPanel("Upload Data",
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
         
             radioButtons("disp", "Display",
                          choices = c(Head = "head",
                                      All = "all"),
                          selected = "head")
             ),
    tabPanel("Merge Tables",
             
    ),
    
    mainPanel(
        tableOutput("contents")
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {

    output$contents = renderTable({
        req(input$file1)
        
        df = read.csv(input$file1$datapath,
                      header = input$header,
                      sep = input$sep,
                      quote = input$quote)
        
        if(input$disp == "head") {
            return(head(df))
        } else {
            return(df)
        }
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
