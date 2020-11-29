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
library(stringr)
rm(list=ls())
#max file upload size set to 30MB
options(shiny.maxRequestSize = 30*1024^2)

# Define UI for application that draws a histogram
ui <- navbarPage("Shiny Copy Numbers!",

    # Sidebar with a slider input for number of bins 
    tabPanel("Upload readCounter Output",
             sidebarPanel(
                 fileInput("samplewig", "Choose readCounter wig",
                           multiple = TRUE,
                           accept = c("wig",
                                      "readCounter wig from bam",
                                      c(".wig"))),
                 
                 selectInput("window", "readCounter Window",
                             choices = c("1,000 bp" = 1000,
                                         "10,000 bp" = 10000,
                                         "100,000 bp" = 100000,
                                         "1,000,000 bp" = 1000000),
                             selected = 1000000),
                 tags$hr(),
                 
                 selectInput("genome", "Genome Version",
                              choices = c(None = NULL,
                                          "hg19" = "hg19",
                                          "GRCh37" = "GRCh37",
                                          "b37 (decoy)" = "b37"),
                              selected = "b37"),
                 tags$hr(),
                 
                 width = 3
             ),
             
             mainPanel(type="tabs",
                       tabPanel("Upload readCounter Output", DT::dataTableOutput("inputwig"))
             )
    ),
    
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
             sidebarPanel(
                 downloadButton("downloadData", "Download")
             ),
             
             mainPanel(type="tabs",
                       tabPanel("Merge Tables",  DT::dataTableOutput("mergedTable"))
                       )
             )
    )

# Define server logic required to draw a histogram
server <- function(input, output) {
    
    output$inputwig = DT::renderDataTable({
        req(input$samplewig)
        
        if(length(input$samplewig[,1]) > 1){
            progress = shiny::Progress$new()
            on.exit(progress$close())
            
            progress$set(message = "Merging files")
            
            for(i in 1:length(input$samplewig[,1])){
                temp <- read.csv(input$samplewig[[i, 'datapath']], header=FALSE)
                message(substr(input$samplewig[[i, 'name']], 1, nchar(input$samplewig[[i, 'name']])-4))
                
                
                progress$inc(1/length(input$samplewig[,1]), detail=paste("Working on file #", i))
            }
            
            
        }else{
            datawig = read.csv(input$samplewig$datapath,
                               header = )
        }
        
    })

    output$rawTable1 = DT::renderDataTable({
        req(input$file1)
        
        df_file1 = read.csv(input$file1$datapath,
                      header = input$header,
                      sep = input$sep,
                      quote = input$quote)
        df_file1 = Filter(function(x)!all(is.na(x)),df_file1)
        assign('file1', df_file1, envir=.GlobalEnv)
        DT::datatable(df_file1, width = 800)
    })
    
    output$rawTable2 = DT::renderDataTable({
        req(input$file2)
        
        df_file2 = read.csv(input$file2$datapath,
                      header = input$header,
                      sep = input$sep,
                      quote = input$quote)
        df_file2 = Filter(function(x)!all(is.na(x)),df_file2)
        assign('file2', df_file2, envir=.GlobalEnv)
        DT::datatable(df_file2, width = 800)
    })
    
    output$mergedTable = DT::renderDataTable({
        req(input$file1)
        req(input$file2)
        
        df_merged = merge(file1, file2, by=intersect(colnames(file1)[apply(sample, MARGIN = 2, FUN = function(x) !any(is.na(x)))],
                                                     colnames(file2)[apply(sample, MARGIN = 2, FUN = function(x) !any(is.na(x)))]), all=TRUE)
        colnames(df_merged) = gsub('\\.x',
                                   paste(".",
                                         substr(input$file1$name,
                                                start = 1, 
                                                stop = nchar(input$file1$name)-4),
                                         sep=""),
                                   colnames(df_merged))
        colnames(df_merged) = gsub('\\.y',
                                   paste(".",
                                         substr(input$file2$name,
                                                start = 1, 
                                                stop = nchar(input$file2$name)-4),
                                         sep=""),
                                   colnames(df_merged))
        assign('df_merged', df_merged, envir=.GlobalEnv)
        DT::datatable(df_merged, width=100)
    })
    
    output$downloadData = downloadHandler(
        filename = function(){
            paste("merged-",Sys.Date(),".csv",sep="")
        },
        content = function(file){
            write.csv(df_merged, file)
        }
    )
}

# Run the application 
shinyApp(ui = ui, server = server)
