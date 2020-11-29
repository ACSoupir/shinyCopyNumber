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
library(HMMcopy)
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
                             choices = c("1,000 bp" = "1000",
                                         "10,000 bp" = "10000",
                                         "100,000 bp" = "100000",
                                         "1,000,000 bp" = "1000000"),
                             selected = "1000000"),
                 tags$hr(),
                 
                 selectInput("genome", "Genome Version",
                              choices = c(None = NULL,
                                          "hg19" = "GCF_000001405.25_GRCh37.p13_genomic.fna",
                                          "GRCh37" = "hg19.fa",
                                          "b37 (decoy)" = "Homo_sapiens_assembly19.fasta"),
                              selected = "Homo_sapiens_assembly19.fasta"),
                 tags$hr(),
                 
                 actionButton("copyNumberCalculate", "Calculate Copy Number"),
                 numericInput("read_thresh", "Read Threshold",
                              value = 0.01, min = 0.0001, max = 0.1,
                              step = 0.0001),
                 numericInput("gc_thresh", "GC Threshold",
                              value = 0.001, min = 0.0001, max = 0.1,
                              step = 0.0001),
                 
                 width = 3
             ),
             
             mainPanel(type="tabs",
                       tabPanel("Upload readCounter Output", DT::dataTableOutput("inputwig")),
                       tabPanel("Upload readCounter Output", DT::dataTableOutput("copyNumberTable"))
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
    
    buttons <- reactiveValues(data = NULL)
    
    output$inputwig = DT::renderDataTable({
        req(input$samplewig)
        
        if(length(input$samplewig[,1]) > 1){
            progress = shiny::Progress$new()
            on.exit(progress$close())
            
            progress$set(message = "Merging files")
            
            for(i in 1:length(input$samplewig[,1])){
                temp <- read.csv(input$samplewig[[i, 'datapath']], header=FALSE)
                colnames(temp) = substr(input$samplewig[[i, 'name']], 1, nchar(input$samplewig[[i, 'name']])-4)
                if(!exists("datawig")){
                    datawig = temp
                }else{
                    datawig = cbind(datawig, temp)
                }
                
                progress$inc(1/length(input$samplewig[,1]), detail=paste("Working on file #", i))
            }
            
            
        }else{
            datawig = read.csv(input$samplewig$datapath,
                               header = )
        }
        
        assign('sample_wig', datawig, envir=.GlobalEnv)
        DT::datatable(datawig, width = 800)
        
    })
    
    observeEvent(input$copyNumberCalculate, {
        buttons$data = 1
    })
    
    output$copyNumberTable = DT::renderDataTable({
        req(input$samplewig)
        if(is.null(buttons$data)) return()
        
        progress = shiny::Progress$new()
        on.exit(progress$close())
        
        progress$set(message = "calculating copy number")
        
        gccontent = read.csv(gzfile(paste("gccontent/",genome,".gc",window,".wig.gz",sep="")),header=FALSE)
        mappability = read.csv(gzfile(paste("mappability/",genome,".map",window,".wig.gz",sep="")),header=FALSE)
        for(i in 1:ncol(sample_wig)){
            uncorrected_reads = wigsToRangedData2(sample_wig[,i], gccontent[,1], mappability[,1])
            corrected_copy = correctReadcount2(uncorrected_reads, routlier = input$read_thresh, doutlier = input$gc_thresh)
            
            progress$inc(1/length(input$samplewig[,1]), detail=paste("Calculating Copy Number for ", colnames(sample_wig)[i]))
        }
        
        DT::datatable(sample_wig, width = 800)
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

wigToRangedData2 = function (wigfile, verbose = TRUE){
    if (verbose) {
        message(paste("Slurping:", wigfile))
    }
    input <- wigfile
    breaks <- c(grep("fixedStep", input), length(input) + 
                    1)
    temp <- NULL
    span <- NULL
    for (i in 1:(length(breaks) - 1)) {
        data_range <- (breaks[i] + 1):(breaks[i + 1] - 1)
        track_info <- input[breaks[i]]
        if (verbose) {
            message(paste("Parsing:", track_info))
        }
        tokens <- strsplit(sub("fixedStep chrom=(\\S+) start=(\\d+) step=(\\d+) span=(\\d+)", 
                               "\\1 \\2 \\3 \\4", track_info, perl = TRUE), 
                           " ")[[1]]
        span <- as.integer(tokens[4])
        chr <- rep.int(tokens[1], length(data_range))
        pos <- seq(from = as.integer(tokens[2]), by = as.integer(tokens[3]), 
                   length.out = length(data_range))
        val <- as.numeric(input[data_range])
        temp <- c(temp, list(data.frame(chr, pos, val)))
    }
    if (verbose) {
        message("Sorting by decreasing chromosome size")
    }
    lengths <- as.integer(lapply(temp, nrow))
    temp <- temp[order(lengths, decreasing = TRUE)]
    temp = do.call("rbind", temp)
    output <- data.table(chr = temp$chr, start = temp$pos, end = temp$pos + 
                             span, value = temp$val)
    return(output)
}

wigToArray2 = function (wigfile, verbose = TRUE){
    if (verbose) {
        message(paste("Slurping:", wigfile))
    }
    input <- wigfile
    breaks <- c(grep("fixedStep", input), length(input) + 
                    1)
    temp <- NULL
    for (i in 1:(length(breaks) - 1)) {
        data_range <- (breaks[i] + 1):(breaks[i + 1] - 1)
        track_info <- input[breaks[i]]
        if (verbose) {
            message(paste("Parsing:", track_info))
        }
        tokens = strsplit(sub("fixedStep chrom=(\\S+) start=(\\d+) step=(\\d+) span=(\\d+)", 
                              "\\1 \\2 \\3 \\4", track_info, perl = TRUE), 
                          " ")[[1]]
        val <- as.numeric(input[data_range])
        temp <- c(temp, list(val))
    }
    if (verbose) {
        message("Sorting by decreasing chromosome size")
    }
    lengths <- as.integer(lapply(temp, length))
    temp <- temp[order(lengths, decreasing = TRUE)]
    output <- unlist(temp)
    return(output)
}

wigsToRangedData2 = function (readfile, gcfile, mapfile, verbose = FALSE){
    output <- wigToRangedData2(readfile, verbose)
    colnames(output)[4] <- c("reads")
    output$reads <- as.integer(output$reads)
    gc <- wigToArray2(gcfile, verbose)
    if (nrow(output) != length(gc)) {
        stop(paste("Number of readcount bins (", nrow(output), 
                   ") differs from GC count bins (", length(gc), 
                   ")", sep = ""))
    }
    map = wigToArray2(mapfile, verbose)
    if (nrow(output) != length(map)) {
        stop(paste("Number of readcount bins (", nrow(output), 
                   ") differs from mappability bins (", length(map), 
                   ")", sep = ""))
    }
    output$gc <- gc
    output$map <- map
    return(output)
}

correctReadcount2 = function (x, mappability = 0.9, samplesize = 50000, routlier = 0.01, doutlier = 0.001,  verbose = TRUE){
    if (length(x$reads) == 0 | length(x$gc) == 0 | length(x$map) == 
        0) {
        stop("Missing one of required columns: reads, gc, map")
    }
    if (verbose) {
        message("Applying filter on data...")
    }
    x$valid <- TRUE
    x$valid[x$reads <= 0 | x$gc < 0] <- FALSE
    x$ideal <- TRUE
    
    range <- quantile(x$reads[x$valid], prob = c(0, 1 - routlier), 
                      na.rm = TRUE)
    
    domain <- quantile(x$gc[x$valid], prob = c(doutlier, 1 - 
                                                   doutlier), na.rm = TRUE)
    x$ideal[!x$valid | x$map < mappability | x$reads <= range[1] | 
                x$reads > range[2] | x$gc < domain[1] | x$gc > domain[2]] <- FALSE
    if (verbose) {
        message("Correcting for GC bias...")
    }
    set <- which(x$ideal)
    select <- sample(set, min(length(set), samplesize))
    rough = loess(x$reads[select] ~ x$gc[select], span = 0.03)
    i <- seq(0, 1, by = 0.001)
    final = loess(predict(rough, i) ~ i, span = 0.3)
    x$cor.gc <- x$reads/predict(final, x$gc)
    if (verbose) {
        message("Correcting for mappability bias...")
    }
    coutlier <- 0.01
    range <- quantile(x$cor.gc[which(x$valid)], prob = c(0, 1 - 
                                                             coutlier), na.rm = TRUE)
    set <- which(x$cor.gc < range[2])
    select <- sample(set, min(length(set), samplesize))
    final = approxfun(lowess(x$map[select], x$cor.gc[select]))
    x$cor.map <- x$cor.gc/final(x$map)
    x$copy <- x$cor.map
    x$copy[x$copy <= 0] = NA
    x$copy <- log(x$copy, 2)
    return(x)
}

# Run the application 
shinyApp(ui = ui, server = server)
