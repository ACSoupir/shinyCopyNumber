#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
rm(list=ls())
library(shiny)
library(DT)
library(stringr)
library(BiocManager)
options(repos = BiocManager::repositories())
library(HMMcopy)
library(copynumber)

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
                 
                 selectInput("correctBy", "Value In New Table",
                             choices = c("GC Corrected" = "cor.gc",
                                         "GC and Map Corrected" = "cor.map",
                                         "Log2 (GC and Map Corrected)" = "copy"),
                             selected = "copy"),
                 tags$hr(),
                 
                 actionButton("exampleData", "Load Example Data"),
                 
                 width = 3
             ),
             
             mainPanel(type="tabs",
                       tabPanel("Upload readCounter Output", DT::dataTableOutput("inputwig", width = 1200)),
                       tabPanel("Upload readCounter Output", DT::dataTableOutput("copyNumberTable", width = 1200))
             )
    ),
    
    tabPanel("Visualize Copy Number",
             sidebarPanel(
               
               uiOutput("choose_genome_sample"),
               uiOutput("choose_genome_control"),
                 tags$hr()
             ),
             
             mainPanel(type="tabs",
                       plotOutput("sampleGenomePlot", width = 800),
                       plotOutput("controlGenomePlot", width = 800)
             ),
    ),
    tabPanel("Plot Sample",
             sidebarPanel(
               uiOutput("choose_sample_to_plot"),
               uiOutput("choose_chromosome")
             ),
             mainPanel(type="tabs",
                       plotOutput("sampleSegmentPlot"))
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
                       tabPanel("Upload Data", DT::dataTableOutput("rawTable1", width = 1200), DT::dataTableOutput("rawTable2", width = 1200))
             )
    ),
    
    tabPanel("Merge Tables",
             sidebarPanel(
                 downloadButton("downloadData", "Download")
             ),
             
             mainPanel(type="tabs",
                       tabPanel("Merge Tables",  DT::dataTableOutput("mergedTable", width = 1200))
                       )
             )
    )

# Define server logic required to draw a histogram
server <- function(input, output) {
    
    buttons <- reactiveValues(data = NULL)
    buttons <- reactiveValues(eData = NULL)
    
    observeEvent(input$copyNumberCalculate, {
        buttons$data = 1
    })
    
    observeEvent(input$exampleData, {
        buttons$eData = 1
    })
    
    output$inputwig = DT::renderDataTable({
        if(is.null(buttons$eData)){
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
                                   header = FALSE)
                colnames(datawig) = substr(input$samplewig$name, 1, nchar(input$samplewig$name)-4)
            }
            
            assign('sample_wig', datawig, envir=.GlobalEnv)
            DT::datatable(datawig, options = list(scrollX = TRUE))
        } else {
            progress = shiny::Progress$new()
            on.exit(progress$close())
            
            progress$set(message="Loading Example Data")
            
            for(i in 1:length(list.files("readCounterWig/b37/"))){
                temp = read.csv(paste("readCounterWig/b37/",list.files("readCounterWig/b37/")[i],sep=""),header=FALSE)
                colnames(temp) = substr(list.files("readCounterWig/b37/")[i], 1, nchar(list.files("readCounterWig/b37/")[i])-4)
                
                if(!exists("datawig")){
                    datawig = temp
                }else{
                    datawig = cbind(datawig, temp)
                }
                
                progress$inc(1/length(list.files("readCounterWig/b37/")), detail=paste("Working on file #", i))
            }
            
            assign('sample_wig', datawig, envir=.GlobalEnv)
            DT::datatable(datawig, options = list(scrollX = TRUE))
        }
    })
    
    output$copyNumberTable = DT::renderDataTable({
        if(is.null(buttons$data)) return()
        
        progress = shiny::Progress$new()
        on.exit(progress$close())
        
        progress$set(message = "calculating copy number")
        
        gccontent = read.csv(gzfile(paste("gccontent/",input$genome,".gc",input$window,".wig.gz",sep="")),header=FALSE)
        mappability = read.csv(gzfile(paste("mappability/",input$genome,".map",input$window,".wig.gz",sep="")),header=FALSE)
        for(i in 1:ncol(sample_wig)){
            uncorrected_reads = wigsToRangedData2(as.vector(sample_wig[,i]), as.vector(gccontent[,1]), as.vector(mappability[,1]))
            corrected_copy = as.data.frame(correctReadcount2(uncorrected_reads,
                                                             routlier = input$read_thresh,
                                                             doutlier = input$gc_thresh),
                                           stringsAsFactors=FALSE)
            
            if(!exists("merged_copy")){
                merged_copy = corrected_copy[,c("chr","start","end",input$correctBy)]
            } else {
                merged_copy = cbind(merged_copy, corrected_copy[,input$correctBy])
            }
            
            colnames(merged_copy)[i+3] = colnames(sample_wig)[i]
            
            progress$inc(1/length(input$samplewig[,1]), detail=paste("Calculating Copy Number for ", colnames(sample_wig)[i]))
        }
        
        assign("merged_copy_numbers", merged_copy, envir=.GlobalEnv)
        DT::datatable(merged_copy, options = list(scrollX = TRUE))
    })
    
    output$choose_genome_sample = renderUI({
      if(is.null(merged_copy_numbers)){
        return()
      }
      
      sampleNames = names(merged_copy_numbers)[4:length(merged_copy_numbers)]
      
      selectInput("sampleGenomePlot", "Choose Sample",
                  choices = sampleNames,
                  selected = sampleNames)
    })
    
    output$choose_genome_control = renderUI({
      if(is.null(merged_copy_numbers)){
        return()
      }
      
      sampleNames = names(merged_copy_numbers)[4:length(merged_copy_numbers)]
      
      selectInput("controlGenomePlot", "Choose Control",
                  choices = sampleNames,
                  selected = sampleNames)
    })
    
    output$sampleGenomePlot = renderPlot({
      if(is.null(input$sampleGenomePlot)){
        return()
      }
      
      if(is.null(buttons$data)) return()
      
      sample_name = input$sampleGenomePlot
      
      
      temp_copy_numbers = merged_copy_numbers[,1:3]
      temp_copy_numbers$mid_window = as.integer(merged_copy_numbers[,3] - (merged_copy_numbers[,3] - merged_copy_numbers[,2]) / 2)
      temp_copy_numbers = temp_copy_numbers[,-c(2:3)]
      temp_copy_numbers = cbind(temp_copy_numbers, merged_copy_numbers[,4:ncol(merged_copy_numbers)])
      
      temp_winsorize = winsorize(data=temp_copy_numbers)
      test_winsorize = temp_winsorize[complete.cases(temp_winsorize),]
      test_winsorize = test_winsorize[order(test_winsorize$chrom),]
      single.seg = pcf(data=test_winsorize, gamma=40)
      print(input$choose_genome_sample)
      plotGenome(data=temp_copy_numbers,
                 segments = single.seg,
                 sample = grep(sample_name,
                               colnames(test_winsorize)[-c(1:2)]),
                 main=paste("Tumor:",sample_name),
                 connect=FALSE,
                 col="black",
                 q.col="blue" #trucated value color
                 ,cex=1
                 ,pch=20)
    })
    
    output$controlGenomePlot = renderPlot({
      if(is.null(input$controlGenomePlot)){
        return()
      }
      
      if(is.null(buttons$data)) return()
      
      control_name = input$controlGenomePlot
      
      
      temp_copy_numbers = merged_copy_numbers[,1:3]
      temp_copy_numbers$mid_window = as.integer(merged_copy_numbers[,3] - (merged_copy_numbers[,3] - merged_copy_numbers[,2]) / 2)
      temp_copy_numbers = temp_copy_numbers[,-c(2:3)]
      temp_copy_numbers = cbind(temp_copy_numbers, merged_copy_numbers[,4:ncol(merged_copy_numbers)])
      
      temp_winsorize = winsorize(data=temp_copy_numbers)
      test_winsorize = temp_winsorize[complete.cases(temp_winsorize),]
      test_winsorize = test_winsorize[order(test_winsorize$chrom),]
      single.seg = pcf(data=test_winsorize, gamma=40)
      print(input$choose_genome_control)
      plotGenome(data=temp_copy_numbers,
                 segments = single.seg,
                 sample = grep(control_name,
                               colnames(test_winsorize)[-c(1:2)]),
                 main=paste("Control:", control_name),
                 connect=FALSE,
                 col="black",
                 q.col="blue" #trucated value color
                 ,cex=1
                 ,pch=20)
    })
    
    output$choose_chromosome = renderUI({
      if(is.null(merged_copy_numbers)){
        return()
      }
      X=NULL
      Y=NULL
      
      if("X" %in% merged_copy_numbers$chr){
        X=TRUE
      }
      if("Y" %in% merged_copy_numbers$chr){
        Y=TRUE
      }
      chromNames = unique(merged_copy_numbers$chr)
      chromNames = as.numeric(chromNames)[!is.na(as.numeric(chromNames))]
      chromNames = sort(chromNames, decreasing = FALSE)
      chromNames = append(chromNames, c(if(X) "X", if(Y) "Y"))
      
      selectInput("sampleSegmentChromosome", "Choose Chromosome",
                  choices = chromNames,
                  selected = chromNames[1])
    })
    
    output$controlGenomePlot = renderPlot({
      if(is.null(input$controlGenomePlot)){
        return()
      }
      
      if(is.null(buttons$data)) return()
      
      control_name = input$controlGenomePlot
      
      
      temp_copy_numbers = merged_copy_numbers[,1:3]
      temp_copy_numbers$mid_window = as.integer(merged_copy_numbers[,3] - (merged_copy_numbers[,3] - merged_copy_numbers[,2]) / 2)
      temp_copy_numbers = temp_copy_numbers[,-c(2:3)]
      temp_copy_numbers = cbind(temp_copy_numbers, merged_copy_numbers[,4:ncol(merged_copy_numbers)])
      
      temp_winsorize = winsorize(data=temp_copy_numbers)
      test_winsorize = temp_winsorize[complete.cases(temp_winsorize),]
      test_winsorize = test_winsorize[order(test_winsorize$chrom),]
      single.seg = pcf(data=test_winsorize, gamma=40)
      print(input$choose_genome_control)
      plotGenome(data=temp_copy_numbers,
                 segments = single.seg,
                 sample = grep(control_name,
                               colnames(test_winsorize)[-c(1:2)]),
                 main=paste("Control:", control_name),
                 connect=FALSE,
                 col="black",
                 q.col="blue" #trucated value color
                 ,cex=1
                 ,pch=20)
    })

    output$rawTable1 = DT::renderDataTable({
        if(is.null(buttons$eData)){
            req(input$file1)
            
            df_file1 = read.csv(input$file1$datapath,
                          header = input$header,
                          sep = input$sep,
                          quote = input$quote)
            df_file1 = Filter(function(x)!all(is.na(x)),df_file1)
            assign('file1', df_file1, envir=.GlobalEnv)
            DT::datatable(df_file1, options = list(scrollX = TRUE))
        } else {
            df_file1 = read.csv("donorInformation/specimen.tsv",
                                header = input$header,
                                sep = "\t",
                                quote = input$quote)
            df_file1 = Filter(function(x)!all(is.na(x)),df_file1)
            assign('file1', df_file1, envir=.GlobalEnv)
            DT::datatable(df_file1, options = list(scrollX = TRUE))
        }
    })
    
    output$rawTable2 = DT::renderDataTable({
        if(is.null(buttons$eData)){
            req(input$file2)
            
            df_file2 = read.csv(input$file2$datapath,
                          header = input$header,
                          sep = input$sep,
                          quote = input$quote)
            df_file2 = Filter(function(x)!all(is.na(x)),df_file2)
            assign('file2', df_file2, envir=.GlobalEnv)
            DT::datatable(df_file2, options = list(scrollX = TRUE))
        } else {
            df_file2 = read.csv("donorInformation/sample.tsv",
                                header = input$header,
                                sep = "\t",
                                quote = input$quote)
            df_file2 = Filter(function(x)!all(is.na(x)),df_file2)
            assign('file2', df_file2, envir=.GlobalEnv)
            DT::datatable(df_file2, options = list(scrollX = TRUE))
        }
    })
    
    output$mergedTable = DT::renderDataTable({
        
        df_merged = merge(file1, file2, by=intersect(colnames(file1)[apply(file1, MARGIN = 2, FUN = function(x) !any(is.na(x)))],
                                                     colnames(file2)[apply(file2, MARGIN = 2, FUN = function(x) !any(is.na(x)))]), all=TRUE)
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
        DT::datatable(df_merged, options = list(scrollX = TRUE))
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

# checkAndRetrievePlotInput2 <- function(data,segments,winsoutliers,type,xaxis,pos.unit,sample,chrom=NULL){
#     
#     if(type=="aspcf"){
#         logR <- data
#         BAF <- data$BAF
#         #Check that at least one of of logR, BAF and segments has been specified
#         if(all(sapply(list(logR,BAF,segments),is.null))){
#             stop("Arguments 'logR' and 'BAF', or 'segments', must be specified!",call.=FALSE)  
#         }
#         if((is.null(logR)&&!is.null(BAF))||(is.null(BAF)&&!is.null(logR))){
#             stop("'BAF' must be specified if 'logR' is specified, and vice versa",call.=FALSE)
#         }
#         if(!is.null(logR)){
#             #logR and BAF could possibly have been winsorized: make sure it is a data frame:
#             logR <- pullOutContent(logR,what="wins.data") 
#             BAF <- pullOutContent(BAF,what="wins.data")
#             #Check dimensions of logR and BAF:
#             stopifnot(ncol(logR)>=3)
#             stopifnot(ncol(logR)==ncol(BAF),nrow(logR)==nrow(BAF))
#         }
#         data <- logR    
#     }else{
#         #Check that at least of of data and segments has been specified
#         if(is.null(data)&&is.null(segments)){
#             stop("One of the arguments 'data' and 'segments' must be specified!",call.=FALSE)
#         }
#     }
#     
#     #Check and retrieve data input:
#     if(!is.null(data)){
#         #data could possibly come from winsorize, make sure to pull out wins.data data frame
#         data <- pullOutContent(data,what="wins.data")
#         stopifnot(ncol(data)>=3)  #something is missing in input data
#         #Make sure chromosomes are numeric:
#         data[,1] <- numericChrom(data[,1])
#         
#         #Check winsoutliers
#         if(!is.null(winsoutliers)){
#             winsoutliers <- checkWinsoutliers(winsoutliers,data)
#         }
#         
#     }#endif
#     
#     #Check, modify and retrieve segments input:
#     if(!is.null(segments)){
#         segments <- checkSegments(segments,type)
#     }#endif
#     
#     
#     #Check sampleIDs to be plotted (only plot sampleIDs found in both data and segments)
#     sampleID <- checkSampleID(data,segments,sample)
#     
#     #Check and if necessary modify chrom to be plotted:
#     if(type!="genome"){
#         chrom <- checkChrom(data,segments,chrom)
#     }
#     #Check xaxis input:
#     if(!xaxis %in% c("pos","index")){
#         stop("xaxis must be one of 'pos' and 'index'",call.=FALSE)
#     }
#     
#     
#     #Check that pos.unit has been specified if xaxis="pos"
#     if(xaxis=="pos"){
#         #Check pos.unit input:
#         if(!pos.unit %in% c("bp","kbp","mbp")){
#             stop("pos.unit must be one of bp, kbp and mbp",call.=FALSE)
#         }
#     }
#     
#     if(type=="aspcf"){
#         #Return data as list
#         data <- list(logR=data,BAF=BAF)
#     }
#     
#     return(list(data=data,segments=segments,sampleID=sampleID,chrom=chrom,winsoutliers=winsoutliers))
# } 
# 
# pullOutContent <- function(res,what="segments"){
#     
#     #check if input is data frame or list
#     if(!is.data.frame(res)){
#         #res could either be a list containing the two segmentation elements segments and estimates, a list containing the two winsorize elements wins.data and wins.outliers, a list containing several segments as data frames, or a list containing several lists with segmentation results
#         if("segments" %in% names(res)){
#             #can assume that the list contains the output from one of the segmentation algorithms and has names segments and estimates
#             #pick out the desired data frame depending on input in what:
#             if(what=="estimates"){
#                 if("logR_estimates" %in% names(res)){
#                     #Segmentation results come from aspcf which returns a different name for estimates
#                     res <- res$logR_estimates  
#                 }else{
#                     res <- res$estimates
#                 }
#             }else if(what=="segments"){
#                 res <- res$segments
#             }
#         }else if("wins.data" %in% names(res)){
#             #can assume that the list contains output from winsorize and has names wins.data and wins.outliers
#             #pick out the desired data frame depending on input in what:
#             if(what %in% c("wins.data","estimates")){
#                 res <- res$wins.data
#             }else if(what=="wins.outliers"){
#                 res <- res$wins.outliers
#             }
#         }else{
#             #assume that the list contains different segmentation results
#             #if each element in the list is itself a list containing segments and estimates need to pull out the segments (functions that take estimates as input does not have the option of specifying list of estimates)
#             nSeg <- length(res)
#             for(l in 1:nSeg){
#                 if(!is.data.frame(res[[l]])){
#                     #pull out the segments data frame
#                     res[[l]] <- res[[l]]$segments
#                 }    
#             }
#         } 
#     }
#     #If a data frame, res should be already be a data frame with the correct information, and does not need modification
#     
#     return(res)
#     
# }
# 
# numericChrom <- function(chrom){ 
#     if(!is.numeric(chrom)){
#         if(is.factor(chrom)){
#             #If chrom is factor; need to convert to character first
#             chrom <- as.character(chrom)
#         }
#         #Replace X by 23:
#         chrx <- c(which(chrom=="x"),which(chrom=="X"))
#         chrom[chrx] <- 23
#         #Replace Y by 24
#         chry <- c(which(chrom=="y"),which(chrom=="Y"))
#         chrom[chry] <- 24
#         
#         chrom <- as.numeric(chrom)
#     }
#     return(chrom)
# }
# 
# checkSegments <- function(segments,type){
#     
#     #Check and pull out relevant information in segments
#     segments <- pullOutContent(res=segments,what="segments")
#     
#     #Get on list form if segments is matrix or data frame:
#     if(class(segments)!="list"){
#         segments <- list(seg=segments)
#     }
#     
#     nSeg <- length(segments)
#     seg.names <- names(segments)
#     if(is.null(seg.names)){
#         seg.names <- paste("Seg",1:nSeg,"")
#     }
#     #Segments could be on uniseg or multiseg form, convert all to uniseg form:
#     new.segments <- vector("list",nSeg)  #empty list
#     for(s in 1:nSeg){
#         use.seg <- segments[[s]]
#         multi <- is.multiseg(use.seg)
#         if(multi){
#             #stopifnot(ncol(use.seg)>=6)
#             if(ncol(use.seg)<6){
#                 stop("segments format is not correct",call.=FALSE)
#             }
#             #Convert to uniseg format:
#             use.seg <- getUnisegFormat(use.seg)
#         }else{
#             if(type!="aspcf"){
#                 if(ncol(use.seg)<7){
#                     stop("segments format is not correct",call.=FALSE)
#                     #stopifnot(ncol(use.seg)>=7)
#                 }
#             }else{
#                 if(ncol(use.seg)<8){
#                     stop("segments format is not correct",call.=FALSE)
#                 }   
#             } 
#         }
#         #Make sure chromosomes in column 2 are numeric:
#         use.seg[,2] <- numericChrom(use.seg[,2])
#         #Make sure arms in column 3 are character:
#         use.seg[,3] <- as.character(use.seg[,3])
#         
#         new.segments[[s]] <- use.seg
#     }
#     
#     names(new.segments) <- seg.names
#     
#     return(new.segments)
#     
# }
# 
# is.multiseg <- function(segments){
#     #If not multisegment, the first column name should be "sampleID"
#     multi <- ifelse(colnames(segments)[1]=="sampleID",FALSE,TRUE)
#     
#     return(multi)
# }
# 
# checkSampleID <- function(data,segments,sample){
#     
#     sampleID <- NULL
#     
#     if(!is.null(data)){
#         sample.names <- colnames(data)[-c(1:2)]
#         #Pick out ID for samples to be plotted
#         if(!is.null(sample)){
#             sampleID <- sample.names[sample]
#             if(length(sampleID)==0){
#                 stop("Input in 'sample' is larger than the number of samples found in 'data'",.call=FALSE)
#             }
#         }else{
#             sampleID <- sample.names
#         }
#     }
#     
#     if(!is.null(segments)){
#         all.segid <- sapply(segments,"[",i=1)   #returns a list with all sampleID for each segmentation
#         all.segid <- lapply(all.segid,as.character)   #convert from factor to charachter
#         all.segid <- lapply(all.segid,unique)         #get unique sampleids in each segmentation
#         
#         #Find common sampleIDs in data and all segmentations, given input in sample
#         i <- 1
#         if(is.null(sampleID)){    #data is NULL
#             #Get sampleIDs to be plotted for the first segmentation result
#             sampleID <- all.segid[[1]] 
#             if(!is.null(sample)){
#                 sampleID <- sampleID[sample]
#                 if(length(sampleID)==0){
#                     stop("Input in 'sample' is larger than the number of samples found in 'segments'",.call=FALSE)
#                 }
#             }
#             i <- i+1
#         } 
#         #Get the sampleIDs that are also found in other segmentations (if more than one) 
#         while(i<=length(all.segid)){
#             sampleID <- intersect(sampleID,all.segid[[i]])
#             i <- i+1
#         }
#         if(length(sampleID)==0){
#             if(!is.null(data)){
#                 stop("no sampleIDs are common in 'data' and 'segments'",.call=FALSE)
#             }else{
#                 stop("no sampleIDs are common in all components of 'segments'",.call=FALSE)
#             }
#         }
#     }  
#     
#     
#     #Check input sampleID and print errors or warnings if necessary:  
#     sampleID <- sampleID[!is.na(sampleID)]  #could be NA if 'sample' is outside the number of samples represented in data/segments
#     
#     return(sampleID)
#     
# }
# 
# getPlotParameters <- function(type,cr,nSeg,sampleID=NULL,chrom=NULL,plot.ideo,xaxis,assembly,...){
#     
#     #Apply a scaling factor according to number of columns and rows in plot:
#     #seems to work ok:
#     f <- 1-0.013*cr
#     
#     
#     #List with default plotting parameters:
#     arg <- list(title="", 
#                 dir.print = NULL, 
#                 file.name = NULL, 
#                 onefile = TRUE,
#                 col="grey",
#                 wins.col="magenta",
#                 q.col="grey",
#                 pch=46,
#                 wins.pch=42,
#                 q.pch=42,
#                 ylab="Log R",
#                 las=1,
#                 h=0,
#                 h.lty=5,
#                 equalRange=TRUE,
#                 h.col="darkgrey",
#                 cyto.text=FALSE,
#                 plot.unit="mbp",
#                 seg.col=rainbow(nSeg),
#                 seg.lty=rep(1,nSeg),
#                 connect=TRUE,
#                 equalRange=TRUE,
#                 legend=ifelse(nSeg>1,TRUE,FALSE),
#                 plot.size=c(11.6,8.2),
#                 q=0.01,
#                 #Parameters that depend on the number grid layout in plot:
#                 f=f,
#                 mar=if(plot.ideo) c(0.2*f,3*f,2.5*f,f) else c(1.5*f,3*f,2.5*f,f),
#                 cex=2.5*f,
#                 wins.cex=0.4,
#                 q.cex=0.4,
#                 cex.lab=0.9*f,
#                 cex.main=f,
#                 cex.axis=0.8*f,
#                 cex.cytotext=0.7*f,
#                 cex.chrom=0.8*f,
#                 main.line=0.6*f,
#                 tcl=-0.3*f,
#                 h.lwd=2*f,
#                 seg.lwd=rep(3.5*f,nSeg),
#                 #Parameters that are set at later stage:
#                 xlab=NULL,
#                 main=NULL,
#                 xlim=NULL,
#                 ylim=NULL,
#                 at.x=NULL,
#                 at.y=NULL,
#                 mgp=NULL,
#                 ideo.frac=NA,
#                 assembly=assembly
#     )
#     
#     
#     if(type=="genome"){
#         arg$main.line=1.7*f
#         arg$xlab = ""
#     }
#     
#     if(type=="sample"){
#         arg$title <- sampleID
#     }
#     if(type=="chromosome"){
#         arg$title <- paste("Chromosome",chrom,sep=" ")
#     } 
#     
#     if(type=="aspcf"){
#         arg$ylab <- c("logR","BAF")
#         arg$h <- c(0,0.5)
#         arg$title <- sampleID
#     }
#     
#     
#     #Check for USER MODIFICATIONS:
#     arg <- modifyList(arg,list(...))
#     
#     #Set assembly to refer to stored data instead of character string:
#     arg$assembly <- get(arg$assembly)
#     
#     #Make sure ideogram is not plotted if xlim is specified:
#     if(!is.null(arg$xlim)){
#         plot.ideo = FALSE
#     }
#     
#     #X-AXIS labels:
#     if(is.null(arg$xlab)){
#         if(xaxis=="index"){
#             arg$xlab <- "Probe index"
#         }else if(plot.ideo){
#             arg$xlab <- ""
#         }else{
#             arg$xlab <- paste("Local position (",arg$plot.unit,")",sep="")
#         }
#     }#endif
#     
#     #Modify segment legend:
#     if(is.logical(arg$legend)){
#         if(!arg$legend){
#             arg$legend <- NULL
#         }else{
#             arg$legend <- paste("Seg",1:nSeg,sep="")
#         }
#     }else{
#         #Check length of user input for legend:
#         if(!length(arg$legend)==nSeg){
#             warning("Length of 'legend' does not match the number of segmentations, default legends are used instead",call.=FALSE)
#             arg$legend <- paste("Seg",1:nSeg,sep="")
#         }
#     }#endif
#     
#     #Set default ideo.frac and ideogram margins:
#     if(plot.ideo){
#         #ideogram margins:
#         arg$mar.i <- c(0.2*f,3*f,0,f)
#         if(arg$cyto.text){
#             #Need to increase bottom margin:
#             arg$mar.i <- arg$mar.i + c(2,0,0,0)
#         }
#         #Make sure left and right margins are equal for mar and mar.i:
#         arg$mar.i[c(2,4)] <- arg$mar[c(2,4)] 
#         if(is.na(arg$ideo.frac)){
#             #ideo.frac has not been defined by user:
#             arg$ideo.frac <- 0.05*sqrt(sqrt(cr))
#             if(arg$cyto.text){
#                 #Need larger space for ideogram:
#                 arg$ideo.frac <- arg$ideo.frac*2
#             }
#         }
#     }else{
#         arg$ideo.frac <- 0
#     } 
#     
#     
#     return(arg)
#     
# }
# 
# plotGenome = function (data = NULL, segments = NULL, pos.unit = "bp", 
#                        sample = NULL, assembly = "hg19", winsoutliers = NULL, 
#                        xaxis = "pos", layout = c(1, 1), ...) 
# {
#     input <- checkAndRetrievePlotInput2(data = data, segments = segments, 
#                                        winsoutliers = winsoutliers, type = "genome", xaxis = xaxis, 
#                                        pos.unit = pos.unit, sample = sample)
#     data <- input$data
#     segments <- input$segments
#     sampleID <- input$sampleID
#     winsoutliers <- input$winsoutliers
#     nSample <- length(sampleID)
#     nSeg <- length(segments)
#     sample.names <- colnames(data)[-c(1:2)]
#     nr <- layout[1]
#     nc <- layout[2]
#     rc <- nr * nc
#     arg <- getPlotParameters(type = "genome", cr = nc * 
#                                  nr, nSeg = nSeg, sampleID = sampleID, plot.ideo = FALSE, 
#                              xaxis = xaxis, assembly = assembly, ...)
#     if (is.null(arg$xlim) && xaxis == "pos") {
#         if (!is.null(data)) {
#             chr <- unique(data[, 1])
#         }
#         else {
#             chr <- unique(unlist(sapply(segments, function(seg) {
#                 unique(seg[, 2])
#             })))
#         }
#         arg$xlim <- getGlobal.xlim(op = arg, pos.unit = pos.unit, 
#                                    chrom = chr)
#     }
#     if (!is.null(data) && arg$equalRange) {
#         all.sample <- which(sample.names %in% sampleID)
#         data.lim <- quantile(data[, all.sample + 2], probs = c(arg$q/2, 
#                                                                (1 - arg$q/2)), names = FALSE, type = 4, na.rm = TRUE)
#     }
#     nPage <- ifelse(arg$onefile, 1, ceiling(nSample/(rc)))
#     file.name <- getFilename(nPage, arg$file.name, ID = sampleID, 
#                              type = "genome")
#     nPlot <- 1
#     for (j in 1:length(file.name)) {
#         if (!is.null(arg$dir.print)) {
#             pdf(file = paste(arg$dir.print, "/", file.name[j], 
#                              ".pdf", sep = ""), width = arg$plot.size[1], 
#                 height = arg$plot.size[2], onefile = TRUE, paper = "a4")
#         }
#         else {
#             if (dev.cur() <= j) {
#                 dev.new(width = arg$plot.size[1], height = arg$plot.size[2], 
#                         record = TRUE)
#             }
#         }
#         row = 1
#         clm = 1
#         new = FALSE
#         frames <- framedim(nr, nc)
#         if (!arg$onefile) {
#             use.sampleID <- sampleID[nPlot:(rc * j)]
#             use.sampleID <- use.sampleID[!is.na(use.sampleID)]
#         }
#         else {
#             use.sampleID <- sampleID
#         }
#         for (i in 1:length(use.sampleID)) {
#             fig.c <- c(frames$left[clm], frames$right[clm], frames$bot[row], 
#                        frames$top[row])
#             par(fig = fig.c, new = new, oma = c(0, 0, 0.5, 0), 
#                 mar = arg$mar)
#             frame.c <- list(left = frames$left[clm], right = frames$right[clm], 
#                             bot = frames$bot[row], top = frames$top[row])
#             id <- use.sampleID[i]
#             ind.sample <- which(sample.names == id)
#             if (!is.null(segments)) {
#                 seg.lim <- sapply(segments, get.seglim, equalRange = arg$equalRange, 
#                                   sampleID = id)
#                 seg.lim <- c(min(seg.lim[1, ]), max(seg.lim[2, 
#                 ]))
#             }
#             else {
#                 seg.lim <- NULL
#             }
#             add = FALSE
#             if (!is.null(data)) {
#                 if (!arg$equalRange) {
#                     data.lim <- quantile(data[, ind.sample + 2], 
#                                          probs = c(arg$q/2, (1 - arg$q/2)), names = FALSE, 
#                                          type = 4, na.rm = TRUE)
#                 }
#                 plotObs(y = data[, ind.sample + 2], pos = data[, 
#                                                                2], unit = pos.unit, winsoutliers = winsoutliers[, 
#                                                                                                                 ind.sample + 2], type = "genome", xaxis = xaxis, 
#                         sampleID = id, chromosomes = data[, 1], frame = frame.c, 
#                         new = new, op = arg, data.lim = data.lim, seg.lim = seg.lim)
#                 add = TRUE
#             }
#             else {
#                 data.lim <- NULL
#             }
#             if (!is.null(segments)) {
#                 sample.segments <- lapply(segments, function(seg, 
#                                                              id) {
#                     seg[seg[, 1] == id, ]
#                 }, id = id)
#                 for (s in 1:nSeg) {
#                     use.segments <- sample.segments[[s]]
#                     plotSegments(use.segments, type = "genome", 
#                                  xaxis = xaxis, add = add, col = arg$seg.col[s], 
#                                  sampleID = id, lty = arg$seg.lty[s], lwd = arg$seg.lwd[s], 
#                                  frame = frame.c, new = new, unit = pos.unit, 
#                                  seg.lim = seg.lim, data.lim = data.lim, op = arg)
#                     add <- TRUE
#                 }
#                 if (!is.null(arg$legend)) {
#                     legend("topright", legend = arg$legend, 
#                            col = arg$seg.col, lty = arg$seg.lty, cex = arg$cex.axis)
#                 }
#             }
#             if (i%%(nr * nc) == 0) {
#                 title(arg$title, outer = TRUE)
#                 if (is.null(arg$dir.print)) {
#                     devAskNewPage(ask = TRUE)
#                 }
#                 clm = 1
#                 row = 1
#                 new = FALSE
#             }
#             else {
#                 if (clm < nc) {
#                     clm <- clm + 1
#                 }
#                 else {
#                     clm <- 1
#                     row <- row + 1
#                 }
#                 new = TRUE
#             }
#         }
#         title(arg$title, outer = TRUE)
#         if (!is.null(arg$dir.print)) {
#             cat("Plot was saved in ", paste(arg$dir.print, 
#                                             "/", file.name[j], ".pdf", sep = ""), 
#                 "\n")
#             graphics.off()
#         }
#         nPlot <- rc * j + 1
#     }
# }

# Run the application 
shinyApp(ui = ui, server = server)
