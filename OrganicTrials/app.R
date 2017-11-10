# Style guide -----
# ALLCAPS - global constants
# UpperCamel - functions
# snake_case - variables
# Space between operators
# No space after function call

# Required libraries

library(lsmeans)
library(gridExtra)
library(reshape)
library(lattice)
library(grid)
library(gplots)
library(Hmisc)
library(DT)
library(lme4)
library(stringr)
library(shinythemes)
library(rhandsontable)
library(shinyBS)
library(agricolae)

DATA <- reactiveValues()
TRAITS <- NULL

# Get data from selected file and set as global dataframe

UpdateFile <- function (file_name) {
  
  #First seven lines are metadata
  meta_data <- read.csv(file_name$datapath, nrow=6)
  data <- read.csv(file_name$datapath, skip=7)
  
  data$Block <- factor(data$Block)
  data$Location <- factor(data$Location)
  data$LocBlock <- factor(paste0(data$Location,data$Block))
  
  meta_data <-t(meta_data)
  colnames(meta_data) <- c("Name","Units","Description","Notes","Notes2","Visable")
  
  DATA$data <- data
  DATA$meta_data <- meta_data
  
}

UpdateTraits <- function(data) {
  
  meta_data <- as.data.frame(data$meta_data)
  traits <- rownames(subset(meta_data,Visable == "Visable"))
  
  return(traits)
}

# Define UI for application 
ui <- shinyUI(fluidPage(
  
  tags$head(tags$link(rel="shortcut icon", href="favicon.ico")),
  
  navbarPage(theme = shinytheme("united"), "Variety Trial Tool",
             
             tabPanel("Get Started",
                      
                      # Code for getting started
                      
                      h1("Getting started with the Variety Trial Tool"),
                      h4("Welcome to the online Variety Trial Tool. This tool can help you plan 
      simple variety trials and view the results. Below are some instructions."),
                      HTML("<br><br>"),
                      h2("Table of Contents"),
                      h3("1. Planning your trial"),
                      h3("2. Getting the data cleaned up and ready to analyze"),
                      h3("3. Analyzing your data online"),
                      h3("4. Creating a downloadable report"),
                      h3("5. Sharing your data")
                      
                      
             ),
             
             tabPanel("Plan Your Trial",
                      
                      sidebarLayout(
                        sidebarPanel(
                          
                          selectInput("trial_type", "Trial Type", 
                                      list(
                                        "RCBD" = "rcbd")
                          ),
                          
                          numericInput("reps", "Replications", 3, min = 1, max = 100),
                          
                          textAreaInput("entry_names", "Entry Names", rows = 8),
                          
                          actionButton("generate_trial", "Create Trial")
                          
                        ),
                        
                        # Show results
                        mainPanel(
                          rHandsontableOutput("planning_output"),
                          
                          uiOutput("hidden_download")
                        )
                      )
             ),
             
             tabPanel("Get Your Data Ready",
                      
                      # Code for quality control
                      # 1. File upload
                      # 2. Editable data
                      # 3. Select entry columns, rep columns, row and col column, numeric columns, text columns
                      # 4. Ways to show out of range values
                      # 5. Ways to show inconsistant entry names
                      # 6. Ways to show text in numberic entries
                      
                      sidebarLayout(
                        sidebarPanel(
                          fileInput("file", "Choose a .csv File",
                                    multiple = FALSE,
                                    accept = c("text/csv",
                                               "text/comma-separated-values,text/plain",
                                               ".csv")
                          ), 
                          bsTooltip(id = "file", title = "To create .csv file from Excel select Save As and choose comma deliminated (.csv)", 
                                    placement = "left", trigger = "hover"),
                          selectInput("qc_analyis", "Quality Control Tools", 
                                      list(
                                        "Editable table" = "editable",
                                        "Ranges" = "ranges",
                                        "Factor names" = "factor_names")
                          )
                        ),
                        mainPanel(
                          rHandsontableOutput("uploaded_data")
                          
                        )
                      )
                      
             ),
             
             tabPanel("Analyze Online", 
                      sidebarLayout(
                        sidebarPanel(
                          
                          # Moved file input to quality control
                          
                          # Things needed:
                          # 1. color coded summaries
                          # 2. Text summaries
                          
                          
                          selectInput("analysis", "Analysis Type", 
                                      list(
                                        "XY Plot" = "xyplot",
                                        "Standized XY plot" = "normal", 
                                        "Means Table" = "table",
                                        "ANOVA" = "anova",
                                        "Spearman" = "spearman")
                          ),
                          
                          selectInput("trait", "Trait:",
                                      TRAITS)
                          
                        ),
                        
                        # Show results
                        mainPanel(
                          uiOutput("results")
                        )
                      )
             ),
             
             tabPanel("Download Results"
                      
                      # Code for report downloading here
                      
             ),
             
             tabPanel("Save And Share"
                      
                      # Code for data saving here
                      
             )
             
  )
))

# Define server logic
server <- shinyServer(function(input, output, session) {
  
  # Server code for 'Plan Your Trial' page
  output$planning_output <- renderRHandsontable({
    
    # React to button
    req(input$generate_trial)
    input$generate_trial 
    
    # Don't react to changes in reps and names
    reps <- isolate(input$reps)
    trt <- isolate(unlist(strsplit(x = input$entry_names, split = '[\r\n]' )))

    outdesign <-design.rcbd(trt = trt, r = reps, serie = 2) # seed = 986
    book <- outdesign$book # field book
    fieldbook <- zigzag(outdesign)
    outdesign <- design.rcbd(trt, reps, serie = 2, continue = TRUE)

#    print(outdesign$sketch)
#    print(matrix(fieldbook[, 1], byrow = TRUE, ncol = 5))
    
    DATA$design <- outdesign$book # save for download
    
    rhandsontable(outdesign$book)

  })
  
  # Hiding download button until trial is created
  output$hidden_download <- renderUI({
    req(input$generate_trial)
    
    return(
      list(
        HTML("<br><br>"),
        downloadButton("download_trial", "Download Trial Design")
        )
      )
  })
  
  
  # Server code for 'Get Your Data Ready' page
  output$uploaded_data <- renderRHandsontable({
    req(input$file) # make sure file has been uploaded before displaying
    if (!is.null(input$uploaded_data)) {
      DATA$data <- hot_to_r(input$uploaded_data)
    }
    rhandsontable(DATA$data)
  })
  
  # Download .csv of trial design
  output$download_trial <- downloadHandler(
    filename = function() {
      paste('design-', Sys.Date(), '.csv', sep='')
    },
    content = function(con) {
      write.csv(DATA$design, con)
    }
  )

  
  # Server code for 'Analyze Online' page
  output$results <- renderUI({
    req(input$file, input$analysis)
    
    switch (input$analysis, 
            xyplot = plotOutput("xyplot"),
            normal = plotOutput("normal"),
            table = DT::dataTableOutput("table") ,
            anova =  verbatimTextOutput("anova"),
            spearman =  verbatimTextOutput("spearman"))
    
  })
  
  output$xyplot <- renderPlot({XyplotFunc(input, DATA$data)})
  output$normal <- renderPlot({NormalFunc(input, DATA$data)})
  output$table <- DT::renderDataTable({TableFunc(input,DATA$data)}, rownames=FALSE)
  output$anova <- renderPrint({AnovaFunc(input, DATA$data)})
  output$spearman <- renderPrint({SpearmanFunc(input, DATA$data)})
  
  # Global observing
  observeEvent(input$file, {
    
    UpdateFile(input$file)
    TRAITS <<- UpdateTraits(DATA)
    updateSelectInput(session, "trait",
                      choices = TRAITS)
  })
  
})

# Analysis functions

XyplotFunc <- function(input, data) {
  
  eval(parse(text = paste0("byaverage <-with(data, reorder(EntryName, -",input$trait,", na.rm=TRUE))")))
  formula_name <- as.formula(paste0(input$trait, " ~ byaverage"))
  print(xyplot(formula_name, group=Location, data=data, auto.key=list(space='right'), jitter.x=TRUE, scales=list(x=list(rot=55)))) 
  
}

NormalFunc <- function(input, data) {
  
  eval(parse(text = paste0("data <- transform(data, ",input$trait," =ave(",input$trait,", Location, FUN = scale))")))
  eval(parse(text = paste0("byaverage <-with(data, reorder(EntryName, -",input$trait,", na.rm=TRUE))")))
  formula_name <- as.formula(paste0(input$trait, " ~ byaverage"))
  print(xyplot(formula_name, group=Location, data=data, auto.key=list(space='right'), jitter.x=TRUE, scales=list(x=list(rot=55)))) 
  
}

GetRegionalMeanCI <- function (trait_name, trait_env, data) {
  
  regional_data <- data[data$Location == trait_env, ]
  regional_data <- droplevels(regional_data)
  
  formula_name <- as.formula(paste0(trait_name,"~ EntryName + (1|Block)"))
  trait.lm <- lmer (formula_name, data=regional_data)
  trait.lsm <- lsmeans(trait.lm, ~ EntryName, data=regional_data) ### REQUIRES lsmeans
  trait.cld<-cld(trait.lsm, Letters=letters)
  
  trait.cld[,trait_name] <- paste0(format(round(trait.cld$lsmean, 2), nsmall = 2),"  ", as.character(trait.cld$.group))
  
  trait.cld <- trait.cld[,c("EntryName",trait_name)]
  colnames(trait.cld) <- c("EntryName",trait_env)
  
  return(trait.cld)
  
}


TableFunc <- function (input, data) {
  
  env_list <- list()
  eval(parse(text = paste0("env_subset <- levels(droplevels(subset(data, !is.na(",input$trait,"), select = Location))$Location)")))
  
  for (env_num in 1:length(env_subset)) {
    
    env_name <- env_subset[env_num]
    env_list[[env_name]] <- GetRegionalMeanCI(input$trait, env_name, data)
    
  }
  
  trait_table<-Reduce(function(x, y) merge(x, y, by="EntryName", all=T),env_list, accumulate=F)
  rownames(trait_table) <- trait_table$EntryName
  #trait_table <- trait_table[,-which(names(trait_table)=="EntryName")]
  
  return (trait_table)
  
}

AnovaFunc <- function(input, data) {
  
  formula_name <- as.formula(paste0(input$trait, " ~ EntryName * Location + (1|LocBlock)"))
  
  return(anova((lmer(formula_name, data = data))))  ### REQUIRES lme4
  
}

SpearmanFunc <- function(input, data) {
  
  trait_loc <- aggregate(as.formula(paste0(input$trait," ~ EntryName:Location")), data = data, mean, na.rm = TRUE)
  trait_loc <- cast(trait_loc, EntryName ~ Location, value = c(as.character(input$trait))) ### REQUIRES reshape
  
  return(rcorr(as.matrix(trait_loc), type = "spearman"))
  
}

# Run the application 
shinyApp(ui = ui, server = server)
