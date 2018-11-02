# Style guide -----
# ALLCAPS - global constants
# UpperCamel - functions
# snake_case - variables
# Page name in front of input / output variables
# Space between operators
# No space after function call

# Required libraries

library(rdrop2)
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
library(ggplot2)
library(doBy)
#library(htmlwidgets)
library(manipulateWidget)
library(webshot)
library(plotly)
library(lsmeans)
library(lmerTest)
library(multcompView)
library(knitr)
library(kableExtra)
library(dplyr)
library(gdata)
library(shinyjs)

TOKEN <- readRDS("droptoken.rds")
DB_PATH <- "/CompiledData"

TRAITS <- NULL

REACTIVES <- reactiveValues()

REACTIVES$report_length <- 0
REACTIVES$do_plot <- FALSE
REPORT_OBJECTS <- list()
REPORT_TYPELIST <- list()
CURRENT_OBJECT <- NULL
CURRENT_TYPE <- NULL

# Strip problematic characters from input

CleanText <- function (text) { 
  
  return(gsub("[{}%&$#_^~\\]","", text))
  
}

# Get data from selected file and save in reactive global object
UpdateData <- function (data) {
  
#  data$Plot <- factor(data$Plot)
  data$Rep <- CleanText(data$Rep)
  data$Rep <- factor(data$Rep)
  data$EntryName <- CleanText(data$EntryName)
  data$EntryName <- factor(data$EntryName)
  
  REACTIVES$data <- data
  
}

UpdateTraits <- function(data) {

  hidden_cols <- c("Plot", "Rep", "Block", "Check", "EntryName", "Notes")
  traits <- colnames(data)[!(data %>% colnames %in% hidden_cols)]

  return(traits)
}

# Define UI for application 
ui <- shinyUI(fluidPage(
  
  tags$head(tags$link(rel="shortcut icon", href="favicon.ico")),
  shinyjs::useShinyjs(),
  
  navbarPage(theme = shinytheme("united"), title = "Variety Trial Tool",
             
             tabPanel("1. Get Started",
                      
                      # Code for getting started page ui
                      # Page code = start
                      HTML("<center><h1>Variety Trial Tool<sup><i>beta</i></sup></h1></center> "),
                      HTML("<br>"),
                      h4("Welcome to OSA's online variety trial tool, created in cooperation with the USDA Risk Management Agency."),
                      h4("This tool can help you plan simple variety trials and view the results."),
                      HTML("<br>"),
                      h3("Getting started with the Variety Trial Tool"),
                      h4("Most people will use this site by beginning at the 'Plan Your Trial' tab."),
                      h4("If you already have trial data collected, skip to the 'Upload Your Data' tab."),
                      h4("If you want to see other people's trial data, go to the 'See Other Trials' tab."),
                      HTML("<h4><a href=https://www.youtube.com/watch?v=ItiHAN8O1fU&feature=youtu.be&t=3023>Go here for a video walkthrough of the trial tool</a></h4><br><br>"),
                      HTML("<br>"),
                      HTML("<h4>For more information on how to conduct variety trials, go to <a href=http://www.seedalliance.org>http://www.seedalliance.org</a></h4><br><br>"),
                      div(img(src='2017Green-transparent-background-OSA-logo.png', height = '100'),img(src='RMA-highRez.jpg', height = '100'), style="text-align: center;"),
                      HTML("<h5>This material is funded in partnership by USDA, Risk Management Agency, under award number RM17RMEPP522C027/4500075447</h5>"),
                      HTML("<h5>This page is still under beta testing and active construction. If you have any problems, feedback or questions related to this site, contact jared@seedalliance.org</h5>")
             ),
             
             tabPanel("2. Plan Your Trial",
                      
                      # Code for trial planning page ui
                      # Page code = plan
                      
                      sidebarLayout(
                        sidebarPanel(
                          div(id="plan_design_text",h3("1. Design Trial")),
                          selectInput("trial_type", "Trial Type", 
                                      list(
                                        "RCBD" = "rcbd",
                                        "Augmented" = "augmented")
                          ),
                          
                          uiOutput("plan_inputs"),
                          actionButton("generate_trial", "Create Trial"),
                          shinyjs::hidden(
                            div(id="plan_map_text",h3("2. Create a map")),
                            textInput("plan_rows", "How many rows wide will your trial be?"),
                            actionButton("generate_map", "Create Map")
                          )
                        ),
                        
                        # Show results
                        mainPanel(
                          div(id="plan_text",HTML("<h4><b>This page can help you create your trial design
                               and will let you download a trial map and a datasheet.</b>
                               <br><br>
                               The menus and fields on the left allow you to choose 
                               what kind of trial design you want, the entries you want to
                               include, the number of times each of those entries will be
                               replicated in the trial, and the traits that you plan on
                               evluating.
                               <br><br>
                               <b>Trial Type?</b>
                               <br>
                               This page lets you choose between different trial designs:
                               <b>Randomized Complete Block Design (RCBD)</b> and <b>Augmented</b>. For most 
                               purposes, the RCBD will be the best choice. Augmented designs are 
                               good when you have too many experimental entries to replicate them all.
                               <br><br>
                               To generate an RCBD, select the RCBD trial type, then enter your entry names seperated 
                               by commas, enter the number of replications of each entry you wish to grow, and enter the names of the traits that you wish 
                               to evaluate.
                               <br><br>
                               To generate an augmented design, select the Augmented trial type, select the number of complete replications,
                              then select the number of incomplete blocks within each replication. Enter the names of the checks, the names of the experimental entries, and the trait names
                              <br><br>
                              Once you have created the list of trial plots, you may create a map by entering the number of rows wide you want the trial to be</h4>")),
                          
                          rHandsontableOutput("planning_output"),
                          uiOutput("hidden_download"),
                          
                          rHandsontableOutput("plan_map"),
                          uiOutput("hidden_map_download")
                          
                        )
                      )
             ),
             
             tabPanel("3. Upload Your Data",
                      
                      # Code for quality control page ui
                      # Page code = qc
                      
                      # Component ideas:
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
                          )
                          # selectInput("qc_analyis", "Quality Control Tools", 
                          #             list(
                          #               "Editable table" = "editable",
                          #               "Ranges" = "ranges",
                          #               "Factor names" = "factor_names")
                          #)
                        ),
                        mainPanel(
                          div(id="qc_text", HTML("<h4><b>Use this page to upload your raw trial data.</b><br><br>
                                                The file needs to be in the '.csv' format. If your data is in
                                                Excel, you can export to the '.csv' format using 'Save As'. <br><br>
                                                The two columns that need to be in your file are 'EntryName'
                                                which contains the entry names for each plot, and 'Rep' which contains
                                                the replication of each plot. <br><br>
                                                If you are unsure how the data file should look, you can download <a href='./rcbd.csv'>this template for 
                                                RCBD</a> and <a href='./augment.csv'>this template for an augmented design.</a><br><br>
                                                Right now this tool can handle single location trials, so multi-location trials will 
                                                 need to be split up by location for analysis. <br><br>
                                                 Finally, there are a couple of data formatting requirements. Missing data should be 
                                                 indicated by a blank cell. Also, the tool removes the following symbols from data, as they cause problems rendering the PDF reports: {}%&$#_^~\\.")),
                          rHandsontableOutput("uploaded_data")
                          
                        )
                      )
                      
             ),
             
             tabPanel(" ... or See Other Trials",
                      sidebarLayout(
                        sidebarPanel(
                          actionButton("other_load_trial", "Load Selected Trial")
                        ),
                        mainPanel(
                          div(id="other_text", HTML("<h4>To load saved data for viewing and analysis, select a trial below and click 'Load Selected Trial'.<br><br></h4>All data deposited here adheres to the <a href='https://www.nature.com/nature/journal/v461/n7261/full/461168a.html'>Toronto Agreement</a> on prepublication data release. 
                                                   Data users are expected to respect scientific etiquette and allow data producers the first global 
                                                    analyses of their data set, and should be aware that pre-publication data may not have been subject
                                                    to full quality control and peer review, so caution must be applied when utilizing these data.<br><br>")),
                          DT::dataTableOutput("other_index")
                        )
                      )
                      
                      # Code for trial list page ui
                      # Page code = other
                      
                      # Component ideas:
                      
                      
             ),
             
             tabPanel("4. Analyze", 
                      sidebarLayout(
                        sidebarPanel(
                          
                          # Code for analysis page ui
                          # Page code = analyze
                          
                          # Component ideas:
                          # Things needed:
                          # 1. color coded summaries
                          # 2. Text summaries
                          
                          # Maybe be able to select from dropbox files or data from previous page
                          
                          h3("1. Create and View Figures:"),
                          selectInput("analysis", "Analysis Type", 
                                      list(
                                        "Means Table" = "table",
                                        "Data Plot" = "dataplot",
                                        "Notes" = "notes")

                          ),
                          
                          shinyjs::hidden(selectInput("trait", "Trait:",
                                      TRAITS)),
                          actionButton("analysis_button", "Run Analysis"),
                          shinyjs::hidden(
                            div(id="analyze_addfigtext", HTML("<br><h3>2. Add Figures to Report</h3>")),
                            actionButton("add_chart", "Attach To Report"),
                            div(id="analyze_reporttext", HTML("<br><h3>3. Create Report</h3>")),
                            textInput("analyze_name", "Trial Name"),
                            textInput("analyze_save_location", "Location"),
                            textInput("analyze_save_crop", "Crop"),
                            textInput("analyze_save_description", "Trial Description"),
                            downloadButton("download_chart", "Download PDF Report")
                          )
                        ),
                        
                        # Show results
                        mainPanel(
                          div(id="analyze_text", HTML("<h4><b>This page is where you can analyze your data and 
                                                create a downloadable report</b><br><br>Once you have added
                                                a dataset of your own or loaded someone else's, you can select
                                                an analysis from the menu on the left to begin creating figures.<br><br>
                                                Currently there are two analyses: a table of means and groups for all traits, and plots
                                                of means and confidence intervals for individual traits. <br><br>
                                                As you create figures, a button will appear that will allow you to add them to a report
                                                <br><br>Once you've added figures, you can label your report and download a PDF.<br><br><small>
                                                Details: Traits in the means table are color-coded with the Viridis colorblind-friendly coloring system, with yellow being high,
                                                and dark purple being low. Means are modeled using the lme4 package in R, with the mixed model
                                                of Trait ~ Trait ~ EntryName + (1|Rep) for RCBD and Trait ~ EntryName + (1|Rep) + (1|Block) for 
                                                augmented designs. For the 95% confidence intervals in the plots, the intervals are calculated 
                                                with the lsmeans function in the lsmeans package using the above models with the default alpha
                                                of 0.05. Degrees of freedom for unbalanced trials are calculated by the Satterthwaite method.
                                                For the means table, the means groupings are calculated by the cld function in the lsmeans package
                                                with an alpha of 0.05. More details on it are here: https://cran.r-project.org/web/packages/lsmeans/lsmeans.pdf</small>")),
                          uiOutput("results")
                        )
                      )
             ),
             
             tabPanel("5. Save And Share",
                      sidebarLayout(
                        sidebarPanel(
                          textInput("save_name", "Trial Name"),
                          textInput("save_location", "Location"),
                          textInput("save_crop", "Crop"),
                          textInput("save_description", "Trial Description"),
                          textInput("save_email", "Email Address"),
                          actionButton("upload_to_dropbox", "Share Data")
                        ),
                        mainPanel(
                          div(id="save_text", HTML("<h4>To save your data to our server and allow other accesss to it, 
                                                   fill out the details on the left and click 'Share Data'. <br><br>Your email address
                                                   will be kept private and only used by the site administrators if we need to contact
                                                   you.</h4><br>All data deposited here adheres to the <a href='https://www.nature.com/nature/journal/v461/n7261/full/461168a.html'>Toronto Agreement</a> on prepublication data release. 
                                                   Data users are expected to respect scientific etiquette and allow data producers the first global 
                                                   analyses of their data set, and should be aware that pre-publication data may not have been subject
                                                   to full quality control and peer review, so caution must be applied when utilizing these data. "))
                        )
                      )
                      
                      # Code for data saving page ui
                      # Page code = save
                      
                      # Component ideas:
                      # 1. Save data to dropbox and provide link
                      
             )
          
             
  )
))

# Define server logic
server <- shinyServer(function(input, output, session) {
  
  ########## Server code for 'Plan Your Trial' page ##########
  
  output$plan_inputs <- renderUI({
    
    switch (input$trial_type,
            
      rcbd = tagList(
        textAreaInput("plan_entry_names", "Entry Names", rows = 2, placeholder = "VarietyName1, VarietyName2, VarietyName3"),
        numericInput("plan_reps", "Replications", 3, min = 1, max = 100),
        textAreaInput("plan_traits", "Trait Names", rows = 2, placeholder = "TraitName1, TraitName2, TraitName3")),
      
      augmented = tagList(
        numericInput("plan_reps", "Number of Complete Replications", 1, min = 1, max = 100),
        numericInput("plan_blocks", "Number of Incomplete Blocks within Complete Replications", 3, min = 1, max = 100),
        textAreaInput("plan_check_names", "Check Entry Names", rows = 2, placeholder = "CheckName1, CheckName2"),
        textAreaInput("plan_entry_names", "Experiemental Entry Names", rows = 2, placeholder = "Experimental1, Experimental2, Experimental3"),
        textAreaInput("plan_traits", "Trait Names", rows = 2, placeholder = "TraitName1, TraitName2, TraitName3"))
        
      )

  })
  
  output$planning_output <- renderRHandsontable({
    
    # React to button
    req(input$generate_trial)
    
    # Hide original text
#    shinyjs::hide("plan_text")
    
    # Show map input
    shinyjs::show("plan_map_text")
    shinyjs::show("plan_rows")
    shinyjs::show("generate_map")
    
    plot_list <- switch (input$trial_type,
                    
                    rcbd = RCBDOutput(),
                    augmented = AugmentedOutput()
    )
  
    REACTIVES$design <- plot_list # save for download
    return (rhandsontable(plot_list, row.names = FALSE))
    
  })
  
  RCBDOutput <- function () {
    
    # Don't react to changes in reps and names
    reps <- as.numeric(CleanText(input$plan_reps))
    trt <- trim(unlist(strsplit(x = CleanText(input$plan_entry_names), split = '[\n,]+[:space:]*' )))
    traits <- trim(unlist(strsplit(x = CleanText(input$plan_traits), split = '[\n,]+[:space:]*' )))
    
    outdesign <- design.rcbd(trt, reps, serie = 2, continue = TRUE)
    
    # Convert from agricolae format to our format
    plot_list <- as.data.frame(outdesign$book)
    names(plot_list)[names(plot_list)=="trt"] <- "EntryName"
    plot_list$EntryName <- factor(plot_list$EntryName)
    names(plot_list)[names(plot_list)=="block"] <- "Rep"
    plot_list$Rep <- factor(plot_list$Rep)
    names(plot_list)[names(plot_list)=="plots"] <- "Plot"
    plot_list$Plot <- factor(plot_list$Plot)
    
    # Add trait columns
    plot_list[,traits] <- NA
    plot_list["Notes"] <- NA
    return(plot_list)
  }
  
  AugmentedOutput <- function () {
    
    reps <- as.numeric(CleanText(input$plan_reps))
    blocks <- as.numeric(CleanText(input$plan_blocks))
    
    # RegExp allows newlines and commas as delimiters and strips white space around entries
    trt <- trim(unlist(strsplit(x = CleanText(input$plan_entry_names), split = '[\n,]+[:space:]*' )))
    checks <- trim(unlist(strsplit(x = CleanText(input$plan_check_names), split = '[\n,]+[:space:]*' )))
    traits <- trim(unlist(strsplit(x = CleanText(input$plan_traits), split = '[\n,]+[:space:]*' )))
    
    
    all_plots <- setNames(data.frame(matrix(ncol = 5, nrow = 0)), c("Plot","Rep","Block","EntryName","Check"))
    
    for (i in 1:reps) {
      outdesign <- design.dau(trt1 = checks, trt2 = trt, r=blocks, serie = 2)
      plot_list <- as.data.frame(outdesign$book)
      
      # Convert from agricolae format to our format
      names(plot_list)[names(plot_list)=="trt"] <- "EntryName"
      plot_list$EntryName <- factor(plot_list$EntryName)
      names(plot_list)[names(plot_list)=="block"] <- "Block"
      plot_list$Block <- factor(((i-1)*blocks)+as.numeric(plot_list$Block))
      plot_list$Rep <- factor(i)
      names(plot_list)[names(plot_list)=="plots"] <- "Plot"
      plot_list$Plot <- factor(((i-1)*(blocks*100))+plot_list$Plot)
      
      # Note which entries were checks 
      plot_list$Check <- NA
      plot_list[plot_list$EntryName %in% checks, "Check"] <- "Check"
      
      all_plots <- rbind(all_plots, plot_list)
      
    }
    
    # Add trait columns
    all_plots[,traits] <- NA
    all_plots["Notes"] <- NA
    return(all_plots)
  }
  
  # Hiding download button until trial is created
  output$hidden_download <- renderUI({
    req(input$generate_trial)
    
    return(
      list(
        HTML("<br>"),
        downloadButton("download_trial", "Download Trial Design"),
        HTML("<br><br>")
        )
      )
  })
  
  # Download .csv of trial design
  output$download_trial <- downloadHandler(
    filename = function() {
      paste('design-', Sys.Date(), '.csv', sep='')
    },
    content = function(con) {
      write.csv(REACTIVES$design, con, row.names = FALSE, na="")
    }
  )
  
  output$plan_map <- renderRHandsontable({
    req(input$generate_map)
    req(input$plan_rows)
    map_rows <- as.numeric(CleanText(input$plan_rows))
    
    plots <- paste0(REACTIVES$design$Plot," - ",REACTIVES$design$EntryName)
    
    # Pad trial with fill
    if ((length(plots) %% map_rows) == 0) 
      { fill <- 0 
    } else {
      fill <- map_rows - (length(plots) %% map_rows)
    }
    
    plots <- c(plots, rep("Fill", fill))
    map <- matrix(plots, nrow=map_rows)
    
    # Zigzag
    swap <- seq(2,ncol(map),by=2)
    map[,swap] <- apply(X = as.matrix(map[,swap]), MARGIN = 2, FUN = rev)
    
    REACTIVES$map <- map # save for download
    return (rhandsontable(map, row.names = FALSE))
    
  })
  
  # Hiding download button until trial is created
  output$hidden_map_download <- renderUI({
    req(input$generate_map)
    
    return(
      list(
        HTML("<br>"),
        downloadButton("download_map", "Download Map")
      )
    )
  })
  
  output$download_map <- downloadHandler(
    filename = function() {
      paste('map-', Sys.Date(), '.csv', sep='')
    },
    content = function(con) {
      write.csv(REACTIVES$map, con, row.names = FALSE, na="")
    }
  )
  
  ########## Server code for 'Get Your Data Ready' page ##########
  output$uploaded_data <- renderRHandsontable({
    req(REACTIVES$data) # make sure file has been uploaded before displaying
 #   shinyjs::hide("qc_text")
    if (!is.null(input$uploaded_data)) {
      REACTIVES$data <- hot_to_r(CleanText(input$uploaded_data))
    }
    rhandsontable(REACTIVES$data, readOnly = FALSE)
  })
  
  observeEvent(input$file, {
    req(input$file)
    file_name <- input$file
    data <- read.csv(file_name$datapath)
    UpdateData(data)
    TRAITS <<- UpdateTraits(REACTIVES$data)
    updateSelectInput(session, "trait",
                      choices = TRAITS)
    
    output$uploaded_data <- renderRHandsontable({
      rhandsontable(REACTIVES$data, readOnly = FALSE)
    })
    
  })
  
  ########## Server code for 'Analyze Online' page ##########
  
  
  observeEvent(input$analysis_button, {

    REACTIVES$do_plot <- input$analysis_button
#    shinyjs::hide("analyze_text")
    
  })
  
  observeEvent(input$analysis, {
    REACTIVES$do_plot <- FALSE
    if (input$analysis == "dataplot") {
      shinyjs::show("trait")
    }
    else if (input$analysis == "table" || input$analysis == "notes") {
      shinyjs::hide("trait")
    }
  }) 
  
  observeEvent(input$trait, {
    REACTIVES$do_plot <- FALSE
  }) 
  

  output$results <- renderUI({
    req(input$analysis_button)
    shinyjs::show("analyze_addfigtext")
    shinyjs::show("add_chart")
    shinyjs::enable("add_chart")
    shinyjs::html("add_chart","Attach To Report")
    
    switch (input$analysis, 
                
        table = tableOutput("table"),
        dataplot = plotOutput("dataplot"),
        notes = tableOutput("notes"))
      
    
  })
  
  output$table <- function() {
      req(REACTIVES$data, input$analysis, REACTIVES$do_plot)
      isolate(TableFunc(input,REACTIVES$data))
  }
  
  output$dataplot <- renderPlot({
      req(REACTIVES$data, input$analysis, REACTIVES$do_plot)
      isolate(DataPlotFunc(input, REACTIVES$data))
  })
  
  output$notes <- function() {
    req(REACTIVES$data, input$analysis, REACTIVES$do_plot)
    isolate(NotesFunc(input,REACTIVES$data))
  }
  
  output$download_chart <- downloadHandler(
    "TrialReport.pdf",
    content = 
      function(file)
      {
        withProgress(message = 'Creating report:', value = 0, {
          
          # Copy info to 'Save and Share' page
          updateTextInput(session, "save_name", value = CleanText(input$analyze_name))
          updateTextInput(session, "save_location", value = CleanText(input$analyze_save_location))
          updateTextInput(session, "save_crop", value = CleanText(input$analyze_save_crop))
          updateTextInput(session, "save_description", value = CleanText(input$analyze_save_description))
          incProgress(0.5, detail = "exporting data")
          rmarkdown::render(
            input = "report_file.Rmd",
            output_file = "built_report.pdf",
            params = list( 
              name = CleanText(input$analyze_name),
              location = CleanText(input$analyze_save_location),
              crop = CleanText(input$analyze_save_crop),
              description = CleanText(input$analyze_save_description),
              typelist = REPORT_TYPELIST, 
              objects = REPORT_OBJECTS)
          ) 
          incProgress(0.5, detail = "rendering pdf")
          readBin(con = "built_report.pdf", 
                  what = "raw",
                  n = file.info("built_report.pdf")[, "size"]) %>%
            writeBin(con = file)
        })
      }
  )
  
  observeEvent(input$add_chart, {
    req(input$add_chart)
    shinyjs::disable("add_chart")
    shinyjs::html("add_chart","Attached")
    show("analyze_reporttext")
    show("analyze_name")
    show("analyze_save_location")
    show("analyze_save_crop")
    show("analyze_save_description")
    show("download_chart")
    REACTIVES$report_length <- REACTIVES$report_length + 1
    REPORT_OBJECTS[[REACTIVES$report_length]] <<- CURRENT_OBJECT
    REPORT_TYPELIST[[REACTIVES$report_length]] <<- CURRENT_TYPE
    
  })
  
  ### Analysis functions
  
  GetFormula <- function (trait_name, data) {
    
    if ("Block" %in% names(data) && levels(data$Rep)>1) { # Augmented Design with more than 1 rep
      formula_name <- as.formula(paste0(trait_name,"~ EntryName + (1|Rep) + (1|Block)"))
    }
    else  if ("Block" %in% names(data)) { # Augmented Design with 1 rep
      formula_name <- as.formula(paste0(trait_name,"~ EntryName + (1|Block)"))
    }
    else 
    { # RCBD
      formula_name <- as.formula(paste0(trait_name,"~ EntryName + (1|Rep)"))
    }
    
    return (formula_name)
    
  }
  
  # Gets means and grouping for one data column
  GetMeanCLD <- function (trait_name, data) {
    
    if (levels(data$Rep)==1) { # Unreplicated
      
      
      
    }
    
   formula_name <- GetFormula(trait_name, data)
    
    trait.lm <- lmer (formula_name, data=data)
    #    if (require("lmerTest")) detach("package:lmerTest")
    trait.lsm <- lsmeans::lsmeans(trait.lm, ~ EntryName, data=data, lmer.df = "k") ### REQUIRES lsmeans
    trait.cld <- lsmeans::cld(trait.lsm, reversed = TRUE, Letters=letters)
    
    trait.cld[,trait_name] <- as.numeric(format(round(trait.cld$lsmean, 1), nsmall = 1))
    trait.cld[,paste0(".group.",trait_name)] <- trait.cld[,".group"]
    return(trait.cld[,c("EntryName",trait_name,paste0(".group.",trait_name))])  
  }
  
  # Gets means to use in DataPlot -- should be generalized and combined with GetMeansCLD
  GetSummary <- function (trait_name, data) {
    
    formula_name <- GetFormula(trait_name, data)
    
    trait.lm <- lmer (formula_name, data=data)
    #    if (require("lmerTest")) detach("package:lmerTest")
    trait.lsm <- lsmeans::lsmeans(trait.lm, ~ EntryName, data=data, lmer.df = "k") ### REQUIRES lsmeans
    trait.cld <- lsmeans::cld(trait.lsm, reversed = TRUE, Letters=letters)
    
    return(trait.cld)  
  }
  
  
  TableFunc <- function (input, data) {
    
    # Compile means and groupings
    trait_list <- list()
    withProgress(message = 'Calculating means for', value = 0, {
      for (trait_num in 1:length(TRAITS)) {
        
        trait_name <- TRAITS[trait_num]
        incProgress(1/length(TRAITS), detail = trait_name)
        
        trait_list[[trait_name]] <- GetMeanCLD(trait_name, data)
        
      }
    })
    
    trait_table<-Reduce(function(x, y) merge(x, y, by = "EntryName", all=T),trait_list, accumulate=F)
    rownames(trait_table) <- trait_table$EntryName
    new_col_names <- sub(".group.*", " ", colnames(trait_table))
    
    CURRENT_OBJECT <<- trait_table
    CURRENT_TYPE <<- "table"
    
    trait_table %<>% 
      mutate_if(is.numeric, function(x) {
        cell_spec(x, "html", color = "white", bold = T, background = spec_color(x, end = 0.9))
      }) %>% 
      knitr::kable("html", escape = F, caption = "Table of Means and Groups", col.names = new_col_names) %>%
      kable_styling("striped", full_width = F) %>%
      footnote(general = "Letters after trait means indicate groups of entries which are not statistically different 
               for that trait. For example, all entries with an 'a' after a trait mean are not statisically different for that
               trait due to the uncertainty in their means")
    
    return (trait_table)
    
  }
  
  DataPlotFunc <- function(input, data) {
    withProgress(message = paste0("Generating plot for ",input$trait,": "), value = 0, {
      
      incProgress(0.5, detail = "getting summary data")
      eval(parse(text = paste0("data_sum <- GetSummary(trait = '", input$trait, "' , data = data)")))
      
      incProgress(0.5, detail = "rendering plot")
      eval(parse(text = paste0("p <- ggplot(data, aes(y=", input$trait, ", x=EntryName, color='black')) + labs(title = 'Plot of means and confidence intervals for ",input$trait,"') + geom_jitter(position=position_jitter(w=0.05, h=0)) + geom_point(alpha = 0.5, data=data_sum, aes(x = EntryName, y = lsmean), size=3, color='red') + geom_errorbar(alpha = 0.5, data = data_sum, mapping = aes(x = EntryName, y = lsmean, ymin = lower.CL, ymax = upper.CL, color='red'), size=.75, width=.25) + theme_bw()+ theme(axis.text.x = element_text(angle = 55,  hjust = 1)) + scale_colour_manual(name = 'Key', guide = 'legend', values =c('black'='black','red'='red'), labels = c('Data Points','Adjusted Mean and 95% CI'))")))
      CURRENT_OBJECT <<- p
      CURRENT_TYPE <<- "plot"
      print(p)
      
    })
    
  }
  
  NotesFunc <- function (input, data) {
    withProgress(message = paste0("Generating notes:"), value = 0, {
      
      incProgress(0.5, detail = "compiling notes")
      data$RepNotes <- paste0("Rep ",data$Rep,": ",data$Notes)
      notes_table <- aggregate(RepNotes ~ EntryName, data = data, FUN = paste, collapse = "; ")
      data$RepNotes <- NA
      
      CURRENT_OBJECT <<- notes_table
      CURRENT_TYPE <<- "notes"
      
      incProgress(0.5, detail = "rendering table")
      
      notes_table %<>% 
        knitr::kable("html", escape = F, caption = "Notes") %>%
        kable_styling("striped", full_width = F) %>%
        footnote(general = "Commas seperate notes compiled across replications.")
      
      return (notes_table)
      
    })
    
    
  }
  
  ########## Server code for 'Save and Share' page ##########
  
  observeEvent(input$upload_to_dropbox, {
    req(input$upload_to_dropbox)
    withProgress(message = "Sharing trial data: ", value = 0, {
      
      incProgress(0.5, detail = "saving data")
      fileName <- sprintf("%s_%s.csv", as.integer(Sys.time()), digest::digest(REACTIVES$data))
      # Write the data to a temporary file locally
      filePath <- file.path(tempdir(), fileName)
      write.csv(REACTIVES$data, filePath, row.names = FALSE, quote = TRUE)
      # Upload the file to Dropbox
      drop_upload(file = filePath, path = "ParticipantUploads", dtoken = TOKEN)
      
      incProgress(0.5, detail = "updating file list")
      index <- drop_read_csv("ParticipantUploads/index.csv", dtoken = TOKEN)
      index <- rbind(index, data_frame(
        Name = CleanText(input$save_name), 
        Location = CleanText(input$save_location), 
        Crop = CleanText(input$save_crop), 
        Description = CleanText(input$save_description), 
        Email = input$save_email,
        Filename = fileName))
      
      # Write the data to a temporary file locally
      indexfilePath <- file.path(tempdir(), "index.csv")
      write.csv(index, indexfilePath, row.names = FALSE, quote = TRUE)
      # Upload the file to Dropbox
      drop_upload(file = indexfilePath, path = "ParticipantUploads", dtoken = TOKEN)
      
    })
  })
  
  ########## Server code for 'See Other Trials' page #########
  
  output$other_index <- DT::renderDataTable({
    withProgress(message = "Loading trial directory: ", value = 0, {
     
      incProgress(0.5, detail = "retrieving index")
      REACTIVES$file_index <- drop_read_csv("ParticipantUploads/index.csv", dtoken = TOKEN)
      
      incProgress(0.5, detail = "rendering")
      display_index <- subset(REACTIVES$file_index, select= -c(Email,Filename))
      
      return(display_index)
      
    })
  }, selection = "single")
  
  observeEvent(input$other_load_trial, {
#    req(input$input$other_load_trial)
    withProgress(message = "Loading trial data: ", value = 0, {
      
      id<-input$other_index_rows_selected
      
      incProgress(0.5, detail = paste0("retrieving file ",id))
      filename <- REACTIVES$file_index[id,"Filename"]
      
      incProgress(0.5, detail = paste0("retrieving file ",filename))
      data <- drop_read_csv(paste0("ParticipantUploads/",filename), dtoken = TOKEN)
      
      UpdateData(data)
      TRAITS <<- UpdateTraits(REACTIVES$data)
      updateSelectInput(session, "trait",
                        choices = TRAITS)
      
      output$uploaded_data <- renderRHandsontable({
        rhandsontable(REACTIVES$data, readOnly = FALSE)
      })
      shinyjs::hide("qc_text")
      
      # Update names in Analyze Data tab
      
      updateTextInput(session, "analyze_name", value = CleanText(input$save_name))
      updateTextInput(session, "analyze_save_location", value = CleanText(input$save_location))
      updateTextInput(session, "analyze_save_crop", value = CleanText(input$save_crop))
      updateTextInput(session, "analyze_save_description", value = CleanText(input$save_description))
      
    })
  })

})

# Run the application 
shinyApp(ui = ui, server = server)
