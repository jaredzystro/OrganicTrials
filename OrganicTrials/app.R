# Required libraries
library(googlesheets)
library(lmerTest)
library(gridExtra)
library(reshape)
library(lattice)
library(grid)
library(gplots)
library(Hmisc)
library(DT)

# Get data from Google sheets and set as global dataframe

update_gs <- function (url) {
  
  TOMI_gs <<- gs_key(extract_key_from_url(url))
  DATA <<- gs_read(TOMI_gs)
  DATA$Block <<- factor(DATA$Block)
  DATA$LocBlock <<- factor(paste0(DATA$Location,DATA$Block))
  traits <<- colnames(DATA[,7:length(colnames(DATA))])
  
}

update_gs("https://docs.google.com/spreadsheets/d/1MzHAMhjHnF0h_-l0t-b107gmMA_1X7j79qYYnVP0Awc/pub?gid=1645815788&single=true&output=csv")

# Define UI for application 
ui <- shinyUI(fluidPage(
  
  # Application title
  #   titlePanel(TOMI_gs$sheet_title),
  
  # Sidebar 
  sidebarLayout(
    sidebarPanel(
      textInput("sheet", "Google Sheet Public URL", placeholder = "https://docs.google.com/spreadsheets/d/1MzHAMhjHnF0h_-l0t-b107gmMA_1X7j79qYYnVP0Awc/pub?gid=1645815788&single=true&output=csv"),
      actionButton("submit", "Load New Sheet"),
      selectInput("analysis", "Analysis Type", 
                  list(
                    "XY Plot" = "xyplot",
                    "Normal XY plot" = "normal", 
                    "Data Table" = "table",
                    "ANOVA" = "anova",
                    "Spearman" = "spearman")
      ),
      selectInput("trait", "Trait:",
                  traits)
      
    ),
    
    # Show results
    mainPanel(
      uiOutput("results")
    )
  )
))

# Define server logic
server <- shinyServer(function(input, output) {
  
  output$results <- renderUI({
    
    switch (input$analysis, 
            xyplot = plotOutput("xyplot"),
            normal = plotOutput("normal"),
            table = DT::dataTableOutput("table"),
            anova =  verbatimTextOutput("anova"),
            spearman =  verbatimTextOutput("spearman"))
    
  })
  
  output$xyplot <- renderPlot({xyplot_func(input, DATA)})   
  output$normal <- renderPlot({normal_func(input, DATA)})
  output$table <- DT::renderDataTable({table_func(input,DATA)})
  output$anova <- renderPrint({anova_func(input, DATA)})
  output$spearman <- renderPrint({spearman_func(input, DATA)})
  
  observeEvent(input$submit, {
    update_gs(input$sheet)
  })
  
})

# Analysis functions

xyplot_func <- function(input, DATA) {
  
  eval(parse(text = paste0("byaverage <-with(DATA, reorder(EntryName, -",input$trait,", na.rm=TRUE))")))
  formula_name <- as.formula(paste0(input$trait, " ~ byaverage"))
  print(xyplot(formula_name, group=Location, data=DATA, auto.key=list(space='right'), jitter.x=TRUE, scales=list(x=list(rot=55)))) 
  
}

normal_func <- function(input, DATA) {
  
  eval(parse(text = paste0("DATA <- transform(DATA, ",input$trait," =ave(",input$trait,", Location, FUN = scale))")))
  eval(parse(text = paste0("byaverage <-with(DATA, reorder(EntryName, -",input$trait,", na.rm=TRUE))")))
  formula_name <- as.formula(paste0(input$trait, " ~ byaverage"))
  print(xyplot(formula_name, group=Location, data=DATA, auto.key=list(space='right'), jitter.x=TRUE, scales=list(x=list(rot=55)))) 
  
}

table_func <- function (input, DATA) {
  
  return (DATA)
  
}

anova_func <- function(input, DATA) {
  
  formula_name <- as.formula(paste0(input$trait, " ~ EntryName * Location + (1|LocBlock)"))
  return(anova((lmer(formula_name, data = DATA))))  ### REQUIRES lme4
  
}

spearman_func <- function(input, DATA) {
  
  trait_loc <- aggregate(as.formula(paste0(input$trait," ~ EntryName:Location")), data = DATA, mean, na.rm = TRUE)
  trait_loc <- cast(trait_loc, EntryName ~ Location, value = c(as.character(input$trait))) ### REQUIRES reshape
  
  return(rcorr(as.matrix(trait_loc), type = "spearman"))
  
}

# Run the application 
shinyApp(ui = ui, server = server)