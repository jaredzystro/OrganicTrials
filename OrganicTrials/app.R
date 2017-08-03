# Style guide -----
# ALLCAPS - global constants
# UpperCamel - functions
# snake_case - variables
# Space between operators
# No space after function call

# Required libraries
library(rdrop2)
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

# Dropbox authenication done only once. May need to be refreshed...
# TOKEN<-drop_auth()
# saveRDS(TOKEN, "droptoken.rds")

# Set TOKEN from token file
TOKEN <- readRDS("droptoken.rds")

# Path to trial data
PATH <- "/CompiledData"

# Get file list
CreateFileList <- function (path) {
  files <- as.data.frame(drop_dir(path = path, dtoken = TOKEN))[ ,c("path")]
  files <- str_match(files, ".*/(.*)")
  file_list <- as.list(files[,1])
  names(file_list) <- files[,2]
  
  return (file_list)
}
FILELIST <- CreateFileList(PATH)

# Get data from selected file and set as global dataframe
UpdateFile <- function (file_name) {
  
  #First seven lines are metadata
  meta_data <- drop_read_csv(file_name, dtoken = TOKEN, nrow=6)
  data <- drop_read_csv(file_name, dtoken = TOKEN, skip=7)
  
  data$Block <- factor(data$Block)
  data$Location <- factor(data$Location)
  data$LocBlock <- factor(paste0(data$Location,data$Block))
  
  meta_data <-t(meta_data)
  colnames(meta_data) <- c("Name","Units","Description","Notes","Notes2","Visable")
  
  return(list(data = data, meta_data = meta_data))  
  
}
DATA <- UpdateFile(as.character(FILELIST[1]))

UpdateTraits <- function(data) {
  
  meta_data <- as.data.frame(data$meta_data)
  traits <- rownames(subset(meta_data,Visable == "Visable"))
  
  return(traits)
}
TRAITS <- UpdateTraits(DATA)

# Define UI for application 
ui <- shinyUI(fluidPage(
  
  # Application title
  #   titlePanel(TOMI_gs$sheet_title),
  
  # Sidebar 
  sidebarLayout(
    sidebarPanel(
      selectInput("file", "Data File", FILELIST),
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
))

# Define server logic
server <- shinyServer(function(input, output, session) {
  
  output$results <- renderUI({
    
    switch (input$analysis, 
            xyplot = plotOutput("xyplot"),
            normal = plotOutput("normal"),
            table = DT::dataTableOutput("table"),
            anova =  verbatimTextOutput("anova"),
            spearman =  verbatimTextOutput("spearman"))
    
  })
  
  output$xyplot <- renderPlot({XyplotFunc(input, DATA$data)})   
  output$normal <- renderPlot({NormalFunc(input, DATA$data)})
  output$table <- DT::renderDataTable({TableFunc(input,DATA$data)})
  output$anova <- renderPrint({AnovaFunc(input, DATA$data)})
  output$spearman <- renderPrint({SpearmanFunc(input, DATA$data)})
  
  observeEvent(input$file, {
    DATA <<- UpdateFile(input$file)
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
