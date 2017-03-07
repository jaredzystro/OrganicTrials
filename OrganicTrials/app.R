# Required libraries
library(googlesheets)
library(lsmeans)
library(gridExtra)
library(reshape)
library(lattice)
library(grid)
library(gplots)
library(Hmisc)
library(DT)
library(lme4)

# Google sheet authenication needs to be refreshed if not used for six months

#token <- gs_auth()
#saveRDS(token, file = "googlesheets_token.rds")

# Set sheet list
googlesheets::gs_auth(token = "googlesheets_token.rds")
sheet_df <<- as.data.frame(gs_ls("-ds"))[,c("sheet_title","sheet_key")]
sheet_list <<- setNames(as.list(sheet_df$sheet_key), sheet_df$sheet_title)

# Get data from Google sheets and set as global dataframe
update_gs <- function (key) {
  
  DATA_gs <<- gs_key(key)
  DATA <<- gs_read(DATA_gs,ws="Data")
  DATA$Block <<- factor(DATA$Block)
  DATA$Location <<- factor(DATA$Location)
  DATA$LocBlock <<- factor(paste0(DATA$Location,DATA$Block))

  
  META <<- gs_read(DATA_gs,ws="Metadata")
  trait_df <<- subset(META,Visable=="Yes")
  traits <<- trait_df$TraitID
  
}

update_gs("1je8VIIr7bf0ny_tTvZ8Rm5edkWIWXX85Zq-mbco7WA0")

# Define UI for application 
ui <- shinyUI(fluidPage(
  
  # Application title
  #   titlePanel(TOMI_gs$sheet_title),
  
  # Sidebar 
  sidebarLayout(
    sidebarPanel(
      selectInput("sheet", "Data Sheet", sheet_list),
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
server <- shinyServer(function(input, output, session) {
  
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
  
  observeEvent(input$sheet, {
    update_gs(input$sheet)
    updateSelectInput(session, "trait",
                      choices = traits)
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

GetRegionalMeanCI <- function (trait_name, trait_env) {
  
  regional_data <- DATA[DATA$Location == trait_env, ]
  regional_data <- droplevels(regional_data)

  formula_name <- as.formula(paste0(trait_name,"~ EntryName + (1|Block)"))
    
  trait.lm <- lmer (formula_name, data=regional_data)
  trait.lsm <- lsmeans(trait.lm, ~ EntryName, data=regional_data) ### REQUIRES lsmeans
  trait.cld<-cld(trait.lsm)
  
  trait.cld$CI <- (trait.cld$upper.CL - trait.cld$lower.CL)/2
  trait.cld[,trait_name] <- paste0(format(round(trait.cld$lsmean, 2), nsmall = 2),
                                   " ± ", format(round(trait.cld$CI,1), nsmall=1))
  trait.cld <- trait.cld[,c("EntryName",trait_name)]
  colnames(trait.cld) <- c("EntryName", trait_env)
  return(trait.cld)
  
}


table_func <- function (input, DATA) {
  
  env_list <- list()
  
  eval(parse(text = paste0("env_subset <- levels(droplevels(subset(DATA, !is.na(",input$trait,"), select = Location))$Location)")))
  
  for (env_num in 1:length(env_subset)) {
    
    env_name <- env_subset[env_num]
    
    env_list[[env_name]] <- GetRegionalMeanCI(input$trait, env_name)
    
  }
  
  trait_table<-Reduce(function(x, y) merge(x, y, all=T),env_list, accumulate=F)
  rownames(trait_table) <- trait_table$EntryName
  trait_table <- trait_table[,-which(names(trait_table)=="EntryName")]
  
  return (trait_table)
  
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