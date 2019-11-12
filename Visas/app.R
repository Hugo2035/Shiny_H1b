#
# This is a Shiny web application.
#


#Loading dependent packages
rqrd_Pkg = c('shiny','data.table','plotly','plyr','tidyverse')
for(p in rqrd_Pkg){
  if(!require(p,character.only = TRUE)) 
    install.packages(p);
  library(p,character.only = TRUE)
}

# Define UI for application that draws a histogram
ui <- navbarPage("H1B Visa Approvals",
             tabPanel("All Data",
                      DT::dataTableOutput("table1")), 
             tabPanel("Plot",
                      plotlyOutput("plot1"))
)

# Define server logic required to draw a histogram  -----------
server <- function(input, output) {
    
  file_link = "https://www.uscis.gov/sites/default/files/USCIS/Data/Employment-based/H-1B/h1b_datahubexport-All.zip"
  temp <- tempfile()
  download.file(file_link, temp)
  temp_2 = unzip(temp)
  
  #Load the Data 
  Data <- ldply(temp_2, fread)
  columns <-c("Initial Approvals", "Initial Denials", "Continuing Approvals", "Continuing Denials")
  Data[, columns] <- lapply(columns, function(x) as.numeric(Data[[x]]))
  
  # Data Cleaning and transformation
  colnames(Data) <- c("Year","Employer","Initial_Approvals","Initial_Denials"    
                      ,"Continuing_Approvals","Continuing_Denials","NAICS","Tax_ID"              
                      ,"State", "City","ZIP")
  #apply(Data, 2, function(x){sum(is.na(x))})  #check missing values
  Data[is.na(Data)] <- 0
  Data  <- Data %>%
    filter(Year > 2009)
  Data <- cbind(apply(Data[,c(1,2)],2,as.factor),Data[3:6],apply(Data[7:11],2,as.factor))
  
  ## Top Industries with most approvals/denials -- NAICS
  NAICS <-  c(11,21,22,23,31,32,33,42,44,45,48,49,51,52,53,54,55,56,61,62,71,72,81,92,99)
  Dept_Name <- c(' Agriculture, Forestry, Fishing and Hunting',
                 ' Mining, Quarrying, and Oil and Gas Extraction',
                 ' Utilities',
                 ' Construction',
                 ' Manufacturing',
                 ' Manufacturing',
                 ' Manufacturing',
                 ' Wholesale Trade',
                 ' Retail Trade',
                 ' Retail Trade',
                 ' Transportation and Warehousing',
                 ' Transportation and Warehousing',
                 ' Information',
                 ' Finance and Insurance',
                 ' Real Estate and Rental and Leasing',
                 ' Professional and Technical Services',
                 ' Management of Companies and Enterprises',
                 ' Administrative and Waste Services',
                 ' Educational Services',
                 ' Health Care and Social Assistance',
                 ' Arts, Entertainment, and Recreation',
                 ' Accommodation and Food Services',
                 ' Other Services',
                 ' Public Administration',
                 ' Unknown'
  )
  Dept <- data.frame(NAICS, Dept_Name)
  Dept$NAICS <- as.factor(Dept$NAICS)
  
  c <- left_join(Data, Dept)
  
  Dept_Data <- c %>%
    group_by(Year, Dept_Name) %>%
    summarize(Approvals = sum(Initial_Approvals), Denials = sum(Initial_Denials),
              C_Approvals = sum(Continuing_Approvals), C_Denials = sum(Continuing_Denials)) %>%
    mutate(Approvals = Approvals, Denials = Denials, C_Approvals = C_Approvals, C_Denials = C_Denials)
  
  #Display All Data:
  output$table1 <- DT::renderDataTable({
    Data
  }, filter='top', 
  options = list(pageLength = 10, scrollX=TRUE, autoWidth = TRUE, columnDefs = list(list(width = '200px', targets = 2))))
  
  
  #The plot of approvals & denials
  a <- Data %>%
    group_by(Year) %>%
    summarize(Approvals = sum(Initial_Approvals), Denials = sum(Initial_Denials),
              C_Approvals = sum(Continuing_Approvals), C_Denials = sum(Continuing_Denials)) %>%
    mutate(Approvals = Approvals, Denials = Denials, C_Approvals = C_Approvals, C_Denials = C_Denials)
  
  #plotly chart:
  output$plot1 <- renderPlotly({
  plot_ly(a , x = ~Year, y = ~Approvals, type = "scatter", mode = "lines", color = I('dark green'), name = "Approvals") %>%
    add_trace(x = ~Year, y = ~Denials, type = "scatter", mode = "lines", color = I('red'), name = "Denials") %>%
    layout(title = "H-1B Visas",
           xaxis = list(title = "Year"),
           yaxis = list (title = "Count"))
  })
}

# Run the application 
shinyApp(ui = ui, server = server)

