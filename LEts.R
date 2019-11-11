


#Loading dependent packages  ----
rqrd_Pkg = c('shiny','data.table','plotly','plyr','tidyverse')
for(p in rqrd_Pkg){
  if(!require(p,character.only = TRUE)) 
install.packages(p);
  library(p,character.only = TRUE)
}


##Download the Data from Web ----
file_link = "https://www.uscis.gov/sites/default/files/USCIS/Data/Employment-based/H-1B/H-1b_datahubexport-All.zip"
temp <- tempfile()
download.file(file_link, temp)
temp_2 = unzip(temp)


# Data Cleaning and transformation ----
Data <- ldply(temp_2, fread)
columns <-c("Initial Approvals", "Initial Denials", "Continuing Approvals", "Continuing Denials")
Data[, columns] <- lapply(columns, function(x) as.numeric(Data[[x]]))


colnames(Data) <- c("Year","Employer","Initial_Approvals","Initial_Denials"
,"Continuing_Approvals","Continuing_Denials","NAICS","Tax_ID"  
,"State", "City","ZIP")

#apply(Data, 2, function(x){sum(is.na(x))})  #check missing values
Data[is.na(Data)] <- 0  #Replace missing values
Data  <- Data %>%
  filter(Year > 2009)  



### Is there a decline in approvals----
a <- Data %>%
  group_by(Year) %>%
  summarize(Approvals = sum(Initial_Approvals), Denials = sum(Initial_Denials),
  C_Approvals = sum(Continuing_Approvals), C_Denials = sum(Continuing_Denials)) %>%
  mutate(Approvals = Approvals, Denials = Denials, C_Approvals = C_Approvals, C_Denials = C_Denials)

ggplot(a)+
  geom_line(aes(x=Year,y=Approvals, group=1, colour = 'Approvals'), size=1.1)+
  geom_line(aes(x=Year,y=Denials, group=2, colour = 'Denials'), size=1.1)+
  geom_line(aes(x=Year,y=C_Approvals, group=1, colour = 'C.Approvals'), size=1.1)+
  geom_line(aes(x=Year,y=C_Denials, group=2, colour = 'C.Denials'), size=1.1)+
  geom_point(aes(x=Year,y=Approvals, group=1, colour = 'Approvals'), size=1.2)+
  geom_point(aes(x=Year,y=Denials, group=2, colour = 'Denials'), size=1.2)+
  labs(title = "H-1B Visa", x= 'Year', y = 'Count') +
  theme_minimal()

#plotly chart:
plot_ly(a , x = ~Year, y = ~Approvals, type = "scatter", mode = "lines", color = I('dark green'), name = "Approvals") %>%
  add_trace(x = ~Year, y = ~Denials, type = "scatter", mode = "lines", color = I('red'), name = "Denials") %>%
  layout(title = "H-1B Visas",
 xaxis = list(title = "Year"),
 yaxis = list (title = "Count"))

# So, it is true what we are hearing in the media. Not only Denials are increasing, but Approvals are reducing too!
#   * I have focused on Initial Approvals and Initial Denials in this plot.



### Top Employers, Industries with most accepts, most denials? -----

## Need to clean 'Employer' field
length(unique(Data$Employer))
trim <- function (x) gsub("^\\s+|\\s+$", "", x)
length(unique(trim(Data$Employer)))


b <- Data %>%
  group_by(Employer) %>%
  summarize(Approvals = sum(Initial_Approvals), Denials = sum(Initial_Denials),
  C_Approvals = sum(Continuing_Approvals), C_Denials = sum(Continuing_Denials)) %>%
  mutate(Approvals = Approvals, Denials = Denials, C_Approvals = C_Approvals, C_Denials = C_Denials)


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

c <- c %>%
  group_by(Year, Dept_Name) %>%
  summarize(Approvals = sum(Initial_Approvals), Denials = sum(Initial_Denials),
  C_Approvals = sum(Continuing_Approvals), C_Denials = sum(Continuing_Denials)) %>%
  mutate(Approvals = Approvals, Denials = Denials, C_Approvals = C_Approvals, C_Denials = C_Denials)
  
dept_approval <- c %>%
  top_n(5,Approvals)

ggplot(dept_approval, aes(x=Year, y=Approvals, color=Dept_Name)) +
  geom_line()

plot_ly(dept_approval, x = ~Year, y = ~Approvals, type='scatter',mode = 'lines', color = ~Dept_Name) %>%
  #add_lines()%>%
  #add_trace(x = ~Year, y = ~Denials, type = "scatter", mode = "lines", color = I('red'), name = "Denials") %>%
  layout(title = "H-1B Visas",
 xaxis = list(title = "Year"),
 yaxis = list (title = "Count"))


### State, City Wise Analysis ----



