

#setwd("~/Acad/Projects/H1B")

#Loading dependent packages
rqrd_Pkg = c('shiny','data.table','plotly','plyr','tidyverse')
for(p in rqrd_Pkg){
  if(!require(p,character.only = TRUE)) 
    install.packages(p);
  library(p,character.only = TRUE)
}

##Download the file from Web
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

apply(Data, 2, function(x){sum(is.na(x))})  #check missing values

Data[is.na(Data)] <- 0

### Is there a decline in approvals----
Data %>%
  filter(year==2019)
  
str(Data)

a <- Data %>%
  group_by(Year) %>%
  summarize(Approvals = sum(Initial_Approvals), Denials = sum(Initial_Denials),
            C_Approvals = sum(Continuing_Approvals), C_Denials = sum(Continuing_Denials)) %>%
  mutate(Approvals = Approvals, Denials = Denials, C_Approvals = C_Approvals, C_Denials = C_Denials)

ggplot(a)+
  geom_line(aes(x=Year,y=Approvals, group=1, colour = 'Approvals'))+
  geom_line(aes(x=Year,y=Denials, group=2, colour = 'Denials'))+
  geom_line(aes(x=Year,y=C_Approvals, group=1, colour = 'C.Approvals'))+
  geom_line(aes(x=Year,y=C_Denials, group=2, colour = 'C.Denials'))+
  geom_point(aes(x=Year,y=Approvals, group=1, colour = 'Approvals'))+
  geom_point(aes(x=Year,y=Denials, group=2, colour = 'Denials'))+
  labs(title = "H1B Visa", xlabel = 'Year', ylabel = 'Count') +
  theme_minimal()


### Top Employers, Industries with most accepts, most denials? -----

length(unique(trim(Data$Employer)))

## Need to clean 'Employer' field
b <- Data %>%
  group_by(Employer) %>%
  summarize(Approvals = sum(Initial_Approvals), Denials = sum(Initial_Denials),
            C_Approvals = sum(Continuing_Approvals), C_Denials = sum(Continuing_Denials)) %>%
  mutate(Approvals = Approvals, Denials = Denials, C_Approvals = C_Approvals, C_Denials = C_Denials)


### State, City Wise Analysis ----







