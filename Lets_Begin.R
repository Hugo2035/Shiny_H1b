

#setwd("~/Acad/Projects/H-1B")

#Updates to do : 
# Create better plotly plots allowing user inputs


#Loading dependent packages  ----
rqrd_Pkg = c('shiny','data.table','plotly','plyr','tidyverse','ggmap')
for(p in rqrd_Pkg){
  if(!require(p,character.only = TRUE)) 
  install.packages(p);
  library(p,character.only = TRUE)
}

##Download the Data from Web ----
file_link = "https://www.uscis.gov/sites/default/files/USCIS/Data/Employment-based/H-1B/h1b_datahubexport-All.zip"
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
  mutate(Denial_Rate = Denials/(Approvals+Denials)*100)

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

## Top Industries with most approvals/denials -- NAICS -
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


# ----
c <- left_join(Data, Dept)

c <- c %>%
  group_by(Year, Dept_Name) %>%
  summarize(Approvals = sum(Initial_Approvals), Denials = sum(Initial_Denials),
  C_Approvals = sum(Continuing_Approvals), C_Denials = sum(Continuing_Denials)) %>%
  mutate(Denial_Rate = Denials/(Approvals+Denials)*100)

top_apr_dept <- c %>%
  group_by(Dept_Name) %>%
  summarize(All_apr = sum(Approvals)) %>%
  arrange(desc(All_apr)) %>%
  top_n(7) %>%
  select(Dept_Name)

dept_approval <- c %>%
  filter(Dept_Name %in% top_apr_dept$Dept_Name)

dept_approval$Year <- as.numeric(dept_approval$Year)

#Wordcloud
#require(wordcloud2)
#wordcloud2(data = dept_approval[c("Dept_Name","Approvals")])

apr_plot <- spread(dept_approval[,c('Year','Dept_Name','Approvals')], Dept_Name, Approvals)

# ggplot(dept_approval, aes(x=Year, y=Approvals, color=Dept_Name)) +
#   geom_line()+
#   theme_minimal()

#No. of approvals plot
plot_ly(c, x = ~Year, y=~Approvals, color =~Dept_Name, type='scatter', mode = 'line') %>%
  #add_lines()%>%
  #add_trace(x = ~Year, y = ~Denials, type = "scatter", mode = "lines", color = I('red'), name = "Denials") %>%
  layout(title = "H-1B Visas Approvals by Dept",
         xaxis = list(title = "Year"),
         yaxis = list (title = "Count"))

#Denial rate plot
plot_ly(c, x = ~Year, y=~Denial_Rate, color =~Dept_Name, type='scatter', mode = 'line') %>%
  layout(title = "H-1B Visas Denials Rate by Dept",
         xaxis = list(title = "Year"),
         yaxis = list (title = "Count"))

plot_ly(apr_plot, x = ~Year, y=~apr_plot$` Educational Services` , type='scatter', mode='lines') %>%
  add_lines()%>%
  #add_trace(x = ~Year, y = ~Denials, type = "scatter", mode = "lines", color = I('red'), name = "Denials") %>%
  layout(title = "H-1B Visas",
 xaxis = list(title = "Year"),
 yaxis = list (title = "Count"))

#### By Employer --------
## Need to clean 'Employer' field
length(unique(Data$Employer))
trim <- function (x) gsub("^\\s+|\\s+$", "", x)
drop_words <- function (x) gsub("INC|LLC|L L C|LLP|CORPORATION|CORP", "", x)   #Drop these words
length(unique(Data$Employer))
length(unique(trim(Data$Employer)))
length(unique(trim(drop_words(Data$Employer))))

Data$Employer <- trim(drop_words(Data$Employer))

#Employer wise plot for last two years:
b <- Data %>%
  filter(Year > 2017) %>%
  group_by(Employer) %>%
  summarize(Approvals = sum(Initial_Approvals), Denials = sum(Initial_Denials),
            C_Approvals = sum(Continuing_Approvals), C_Denials = sum(Continuing_Denials)) %>%
  mutate(Denial_Rate = Denials/(Approvals+Denials)*100)


employer_wc <- b%>% select(Employer, Approvals) %>% arrange(desc(Approvals)) %>% top_n(15, Approvals)

set.seed(2344575)
wordcloud2(employer_wc, size = .3, minRotation = 0, maxRotation = 0, color=rep_len( c("blueviolet","darkviolet","blue","darkmagenta","purple","deeppink","darkorchid","brown"), nrow(demoFreq) ), backgroundColor = "white")

### State, City Wise Analysis ----
#Plotting top 10 states with max approvals in last 2 years using ggplot
Data %>%
  filter(Year > 2017) %>%
  group_by(State) %>%
  summarize(Approvals = sum(Initial_Approvals), Denials = sum(Initial_Denials),
            C_Approvals = sum(Continuing_Approvals), C_Denials = sum(Continuing_Denials)) %>%
  arrange(desc(Approvals)) %>%
  top_n(10, Approvals) %>%
  plot_ly(x= ~(factor(State, levels=unique(State))[order(Approvals, decreasing = TRUE)]), 
          y=~Approvals, type='bar') %>%
  layout(title = "Top 10 States with highest Approvals in 2018, 2019",
         xaxis = list(title = "State"),
         yaxis = list (title = "Approvals"))

# ## GGplot ---
# Data %>%
#   filter(Year > 2017) %>%
#   group_by(State) %>%
#   summarize(Approvals = sum(Initial_Approvals), Denials = sum(Initial_Denials),
#             C_Approvals = sum(Continuing_Approvals), C_Denials = sum(Continuing_Denials)) %>%
#   arrange(desc(Approvals)) %>%
#   top_n(10, Approvals) %>%
#   ggplot(aes(x=reorder(State, -Approvals), y=Approvals)) +
#     geom_bar(aes(fill= I('Purple'), color=I('Violet')), stat='identity', width=0.7) +
#     theme_minimal() +
#     labs(title = "Top 10 states with most Approvals in 2018 & 2019", x="State")

cities <- Data %>%
  filter(Year > 2017) %>%
  group_by(City) %>%
  summarize(Approvals = sum(Initial_Approvals), Denials = sum(Initial_Denials),
            C_Approvals = sum(Continuing_Approvals), C_Denials = sum(Continuing_Denials)) %>%
  arrange(desc(Approvals)) %>%
  top_n(100, Approvals)


# Use geocode from ggmap to get lat,lon coordinates
get_coords <- function(City){
  coords <- geocode(City, source = 'dsk')
  df <- cbind(City, coords)
  return(df)
}

#get_coords(Data[["City"]][1001])  #Google API Limit is 2500 requests per day
#unique(Data[["City"]])[999]

coords_cities <- get_coords(cities$City)
cities <- cbind(cities, coords_cities[c("lon","lat")])

# geo styling
g <- list(
  scope = 'usa',
  showland = TRUE,
  landcolor = toRGB("grey83"),
  subunitcolor = toRGB("white"),
  countrycolor = toRGB("white"),
  showlakes = TRUE,
  lakecolor = toRGB("white"),
  showsubunits = TRUE,
  showcountries = TRUE,
  resolution = 50,
  projection = list(
    type = 'conic conformal',
    rotation = list(lon = -100)
  ),
  lonaxis = list(
    showgrid = TRUE,
    gridwidth = 0.5,
    range = c(-140, -55),
    dtick = 5
  ),
  lataxis = list(
    showgrid = TRUE,
    gridwidth = 0.5,
    range = c(15, 70),
    dtick = 5
  )
)

plot_geo(cities, lat = ~lat, lon = ~lon, color = ~Approvals, size = ~Approvals) %>%
  add_markers(hovertext = ~(paste("City:", City, "\nNumber of Approvals:", Approvals))) %>%
  layout(title = 'Top cities with H-1B for 2018 & 2019', geo=g)


