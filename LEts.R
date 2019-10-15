

#setwd("~/Acad/Projects/H1B")
library(plyr)
library(data.table)
library(tidyverse)

##Download the file from Web
file_link = "https://www.uscis.gov/sites/default/files/USCIS/Data/Employment-based/H-1B/h1b_datahubexport-All.zip"
temp <- tempfile()
download.file(file_link, temp)

temp_2 = unzip(temp)
Data <- fread(temp_2[1])

Data_2 <- ldply(temp_2, read_csv)

Data_3 <- ldply(temp_2, fread)








