#
# This is a Shiny web application for interacting with H1-B Data.
# Author: Suraj Malpani
#
#########---######---

require(shiny)

source("global.R")
source("ui.R")
source("server.R")

# Run the application 
shinyApp(ui = ui, server = server)

