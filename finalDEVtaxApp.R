#################################
#Bangor Business school tax app #
#################################
#Last edited: 27/08/24          #
#Edited by: Ollie Barbaresi     #
#eResearch                      #
#################################

source("global.R")
source("ui.R")
source("server.R")


#run the app:
shinyApp(ui = ui, server = server)