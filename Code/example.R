rm(list = ls())
path <- "your path where the functions in google drive are"
#path <- "C:/Users/Angela Andreella/Desktop/TSUNAMI/"
level = "Soreu"
variable = "events"

#Create dataset
source(paste0(path,"library.R"))

source(paste0(path,"importDataHour.R"))
dataset <- importDataHour(path = paste0(path, "Data/"), variable = variable, level = level)

#load("C:/Users/Angela Andreella/Desktop/TSUNAMI/events_Soreu_hour.RData")
#dataset <- out
#Arguments TSUNAMImodel function

ps <- "ps"
cr <- "cr"
cc <- "cc"
form.mod <- "Y ~ s(as.numeric(Hour), bs = cr, k = 24) + Temp.delta161 + 
  s(Day_n, bs = ps, k = 7) + Y_lagDayTot1 + Y_lagDay1 + Y_lagDayTot7 +  Y_lagDayTot2 +
  Y_lagDay3 + te(Day_n, as.numeric(Hour), 
                 k = c(7, 24), bs = c(cr, ps)) + Flu + R.medio1 + Y_lagDay2 + Y_lagDay3 + 
  s(as.numeric(Quarter), bs = cr, k = 4)"

zona = "Pianura"

nday <- 1

source(paste0(path, "TSUNAMImodel.R"))

out <- TSUNAMImodel(level = level, 
                    variable = variable, 
                    form.mod = form.mod, 
                    dataset = dataset, 
                    zona = zona, 
                    nday = nday, 
                    path = path)


  