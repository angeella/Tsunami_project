
#level: province or soreu

#variable: missions or Y

#form.mod: formula

#dataset: Coming from importDataHour

#zona: one type of soreu or one type of province

#nday: how many day ahead 

#path: where the R functions needed are (must end with /)


TSUNAMImodel <- function(level, variable, form.mod, analysis, dataset, zona, nday, model = "gam", years_sel = c(2020:2021), path = "~/Github/TSUNAMI/Code/Angela/"){
  source(paste0(path,"library.R"))
  source(paste0(path, "utils.R"))
  source(paste0(path,"addCovariateHour.R"))
  source(paste0(path,"create_newdataHour.R"))
  source(paste0(path,"models_predict.R"))
  
  #Check arguments
  level_set <- c("soreu", "province")
  variable_set <- c("calls", "events")
  #analysis_set <- c("1", "2", "3", "4", "5", "7")
  zona_set <- c("pianura", "laghi", "como", "cremona", "lecco", "lodi", "mantova", "pavia", "varese", "monza")
  
  level <- match.arg(tolower(level), level_set)
  variable <- match.arg(tolower(variable), variable_set)
  #analysis <- match.arg(analysis, analysis_set)
  #  zona <- match.arg(tolower(zona), zona_set)
  
  #check consistency zona-level
  if(level =="soreu" & zona %in% c("como", "cremona", "lecco", "lodi", "mantova", "pavia", "varese")){
    stop("Please insert laghi or pianura")}
  if(level =="province" & zona %in% c("pianura", "laghi")){
    stop("Please insert a province")}
  
  if(level == "soreu") {zona <- toupper(zona)}
  if(level == "province") {zona <- firstup(zona)}
  
  
  #Filter years
  dataset <- dataset %>% dplyr::filter(Year %in% years_sel)
  
  #addCovariate
  Train <- addCovariateHour(level = level, 
                            variable = variable, 
                            dataset = dataset, 
                            zona = zona, 
                            path = path)
  
  Test <- create_newdataHour(DB = Train, level = level, zona = zona, nday = nday,
                             full = FALSE)
  
  mod <- models_predict(form.mod = form.mod, DB = Train,
                        new_data = Test, model = "gam", 
                        nday = nday)
  
  Test$Y <- mod$out$Y
  
  res <- aggregateResultsHour(new_data = Test)
  
  return(res)
}