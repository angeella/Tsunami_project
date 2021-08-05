#level: province or soreu

#variable: events or Y

#dataset

#zona: one type of soreu or one type of province

#path where utils and library scripts are must end with /

addCovariateHour <- function(level, variable, dataset, zona, path){
  
  #Load libraries and utils functions
  source(paste0(path,"library.R"))
  source(paste0(path,"utils.R"))
  
  #Check arguments
  level_set <- c("soreu", "province")
  variable_set <- c("calls", "events")
  #analysis_set <- c("1", "2", "3", "4", "5", "7")
  zona_set <- c("pianura", "laghi", "como", "cremona", "lecco", "lodi", "mantova", "pavia", "varese", "monza")
  
  level <- match.arg(tolower(level), level_set)
  variable <- match.arg(tolower(variable), variable_set)
  
  #Check consistency zona-level
  if(level =="soreu" & zona %in% c("como", "cremona", "lecco", "lodi", "mantova", "pavia", "varese")){
    stop("Please insert laghi or pianura")}
  if(level =="province" & zona %in% c("pianura", "laghi")){
    stop("Please insert a province")}
  
  if(level == "soreu") {zona <- toupper(zona)}
  if(level == "province") {zona <- firstup(zona)}
  
  #Filter by zona
  if(level == "province"){
    dataset <- dataset %>% dplyr::filter(Province %in% as.character(zona))
    
    colnames(dataset)[grepl("calls", colnames(dataset))] <- "Y"
    colnames(dataset)[grepl("events", colnames(dataset))] <- "Y"
    
    
    
    if(variable == "events"){
      dataset <- dataset %>% dplyr::select(-Soreu) %>%
        group_by(Province, Date) %>%
        mutate(Y = sum(Y))  %>%
        mutate(SoreuY = sum(SoreuY),
               Chiamate = sum(Chiamate)) %>% distinct()
      
    }else{
      dataset <- dataset %>% dplyr::select(-Soreu) %>%
        group_by(Province, Date) %>%
        mutate(Y = sum(Y))  %>%
        mutate(SoreuY = sum(SoreuY)) %>% distinct()
      dataset$Chiamate <- 0
    }
  }
  
  if(level == "soreu"){
    
    dataset <- dataset %>% dplyr::filter(Soreu == zona)
    
    colnames(dataset)[grepl("calls", colnames(dataset))] <- "Y"
    colnames(dataset)[grepl("events", colnames(dataset))] <- "Y"
    
    dataset$Chiamate <- 0
    dataset$SoreuY <- 0
    dataset$Chiamate_lagDay1 <- 0
  }
  
  out <- dataset %>%
    arrange(Date, Hour) %>%
    mutate(Y_lagDay1 = dplyr::lag(Y, 24, order_by = Date),
           Y_lagDay2 = dplyr::lag(Y, n = 2*24, default = NA, order_by = Date),
           Y_lagDay3 = dplyr::lag(Y, n = 3*24, default = NA, order_by = Date),
           Y_lagDay4 = dplyr::lag(Y, n = 4*24, default = NA, order_by = Date),
           Y_lagDay5 = dplyr::lag(Y, n = 5*24, default = NA, order_by = Date),
           Y_lagDay6 = dplyr::lag(Y, n = 6*24, default = NA, order_by = Date),
           Y_lagDay7 = dplyr::lag(Y, n = 7*24, default = NA, order_by = Date),
           Y_lagDay8 = dplyr::lag(Y, n = 8*24, default = NA, order_by = Date),
           Y_lagDay9 = dplyr::lag(Y, n = 9*24, default = NA, order_by = Date),
           Y_lagDay10 = dplyr::lag(Y, n = 10*24, default = NA, order_by = Date),
           Y_lagDay12 = dplyr::lag(Y, n = 12*24, default = NA, order_by = Date),
           Y_lagDay14 = dplyr::lag(Y, n = 14*24, default = NA, order_by = Date),
           Y_lagDay16 = dplyr::lag(Y, n = 16*24, default = NA, order_by = Date),
           Y_lagDay18 = dplyr::lag(Y, n = 18*24, default = NA, order_by = Date),
           Y_lagDay19 = dplyr::lag(Y, n = 19*24, default = NA, order_by = Date),
           Y_lagDay20 = dplyr::lag(Y, n = 20*24, default = NA, order_by = Date),
           Y_lagDay21 = dplyr::lag(Y, n = 21*24, default = NA, order_by = Date),
           Y_lagDay22 = dplyr::lag(Y, n = 22*24, default = NA, order_by = Date),
           Y_lagDay30 = dplyr::lag(Y, n = 30*24, default = NA, order_by = Date),
           Flu1 = dplyr::lag(Flu, n = 1*24*7, default = NA, order_by = Date),
           Flu2 = dplyr::lag(Flu, n = 2*24*7, default = NA, order_by = Date),
           R.medio1 = dplyr::lag(R.medio, n = 1*24, default = NA, order_by = Date),
           R.medio2 = dplyr::lag(R.medio, n = 2*24, default = NA, order_by = Date),
           totale_ospedalizzati_lag1 = dplyr::lead(totale_ospedalizzati, n = 1*24, default = NA, order_by = Date),
           R.medio_lead1 = dplyr::lead(R.medio, n = 1*24, default = NA, order_by = Date),
           R.medio_lead2 = dplyr::lead(R.medio, n = 2*24, default = NA, order_by = Date),
           R.medio_lead3 = dplyr::lead(R.medio, n = 3*24, default = NA, order_by = Date),
           R.medio_lead4 = dplyr::lead(R.medio, n = 4*24, default = NA, order_by = Date),
           R.medio_lead5 = dplyr::lead(R.medio, n = 5*24, default = NA, order_by = Date),
           R.medio_lead6 = dplyr::lead(R.medio, n = 6*24, default = NA, order_by = Date),
           R.medio_lead7 = dplyr::lead(R.medio, n = 7*24, default = NA, order_by = Date),
           Rt_hosp_lead1 = dplyr::lead(Rt_hosp, n = 1*24, default = NA, order_by = Date),
           Rt_hosp_lead2 = dplyr::lead(Rt_hosp, n = 2*24, default = NA, order_by = Date),
           Temp.medio1 = dplyr::lag(Temp.medio, n = 1*24, default = NA, order_by = Date),
           Temp.medio2 = dplyr::lag(Temp.medio, n = 2*24, default = NA, order_by = Date),
           Temp.delta1 = dplyr::lag(Temp.delta, n = 1*24, default = NA, order_by = Date),
           Temp.delta2 = dplyr::lag(Temp.delta, n = 2*24, default = NA, order_by = Date),
           festivoSenzaFeste1 = dplyr::lag(festivoSenzaFeste, n = 1*24, default = NA, order_by = Date),
           festivoSenzaFeste2 = dplyr::lag(festivoSenzaFeste, n = 2*24, default = NA, order_by = Date),
           festivoConFeste1 = dplyr::lag(festivoConFeste, n = 1*24, default = NA, order_by = Date),
           festivoConFeste2 = dplyr::lag(festivoConFeste, n = 2*24, default = NA, order_by = Date),
           Temp.delta161 = dplyr::lag(Temp.delta16, n = 1*24, default = NA, order_by = Date),
           Temp.delta162 = dplyr::lag(Temp.delta16, n = 2*24, default = NA, order_by = Date),
           Eventi_piu1 = dplyr::lag(Eventi_piu, n = 1*24, default = NA, order_by = Date),
           Eventi_piu2 = dplyr::lag(Eventi_piu, n = 2*24, default = NA, order_by = Date)
    )
  
  
  dataset <- out %>% mutate(CriticDay = ifelse(festivoConFeste1 == 1 | Day == "Monday", 1, 0))
  
  dataset <- dataset %>%
    mutate(Temp.delta16sq = Temp.delta16^2,
           MidWeek = ifelse(DayWeekend == "Weekdays" & festivoConFeste == "Festivo",1,0))
  
  #transform factor variables
  total_db <- dataset %>%
    mutate(Week = as.factor(Week),
           Month = as.factor(Month),
           Year = as.factor(Year),
           School = as.factor(School),
           Snow = as.factor(Snow),
           DayWeekend = as.factor(DayWeekend),
           Year.dummy = as.factor(Year.dummy),
           Day = as.factor(Day),
           MidWeek = as.factor(MidWeek),
           Quarter = as.factor(Quarter),
           festivoConFeste = as.factor(festivoConFeste),
           festivoSenzaFeste = as.factor(festivoSenzaFeste),
           d_agosto = as.factor(d_agosto),
           Eventi_piu = as.factor(Eventi_piu),
           festivoConFeste1 = as.factor(festivoConFeste1),
           festivoSenzaFeste1 = as.factor(festivoSenzaFeste1),
           Eventi_piu1 = as.factor(Eventi_piu1),
           festivoConFeste2 = as.factor(festivoConFeste2),
           festivoSenzaFeste2 = as.factor(festivoSenzaFeste2),
           Eventi_piu2 = as.factor(Eventi_piu2),
           CriticDay = as.factor(CriticDay),
           Month_n = as.numeric(Month),
           Day_n = as.numeric(Day),
           Year_n = as.numeric(Year)) 
  total_db$Year_f <- factor(total_db$Year, ordered = T)
  total_db$Day_f <- factor(total_db$Day, ordered = T)
  total_db$Month_f <- factor(total_db$Month, ordered = T)
  #put mean over month if NA

  total_db <- total_db %>%
    arrange(Date, Hour) %>%
    mutate(Y_roll2_mean = rollmean(Y, 2*24, na.pad = T, align = "right"),
           Y_roll3_mean = rollmean(Y, 3*24, na.pad = T, align = "right"),
           Y_roll4_mean = rollmean(Y, 4*24, na.pad = T, align = "right"),
           Y_roll5_mean = rollmean(Y, 5*24, na.pad = T, align = "right"),
           Y_roll6_mean = rollmean(Y, 6*24, na.pad = T, align = "right"),
           Y_roll7_mean = rollmean(Y, 7*24, na.pad = T, align = "right"),
           Y_roll8_mean = rollmean(Y, 8*24, na.pad = T, align = "right"),
           Y_roll9_mean = rollmean(Y, 9*24, na.pad = T, align = "right"),
           Y_roll10_mean = rollmean(Y, 10*24, na.pad = T, align = "right"),
           Y_roll11_mean = rollmean(Y, 11*24, na.pad = T, align = "right"),
           Y_roll12_mean = rollmean(Y, 12*24, na.pad = T, align = "right"),
           Y_roll13_mean = rollmean(Y, 13*24, na.pad = T, align = "right"),
           Y_roll14_mean = rollmean(Y, 14*24, na.pad = T, align = "right"),
           Y_roll30_mean = rollmean(Y, 30*24, na.pad = T, align = "right"),
           Tot_casi_lead1 = dplyr::lead(dataset$Tot_casi, n = 1*24, default = NA, order_by = Date),
           Tot_casi_lead2 = dplyr::lead(dataset$Tot_casi, n = 2*24, default = NA, order_by = Date),
           Tot_casi_lead3 = dplyr::lead(dataset$Tot_casi, n = 3*24, default = NA, order_by = Date),
           Tot_casi_lead4 = dplyr::lead(dataset$Tot_casi, n = 4*24, default = NA, order_by = Date),
           Tot_casi_lead5 = dplyr::lead(dataset$Tot_casi, n = 5*24, default = NA, order_by = Date),
           Y_roll7_lagDay1 = rollmean(dataset$Y_lagDay1, 7*24, na.pad = T, align = "right"),
           Y_roll14_lagDay1 = rollmean(Y_lagDay1, 14*24, na.pad = T, align = "right"),
           Y_roll30_lagDay1 = rollmean(Y_lagDay1, 30*24, na.pad = T, align = "right"),
           Y_roll7_lagDay2 = rollmean(Y_lagDay2, 7*24, na.pad = T, align = "right"),
           Y_roll14_lagDay2 = rollmean(Y_lagDay2, 14*24, na.pad = T, align = "right"),
           Y_roll30_lagDay2 = rollmean(Y_lagDay2, 30*24, na.pad = T, align = "right"),
           Temp.delta16_lead1 = dplyr::lead(dataset$Temp.delta, n = 1*24, default = NA, order_by = Date),
           Temp.delta16_lead2 = dplyr::lead(dataset$Temp.delta, n = 2*24, default = NA, order_by = Date)
    )
  
  total_db <- total_db %>%
    group_by(Date) %>%
    mutate(Y_lagDayTot1 = sum(Y_lagDay1),
           Y_lagDayTot2 = sum(Y_lagDay2),
           Y_lagDayTot3 = sum(Y_lagDay3),
           Y_lagDayTot4 = sum(Y_lagDay4),
           Y_lagDayTot5 = sum(Y_lagDay5),
           Y_lagDayTot6 = sum(Y_lagDay6),
           Y_lagDayTot7 = sum(Y_lagDay7)) %>% ungroup()
  
  return(total_db)
}