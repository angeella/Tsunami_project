
#path = path where the files are, must finish with /
#level= soreu or province
#variable = Calls or events

#return data.frame
importDataHour <- function(path, level, variable){
  #Library
  source(paste0(path,"library.R"))
  #source("D:/GITHUB_LOCAL/TSUNAMI/Code/Angela/library.R")
  #Some functions
  `%notin%` <- Negate(`%in%`)
  trim <- function (x) gsub("^\\s+|\\s+$", "", x)
  #Check argument
  level_set <- c("soreu", "province")
  variable_set <- c("calls", "events")
  level <- match.arg(tolower(level), level_set)
  variable <- match.arg(tolower(variable), variable_set)
  
  #Load DB Tsunami_20210303-20210325
  calls00 <- read.table(paste0(path,"SCHEDE_SOREU_210401-210509.tab"), header = T, sep = "\t", fill = TRUE, quote = "")
  
  calls0 <- read.table(paste0(path,"SCHEDE_SOREU_2021_1QUARTER.tab"), header = T, sep = "\t", fill = TRUE, quote = "")
  calls1 <- read.table(paste0(path,"SCHEDE_SOREU_2020.tab"), header = T, sep = "\t", fill = TRUE, quote = "")
  calls2 <- read.table(paste0(path,"SCHEDE_SOREU_2019.tab"), header = T, sep = "\t", fill = TRUE, quote = "")
  calls3 <- read.table(paste0(path,"SCHEDE_SOREU_2018.tab"), header = T, sep = "\t", fill = TRUE, quote = "")
  calls4 <- read.table(paste0(path,"SCHEDE_SOREU_2017.tab"), header = T, sep = "\t", fill = TRUE, quote = "")
  calls5 <- read.table(paste0(path,"SCHEDE_SOREU_2016.tab"), header = T, sep = "\t", fill = TRUE, quote = "")
  calls6 <- read.table(paste0(path,"SCHEDE_SOREU_2015.tab"), header = T, sep = "\t", fill = TRUE, quote = "")
 
  names(calls00) <- names(calls0)
  names(calls1) <- names(calls0)
  names(calls2) <- names(calls0)
  names(calls3) <- names(calls0)
  names(calls4) <- names(calls0)
  names(calls5) <- names(calls0)
  names(calls6) <- names(calls0) 
  
  
  calls <- rbind(calls6, calls5, calls4, calls3, calls2, calls1, calls0, calls00)
  rm(calls00, calls0,calls1,calls2,calls3,calls4,calls5, calls6)
  
  calls$AAT <- ifelse(calls$AAT == "", "Undefined", calls$AAT)
  calls <- calls %>% filter(SOREU %in% c("ALPINA", "LAGHI", "METROP", "PIANURA"))
  calls <- calls %>% filter(AAT != "Altro fuori Regione")
  
  #Time variables
  calls$DT_118 <- dmy_hms(calls$DT_118) # convert to date format
  calls$DT_EMERG_DAY <- as.Date(calls$DT_118) # extract the date
  calls$day <- weekdays(calls$DT_EMERG_DAY) # extract the day of the week
  calls$hour <- hour(calls$DT_118)                           # Extract hour
  #DB <- CALLS_FULL %>% dplyr::select(Soreu, Date, Time, Day, Day_num, Week, Month, Quarter)  
  
  ######################################SOREU###########################################
  
  if(level == "soreu"){
    
    
    calls$Check <- ifelse(calls$SOREU == "METROP" & calls$AAT %in% c("Monza", "Milano"), 1,
                          ifelse(calls$SOREU == "PIANURA" & calls$AAT %in% c("Pavia",
                                                                             "Lodi",
                                                                             "Cremona",
                                                                             "Mantova"), 1,
                                 ifelse(calls$SOREU == "ALPINA" & calls$AAT %in% c("Bergamo",
                                                                                   "Brescia",
                                                                                   "Sondrio"), 1,
                                        ifelse(calls$SOREU == "LAGHI" & calls$AAT %in% c("Como",
                                                                                         "Lecco",
                                                                                         "Varese"), 1, 0))))
    calls <- calls %>% filter(Check ==1)
    
    if(variable == "calls"){
      ##  httr::GET  httr::GET THE CALL COUNTS FOR:
      call_counts <- table(calls$SOREU, calls$DT_EMERG_DAY)
      dimensions_cc <- dim(call_counts)
      DB <- as.data.frame(call_counts)
      names(DB) <- c("Soreu", "Date", "calls")
      rm(call_counts)
      
      # levels_reason <- unique(calls$DS_REASON)
      # 
      # for(i in 1:length(levels_reason)){
      #   
      #   filter_calls <- calls %>% filter(DS_REASON == levels_reason[i])
      #   reason <- table(filter_calls$SOREU, filter_calls$DT_EMERG_DAY)
      #   dimensions_cc <- dim(reason)
      #   reason <- as.data.frame(reason)
      #   names(reason) <- c("Soreu", "Date", levels_reason[i])
      #   DB <- left_join(DB, reason, by = c("Soreu", "Date"))
      # }
      
    }else{
      #events
      events <- calls %>% filter(FL_MISSION_EXIST == "S")
      events_counts <- table(events$SOREU, events$DT_EMERG_DAY, events$hour)
      dimensions_cc <- dim(events_counts)
      DB <- as.data.frame(events_counts)
      names(DB) <- c("Soreu", "Date", "Hour", "events")
      rm(events_counts)
      
      #  levels_reason <- unique(events$DS_REASON)
      
      #   for(i in 1:length(levels_reason)){
      
      #     filter_events <- events %>% filter(DS_REASON == levels_reason[i])
      #    reason <- table(filter_events$SOREU, filter_events$DT_EMERG_DAY)
      #    dimensions_cc <- dim(reason)
      #    reason <- as.data.frame(reason)
      #    names(reason) <- c("Soreu", "Date", levels_reason[i])
      #   DB <- left_join(DB, reason, by = c("Soreu", "Date"))
      #  }
      
    }
    
    ## CREATE SOME NEW VARIABLES FOR TIME 
    DB$Time <- as.numeric(as.Date(DB$Date)) - as.numeric(as.Date(DB$Date))[1] +  1
    DB$Day <- factor(weekdays(as.Date(DB$Date)),levels = c("Monday","Tuesday","Wednesday", "Thursday", "Friday","Saturday","Sunday"))
    DB$Day_num <- as.numeric(wday(as.Date(DB$Date)))
    
    DB <- DB %>% 
      mutate(Week = lubridate::week(ymd(Date))) %>%
      mutate(Month = lubridate::month(ymd(Date)),
             Quarter = lubridate::quarter(ymd(Date)),
             Day_num = lubridate::day(ymd(Date)),
             Month_Day = as.factor(paste0(Month,"-", Day_num)),
             Year = lubridate::year(ymd(DB$Date)),
             Date = as.Date(Date),
             Day_year = lubridate::yday(ymd(Date)),
             Day_month = lubridate::mday(ymd(Date)),
             Day_week = lubridate::wday(ymd(Date)),
             Day_quarter = lubridate::qday(ymd(Date)))
    
    DB <- DB %>% 
      mutate(School = ifelse(Month %in% c(9:12,1:6) & Day %notin% c("Saturday","Sunday"), 
                             "School", "NoSchool")) %>%
      mutate(Snow = ifelse(Month %in% c(11:12,1:3), "Winter", "NoWinter"),
             festivoConFeste = ifelse((Month_Day %in% c("1-6", "4-25", "5-1", "6-2",
                                                        "8-15", "11-1", "12-7", "12-8")) |
                                        ( Date %in% c("2020-04-12", "2020-04-13",
                                                      "2019-04-21", "2019-04-22",
                                                      "2018-04-01", "2018-04-02",
                                                      "2017-04-16", "2017-04-17",
                                                      "2016-03-27", "2016-03-28",
                                                      "2015-04-05", "2015-04-06",
                                                      "2019-04-26", "2019-08-16",
                                                      "2019-12-27", "2016-06-03",
                                                      "2016-10-31", "2016-12-09",
                                                      "2015-01-05", "2015-06-01",
                                                      "2015-12-07", "2015-12-31",
                                                      "2017-04-24", "2017-08-14",
                                                      "2018-04-30", "2018-11-02",
                                                      "2018-12-31", "2018-04-26",
                                                      "2018-12-27")) |
                                        (Day == "Sunday"), "Festivo", "NoFestivo"),
             festivoSenzaFeste = ifelse(Day == "Sunday", "Sunday", "NoSunday"),
             d_agosto = ifelse((Date > "2019-07-19" & Date < "2019-09-20") |
                                 (Date > "2018-07-19" & Date < "2018-09-20") |
                                 (Date > "2017-07-19" & Date < "2017-09-20") |
                                 (Date > "2016-07-19" & Date < "2016-09-20") |
                                 (Date > "2015-07-19" & Date < "2015-09-20")|
                                 (Date > "2020-07-19" & Date < "2020-09-20"), "Ferie", "NoFerie"),
             Eventi_piu = ifelse((Month_Day %in% c("1-1")), "Evento", "NoEvento"))
    
    
    
    
    #Check week for flu merging
    DB$Week <- ifelse(DB$Week == max(DB$Week), 
                      52, DB$Week)
    
    #Incidenti stradali
    
    incid_2015 <- read.xlsx(paste0(path,"Incidenti_merged_2015_2019.xls"), 
                            sheetIndex = 1)
    
    incid_2015$Territorio <- trim(incid_2015$Territorio)
    
    Prov <- unique(incid_2015$Territorio)
    db2015 <- list()
    for(i in 2:length(Prov)){
      
      db2015[[i-1]] <- data.frame(Province = rep(Prov[i],12), 
                                  Incidenti = t(incid_2015[which(incid_2015$Territorio == Prov[i]),2:13]),
                                  Month = c(1:12),
                                  Year = rep(2015, 12))
      
    }
    
    db2015 <- rbindlist(db2015) 
    db2015 <- db2015%>%
      mutate(Incid = X2) %>%
      dplyr::select(Province, Month, Year, Incid)
    
    incid_2016 <- read.xlsx(paste0(path,"Incidenti_merged_2015_2019.xls"), sheetIndex = 2)
    
    incid_2016$Territorio <- trim(incid_2016$Territorio)
    
    Prov <- unique(incid_2016$Territorio)
    db2016 <- list()
    for(i in 2:length(Prov)){
      
      db2016[[i-1]] <- data.frame(Province = rep(Prov[i],12), 
                                  Incidenti = t(incid_2016[which(incid_2016$Territorio == Prov[i]),2:13]),
                                  Month = c(1:12),
                                  Year = rep(2016, 12))
      
    }
    
    db2016 <- rbindlist(db2016) 
    db2016 <- db2016%>%
      mutate(Incid = X2) %>%
      dplyr::select(Province, Month, Year, Incid)
    
    
    incid_2017 <- read.xlsx(paste0(path,"Incidenti_merged_2015_2019.xls"), sheetIndex = 3)
    
    incid_2017$Territorio <- trim(incid_2017$Territorio)
    
    Prov <- unique(incid_2017$Territorio)
    db2017 <- list()
    for(i in 2:length(Prov)){
      
      db2017[[i-1]] <- data.frame(Province = rep(Prov[i],12), 
                                  Incidenti = t(incid_2017[which(incid_2017$Territorio == Prov[i]),2:13]),
                                  Month = c(1:12),
                                  Year = rep(2017, 12))
      
    }
    
    db2017 <- rbindlist(db2017) 
    db2017 <- db2017%>%
      mutate(Incid = X2) %>%
      dplyr::select(Province, Month, Year, Incid)
    
    
    incid_2018 <- read.xlsx(paste0(path,"Incidenti_merged_2015_2019.xls"), sheetIndex = 4)
    
    incid_2018$Territorio <- trim(incid_2018$Territorio)
    
    Prov <- unique(incid_2018$Territorio)
    db2018 <- list()
    for(i in 2:length(Prov)){
      
      db2018[[i-1]] <- data.frame(Province = rep(Prov[i],12), 
                                  Incidenti = t(incid_2018[which(incid_2018$Territorio == Prov[i]),2:13]),
                                  Month = c(1:12),
                                  Year = rep(2018, 12))
      
    }
    
    db2018 <- rbindlist(db2018) 
    db2018 <- db2018%>%
      mutate(Incid = X2) %>%
      dplyr::select(Province, Month, Year, Incid)
    
    
    incid_2019 <- read.xlsx(paste0(path,"Incidenti_merged_2015_2019.xls"), sheetIndex = 5)
    
    incid_2019$Territorio <- trim(incid_2019$Territorio)
    
    Prov <- unique(incid_2019$Territorio)
    db2019 <- list()
    for(i in 2:length(Prov)){
      
      db2019[[i-1]] <- data.frame(Province = rep(Prov[i],12), 
                                  Incidenti = t(incid_2019[which(incid_2019$Territorio == Prov[i]),2:13]),
                                  Month = c(1:12),
                                  Year = rep(2019, 12))
      
    }
    
    db2019 <- rbindlist(db2019) 
    db2019 <- db2019%>%
      mutate(Incid = X2) %>%
      dplyr::select(Province, Month, Year, Incid)
    
    db_incid <- rbind(db2015, db2016, db2017,db2018,db2019)
    db_incid$Month <- as.numeric(db_incid$Month)
    
    colnames(db_incid)[1] <- "Soreu"
    
    db_incid$Soreu <- ifelse(db_incid$Soreu %in% c("Monza", "Milano"),"METROP",
                             ifelse(db_incid$Soreu %in%  c("Pavia",
                                                           "Lodi",
                                                           "Cremona",
                                                           "Mantova"),"PIANURA",
                                    ifelse(db_incid$Soreu %in% c("Bergamo",
                                                                 "Brescia",
                                                                 "Sondrio"), "ALPINA", "LAGHI")))
    db_incid <- db_incid %>% 
      group_by(Soreu, Month, Year) %>%
      mutate(Incid = sum(Incid)) %>% distinct()
    
    
    DB <- left_join(DB, db_incid, by=c("Soreu", "Month", "Year")) 
    
    
    ####################################################################################
    ######################################WEATHER DATA##################################
    ####################################################################################
    
    #load list sensors used
    sensors <- readxl::read_xlsx(path = paste0(path, "ARPA/sensori.xlsx"))
    
    #load list files in ARPA folder
    filenames <- list.files(paste0(path,"ARPA"), pattern="*.txt", full.names=F)
    
    ###################################Temperature#######################################
    
    #Bergamo
    sensors_temp <- sensors %>% filter(Tipo == "Temperatura" & Provincia == "Bergamo")
    Bergamo_temp <- read.table(file = paste0(path, "ARPA/", filenames[grepl(sensors_temp$Sensore, filenames)])) %>% distinct()
    colnames(Bergamo_temp) <- c("Sensor", "Date", "Hour", "Medio")
    Bergamo_temp$Province <- rep("Bergamo", nrow(Bergamo_temp))
    
    Bergamo_temp <- Bergamo_temp %>%
      filter(Medio != -999) %>%
      dplyr::select(Date, Medio, Province) %>%
      group_by(Date) %>%
      mutate(
        Date = as.Date(Date),
        Temp.delta = max(Medio) - min(Medio),
        Medio = mean(Medio),
        Temp.delta16 = Medio - 16
      ) %>% distinct()
    
    colnames(Bergamo_temp)[2] <- "Temp.medio"
    
    Date_max <- max(Bergamo_temp$Date)
    res = httr::GET(paste0("https://www.dati.lombardia.it/resource/647i-nhxk.json?idsensore=",sensors_temp$Sensore))
    data = jsonlite::fromJSON(rawToChar(res$content))
    data$Date <- as.Date(data$data)
    data$Province <- rep("Bergamo", nrow(data))
    data$valore <- as.numeric(data$valore)
    data <- data %>% filter(Date >Date_max)
    if(nrow(data)!= 0){
      
      data <- data %>%
        filter(valore != -999) %>%
        dplyr::select(Date, valore, Province) %>%
        group_by(Date) %>%
        mutate(
          Temp.delta = max(valore) - min(valore),
          valore = mean(valore),
          Temp.delta16 = valore - 16
        ) %>% distinct()
      
      colnames(data)[2] <- "Temp.medio"
      Bergamo_temp <- rbind(Bergamo_temp, data)
    }
    
    #Brescia
    sensors_temp <- sensors %>% filter(Tipo == "Temperatura" & Provincia == "Brescia")
    Brescia_temp <- read.table(file = paste0(path, "ARPA/", filenames[grepl(sensors_temp$Sensore, filenames)])) %>% distinct()
    colnames(Brescia_temp) <- c("Sensor", "Date", "Hour", "Medio")
    Brescia_temp$Province <- rep("Brescia", nrow(Brescia_temp))
    
    Brescia_temp <- Brescia_temp %>%
      filter(Medio != -999) %>%
      dplyr::select(Date, Medio, Province) %>%
      group_by(Date) %>%
      mutate(
        Date = as.Date(Date),
        Temp.delta = max(Medio) - min(Medio),
        Medio = mean(Medio),
        Temp.delta16 = Medio - 16
      ) %>% distinct()
    
    colnames(Brescia_temp)[2] <- "Temp.medio"
    
    Date_max <- max(Brescia_temp$Date)
    res =  httr::GET(paste0("https://www.dati.lombardia.it/resource/647i-nhxk.json?idsensore=",sensors_temp$Sensore))
    data = jsonlite::fromJSON(rawToChar(res$content))
    data$Date <- as.Date(data$data)
    data$Province <- rep("Brescia", nrow(data))
    data$valore <- as.numeric(data$valore)
    data <- data %>% filter(Date >Date_max)
    if(nrow(data)!= 0){
      
      data <- data %>%
        filter(valore != -999) %>%
        dplyr::select(Date, valore, Province) %>%
        group_by(Date) %>%
        mutate(
          Temp.delta = max(valore) - min(valore),
          valore = mean(valore),
          Temp.delta16 = valore - 16
        ) %>% distinct()
      
      colnames(data)[2] <- "Temp.medio"
      Brescia_temp <- rbind(Brescia_temp, data)
    }
    
    #Como
    sensors_temp <- sensors %>% filter(Tipo == "Temperatura" & Provincia == "Como")
    Como_temp <- read.table(file = paste0(path, "ARPA/", filenames[grepl(sensors_temp$Sensore, filenames)])) %>% distinct()
    colnames(Como_temp) <- c("Sensor", "Date", "Hour", "Medio")
    Como_temp$Province <- rep("Como", nrow(Como_temp))
    
    Como_temp <- Como_temp %>%
      filter(Medio != -999) %>%
      dplyr::select(Date, Medio, Province) %>%
      group_by(Date) %>%
      mutate(
        Date = as.Date(Date),
        Temp.delta = max(Medio) - min(Medio),
        Medio = mean(Medio),
        Temp.delta16 = Medio - 16
      ) %>% distinct()
    
    colnames(Como_temp)[2] <- "Temp.medio"
    
    Date_max <- max(Como_temp$Date)
    res =  httr::GET(paste0("https://www.dati.lombardia.it/resource/647i-nhxk.json?idsensore=",sensors_temp$Sensore))
    data = jsonlite::fromJSON(rawToChar(res$content))
    data$Date <- as.Date(data$data)
    data$Province <- rep("Como", nrow(data))
    data$valore <- as.numeric(data$valore)
    data <- data %>% filter(Date >Date_max)
    if(nrow(data)!= 0){
      
      data <- data %>%
        filter(valore != -999) %>%
        dplyr::select(Date, valore, Province) %>%
        group_by(Date) %>%
        mutate(
          Temp.delta = max(valore) - min(valore),
          valore = mean(valore),
          Temp.delta16 = valore - 16
        ) %>% distinct()
      
      colnames(data)[2] <- "Temp.medio"
      Como_temp <- rbind(Como_temp, data)
    }
    
    #Cremona
    sensors_temp <- sensors %>% filter(Tipo == "Temperatura" & Provincia == "Cremona")
    Cremona_temp <- read.table(file = paste0(path, "ARPA/", filenames[grepl(sensors_temp$Sensore, filenames)])) %>% distinct()
    colnames(Cremona_temp) <- c("Sensor", "Date", "Hour", "Medio")
    Cremona_temp$Province <- rep("Cremona", nrow(Cremona_temp))
    
    Cremona_temp <- Cremona_temp %>%
      filter(Medio != -999) %>%
      dplyr::select(Date, Medio, Province) %>%
      group_by(Date) %>%
      mutate(
        Date = as.Date(Date),
        Temp.delta = max(Medio) - min(Medio),
        Medio = mean(Medio),
        Temp.delta16 = Medio - 16
      ) %>% distinct()
    
    colnames(Cremona_temp)[2] <- "Temp.medio"
    
    Date_max <- max(Cremona_temp$Date)
    res =  httr::GET(paste0("https://www.dati.lombardia.it/resource/647i-nhxk.json?idsensore=",sensors_temp$Sensore))
    data = jsonlite::fromJSON(rawToChar(res$content))
    data$Date <- as.Date(data$data)
    data$Province <- rep("Cremona", nrow(data))
    data$valore <- as.numeric(data$valore)
    data <- data %>% filter(Date >Date_max)
    if(nrow(data)!= 0){
      
      data <- data %>%
        filter(valore != -999) %>%
        dplyr::select(Date, valore, Province) %>%
        group_by(Date) %>%
        mutate(
          Temp.delta = max(valore) - min(valore),
          valore = mean(valore),
          Temp.delta16 = valore - 16
        ) %>% distinct()
      
      colnames(data)[2] <- "Temp.medio"
      Cremona_temp <- rbind(Cremona_temp, data)
    }
    
    #Lecco
    sensors_temp <- sensors %>% filter(Tipo == "Temperatura" & Provincia == "Lecco")
    Lecco_temp <- read.table(file = paste0(path, "ARPA/", filenames[grepl(sensors_temp$Sensore, filenames)])) %>% distinct()
    colnames(Lecco_temp) <- c("Sensor", "Date", "Hour", "Medio")
    Lecco_temp$Province <- rep("Lecco", nrow(Lecco_temp))
    
    Lecco_temp <- Lecco_temp %>%
      filter(Medio != -999) %>%
      dplyr::select(Date, Medio, Province) %>%
      group_by(Date) %>%
      mutate(
        Date = as.Date(Date),
        Temp.delta = max(Medio) - min(Medio),
        Medio = mean(Medio),
        Temp.delta16 = Medio - 16
      ) %>% distinct()
    
    colnames(Lecco_temp)[2] <- "Temp.medio"
    
    Date_max <- max(Lecco_temp$Date)
    res =  httr::GET(paste0("https://www.dati.lombardia.it/resource/647i-nhxk.json?idsensore=",sensors_temp$Sensore))
    data = jsonlite::fromJSON(rawToChar(res$content))
    data$Date <- as.Date(data$data)
    data$Province <- rep("Lecco", nrow(data))
    data$valore <- as.numeric(data$valore)
    data <- data %>% filter(Date >Date_max)
    if(nrow(data)!= 0){
      
      data <- data %>%
        filter(valore != -999) %>%
        dplyr::select(Date, valore, Province) %>%
        group_by(Date) %>%
        mutate(
          Temp.delta = max(valore) - min(valore),
          valore = mean(valore),
          Temp.delta16 = valore - 16
        ) %>% distinct()
      
      colnames(data)[2] <- "Temp.medio"
      Lecco_temp <- rbind(Lecco_temp, data)
    }
    
    #Lodi
    sensors_temp <- sensors %>% filter(Tipo == "Temperatura" & Provincia == "Lodi")
    Lodi_temp <- read.table(file = paste0(path, "ARPA/", filenames[grepl(sensors_temp$Sensore, filenames)])) %>% distinct()
    colnames(Lodi_temp) <- c("Sensor", "Date", "Hour", "Medio")
    Lodi_temp$Province <- rep("Lodi", nrow(Lodi_temp))
    
    Lodi_temp <- Lodi_temp %>%
      filter(Medio != -999) %>%
      dplyr::select(Date, Medio, Province) %>%
      group_by(Date) %>%
      mutate(
        Date = as.Date(Date),
        Temp.delta = max(Medio) - min(Medio),
        Medio = mean(Medio),
        Temp.delta16 = Medio - 16
      ) %>% distinct()
    
    colnames(Lodi_temp)[2] <- "Temp.medio"
    
    Date_max <- max(Lodi_temp$Date)
    res =  httr::GET(paste0("https://www.dati.lombardia.it/resource/647i-nhxk.json?idsensore=",sensors_temp$Sensore))
    data = jsonlite::fromJSON(rawToChar(res$content))
    data$Date <- as.Date(data$data)
    data$Province <- rep("Lodi", nrow(data))
    data$valore <- as.numeric(data$valore)
    data <- data %>% filter(Date >Date_max)
    if(nrow(data)!= 0){
      
      data <- data %>%
        filter(valore != -999) %>%
        dplyr::select(Date, valore, Province) %>%
        group_by(Date) %>%
        mutate(
          Temp.delta = max(valore) - min(valore),
          valore = mean(valore),
          Temp.delta16 = valore - 16
        ) %>% distinct()
      
      colnames(data)[2] <- "Temp.medio"
      Lodi_temp <- rbind(Lodi_temp, data)
    }
    
    #Mantova
    sensors_temp <- sensors %>% filter(Tipo == "Temperatura" & Provincia == "Mantova")
    Mantova_temp <- read.table(file = paste0(path, "ARPA/", filenames[grepl(sensors_temp$Sensore, filenames)])) %>% distinct()
    colnames(Mantova_temp) <- c("Sensor", "Date", "Hour", "Medio")
    Mantova_temp$Province <- rep("Mantova", nrow(Mantova_temp))
    
    Mantova_temp <- Mantova_temp %>%
      filter(Medio != -999) %>%
      dplyr::select(Date, Medio, Province) %>%
      group_by(Date) %>%
      mutate(
        Date = as.Date(Date),
        Temp.delta = max(Medio) - min(Medio),
        Medio = mean(Medio),
        Temp.delta16 = Medio - 16
      ) %>% distinct()
    
    colnames(Mantova_temp)[2] <- "Temp.medio"
    
    Date_max <- max(Mantova_temp$Date)
    res =  httr::GET(paste0("https://www.dati.lombardia.it/resource/647i-nhxk.json?idsensore=",sensors_temp$Sensore))
    data = jsonlite::fromJSON(rawToChar(res$content))
    data$Date <- as.Date(data$data)
    data$Province <- rep("Mantova", nrow(data))
    data$valore <- as.numeric(data$valore)
    data <- data %>% filter(Date >Date_max)
    if(nrow(data)!= 0){
      
      data <- data %>%
        filter(valore != -999) %>%
        dplyr::select(Date, valore, Province) %>%
        group_by(Date) %>%
        mutate(
          Temp.delta = max(valore) - min(valore),
          valore = mean(valore),
          Temp.delta16 = valore - 16
        ) %>% distinct()
      
      colnames(data)[2] <- "Temp.medio"
      Mantova_temp <- rbind(Mantova_temp, data)
    }
    
    
    #Milano
    sensors_temp <- sensors %>% filter(Tipo == "Temperatura" & Provincia == "Milano")
    Milano_temp <- read.table(file = paste0(path, "ARPA/", filenames[grepl(sensors_temp$Sensore, filenames)])) %>% distinct()
    colnames(Milano_temp) <- c("Sensor", "Date", "Hour", "Medio")
    Milano_temp$Province <- rep("Milano", nrow(Milano_temp))
    
    Milano_temp <- Milano_temp %>%
      filter(Medio != -999) %>%
      dplyr::select(Date, Medio, Province) %>%
      group_by(Date) %>%
      mutate(
        Date = as.Date(Date),
        Temp.delta = max(Medio) - min(Medio),
        Medio = mean(Medio),
        Temp.delta16 = Medio - 16
      ) %>% distinct()
    
    colnames(Milano_temp)[2] <- "Temp.medio"
    
    Date_max <- max(Milano_temp$Date)
    res =  httr::GET(paste0("https://www.dati.lombardia.it/resource/647i-nhxk.json?idsensore=",sensors_temp$Sensore))
    data = jsonlite::fromJSON(rawToChar(res$content))
    data$Date <- as.Date(data$data)
    data$Province <- rep("Milano", nrow(data))
    data$valore <- as.numeric(data$valore)
    data <- data %>% filter(Date >Date_max)
    if(nrow(data)!= 0){
      
      data <- data %>%
        filter(valore != -999) %>%
        dplyr::select(Date, valore, Province) %>%
        group_by(Date) %>%
        mutate(
          Temp.delta = max(valore) - min(valore),
          valore = mean(valore),
          Temp.delta16 = valore - 16
        ) %>% distinct()
      
      colnames(data)[2] <- "Temp.medio"
      Milano_temp <- rbind(Milano_temp, data)
    }
    
    #Monza
    sensors_temp <- sensors %>% filter(Tipo == "Temperatura" & Provincia == "Monza")
    Monza_temp <- read.table(file = paste0(path, "ARPA/", filenames[grepl(sensors_temp$Sensore, filenames)])) %>% distinct()
    colnames(Monza_temp) <- c("Sensor", "Date", "Hour", "Medio")
    Monza_temp$Province <- rep("Monza", nrow(Monza_temp))
    
    Monza_temp <- Monza_temp %>%
      filter(Medio != -999) %>%
      dplyr::select(Date, Medio, Province) %>%
      group_by(Date) %>%
      mutate(
        Date = as.Date(Date),
        Temp.delta = max(Medio) - min(Medio),
        Medio = mean(Medio),
        Temp.delta16 = Medio - 16
      ) %>% distinct()
    
    colnames(Monza_temp)[2] <- "Temp.medio"
    
    Date_max <- max(Monza_temp$Date)
    res =  httr::GET(paste0("https://www.dati.lombardia.it/resource/647i-nhxk.json?idsensore=",sensors_temp$Sensore))
    data = jsonlite::fromJSON(rawToChar(res$content))
    data$Date <- as.Date(data$data)
    data$Province <- rep("Monza", nrow(data))
    data$valore <- as.numeric(data$valore)
    data <- data %>% filter(Date >Date_max)
    if(nrow(data)!= 0){
      
      data <- data %>%
        filter(valore != -999) %>%
        dplyr::select(Date, valore, Province) %>%
        group_by(Date) %>%
        mutate(
          Temp.delta = max(valore) - min(valore),
          valore = mean(valore),
          Temp.delta16 = valore - 16
        ) %>% distinct()
      
      colnames(data)[2] <- "Temp.medio"
      Monza_temp <- rbind(Monza_temp, data)
    }
    
    #Pavia
    sensors_temp <- sensors %>% filter(Tipo == "Temperatura" & Provincia == "Pavia")
    Pavia_temp <- read.table(file = paste0(path, "ARPA/", filenames[grepl(sensors_temp$Sensore, filenames)])) %>% distinct()
    colnames(Pavia_temp) <- c("Sensor", "Date", "Hour", "Medio")
    Pavia_temp$Province <- rep("Pavia", nrow(Pavia_temp))
    
    Pavia_temp <- Pavia_temp %>%
      filter(Medio != -999) %>%
      dplyr::select(Date, Medio, Province) %>%
      group_by(Date) %>%
      mutate(
        Date = as.Date(Date),
        Temp.delta = max(Medio) - min(Medio),
        Medio = mean(Medio),
        Temp.delta16 = Medio - 16
      ) %>% distinct()
    
    colnames(Pavia_temp)[2] <- "Temp.medio"
    
    Date_max <- max(Pavia_temp$Date)
    res =  httr::GET(paste0("https://www.dati.lombardia.it/resource/647i-nhxk.json?idsensore=",sensors_temp$Sensore))
    data = jsonlite::fromJSON(rawToChar(res$content))
    data$Date <- as.Date(data$data)
    data$Province <- rep("Pavia", nrow(data))
    data$valore <- as.numeric(data$valore)
    data <- data %>% filter(Date >Date_max)
    if(nrow(data)!= 0){
      
      data <- data %>%
        filter(valore != -999) %>%
        dplyr::select(Date, valore, Province) %>%
        group_by(Date) %>%
        mutate(
          Temp.delta = max(valore) - min(valore),
          valore = mean(valore),
          Temp.delta16 = valore - 16
        ) %>% distinct()
      
      colnames(data)[2] <- "Temp.medio"
      Pavia_temp <- rbind(Pavia_temp, data)
    }
    
    #Sondrio
    sensors_temp <- sensors %>% filter(Tipo == "Temperatura" & Provincia == "Sondrio")
    Sondrio_temp <- read.table(file = paste0(path, "ARPA/", filenames[grepl(sensors_temp$Sensore, filenames)])) %>% distinct()
    colnames(Sondrio_temp) <- c("Sensor", "Date", "Hour", "Medio")
    Sondrio_temp$Province <- rep("Sondrio", nrow(Sondrio_temp))
    
    Sondrio_temp <- Sondrio_temp %>%
      filter(Medio != -999) %>%
      dplyr::select(Date, Medio, Province) %>%
      group_by(Date) %>%
      mutate(
        Date = as.Date(Date),
        Temp.delta = max(Medio) - min(Medio),
        Medio = mean(Medio),
        Temp.delta16 = Medio - 16
      ) %>% distinct()
    
    colnames(Sondrio_temp)[2] <- "Temp.medio"
    
    Date_max <- max(Sondrio_temp$Date)
    res =  httr::GET(paste0("https://www.dati.lombardia.it/resource/647i-nhxk.json?idsensore=",sensors_temp$Sensore))
    data = jsonlite::fromJSON(rawToChar(res$content))
    data$Date <- as.Date(data$data)
    data$Province <- rep("Sondrio", nrow(data))
    data$valore <- as.numeric(data$valore)
    data <- data %>% filter(Date >Date_max)
    if(nrow(data)!= 0){
      
      data <- data %>%
        filter(valore != -999) %>%
        dplyr::select(Date, valore, Province) %>%
        group_by(Date) %>%
        mutate(
          Temp.delta = max(valore) - min(valore),
          valore = mean(valore),
          Temp.delta16 = valore - 16
        ) %>% distinct()
      
      colnames(data)[2] <- "Temp.medio"
      Sondrio_temp <- rbind(Sondrio_temp, data)
    }
    
    #Varese
    sensors_temp <- sensors %>% filter(Tipo == "Temperatura" & Provincia == "Varese")
    Varese_temp <- read.table(file = paste0(path, "ARPA/", filenames[grepl(sensors_temp$Sensore, filenames)])) %>% distinct()
    colnames(Varese_temp) <- c("Sensor", "Date", "Hour", "Medio")
    Varese_temp$Province <- rep("Varese", nrow(Varese_temp))
    
    Varese_temp <- Varese_temp %>%
      filter(Medio != -999) %>%
      dplyr::select(Date, Medio, Province) %>%
      group_by(Date) %>%
      mutate(
        Date = as.Date(Date),
        Temp.delta = max(Medio) - min(Medio),
        Medio = mean(Medio),
        Temp.delta16 = Medio - 16
      ) %>% distinct()
    
    colnames(Varese_temp)[2] <- "Temp.medio"
    
    Date_max <- max(Varese_temp$Date)
    res =  httr::GET(paste0("https://www.dati.lombardia.it/resource/647i-nhxk.json?idsensore=",sensors_temp$Sensore))
    data = jsonlite::fromJSON(rawToChar(res$content))
    data$Date <- as.Date(data$data)
    data$Province <- rep("Varese", nrow(data))
    data$valore <- as.numeric(data$valore)
    data <- data %>% filter(Date >Date_max)
    if(nrow(data)!= 0){
      
      data <- data %>%
        filter(valore != -999) %>%
        dplyr::select(Date, valore, Province) %>%
        group_by(Date) %>%
        mutate(
          Temp.delta = max(valore) - min(valore),
          valore = mean(valore),
          Temp.delta16 = valore - 16
        ) %>% distinct()
      
      colnames(data)[2] <- "Temp.medio"
      Varese_temp <- rbind(Varese_temp, data)
    }
    
    Temp_db <- rbind(Bergamo_temp,
                     Brescia_temp,
                     Como_temp,
                     Cremona_temp,
                     Lecco_temp,
                     Lodi_temp,
                     Mantova_temp,
                     Milano_temp,
                     Monza_temp,
                     Pavia_temp,
                     Sondrio_temp,
                     Varese_temp)
    
    
    
    tab <- table(calls$AAT, calls$SOREU)
    tab <- addmargins(tab)
    propA <- tab[,1]/tab[13,1] 
    propL <- tab[,2]/tab[13,2] 
    propM <- tab[,3]/tab[13,3] 
    propP <- tab[,4]/tab[13,4] 
    
    propA <-as.data.frame(propA) %>% filter(propA !=0 & propA!=1)
    propL <-as.data.frame(propL) %>% filter(propL !=0 & propL!=1)
    propM <-as.data.frame(propM) %>% filter(propM !=0 & propM!=1)
    propP <-as.data.frame(propP) %>% filter(propP !=0 & propP!=1)
    colnames(propA) <- "Prop"
    colnames(propL) <- "Prop"
    colnames(propM) <- "Prop"
    colnames(propP) <- "Prop"
    
    prop_tb <- rbind(propA , propL, propM, propP)
    prop_tb$Province <- rownames(prop_tb)
    
    prop_tb$Soreu <- ifelse(prop_tb$Province %in% c("Monza", "Milano"),"METROP",
                            ifelse(prop_tb$Province %in%  c("Pavia",
                                                            "Lodi",
                                                            "Cremona",
                                                            "Mantova"),"PIANURA",
                                   ifelse(prop_tb$Province %in% c("Bergamo",
                                                                  "Brescia",
                                                                  "Sondrio"), "ALPINA", "LAGHI")))
    rownames(prop_tb) <- NULL
    
    Temp_db <- left_join(Temp_db, prop_tb, by = c("Province"))
    
    Temp_db <- Temp_db %>%
      dplyr::select(-Province) %>%
      group_by(Date, Soreu) %>%
      mutate(Temp.medio = weighted.mean(Temp.medio, Prop),
             Temp.delta = weighted.mean(Temp.delta, Prop),
             Temp.delta16 = weighted.mean(Temp.delta16, Prop)) %>%
      dplyr::select(-Prop) %>% distinct()
    
    
    DB <- left_join(DB, Temp_db, by = c("Date", "Soreu"))
    
    ########################################Precipitazione########################################
    
    #Bergamo
    sensors_prep <- sensors %>% filter(Tipo == "Precipitazione" & Provincia == "Bergamo")
    Bergamo_prep <- read.table(file = paste0(path, "ARPA/", filenames[grepl(sensors_prep$Sensore, filenames)])) %>% distinct()
    colnames(Bergamo_prep) <- c("Sensor", "Date", "Hour", "Medio")
    Bergamo_prep$Province <- rep("Bergamo", nrow(Bergamo_prep))
    
    Bergamo_prep <- Bergamo_prep %>%
      filter(Medio != -999) %>%
      dplyr::select(Date, Medio, Province) %>%
      group_by(Date) %>%
      mutate(
        Date = as.Date(Date),
        Prep.delta = max(Medio) - min(Medio),
        Medio = mean(Medio)
      ) %>% distinct()
    
    colnames(Bergamo_prep)[2] <- "Prep.medio"
    
    Date_max <- max(Bergamo_prep$Date)
    res =  httr::GET(paste0("https://www.dati.lombardia.it/resource/647i-nhxk.json?idsensore=",sensors_prep$Sensore))
    data = jsonlite::fromJSON(rawToChar(res$content))
    data$Date <- as.Date(data$data)
    data$Province <- rep("Bergamo", nrow(data))
    data$valore <- as.numeric(data$valore)
    data <- data %>% filter(Date >Date_max)
    if(nrow(data)!= 0){
      
      data <- data %>%
        filter(valore != -999) %>%
        dplyr::select(Date, valore, Province) %>%
        group_by(Date) %>%
        mutate(
          Prep.delta = max(valore) - min(valore),
          valore = mean(valore)
        ) %>% distinct()
      
      colnames(data)[2] <- "Prep.medio"
      Bergamo_prep <- rbind(Bergamo_prep, data)
    }
    
    
    #Brescia
    sensors_prep <- sensors %>% filter(Tipo == "Precipitazione" & Provincia == "Brescia")
    Brescia_prep <- read.table(file = paste0(path, "ARPA/", filenames[grepl(sensors_prep$Sensore, filenames)])) %>% distinct()
    colnames(Brescia_prep) <- c("Sensor", "Date", "Hour", "Medio")
    Brescia_prep$Province <- rep("Brescia", nrow(Brescia_prep))
    
    Brescia_prep <- Brescia_prep %>%
      filter(Medio != -999) %>%
      dplyr::select(Date, Medio, Province) %>%
      group_by(Date) %>%
      mutate(
        Date = as.Date(Date),
        Prep.delta = max(Medio) - min(Medio),
        Medio = mean(Medio)
      ) %>% distinct()
    
    colnames(Brescia_prep)[2] <- "Prep.medio"
    
    Date_max <- max(Brescia_prep$Date)
    res =  httr::GET(paste0("https://www.dati.lombardia.it/resource/647i-nhxk.json?idsensore=",sensors_prep$Sensore))
    data = jsonlite::fromJSON(rawToChar(res$content))
    data$Date <- as.Date(data$data)
    data$Province <- rep("Brescia", nrow(data))
    data$valore <- as.numeric(data$valore)
    data <- data %>% filter(Date >Date_max)
    if(nrow(data)!= 0){
      
      data <- data %>%
        filter(valore != -999) %>%
        dplyr::select(Date, valore, Province) %>%
        group_by(Date) %>%
        mutate(
          Prep.delta = max(valore) - min(valore),
          valore = mean(valore)
        ) %>% distinct()
      
      colnames(data)[2] <- "Prep.medio"
      Brescia_prep <- rbind(Brescia_prep, data)
    }
    
    #Como
    sensors_prep <- sensors %>% filter(Tipo == "Precipitazione" & Provincia == "Como")
    Como_prep <- read.table(file = paste0(path, "ARPA/", filenames[grepl(sensors_prep$Sensore, filenames)])) %>% distinct()
    colnames(Como_prep) <- c("Sensor", "Date", "Hour", "Medio")
    Como_prep$Province <- rep("Como", nrow(Como_prep))
    
    Como_prep <- Como_prep %>%
      filter(Medio != -999) %>%
      dplyr::select(Date, Medio, Province) %>%
      group_by(Date) %>%
      mutate(
        Date = as.Date(Date),
        Prep.delta = max(Medio) - min(Medio),
        Medio = mean(Medio)
      ) %>% distinct()
    
    colnames(Como_prep)[2] <- "Prep.medio"
    
    Date_max <- max(Como_prep$Date)
    res =  httr::GET(paste0("https://www.dati.lombardia.it/resource/647i-nhxk.json?idsensore=",sensors_prep$Sensore))
    data = jsonlite::fromJSON(rawToChar(res$content))
    data$Date <- as.Date(data$data)
    data$Province <- rep("Como", nrow(data))
    data$valore <- as.numeric(data$valore)
    data <- data %>% filter(Date >Date_max)
    if(nrow(data)!= 0){
      
      data <- data %>%
        filter(valore != -999) %>%
        dplyr::select(Date, valore, Province) %>%
        group_by(Date) %>%
        mutate(
          Prep.delta = max(valore) - min(valore),
          valore = mean(valore)
        ) %>% distinct()
      
      colnames(data)[2] <- "Prep.medio"
      Como_prep <- rbind(Como_prep, data)
    }
    #Cremona
    sensors_prep <- sensors %>% filter(Tipo == "Precipitazione" & Provincia == "Cremona")
    Cremona_prep <- read.table(file = paste0(path, "ARPA/", filenames[grepl(sensors_prep$Sensore, filenames)])) %>% distinct()
    colnames(Cremona_prep) <- c("Sensor", "Date", "Hour", "Medio")
    Cremona_prep$Province <- rep("Cremona", nrow(Cremona_prep))
    
    Cremona_prep <- Cremona_prep %>%
      filter(Medio != -999) %>%
      dplyr::select(Date, Medio, Province) %>%
      group_by(Date) %>%
      mutate(
        Date = as.Date(Date),
        Prep.delta = max(Medio) - min(Medio),
        Medio = mean(Medio)
      ) %>% distinct()
    
    colnames(Cremona_prep)[2] <- "Prep.medio"
    
    Date_max <- max(Cremona_prep$Date)
    res =  httr::GET(paste0("https://www.dati.lombardia.it/resource/647i-nhxk.json?idsensore=",sensors_prep$Sensore))
    data = jsonlite::fromJSON(rawToChar(res$content))
    data$Date <- as.Date(data$data)
    data$Province <- rep("Cremona", nrow(data))
    data$valore <- as.numeric(data$valore)
    data <- data %>% filter(Date >Date_max)
    if(nrow(data)!= 0){
      
      data <- data %>%
        filter(valore != -999) %>%
        dplyr::select(Date, valore, Province) %>%
        group_by(Date) %>%
        mutate(
          Prep.delta = max(valore) - min(valore),
          valore = mean(valore)
        ) %>% distinct()
      
      colnames(data)[2] <- "Prep.medio"
      Cremona_prep <- rbind(Cremona_prep, data)
    }
    
    #Lecco
    sensors_prep <- sensors %>% filter(Tipo == "Precipitazione" & Provincia == "Lecco")
    Lecco_prep <- read.table(file = paste0(path, "ARPA/", filenames[grepl(sensors_prep$Sensore, filenames)])) %>% distinct()
    colnames(Lecco_prep) <- c("Sensor", "Date", "Hour", "Medio")
    Lecco_prep$Province <- rep("Lecco", nrow(Lecco_prep))
    
    Lecco_prep <- Lecco_prep %>%
      filter(Medio != -999) %>%
      dplyr::select(Date, Medio, Province) %>%
      group_by(Date) %>%
      mutate(
        Date = as.Date(Date),
        Prep.delta = max(Medio) - min(Medio),
        Medio = mean(Medio)
      ) %>% distinct()
    
    colnames(Lecco_prep)[2] <- "Prep.medio"
    
    Date_max <- max(Lecco_prep$Date)
    res =  httr::GET(paste0("https://www.dati.lombardia.it/resource/647i-nhxk.json?idsensore=",sensors_prep$Sensore))
    data = jsonlite::fromJSON(rawToChar(res$content))
    data$Date <- as.Date(data$data)
    data$Province <- rep("Lecco", nrow(data))
    data$valore <- as.numeric(data$valore)
    data <- data %>% filter(Date >Date_max)
    if(nrow(data)!= 0){
      
      data <- data %>%
        filter(valore != -999) %>%
        dplyr::select(Date, valore, Province) %>%
        group_by(Date) %>%
        mutate(
          Prep.delta = max(valore) - min(valore),
          valore = mean(valore)
        ) %>% distinct()
      
      colnames(data)[2] <- "Prep.medio"
      Lecco_prep <- rbind(Lecco_prep, data)
    }
    
    #Lodi
    sensors_prep <- sensors %>% filter(Tipo == "Precipitazione" & Provincia == "Lodi")
    Lodi_prep <- read.table(file = paste0(path, "ARPA/", filenames[grepl(sensors_prep$Sensore, filenames)])) %>% distinct()
    colnames(Lodi_prep) <- c("Sensor", "Date", "Hour", "Medio")
    Lodi_prep$Province <- rep("Lodi", nrow(Lodi_prep))
    
    Lodi_prep <- Lodi_prep %>%
      filter(Medio != -999) %>%
      dplyr::select(Date, Medio, Province) %>%
      group_by(Date) %>%
      mutate(
        Date = as.Date(Date),
        Prep.delta = max(Medio) - min(Medio),
        Medio = mean(Medio)
      ) %>% distinct()
    
    colnames(Lodi_prep)[2] <- "Prep.medio"
    
    Date_max <- max(Lodi_prep$Date)
    res =  httr::GET(paste0("https://www.dati.lombardia.it/resource/647i-nhxk.json?idsensore=",sensors_prep$Sensore))
    data = jsonlite::fromJSON(rawToChar(res$content))
    data$Date <- as.Date(data$data)
    data$Province <- rep("Lodi", nrow(data))
    data$valore <- as.numeric(data$valore)
    data <- data %>% filter(Date >Date_max)
    if(nrow(data)!= 0){
      
      data <- data %>%
        filter(valore != -999) %>%
        dplyr::select(Date, valore, Province) %>%
        group_by(Date) %>%
        mutate(
          Prep.delta = max(valore) - min(valore),
          valore = mean(valore)
        ) %>% distinct()
      
      colnames(data)[2] <- "Prep.medio"
      Lodi_prep <- rbind(Lodi_prep, data)
    }
    
    #Mantova
    sensors_prep <- sensors %>% filter(Tipo == "Precipitazione" & Provincia == "Mantova")
    Mantova_prep <- read.table(file = paste0(path, "ARPA/", filenames[grepl(sensors_prep$Sensore, filenames)])) %>% distinct()
    colnames(Mantova_prep) <- c("Sensor", "Date", "Hour", "Medio")
    Mantova_prep$Province <- rep("Mantova", nrow(Mantova_prep))
    
    Mantova_prep <- Mantova_prep %>%
      filter(Medio != -999) %>%
      dplyr::select(Date, Medio, Province) %>%
      group_by(Date) %>%
      mutate(
        Date = as.Date(Date),
        Prep.delta = max(Medio) - min(Medio),
        Medio = mean(Medio)
      ) %>% distinct()
    
    colnames(Mantova_prep)[2] <- "Prep.medio"
    
    
    Date_max <- max(Mantova_prep$Date)
    res =  httr::GET(paste0("https://www.dati.lombardia.it/resource/647i-nhxk.json?idsensore=",sensors_prep$Sensore))
    data = jsonlite::fromJSON(rawToChar(res$content))
    data$Date <- as.Date(data$data)
    data$Province <- rep("Mantova", nrow(data))
    data$valore <- as.numeric(data$valore)
    data <- data %>% filter(Date >Date_max)
    if(nrow(data)!= 0){
      
      data <- data %>%
        filter(valore != -999) %>%
        dplyr::select(Date, valore, Province) %>%
        group_by(Date) %>%
        mutate(
          Prep.delta = max(valore) - min(valore),
          valore = mean(valore)
        ) %>% distinct()
      
      colnames(data)[2] <- "Prep.medio"
      Mantova_prep <- rbind(Mantova_prep, data)
    }
    
    #Milano
    sensors_prep <- sensors %>% filter(Tipo == "Precipitazione" & Provincia == "Milano")
    Milano_prep <- read.table(file = paste0(path, "ARPA/", filenames[grepl(sensors_prep$Sensore, filenames)])) %>% distinct()
    colnames(Milano_prep) <- c("Sensor", "Date", "Hour", "Medio")
    Milano_prep$Province <- rep("Milano", nrow(Milano_prep))
    
    Milano_prep <- Milano_prep %>%
      filter(Medio != -999) %>%
      dplyr::select(Date, Medio, Province) %>%
      group_by(Date) %>%
      mutate(
        Date = as.Date(Date),
        Prep.delta = max(Medio) - min(Medio),
        Medio = mean(Medio)
      ) %>% distinct()
    
    colnames(Milano_prep)[2] <- "Prep.medio"
    
    
    Date_max <- max(Milano_prep$Date)
    res =  httr::GET(paste0("https://www.dati.lombardia.it/resource/647i-nhxk.json?idsensore=",sensors_prep$Sensore))
    data = jsonlite::fromJSON(rawToChar(res$content))
    data$Date <- as.Date(data$data)
    data$Province <- rep("Milano", nrow(data))
    data$valore <- as.numeric(data$valore)
    data <- data %>% filter(Date >Date_max)
    if(nrow(data)!= 0){
      
      data <- data %>%
        filter(valore != -999) %>%
        dplyr::select(Date, valore, Province) %>%
        group_by(Date) %>%
        mutate(
          Prep.delta = max(valore) - min(valore),
          valore = mean(valore)
        ) %>% distinct()
      
      colnames(data)[2] <- "Prep.medio"
      Milano_prep <- rbind(Milano_prep, data)
    }
    
    #Monza
    sensors_prep <- sensors %>% filter(Tipo == "Precipitazione" & Provincia == "Monza")
    Monza_prep <- read.table(file = paste0(path, "ARPA/", filenames[grepl(sensors_prep$Sensore, filenames)])) %>% distinct()
    colnames(Monza_prep) <- c("Sensor", "Date", "Hour", "Medio")
    Monza_prep$Province <- rep("Monza", nrow(Monza_prep))
    
    Monza_prep <- Monza_prep %>%
      filter(Medio != -999) %>%
      dplyr::select(Date, Medio, Province) %>%
      group_by(Date) %>%
      mutate(
        Date = as.Date(Date),
        Prep.delta = max(Medio) - min(Medio),
        Medio = mean(Medio)
      ) %>% distinct()
    
    colnames(Monza_prep)[2] <- "Prep.medio"
    
    Date_max <- max(Monza_prep$Date)
    res =  httr::GET(paste0("https://www.dati.lombardia.it/resource/647i-nhxk.json?idsensore=",sensors_prep$Sensore))
    data = jsonlite::fromJSON(rawToChar(res$content))
    data$Date <- as.Date(data$data)
    data$Province <- rep("Monza", nrow(data))
    data$valore <- as.numeric(data$valore)
    data <- data %>% filter(Date >Date_max)
    if(nrow(data)!= 0){
      
      data <- data %>%
        filter(valore != -999) %>%
        dplyr::select(Date, valore, Province) %>%
        group_by(Date) %>%
        mutate(
          Prep.delta = max(valore) - min(valore),
          valore = mean(valore)
        ) %>% distinct()
      
      colnames(data)[2] <- "Prep.medio"
      Monza_prep <- rbind(Monza_prep, data)
    }
    
    #Pavia
    sensors_prep <- sensors %>% filter(Tipo == "Precipitazione" & Provincia == "Pavia")
    Pavia_prep <- read.table(file = paste0(path, "ARPA/", filenames[grepl(sensors_prep$Sensore, filenames)])) %>% distinct()
    colnames(Pavia_prep) <- c("Sensor", "Date", "Hour", "Medio")
    Pavia_prep$Province <- rep("Pavia", nrow(Pavia_prep))
    
    Pavia_prep <- Pavia_prep %>%
      filter(Medio != -999) %>%
      dplyr::select(Date, Medio, Province) %>%
      group_by(Date) %>%
      mutate(
        Date = as.Date(Date),
        Prep.delta = max(Medio) - min(Medio),
        Medio = mean(Medio)
      ) %>% distinct()
    
    colnames(Pavia_prep)[2] <- "Prep.medio"
    
    
    Date_max <- max(Pavia_prep$Date)
    res =  httr::GET(paste0("https://www.dati.lombardia.it/resource/647i-nhxk.json?idsensore=",sensors_prep$Sensore))
    data = jsonlite::fromJSON(rawToChar(res$content))
    data$Date <- as.Date(data$data)
    data$Province <- rep("Pavia", nrow(data))
    data$valore <- as.numeric(data$valore)
    data <- data %>% filter(Date >Date_max)
    if(nrow(data)!= 0){
      
      data <- data %>%
        filter(valore != -999) %>%
        dplyr::select(Date, valore, Province) %>%
        group_by(Date) %>%
        mutate(
          Prep.delta = max(valore) - min(valore),
          valore = mean(valore)
        ) %>% distinct()
      
      colnames(data)[2] <- "Prep.medio"
      Pavia_prep <- rbind(Pavia_prep, data)
    }
    
    #Sondrio
    sensors_prep <- sensors %>% filter(Tipo == "Precipitazione" & Provincia == "Sondrio")
    Sondrio_prep <- read.table(file = paste0(path, "ARPA/", filenames[grepl(sensors_prep$Sensore, filenames)])) %>% distinct()
    colnames(Sondrio_prep) <- c("Sensor", "Date", "Hour", "Medio")
    Sondrio_prep$Province <- rep("Sondrio", nrow(Sondrio_prep))
    
    Sondrio_prep <- Sondrio_prep %>%
      filter(Medio != -999) %>%
      dplyr::select(Date, Medio, Province) %>%
      group_by(Date) %>%
      mutate(
        Date = as.Date(Date),
        Prep.delta = max(Medio) - min(Medio),
        Medio = mean(Medio)
      ) %>% distinct()
    
    colnames(Sondrio_prep)[2] <- "Prep.medio"
    
    Date_max <- max(Sondrio_prep$Date)
    res =  httr::GET(paste0("https://www.dati.lombardia.it/resource/647i-nhxk.json?idsensore=",sensors_prep$Sensore))
    data = jsonlite::fromJSON(rawToChar(res$content))
    data$Date <- as.Date(data$data)
    data$Province <- rep("Sondrio", nrow(data))
    data$valore <- as.numeric(data$valore)
    data <- data %>% filter(Date >Date_max)
    if(nrow(data)!= 0){
      
      data <- data %>%
        filter(valore != -999) %>%
        dplyr::select(Date, valore, Province) %>%
        group_by(Date) %>%
        mutate(
          Prep.delta = max(valore) - min(valore),
          valore = mean(valore)
        ) %>% distinct()
      
      colnames(data)[2] <- "Prep.medio"
      Sondrio_prep <- rbind(Sondrio_prep, data)
    }
    
    #Varese
    sensors_prep <- sensors %>% filter(Tipo == "Precipitazione" & Provincia == "Varese")
    Varese_prep <- read.table(file = paste0(path, "ARPA/", filenames[grepl(sensors_prep$Sensore, filenames)])) %>% distinct()
    colnames(Varese_prep) <- c("Sensor", "Date", "Hour", "Medio")
    Varese_prep$Province <- rep("Varese", nrow(Varese_prep))
    
    Varese_prep <- Varese_prep %>%
      filter(Medio != -999) %>%
      dplyr::select(Date, Medio, Province) %>%
      group_by(Date) %>%
      mutate(
        Date = as.Date(Date),
        Prep.delta = max(Medio) - min(Medio),
        Medio = mean(Medio)
      ) %>% distinct()
    
    colnames(Varese_prep)[2] <- "Prep.medio"
    
    
    Date_max <- max(Varese_prep$Date)
    res =  httr::GET(paste0("https://www.dati.lombardia.it/resource/647i-nhxk.json?idsensore=",sensors_prep$Sensore))
    data = jsonlite::fromJSON(rawToChar(res$content))
    data$Date <- as.Date(data$data)
    data$Province <- rep("Varese", nrow(data))
    data$valore <- as.numeric(data$valore)
    data <- data %>% filter(Date >Date_max)
    if(nrow(data)!= 0){
      
      data <- data %>%
        filter(valore != -999) %>%
        dplyr::select(Date, valore, Province) %>%
        group_by(Date) %>%
        mutate(
          Prep.delta = max(valore) - min(valore),
          valore = mean(valore)
        ) %>% distinct()
      
      colnames(data)[2] <- "Prep.medio"
      Varese_prep <- rbind(Varese_prep, data)
    }
    
    Prep_db <- rbind(Bergamo_prep,
                     Brescia_prep,
                     Como_prep,
                     Cremona_prep,
                     Lecco_prep,
                     Lodi_prep,
                     Mantova_prep,
                     Milano_prep,
                     Monza_prep,
                     Pavia_prep,
                     Sondrio_prep,
                     Varese_prep)
    
    
    tab <- table(calls$AAT, calls$SOREU)
    tab <- addmargins(tab)
    propA <- tab[,1]/tab[13,1] 
    propL <- tab[,2]/tab[13,2] 
    propM <- tab[,3]/tab[13,3] 
    propP <- tab[,4]/tab[13,4] 
    
    propA <-as.data.frame(propA) %>% filter(propA !=0 & propA!=1)
    propL <-as.data.frame(propL) %>% filter(propL !=0 & propL!=1)
    propM <-as.data.frame(propM) %>% filter(propM !=0 & propM!=1)
    propP <-as.data.frame(propP) %>% filter(propP !=0 & propP!=1)
    colnames(propA) <- "Prop"
    colnames(propL) <- "Prop"
    colnames(propM) <- "Prop"
    colnames(propP) <- "Prop"
    
    prop_tb <- rbind(propA , propL, propM, propP)
    prop_tb$Province <- rownames(prop_tb)
    
    prop_tb$Soreu <- ifelse(prop_tb$Province %in% c("Monza", "Milano"),"METROP",
                            ifelse(prop_tb$Province %in%  c("Pavia",
                                                            "Lodi",
                                                            "Cremona",
                                                            "Mantova"),"PIANURA",
                                   ifelse(prop_tb$Province %in% c("Bergamo",
                                                                  "Brescia",
                                                                  "Sondrio"), "ALPINA", "LAGHI")))
    rownames(prop_tb) <- NULL
    
    Prep_db <- left_join(Prep_db, prop_tb, by = c("Province"))
    
    Prep_db <- Prep_db %>%
      dplyr::select(-Province) %>%
      group_by(Date, Soreu) %>%
      mutate(Prep.medio = weighted.mean(Prep.medio, Prop),
             Prep.delta = weighted.mean(Prep.delta, Prop)) %>%
      dplyr::select(-Prop) %>% distinct()
    
    
    DB <- left_join(DB, Prep_db, by = c("Date", "Soreu"))
    
    
    ########################################Neve########################################
    
    #Bergamo
    sensors_snow <- sensors %>% filter(Tipo == "Neve" & Provincia == "Bergamo")
    Bergamo_snow <- read.table(file = paste0(path, "ARPA/", filenames[grepl(sensors_snow$Sensore, filenames)])) %>% distinct()
    colnames(Bergamo_snow) <- c("Sensor", "Date", "Hour", "Medio")
    Bergamo_snow$Province <- rep("Brescia", nrow(Bergamo_snow))
    
    Bergamo_snow <- Bergamo_snow %>%
      filter(Medio != -999) %>%
      dplyr::select(Date, Medio, Province) %>%
      group_by(Date) %>%
      mutate(
        Date = as.Date(Date),
        Snow.delta = max(Medio) - min(Medio),
        Medio = mean(Medio)
      ) %>% distinct()
    
    colnames(Bergamo_snow)[2] <- "Snow.medio"
    
    Date_max <- max(Bergamo_snow$Date)
    res =  httr::GET(paste0("https://www.dati.lombardia.it/resource/647i-nhxk.json?idsensore=",sensors_snow$Sensore))
    data = jsonlite::fromJSON(rawToChar(res$content))
    data$Date <- as.Date(data$data)
    data$Province <- rep("Bergamo", nrow(data))
    data$valore <- as.numeric(data$valore)
    data <- data %>% filter(Date >Date_max)
    if(nrow(data)!= 0){
      
      data <- data %>%
        filter(valore != -999) %>%
        dplyr::select(Date, valore, Province) %>%
        group_by(Date) %>%
        mutate(
          Snow.delta = max(valore) - min(valore),
          valore = mean(valore)
        ) %>% distinct()
      
      colnames(data)[2] <- "Snow.medio"
      Bergamo_snow <- rbind(Bergamo_snow, data)
    }
    
    
    #Brescia
    sensors_snow <- sensors %>% filter(Tipo == "Neve" & Provincia == "Brescia")
    Brescia_snow <- read.table(file = paste0(path, "ARPA/", filenames[grepl(sensors_snow$Sensore, filenames)])) %>% distinct()
    colnames(Brescia_snow) <- c("Sensor", "Date", "Hour", "Medio")
    Brescia_snow$Province <- rep("Brescia", nrow(Brescia_snow))
    
    Brescia_snow <- Brescia_snow %>%
      filter(Medio != -999) %>%
      dplyr::select(Date, Medio, Province) %>%
      group_by(Date) %>%
      mutate(
        Date = as.Date(Date),
        Snow.delta = max(Medio) - min(Medio),
        Medio = mean(Medio)
      ) %>% distinct()
    
    colnames(Brescia_snow)[2] <- "Snow.medio"
    
    Date_max <- max(Brescia_snow$Date)
    res =  httr::GET(paste0("https://www.dati.lombardia.it/resource/647i-nhxk.json?idsensore=",sensors_snow$Sensore))
    data = jsonlite::fromJSON(rawToChar(res$content))
    data$Date <- as.Date(data$data)
    data$Province <- rep("Brescia", nrow(data))
    data$valore <- as.numeric(data$valore)
    data <- data %>% filter(Date >Date_max)
    if(nrow(data)!= 0){
      
      data <- data %>%
        filter(valore != -999) %>%
        dplyr::select(Date, valore, Province) %>%
        group_by(Date) %>%
        mutate(
          Snow.delta = max(valore) - min(valore),
          valore = mean(valore)
        ) %>% distinct()
      
      colnames(data)[2] <- "Snow.medio"
      Brescia_snow <- rbind(Brescia_snow, data)
    }
    
    #Como
    sensors_snow <- sensors %>% filter(Tipo == "Neve" & Provincia == "Como")
    Como_snow <- read.table(file = paste0(path, "ARPA/", filenames[grepl(sensors_snow$Sensore, filenames)])) %>% distinct()
    colnames(Como_snow) <- c("Sensor", "Date", "Hour", "Medio")
    Como_snow$Province <- rep("Como", nrow(Como_snow))
    
    Como_snow <- Como_snow %>%
      filter(Medio != -999) %>%
      dplyr::select(Date, Medio, Province) %>%
      group_by(Date) %>%
      mutate(
        Date = as.Date(Date),
        Snow.delta = max(Medio) - min(Medio),
        Medio = mean(Medio)
      ) %>% distinct()
    
    colnames(Como_snow)[2] <- "Snow.medio"
    
    Date_max <- max(Como_snow$Date)
    res =  httr::GET(paste0("https://www.dati.lombardia.it/resource/647i-nhxk.json?idsensore=",sensors_snow$Sensore))
    data = jsonlite::fromJSON(rawToChar(res$content))
    data$Date <- as.Date(data$data)
    data$Province <- rep("Como", nrow(data))
    data$valore <- as.numeric(data$valore)
    data <- data %>% filter(Date >Date_max)
    if(nrow(data)!= 0){
      
      data <- data %>%
        filter(valore != -999) %>%
        dplyr::select(Date, valore, Province) %>%
        group_by(Date) %>%
        mutate(
          Snow.delta = max(valore) - min(valore),
          valore = mean(valore)
        ) %>% distinct()
      
      colnames(data)[2] <- "Snow.medio"
      Como_snow <- rbind(Como_snow, data)
    }
    
    #Lecco
    sensors_snow <- sensors %>% filter(Tipo == "Neve" & Provincia == "Lecco")
    Lecco_snow <- read.table(file = paste0(path, "ARPA/", filenames[grepl(sensors_snow$Sensore, filenames)])) %>% distinct()
    colnames(Lecco_snow) <- c("Sensor", "Date", "Hour", "Medio")
    Lecco_snow$Province <- rep("Lecco", nrow(Lecco_snow))
    
    Lecco_snow <- Lecco_snow %>%
      filter(Medio != -999) %>%
      dplyr::select(Date, Medio, Province) %>%
      group_by(Date) %>%
      mutate(
        Date = as.Date(Date),
        
        Snow.delta = max(Medio) - min(Medio),
        Medio = mean(Medio)
      ) %>% distinct()
    
    colnames(Lecco_snow)[2] <- "Snow.medio"
    
    Date_max <- max(Lecco_snow$Date)
    res =  httr::GET(paste0("https://www.dati.lombardia.it/resource/647i-nhxk.json?idsensore=",sensors_snow$Sensore))
    data = jsonlite::fromJSON(rawToChar(res$content))
    data$Date <- as.Date(data$data)
    data$Province <- rep("Lecco", nrow(data))
    data$valore <- as.numeric(data$valore)
    data <- data %>% filter(Date >Date_max)
    if(nrow(data)!= 0){
      
      data <- data %>%
        filter(valore != -999) %>%
        dplyr::select(Date, valore, Province) %>%
        group_by(Date) %>%
        mutate(
          Snow.delta = max(valore) - min(valore),
          valore = mean(valore)
        ) %>% distinct()
      
      colnames(data)[2] <- "Snow.medio"
      Lecco_snow <- rbind(Lecco_snow, data)
    }
    
    #Sondrio
    sensors_snow <- sensors %>% filter(Tipo == "Neve" & Provincia == "Sondrio")
    Sondrio_snow <- read.table(file = paste0(path, "ARPA/", filenames[grepl(sensors_snow$Sensore, filenames)])) %>% distinct()
    colnames(Sondrio_snow) <- c("Sensor", "Date", "Hour", "Medio")
    Sondrio_snow$Province <- rep("Sondrio", nrow(Sondrio_snow))
    
    Sondrio_snow <- Sondrio_snow %>%
      filter(Medio != -999) %>%
      dplyr::select(Date, Medio, Province) %>%
      group_by(Date) %>%
      mutate(
        Date = as.Date(Date),
        
        Snow.delta = max(Medio) - min(Medio),
        Medio = mean(Medio)
      ) %>% distinct()
    
    colnames(Sondrio_snow)[2] <- "Snow.medio"
    
    Date_max <- max(Sondrio_snow$Date)
    res =  httr::GET(paste0("https://www.dati.lombardia.it/resource/647i-nhxk.json?idsensore=",sensors_snow$Sensore))
    data = jsonlite::fromJSON(rawToChar(res$content))
    data$Date <- as.Date(data$data)
    data$Province <- rep("Sondrio", nrow(data))
    data$valore <- as.numeric(data$valore)
    data <- data %>% filter(Date >Date_max)
    if(nrow(data)!= 0){
      
      data <- data %>%
        filter(valore != -999) %>%
        dplyr::select(Date, valore, Province) %>%
        group_by(Date) %>%
        mutate(
          Snow.delta = max(valore) - min(valore),
          valore = mean(valore)
        ) %>% distinct()
      
      colnames(data)[2] <- "Snow.medio"
      Sondrio_snow <- rbind(Sondrio_snow, data)
    }
    
    #Varese
    sensors_snow <- sensors %>% filter(Tipo == "Neve" & Provincia == "Varese")
    Varese_snow <- read.table(file = paste0(path, "ARPA/", filenames[grepl(sensors_snow$Sensore, filenames)])) %>% distinct()
    colnames(Varese_snow) <- c("Sensor", "Date", "Hour", "Medio")
    Varese_snow$Province <- rep("Varese", nrow(Varese_snow))
    
    Varese_snow <- Varese_snow %>%
      filter(Medio != -999) %>%
      dplyr::select(Date, Medio, Province) %>%
      group_by(Date) %>%
      mutate(
        Date = as.Date(Date),
        Snow.delta = max(Medio) - min(Medio),
        Medio = mean(Medio)
      ) %>% distinct()
    
    colnames(Varese_snow)[2] <- "Snow.medio"
    
    Date_max <- max(Varese_snow$Date)
    res =  httr::GET(paste0("https://www.dati.lombardia.it/resource/647i-nhxk.json?idsensore=",sensors_snow$Sensore))
    data = jsonlite::fromJSON(rawToChar(res$content))
    data$Date <- as.Date(data$data)
    data$Province <- rep("Varese", nrow(data))
    data$valore <- as.numeric(data$valore)
    data <- data %>% filter(Date >Date_max)
    if(nrow(data)!= 0){
      
      data <- data %>%
        filter(valore != -999) %>%
        dplyr::select(Date, valore, Province) %>%
        group_by(Date) %>%
        mutate(
          Snow.delta = max(valore) - min(valore),
          valore = mean(valore)
        ) %>% distinct()
      
      colnames(data)[2] <- "Snow.medio"
      Varese_snow <- rbind(Varese_snow, data)
    }
    
    
    Snow_db <- rbind(Bergamo_snow,
                     Brescia_snow,
                     Como_snow,
                     Lecco_snow,
                     Sondrio_snow,
                     Varese_snow)
    
    
    tab <- table(calls$AAT, calls$SOREU)
    tab <- addmargins(tab)
    propA <- tab[,1]/tab[13,1] 
    propL <- tab[,2]/tab[13,2] 
    propM <- tab[,3]/tab[13,3] 
    propP <- tab[,4]/tab[13,4] 
    
    propA <-as.data.frame(propA) %>% filter(propA !=0 & propA!=1)
    propL <-as.data.frame(propL) %>% filter(propL !=0 & propL!=1)
    propM <-as.data.frame(propM) %>% filter(propM !=0 & propM!=1)
    propP <-as.data.frame(propP) %>% filter(propP !=0 & propP!=1)
    colnames(propA) <- "Prop"
    colnames(propL) <- "Prop"
    colnames(propM) <- "Prop"
    colnames(propP) <- "Prop"
    
    prop_tb <- rbind(propA , propL, propM, propP)
    prop_tb$Province <- rownames(prop_tb)
    
    prop_tb$Soreu <- ifelse(prop_tb$Province %in% c("Monza", "Milano"),"METROP",
                            ifelse(prop_tb$Province %in%  c("Pavia",
                                                            "Lodi",
                                                            "Cremona",
                                                            "Mantova"),"PIANURA",
                                   ifelse(prop_tb$Province %in% c("Bergamo",
                                                                  "Brescia",
                                                                  "Sondrio"), "ALPINA", "LAGHI")))
    rownames(prop_tb) <- NULL
    
    Snow_db <- left_join(Snow_db, prop_tb, by = c("Province"))
    
    Snow_db <- Snow_db %>%
      dplyr::select(-Province) %>%
      group_by(Date, Soreu) %>%
      mutate(Snow.medio = weighted.mean(Snow.medio, Prop),
             Snow.delta = weighted.mean(Snow.delta, Prop)) %>%
      dplyr::select(-Prop) %>% distinct()
    
    
    DB <- left_join(DB, Snow_db, by = c("Date", "Soreu"))
    
    DB$Snow.medio[is.na(DB$Snow.medio)] <- 0
    DB$Snow.medio[is.na(DB$Snow.delta)] <- 0
    #Totali positivi
    
    Italy_provinces <- readr::read_csv("https://raw.githubusercontent.com/pcm-dpc/COVID-19/master/dati-province/dpc-covid19-ita-province.csv")
    
    
    Italy_provinces <- Italy_provinces %>%
      filter(denominazione_regione == "Lombardia") %>%
      mutate(denominazione_provincia = ifelse(denominazione_provincia == "Monza e della Brianza", "Monza", denominazione_provincia)) %>%
      filter(denominazione_provincia %notin% c("In fase di definizione/aggiornamento","Fuori Regione / Provincia Autonoma"))
    
    tot_casi <- Italy_provinces %>%
      dplyr::select(denominazione_provincia, data, totale_casi)
    
    colnames(tot_casi) <- c("Soreu", "Date", "Tot_casi")
    
    tot_casi$Soreu <- ifelse(tot_casi$Soreu %in% c("Monza", "Milano"),"METROP",
                             ifelse(tot_casi$Soreu %in%  c("Pavia",
                                                           "Lodi",
                                                           "Cremona",
                                                           "Mantova"),"PIANURA",
                                    ifelse(tot_casi$Soreu %in% c("Bergamo",
                                                                 "Brescia",
                                                                 "Sondrio"), "ALPINA", "LAGHI")))
    
    
    tot_casi$Date <- as.Date(ymd_hms(tot_casi$Date))
    
    tot_casi <- tot_casi %>%
      group_by(Soreu, Date) %>%
      mutate(Tot_casi = sum(Tot_casi)) %>% distinct()
    
    tot_casi <- tot_casi %>%
      arrange(Tot_casi) %>%
      group_by(Soreu) %>%
      mutate(Tot_casiLag5 = dplyr::lag(Tot_casi, n = 5, default = NA, order_by = Date))
    
    DB$Date <- as.Date(DB$Date)
    DB <- left_join(DB, tot_casi, by=c("Soreu", "Date"))
    DB <- DB %>% mutate(Tot_casi = ifelse(is.na(Tot_casi), 0, Tot_casi))
    
    ###RT totale casi by province
    
    load(paste0(path, "Rt_TC_Province.RData"))
    df <- df[,c(1:2,5)]
    colnames(df)[2] <- "Rt_TC_Province"
    
    df_db <- left_join(df, prop_tb, by = c("Province"))
    
    df_db <- df_db %>%
      dplyr::select(-Province) %>%
      group_by(Date, Soreu) %>%
      mutate(Rt_TC.medio = weighted.mean(Rt_TC_Province, Prop)) %>%
      dplyr::select(-c(Prop, Rt_TC_Province)) %>% distinct()
    
    
    DB <- left_join(DB, df_db, by = c("Date", "Soreu"))
    
    
  }else{
    
    #######################################Province######################################
    
    if(variable == "calls"){
      
      call_counts <- table(calls$SOREU, calls$DT_EMERG_DAY, calls$AAT)
      dimensions_cc <- dim(call_counts)
      DB <- as.data.frame(call_counts)
      names(DB) <- c("Soreu", "Date", "Province", "calls")
      DB <- DB %>% filter(calls > 0)
      rm(call_counts)
      
      #    levels_reason <- unique(calls$DS_REASON)
      
      #   for(i in 1:length(levels_reason)){
      
      #      filter_calls <- calls %>% filter(DS_REASON == levels_reason[i])
      #      reason <- table(filter_calls$SOREU, filter_calls$DT_EMERG_DAY, filter_calls$AAT)
      #      dimensions_cc <- dim(reason)
      #      reason <- as.data.frame(reason)
      #      names(reason) <- c("Soreu", "Date", "Province", levels_reason[i])
      #      DB <- left_join(DB, reason, by = c("Soreu", "Province", "Date"))
      #    }
      
    }else{
      
      events <- calls %>% filter(FL_MISSION_EXIST == "S")
      events_counts <- table(events$DT_EMERG_DAY, events$AAT)
      dimensions_cc <- dim(events_counts)
      DB <- as.data.frame(events_counts)
      names(DB) <- c("Date", "Province", "events")
      DB <- DB %>% filter(events > 0)
      rm(events_counts)
      
      #  levels_reason <- unique(events$DS_REASON)
      
      #  for(i in 1:length(levels_reason)){
      
      #    filter_events <- events %>% filter(DS_REASON == levels_reason[i])
      #    reason <- table(filter_events$SOREU, filter_events$DT_EMERG_DAY, 
      #                    filter_events$AAT)
      #   dimensions_cc <- dim(reason)
      #    reason <- as.data.frame(reason)
      #   names(reason) <- c("Soreu", "Date", "Province", levels_reason[i])
      #   DB <- left_join(DB, reason, by = c("Soreu", "Province", "Date"))
      #  }
      
    }
    
    ## CREATE SOME NEW VARIABLES FOR TIME 
    DB$Time <- as.numeric(as.Date(DB$Date)) - as.numeric(as.Date(DB$Date))[1] +  1
    DB$Day <- factor(weekdays(as.Date(DB$Date)),levels = c("Monday","Tuesday","Wednesday", "Thursday", "Friday","Saturday","Sunday"))
    DB$Day_num <- as.numeric(wday(as.Date(DB$Date)))
    
    DB <- DB %>% 
      mutate(Week = lubridate::week(ymd(Date))) %>%
      mutate(Month = lubridate::month(ymd(Date)),
             Quarter = lubridate::quarter(ymd(Date)),
             Day_num = lubridate::day(ymd(Date)),
             Month_Day = as.factor(paste0(Month,"-", Day_num)),
             Year = lubridate::year(ymd(DB$Date)),
             Date = as.Date(Date),
             Day_year = lubridate::yday(ymd(Date)),
             Day_month = lubridate::mday(ymd(Date)),
             Day_week = lubridate::wday(ymd(Date)),
             Day_quarter = lubridate::qday(ymd(Date)))
    
    DB <- DB %>% 
      mutate(School = ifelse(Month %in% c(9:12,1:6) & Day %notin% c("Saturday","Sunday"), 
                             "School", "NoSchool")) %>%
      mutate(Snow = ifelse(Month %in% c(11:12,1:3), "Winter", "NoWinter"),
             festivoConFeste = ifelse((Month_Day %in% c("1-6", "4-25", "5-1", "6-2",
                                                        "8-15", "11-1", "12-8", "12-15", "12-16")) |
                                        ( Date %in% c("2019-04-12", "2019-04-13",
                                                      "2018-04-01", "2018-04-02",
                                                      "2017-04-16", "2017-04-17",
                                                      "2016-03-27", "2016-03-28",
                                                      "2015-04-05", "2015-04-06",
                                                      "2019-04-26", "2019-08-16",
                                                      "2019-12-27", "2016-06-03",
                                                      "2016-10-31", "2016-12-09",
                                                      "2015-01-05", "2015-06-01",
                                                      "2015-12-07", "2015-12-31",
                                                      "2017-04-24", "2017-08-14",
                                                      "2018-04-30", "2018-11-02",
                                                      "2018-12-31", "2018-04-26",
                                                      "2018-12-27")) |
                                        (Day == "Sunday"), "Festivo", "NoFestivo"),
             festivoSenzaFeste = ifelse(Day == "Sunday", "Sunday", "NoSunday"),
             d_agosto = ifelse((Date > "2019-07-19" & Date < "2019-09-20") |
                                 (Date > "2018-07-19" & Date < "2018-09-20") |
                                 (Date > "2017-07-19" & Date < "2017-09-20") |
                                 (Date > "2016-07-19" & Date < "2016-09-20") |
                                 (Date > "2015-07-19" & Date < "2015-09-20"), "Ferie", "NoFerie"),
             Eventi_piu = ifelse((Month_Day %in% c("1-1")), "Evento", "NoEvento"))
    
    
    DB$Week <- ifelse(DB$Week == max(DB$Week), 
                      52, DB$Week)
    
    #Incidenti stradali
    
    incid_2015 <- read.xlsx(paste0(path, "Incidenti_merged_2015_2019.xls"), 
                            sheetIndex = 1)
    
    incid_2015$Territorio <- trim(incid_2015$Territorio)
    
    Prov <- unique(incid_2015$Territorio)
    db2015 <- list()
    for(i in 2:length(Prov)){
      
      db2015[[i-1]] <- data.frame(Province = rep(Prov[i],12), 
                                  Incidenti = t(incid_2015[which(incid_2015$Territorio == Prov[i]),2:13]),
                                  Month = c(1:12),
                                  Year = rep(2015, 12))
      
    }
    
    db2015 <- rbindlist(db2015) 
    db2015 <- db2015%>%
      mutate(Incid = X2) %>%
      dplyr::select(Province, Month, Year, Incid)
    
    incid_2016 <- read.xlsx(paste0(path, "Incidenti_merged_2015_2019.xls"), sheetIndex = 2)
    
    incid_2016$Territorio <- trim(incid_2016$Territorio)
    
    Prov <- unique(incid_2016$Territorio)
    db2016 <- list()
    for(i in 2:length(Prov)){
      
      db2016[[i-1]] <- data.frame(Province = rep(Prov[i],12), 
                                  Incidenti = t(incid_2016[which(incid_2016$Territorio == Prov[i]),2:13]),
                                  Month = c(1:12),
                                  Year = rep(2016, 12))
      
    }
    
    db2016 <- rbindlist(db2016) 
    db2016 <- db2016%>%
      mutate(Incid = X2) %>%
      dplyr::select(Province, Month, Year, Incid)
    
    
    incid_2017 <- read.xlsx(paste0(path, "Incidenti_merged_2015_2019.xls"), sheetIndex = 3)
    
    incid_2017$Territorio <- trim(incid_2017$Territorio)
    
    Prov <- unique(incid_2017$Territorio)
    db2017 <- list()
    for(i in 2:length(Prov)){
      
      db2017[[i-1]] <- data.frame(Province = rep(Prov[i],12), 
                                  Incidenti = t(incid_2017[which(incid_2017$Territorio == Prov[i]),2:13]),
                                  Month = c(1:12),
                                  Year = rep(2017, 12))
      
    }
    
    db2017 <- rbindlist(db2017) 
    db2017 <- db2017%>%
      mutate(Incid = X2) %>%
      dplyr::select(Province, Month, Year, Incid)
    
    
    incid_2018 <- read.xlsx(paste0(path, "Incidenti_merged_2015_2019.xls"), sheetIndex = 4)
    
    incid_2018$Territorio <- trim(incid_2018$Territorio)
    
    Prov <- unique(incid_2018$Territorio)
    db2018 <- list()
    for(i in 2:length(Prov)){
      
      db2018[[i-1]] <- data.frame(Province = rep(Prov[i],12), 
                                  Incidenti = t(incid_2018[which(incid_2018$Territorio == Prov[i]),2:13]),
                                  Month = c(1:12),
                                  Year = rep(2018, 12))
      
    }
    
    db2018 <- rbindlist(db2018) 
    db2018 <- db2018%>%
      mutate(Incid = X2) %>%
      dplyr::select(Province, Month, Year, Incid)
    
    
    incid_2019 <- read.xlsx(paste0(path, "Incidenti_merged_2015_2019.xls"), sheetIndex = 5)
    
    incid_2019$Territorio <- trim(incid_2019$Territorio)
    
    Prov <- unique(incid_2019$Territorio)
    db2019 <- list()
    for(i in 2:length(Prov)){
      
      db2019[[i-1]] <- data.frame(Province = rep(Prov[i],12), 
                                  Incidenti = t(incid_2019[which(incid_2019$Territorio == Prov[i]),2:13]),
                                  Month = c(1:12),
                                  Year = rep(2019, 12))
      
    }
    
    db2019 <- rbindlist(db2019) 
    db2019 <- db2019%>%
      mutate(Incid = X2) %>%
      dplyr::select(Province, Month, Year, Incid)
    
    db_incid <- rbind(db2015, db2016, db2017,db2018,db2019)
    db_incid$Month <- as.numeric(db_incid$Month)
    
    
    DB <- left_join(DB, db_incid, by=c("Province", "Month", "Year")) 
    
    
    
    ####################################################################################
    ######################################WEATHER DATA##################################
    ####################################################################################
    
    #load list sensors used
    sensors <- readxl::read_xlsx(path = paste0(path, "ARPA/sensori.xlsx"))
    
    #load list files in ARPA folder
    filenames <- list.files(paste0(path,"ARPA"), pattern="*.txt", full.names=F)
    
    ###################################Temperature#######################################
    
    
    #Bergamo
    sensors_temp <- sensors %>% filter(Tipo == "Temperatura" & Provincia == "Bergamo")
    Bergamo_temp <- read.table(file = paste0(path, "ARPA/", filenames[grepl(sensors_temp$Sensore, filenames)])) %>% distinct()
    colnames(Bergamo_temp) <- c("Sensor", "Date", "Hour", "Medio")
    Bergamo_temp$Province <- rep("Bergamo", nrow(Bergamo_temp))
    
    Bergamo_temp <- Bergamo_temp %>%
      filter(Medio != -999) %>%
      dplyr::select(Date, Medio, Province) %>%
      group_by(Date) %>%
      mutate(
        Date = as.Date(Date),
        Temp.delta = max(Medio) - min(Medio),
        Medio = mean(Medio),
        Temp.delta16 = Medio - 16
      ) %>% distinct()
    
    colnames(Bergamo_temp)[2] <- "Temp.medio"
    
    Date_max <- max(Bergamo_temp$Date)
    res =  httr::GET(paste0("https://www.dati.lombardia.it/resource/647i-nhxk.json?idsensore=",sensors_temp$Sensore))
    data = jsonlite::fromJSON(rawToChar(res$content))
    data$Date <- as.Date(data$data)
    data$Province <- rep("Bergamo", nrow(data))
    data$valore <- as.numeric(data$valore)
    data <- data %>% filter(Date >Date_max)
    if(nrow(data)!= 0){
      
      data <- data %>%
        filter(valore != -999) %>%
        dplyr::select(Date, valore, Province) %>%
        group_by(Date) %>%
        mutate(
          Temp.delta = max(valore) - min(valore),
          valore = mean(valore),
          Temp.delta16 = valore - 16
        ) %>% distinct()
      
      colnames(data)[2] <- "Temp.medio"
      Bergamo_temp <- rbind(Bergamo_temp, data)
    }
    
    #Brescia
    sensors_temp <- sensors %>% filter(Tipo == "Temperatura" & Provincia == "Brescia")
    Brescia_temp <- read.table(file = paste0(path, "ARPA/", filenames[grepl(sensors_temp$Sensore, filenames)])) %>% distinct()
    colnames(Brescia_temp) <- c("Sensor", "Date", "Hour", "Medio")
    Brescia_temp$Province <- rep("Brescia", nrow(Brescia_temp))
    
    Brescia_temp <- Brescia_temp %>%
      filter(Medio != -999) %>%
      dplyr::select(Date, Medio, Province) %>%
      group_by(Date) %>%
      mutate(
        Date = as.Date(Date),
        Temp.delta = max(Medio) - min(Medio),
        Medio = mean(Medio),
        Temp.delta16 = Medio - 16
      ) %>% distinct()
    
    colnames(Brescia_temp)[2] <- "Temp.medio"
    
    Date_max <- max(Brescia_temp$Date)
    res =  httr::GET(paste0("https://www.dati.lombardia.it/resource/647i-nhxk.json?idsensore=",sensors_temp$Sensore))
    data = jsonlite::fromJSON(rawToChar(res$content))
    data$Date <- as.Date(data$data)
    data$Province <- rep("Brescia", nrow(data))
    data$valore <- as.numeric(data$valore)
    data <- data %>% filter(Date >Date_max)
    if(nrow(data)!= 0){
      
      data <- data %>%
        filter(valore != -999) %>%
        dplyr::select(Date, valore, Province) %>%
        group_by(Date) %>%
        mutate(
          Temp.delta = max(valore) - min(valore),
          valore = mean(valore),
          Temp.delta16 = valore - 16
        ) %>% distinct()
      
      colnames(data)[2] <- "Temp.medio"
      Brescia_temp <- rbind(Brescia_temp, data)
    }
    
    #Como
    sensors_temp <- sensors %>% filter(Tipo == "Temperatura" & Provincia == "Como")
    Como_temp <- read.table(file = paste0(path, "ARPA/", filenames[grepl(sensors_temp$Sensore, filenames)])) %>% distinct()
    colnames(Como_temp) <- c("Sensor", "Date", "Hour", "Medio")
    Como_temp$Province <- rep("Como", nrow(Como_temp))
    
    Como_temp <- Como_temp %>%
      filter(Medio != -999) %>%
      dplyr::select(Date, Medio, Province) %>%
      group_by(Date) %>%
      mutate(
        Date = as.Date(Date),
        
        Temp.delta = max(Medio) - min(Medio),
        Medio = mean(Medio),
        Temp.delta16 = Medio - 16
      ) %>% distinct()
    
    colnames(Como_temp)[2] <- "Temp.medio"
    
    Date_max <- max(Como_temp$Date)
    res =  httr::GET(paste0("https://www.dati.lombardia.it/resource/647i-nhxk.json?idsensore=",sensors_temp$Sensore))
    data = jsonlite::fromJSON(rawToChar(res$content))
    data$Date <- as.Date(data$data)
    data$Province <- rep("Como", nrow(data))
    data$valore <- as.numeric(data$valore)
    data <- data %>% filter(Date >Date_max)
    if(nrow(data)!= 0){
      
      data <- data %>%
        filter(valore != -999) %>%
        dplyr::select(Date, valore, Province) %>%
        group_by(Date) %>%
        mutate(
          Temp.delta = max(valore) - min(valore),
          valore = mean(valore),
          Temp.delta16 = valore - 16
        ) %>% distinct()
      
      colnames(data)[2] <- "Temp.medio"
      Como_temp <- rbind(Como_temp, data)
    }
    
    #Cremona
    sensors_temp <- sensors %>% filter(Tipo == "Temperatura" & Provincia == "Cremona")
    Cremona_temp <- read.table(file = paste0(path, "ARPA/", filenames[grepl(sensors_temp$Sensore, filenames)])) %>% distinct()
    colnames(Cremona_temp) <- c("Sensor", "Date", "Hour", "Medio")
    Cremona_temp$Province <- rep("Cremona", nrow(Cremona_temp))
    
    Cremona_temp <- Cremona_temp %>%
      filter(Medio != -999) %>%
      dplyr::select(Date, Medio, Province) %>%
      group_by(Date) %>%
      mutate(
        Date = as.Date(Date),
        
        Temp.delta = max(Medio) - min(Medio),
        Medio = mean(Medio),
        Temp.delta16 = Medio - 16
      ) %>% distinct()
    
    colnames(Cremona_temp)[2] <- "Temp.medio"
    
    Date_max <- max(Cremona_temp$Date)
    res =  httr::GET(paste0("https://www.dati.lombardia.it/resource/647i-nhxk.json?idsensore=",sensors_temp$Sensore))
    data = jsonlite::fromJSON(rawToChar(res$content))
    data$Date <- as.Date(data$data)
    data$Province <- rep("Cremona", nrow(data))
    data$valore <- as.numeric(data$valore)
    data <- data %>% filter(Date >Date_max)
    if(nrow(data)!= 0){
      
      data <- data %>%
        filter(valore != -999) %>%
        dplyr::select(Date, valore, Province) %>%
        group_by(Date) %>%
        mutate(
          Temp.delta = max(valore) - min(valore),
          valore = mean(valore),
          Temp.delta16 = valore - 16
        ) %>% distinct()
      
      colnames(data)[2] <- "Temp.medio"
      Cremona_temp <- rbind(Cremona_temp, data)
    }
    
    #Lecco
    sensors_temp <- sensors %>% filter(Tipo == "Temperatura" & Provincia == "Lecco")
    Lecco_temp <- read.table(file = paste0(path, "ARPA/", filenames[grepl(sensors_temp$Sensore, filenames)])) %>% distinct()
    colnames(Lecco_temp) <- c("Sensor", "Date", "Hour", "Medio")
    Lecco_temp$Province <- rep("Lecco", nrow(Lecco_temp))
    
    Lecco_temp <- Lecco_temp %>%
      filter(Medio != -999) %>%
      dplyr::select(Date, Medio, Province) %>%
      group_by(Date) %>%
      mutate(
        Date = as.Date(Date),
        
        Temp.delta = max(Medio) - min(Medio),
        Medio = mean(Medio),
        Temp.delta16 = Medio - 16
      ) %>% distinct()
    
    colnames(Lecco_temp)[2] <- "Temp.medio"
    
    Date_max <- max(Lecco_temp$Date)
    res =  httr::GET(paste0("https://www.dati.lombardia.it/resource/647i-nhxk.json?idsensore=",sensors_temp$Sensore))
    data = jsonlite::fromJSON(rawToChar(res$content))
    data$Date <- as.Date(data$data)
    data$Province <- rep("Lecco", nrow(data))
    data$valore <- as.numeric(data$valore)
    data <- data %>% filter(Date >Date_max)
    if(nrow(data)!= 0){
      
      data <- data %>%
        filter(valore != -999) %>%
        dplyr::select(Date, valore, Province) %>%
        group_by(Date) %>%
        mutate(
          Temp.delta = max(valore) - min(valore),
          valore = mean(valore),
          Temp.delta16 = valore - 16
        ) %>% distinct()
      
      colnames(data)[2] <- "Temp.medio"
      Lecco_temp <- rbind(Lecco_temp, data)
    }
    
    #Lodi
    sensors_temp <- sensors %>% filter(Tipo == "Temperatura" & Provincia == "Lodi")
    Lodi_temp <- read.table(file = paste0(path, "ARPA/", filenames[grepl(sensors_temp$Sensore, filenames)])) %>% distinct()
    colnames(Lodi_temp) <- c("Sensor", "Date", "Hour", "Medio")
    Lodi_temp$Province <- rep("Lodi", nrow(Lodi_temp))
    
    Lodi_temp <- Lodi_temp %>%
      filter(Medio != -999) %>%
      dplyr::select(Date, Medio, Province) %>%
      group_by(Date) %>%
      mutate(
        Date = as.Date(Date),
        
        Temp.delta = max(Medio) - min(Medio),
        Medio = mean(Medio),
        Temp.delta16 = Medio - 16
      ) %>% distinct()
    
    colnames(Lodi_temp)[2] <- "Temp.medio"
    
    Date_max <- max(Lodi_temp$Date)
    res =  httr::GET(paste0("https://www.dati.lombardia.it/resource/647i-nhxk.json?idsensore=",sensors_temp$Sensore))
    data = jsonlite::fromJSON(rawToChar(res$content))
    data$Date <- as.Date(data$data)
    data$Province <- rep("Lodi", nrow(data))
    data$valore <- as.numeric(data$valore)
    data <- data %>% filter(Date >Date_max)
    if(nrow(data)!= 0){
      
      data <- data %>%
        filter(valore != -999) %>%
        dplyr::select(Date, valore, Province) %>%
        group_by(Date) %>%
        mutate(
          Temp.delta = max(valore) - min(valore),
          valore = mean(valore),
          Temp.delta16 = valore - 16
        ) %>% distinct()
      
      colnames(data)[2] <- "Temp.medio"
      Lodi_temp <- rbind(Lodi_temp, data)
    }
    
    #Mantova
    sensors_temp <- sensors %>% filter(Tipo == "Temperatura" & Provincia == "Mantova")
    Mantova_temp <- read.table(file = paste0(path, "ARPA/", filenames[grepl(sensors_temp$Sensore, filenames)])) %>% distinct()
    colnames(Mantova_temp) <- c("Sensor", "Date", "Hour", "Medio")
    Mantova_temp$Province <- rep("Mantova", nrow(Mantova_temp))
    
    Mantova_temp <- Mantova_temp %>%
      filter(Medio != -999) %>%
      dplyr::select(Date, Medio, Province) %>%
      group_by(Date) %>%
      mutate(
        Date = as.Date(Date),
        
        Temp.delta = max(Medio) - min(Medio),
        Medio = mean(Medio),
        Temp.delta16 = Medio - 16
      ) %>% distinct()
    
    colnames(Mantova_temp)[2] <- "Temp.medio"
    
    Date_max <- max(Mantova_temp$Date)
    res =  httr::GET(paste0("https://www.dati.lombardia.it/resource/647i-nhxk.json?idsensore=",sensors_temp$Sensore))
    data = jsonlite::fromJSON(rawToChar(res$content))
    data$Date <- as.Date(data$data)
    data$Province <- rep("Mantova", nrow(data))
    data$valore <- as.numeric(data$valore)
    data <- data %>% filter(Date >Date_max)
    if(nrow(data)!= 0){
      
      data <- data %>%
        filter(valore != -999) %>%
        dplyr::select(Date, valore, Province) %>%
        group_by(Date) %>%
        mutate(
          Temp.delta = max(valore) - min(valore),
          valore = mean(valore),
          Temp.delta16 = valore - 16
        ) %>% distinct()
      
      colnames(data)[2] <- "Temp.medio"
      Mantova_temp <- rbind(Mantova_temp, data)
    }
    
    
    #Milano
    sensors_temp <- sensors %>% filter(Tipo == "Temperatura" & Provincia == "Milano")
    Milano_temp <- read.table(file = paste0(path, "ARPA/", filenames[grepl(sensors_temp$Sensore, filenames)])) %>% distinct()
    colnames(Milano_temp) <- c("Sensor", "Date", "Hour", "Medio")
    Milano_temp$Province <- rep("Milano", nrow(Milano_temp))
    
    Milano_temp <- Milano_temp %>%
      filter(Medio != -999) %>%
      dplyr::select(Date, Medio, Province) %>%
      group_by(Date) %>%
      mutate(
        Date = as.Date(Date),
        
        Temp.delta = max(Medio) - min(Medio),
        Medio = mean(Medio),
        Temp.delta16 = Medio - 16
      ) %>% distinct()
    
    colnames(Milano_temp)[2] <- "Temp.medio"
    
    Date_max <- max(Milano_temp$Date)
    res =  httr::GET(paste0("https://www.dati.lombardia.it/resource/647i-nhxk.json?idsensore=",sensors_temp$Sensore))
    data = jsonlite::fromJSON(rawToChar(res$content))
    data$Date <- as.Date(data$data)
    data$Province <- rep("Milano", nrow(data))
    data$valore <- as.numeric(data$valore)
    data <- data %>% filter(Date >Date_max)
    if(nrow(data)!= 0){
      
      data <- data %>%
        filter(valore != -999) %>%
        dplyr::select(Date, valore, Province) %>%
        group_by(Date) %>%
        mutate(
          Temp.delta = max(valore) - min(valore),
          valore = mean(valore),
          Temp.delta16 = valore - 16
        ) %>% distinct()
      
      colnames(data)[2] <- "Temp.medio"
      Milano_temp <- rbind(Milano_temp, data)
    }
    
    #Monza
    sensors_temp <- sensors %>% filter(Tipo == "Temperatura" & Provincia == "Monza")
    Monza_temp <- read.table(file = paste0(path, "ARPA/", filenames[grepl(sensors_temp$Sensore, filenames)])) %>% distinct()
    colnames(Monza_temp) <- c("Sensor", "Date", "Hour", "Medio")
    Monza_temp$Province <- rep("Monza", nrow(Monza_temp))
    
    Monza_temp <- Monza_temp %>%
      filter(Medio != -999) %>%
      dplyr::select(Date, Medio, Province) %>%
      group_by(Date) %>%
      mutate(
        Date = as.Date(Date),
        
        Temp.delta = max(Medio) - min(Medio),
        Medio = mean(Medio),
        Temp.delta16 = Medio - 16
      ) %>% distinct()
    
    colnames(Monza_temp)[2] <- "Temp.medio"
    
    Date_max <- max(Monza_temp$Date)
    res =  httr::GET(paste0("https://www.dati.lombardia.it/resource/647i-nhxk.json?idsensore=",sensors_temp$Sensore))
    data = jsonlite::fromJSON(rawToChar(res$content))
    data$Date <- as.Date(data$data)
    data$Province <- rep("Monza", nrow(data))
    data$valore <- as.numeric(data$valore)
    data <- data %>% filter(Date >Date_max)
    if(nrow(data)!= 0){
      
      data <- data %>%
        filter(valore != -999) %>%
        dplyr::select(Date, valore, Province) %>%
        group_by(Date) %>%
        mutate(
          Temp.delta = max(valore) - min(valore),
          valore = mean(valore),
          Temp.delta16 = valore - 16
        ) %>% distinct()
      
      colnames(data)[2] <- "Temp.medio"
      Monza_temp <- rbind(Monza_temp, data)
    }
    
    #Pavia
    sensors_temp <- sensors %>% filter(Tipo == "Temperatura" & Provincia == "Pavia")
    Pavia_temp <- read.table(file = paste0(path, "ARPA/", filenames[grepl(sensors_temp$Sensore, filenames)])) %>% distinct()
    colnames(Pavia_temp) <- c("Sensor", "Date", "Hour", "Medio")
    Pavia_temp$Province <- rep("Pavia", nrow(Pavia_temp))
    
    Pavia_temp <- Pavia_temp %>%
      filter(Medio != -999) %>%
      dplyr::select(Date, Medio, Province) %>%
      group_by(Date) %>%
      mutate(
        Date = as.Date(Date),
        
        Temp.delta = max(Medio) - min(Medio),
        Medio = mean(Medio),
        Temp.delta16 = Medio - 16
      ) %>% distinct()
    
    colnames(Pavia_temp)[2] <- "Temp.medio"
    
    Date_max <- max(Pavia_temp$Date)
    res =  httr::GET(paste0("https://www.dati.lombardia.it/resource/647i-nhxk.json?idsensore=",sensors_temp$Sensore))
    data = jsonlite::fromJSON(rawToChar(res$content))
    data$Date <- as.Date(data$data)
    data$Province <- rep("Pavia", nrow(data))
    data$valore <- as.numeric(data$valore)
    data <- data %>% filter(Date >Date_max)
    if(nrow(data)!= 0){
      
      data <- data %>%
        filter(valore != -999) %>%
        dplyr::select(Date, valore, Province) %>%
        group_by(Date) %>%
        mutate(
          Temp.delta = max(valore) - min(valore),
          valore = mean(valore),
          Temp.delta16 = valore - 16
        ) %>% distinct()
      
      colnames(data)[2] <- "Temp.medio"
      Pavia_temp <- rbind(Pavia_temp, data)
    }
    
    #Sondrio
    sensors_temp <- sensors %>% filter(Tipo == "Temperatura" & Provincia == "Sondrio")
    Sondrio_temp <- read.table(file = paste0(path, "ARPA/", filenames[grepl(sensors_temp$Sensore, filenames)])) %>% distinct()
    colnames(Sondrio_temp) <- c("Sensor", "Date", "Hour", "Medio")
    Sondrio_temp$Province <- rep("Sondrio", nrow(Sondrio_temp))
    
    Sondrio_temp <- Sondrio_temp %>%
      filter(Medio != -999) %>%
      dplyr::select(Date, Medio, Province) %>%
      group_by(Date) %>%
      mutate(
        Date = as.Date(Date),
        
        Temp.delta = max(Medio) - min(Medio),
        Medio = mean(Medio),
        Temp.delta16 = Medio - 16
      ) %>% distinct()
    
    colnames(Sondrio_temp)[2] <- "Temp.medio"
    
    Date_max <- max(Sondrio_temp$Date)
    res =  httr::GET(paste0("https://www.dati.lombardia.it/resource/647i-nhxk.json?idsensore=",sensors_temp$Sensore))
    data = jsonlite::fromJSON(rawToChar(res$content))
    data$Date <- as.Date(data$data)
    data$Province <- rep("Sondrio", nrow(data))
    data$valore <- as.numeric(data$valore)
    data <- data %>% filter(Date >Date_max)
    if(nrow(data)!= 0){
      
      data <- data %>%
        filter(valore != -999) %>%
        dplyr::select(Date, valore, Province) %>%
        group_by(Date) %>%
        mutate(
          Temp.delta = max(valore) - min(valore),
          valore = mean(valore),
          Temp.delta16 = valore - 16
        ) %>% distinct()
      
      colnames(data)[2] <- "Temp.medio"
      Sondrio_temp <- rbind(Sondrio_temp, data)
    }
    
    #Varese
    sensors_temp <- sensors %>% filter(Tipo == "Temperatura" & Provincia == "Varese")
    Varese_temp <- read.table(file = paste0(path, "ARPA/", filenames[grepl(sensors_temp$Sensore, filenames)])) %>% distinct()
    colnames(Varese_temp) <- c("Sensor", "Date", "Hour", "Medio")
    Varese_temp$Province <- rep("Varese", nrow(Varese_temp))
    
    Varese_temp <- Varese_temp %>%
      filter(Medio != -999) %>%
      dplyr::select(Date, Medio, Province) %>%
      group_by(Date) %>%
      mutate(
        Date = as.Date(Date),
        
        Temp.delta = max(Medio) - min(Medio),
        Medio = mean(Medio),
        Temp.delta16 = Medio - 16
      ) %>% distinct()
    
    colnames(Varese_temp)[2] <- "Temp.medio"
    
    Date_max <- max(Varese_temp$Date)
    res =  httr::GET(paste0("https://www.dati.lombardia.it/resource/647i-nhxk.json?idsensore=",sensors_temp$Sensore))
    data = jsonlite::fromJSON(rawToChar(res$content))
    data$Date <- as.Date(data$data)
    data$Province <- rep("Varese", nrow(data))
    data$valore <- as.numeric(data$valore)
    data <- data %>% filter(Date >Date_max)
    if(nrow(data)!= 0){
      
      data <- data %>%
        filter(valore != -999) %>%
        dplyr::select(Date, valore, Province) %>%
        group_by(Date) %>%
        mutate(
          Temp.delta = max(valore) - min(valore),
          valore = mean(valore),
          Temp.delta16 = valore - 16
        ) %>% distinct()
      
      colnames(data)[2] <- "Temp.medio"
      Varese_temp <- rbind(Varese_temp, data)
    }
    
    Temp_db <- rbind(Bergamo_temp,
                     Brescia_temp,
                     Como_temp,
                     Cremona_temp,
                     Lecco_temp,
                     Lodi_temp,
                     Mantova_temp,
                     Milano_temp,
                     Monza_temp,
                     Pavia_temp,
                     Sondrio_temp,
                     Varese_temp)
    
    
    DB <- left_join(DB, Temp_db, by = c("Date", "Province"))
    
    ########################################Precipitazione########################################
    
    #Bergamo
    sensors_prep <- sensors %>% filter(Tipo == "Precipitazione" & Provincia == "Bergamo")
    Bergamo_prep <- read.table(file = paste0(path, "ARPA/", filenames[grepl(sensors_prep$Sensore, filenames)])) %>% distinct()
    colnames(Bergamo_prep) <- c("Sensor", "Date", "Hour", "Medio")
    Bergamo_prep$Province <- rep("Bergamo", nrow(Bergamo_prep))
    
    Bergamo_prep <- Bergamo_prep %>%
      filter(Medio != -999) %>%
      dplyr::select(Date, Medio, Province) %>%
      group_by(Date) %>%
      mutate(
        Date = as.Date(Date),
        
        Prep.delta = max(Medio) - min(Medio),
        Medio = mean(Medio)
      ) %>% distinct()
    
    colnames(Bergamo_prep)[2] <- "Prep.medio"
    
    Date_max <- max(Bergamo_prep$Date)
    res =  httr::GET(paste0("https://www.dati.lombardia.it/resource/647i-nhxk.json?idsensore=",sensors_prep$Sensore))
    data = jsonlite::fromJSON(rawToChar(res$content))
    data$Date <- as.Date(data$data)
    data$Province <- rep("Bergamo", nrow(data))
    data$valore <- as.numeric(data$valore)
    data <- data %>% filter(Date >Date_max)
    if(nrow(data)!= 0){
      
      data <- data %>%
        filter(valore != -999) %>%
        dplyr::select(Date, valore, Province) %>%
        group_by(Date) %>%
        mutate(
          Prep.delta = max(valore) - min(valore),
          valore = mean(valore)
        ) %>% distinct()
      
      colnames(data)[2] <- "Prep.medio"
      Bergamo_prep <- rbind(Bergamo_prep, data)
    }
    
    
    #Brescia
    sensors_prep <- sensors %>% filter(Tipo == "Precipitazione" & Provincia == "Brescia")
    Brescia_prep <- read.table(file = paste0(path, "ARPA/", filenames[grepl(sensors_prep$Sensore, filenames)])) %>% distinct()
    colnames(Brescia_prep) <- c("Sensor", "Date", "Hour", "Medio")
    Brescia_prep$Province <- rep("Brescia", nrow(Brescia_prep))
    
    Brescia_prep <- Brescia_prep %>%
      filter(Medio != -999) %>%
      dplyr::select(Date, Medio, Province) %>%
      group_by(Date) %>%
      mutate(
        Date = as.Date(Date),
        
        Prep.delta = max(Medio) - min(Medio),
        Medio = mean(Medio)
      ) %>% distinct()
    
    colnames(Brescia_prep)[2] <- "Prep.medio"
    
    Date_max <- max(Brescia_prep$Date)
    res =  httr::GET(paste0("https://www.dati.lombardia.it/resource/647i-nhxk.json?idsensore=",sensors_prep$Sensore))
    data = jsonlite::fromJSON(rawToChar(res$content))
    data$Date <- as.Date(data$data)
    data$Province <- rep("Brescia", nrow(data))
    data$valore <- as.numeric(data$valore)
    data <- data %>% filter(Date >Date_max)
    if(nrow(data)!= 0){
      
      data <- data %>%
        filter(valore != -999) %>%
        dplyr::select(Date, valore, Province) %>%
        group_by(Date) %>%
        mutate(
          Prep.delta = max(valore) - min(valore),
          valore = mean(valore)
        ) %>% distinct()
      
      colnames(data)[2] <- "Prep.medio"
      Brescia_prep <- rbind(Brescia_prep, data)
    }
    
    #Como
    sensors_prep <- sensors %>% filter(Tipo == "Precipitazione" & Provincia == "Como")
    Como_prep <- read.table(file = paste0(path, "ARPA/", filenames[grepl(sensors_prep$Sensore, filenames)])) %>% distinct()
    colnames(Como_prep) <- c("Sensor", "Date", "Hour", "Medio")
    Como_prep$Province <- rep("Como", nrow(Como_prep))
    
    Como_prep <- Como_prep %>%
      filter(Medio != -999) %>%
      dplyr::select(Date, Medio, Province) %>%
      group_by(Date) %>%
      mutate(
        Date = as.Date(Date),
        
        Prep.delta = max(Medio) - min(Medio),
        Medio = mean(Medio)
      ) %>% distinct()
    
    colnames(Como_prep)[2] <- "Prep.medio"
    
    Date_max <- max(Como_prep$Date)
    res =  httr::GET(paste0("https://www.dati.lombardia.it/resource/647i-nhxk.json?idsensore=",sensors_prep$Sensore))
    data = jsonlite::fromJSON(rawToChar(res$content))
    data$Date <- as.Date(data$data)
    data$Province <- rep("Como", nrow(data))
    data$valore <- as.numeric(data$valore)
    data <- data %>% filter(Date >Date_max)
    if(nrow(data)!= 0){
      
      data <- data %>%
        filter(valore != -999) %>%
        dplyr::select(Date, valore, Province) %>%
        group_by(Date) %>%
        mutate(
          Prep.delta = max(valore) - min(valore),
          valore = mean(valore)
        ) %>% distinct()
      
      colnames(data)[2] <- "Prep.medio"
      Como_prep <- rbind(Como_prep, data)
    }
    #Cremona
    sensors_prep <- sensors %>% filter(Tipo == "Precipitazione" & Provincia == "Cremona")
    Cremona_prep <- read.table(file = paste0(path, "ARPA/", filenames[grepl(sensors_prep$Sensore, filenames)])) %>% distinct()
    colnames(Cremona_prep) <- c("Sensor", "Date", "Hour", "Medio")
    Cremona_prep$Province <- rep("Cremona", nrow(Cremona_prep))
    
    Cremona_prep <- Cremona_prep %>%
      filter(Medio != -999) %>%
      dplyr::select(Date, Medio, Province) %>%
      group_by(Date) %>%
      mutate(
        Date = as.Date(Date),
        
        Prep.delta = max(Medio) - min(Medio),
        Medio = mean(Medio)
      ) %>% distinct()
    
    colnames(Cremona_prep)[2] <- "Prep.medio"
    
    Date_max <- max(Cremona_prep$Date)
    res =  httr::GET(paste0("https://www.dati.lombardia.it/resource/647i-nhxk.json?idsensore=",sensors_prep$Sensore))
    data = jsonlite::fromJSON(rawToChar(res$content))
    data$Date <- as.Date(data$data)
    data$Province <- rep("Cremona", nrow(data))
    data$valore <- as.numeric(data$valore)
    data <- data %>% filter(Date >Date_max)
    if(nrow(data)!= 0){
      
      data <- data %>%
        filter(valore != -999) %>%
        dplyr::select(Date, valore, Province) %>%
        group_by(Date) %>%
        mutate(
          Prep.delta = max(valore) - min(valore),
          valore = mean(valore)
        ) %>% distinct()
      
      colnames(data)[2] <- "Prep.medio"
      Cremona_prep <- rbind(Cremona_prep, data)
    }
    
    #Lecco
    sensors_prep <- sensors %>% filter(Tipo == "Precipitazione" & Provincia == "Lecco")
    Lecco_prep <- read.table(file = paste0(path, "ARPA/", filenames[grepl(sensors_prep$Sensore, filenames)])) %>% distinct()
    colnames(Lecco_prep) <- c("Sensor", "Date", "Hour", "Medio")
    Lecco_prep$Province <- rep("Lecco", nrow(Lecco_prep))
    
    Lecco_prep <- Lecco_prep %>%
      filter(Medio != -999) %>%
      dplyr::select(Date, Medio, Province) %>%
      group_by(Date) %>%
      mutate(
        Date = as.Date(Date),
        
        Prep.delta = max(Medio) - min(Medio),
        Medio = mean(Medio)
      ) %>% distinct()
    
    colnames(Lecco_prep)[2] <- "Prep.medio"
    
    Date_max <- max(Lecco_prep$Date)
    res =  httr::GET(paste0("https://www.dati.lombardia.it/resource/647i-nhxk.json?idsensore=",sensors_prep$Sensore))
    data = jsonlite::fromJSON(rawToChar(res$content))
    data$Date <- as.Date(data$data)
    data$Province <- rep("Lecco", nrow(data))
    data$valore <- as.numeric(data$valore)
    data <- data %>% filter(Date >Date_max)
    if(nrow(data)!= 0){
      
      data <- data %>%
        filter(valore != -999) %>%
        dplyr::select(Date, valore, Province) %>%
        group_by(Date) %>%
        mutate(
          Prep.delta = max(valore) - min(valore),
          valore = mean(valore)
        ) %>% distinct()
      
      colnames(data)[2] <- "Prep.medio"
      Lecco_prep <- rbind(Lecco_prep, data)
    }
    
    #Lodi
    sensors_prep <- sensors %>% filter(Tipo == "Precipitazione" & Provincia == "Lodi")
    Lodi_prep <- read.table(file = paste0(path, "ARPA/", filenames[grepl(sensors_prep$Sensore, filenames)])) %>% distinct()
    colnames(Lodi_prep) <- c("Sensor", "Date", "Hour", "Medio")
    Lodi_prep$Province <- rep("Lodi", nrow(Lodi_prep))
    
    Lodi_prep <- Lodi_prep %>%
      filter(Medio != -999) %>%
      dplyr::select(Date, Medio, Province) %>%
      group_by(Date) %>%
      mutate(
        Date = as.Date(Date),
        
        Prep.delta = max(Medio) - min(Medio),
        Medio = mean(Medio)
      ) %>% distinct()
    
    colnames(Lodi_prep)[2] <- "Prep.medio"
    
    Date_max <- max(Lodi_prep$Date)
    res =  httr::GET(paste0("https://www.dati.lombardia.it/resource/647i-nhxk.json?idsensore=",sensors_prep$Sensore))
    data = jsonlite::fromJSON(rawToChar(res$content))
    data$Date <- as.Date(data$data)
    data$Province <- rep("Lodi", nrow(data))
    data$valore <- as.numeric(data$valore)
    data <- data %>% filter(Date >Date_max)
    if(nrow(data)!= 0){
      
      data <- data %>%
        filter(valore != -999) %>%
        dplyr::select(Date, valore, Province) %>%
        group_by(Date) %>%
        mutate(
          Prep.delta = max(valore) - min(valore),
          valore = mean(valore)
        ) %>% distinct()
      
      colnames(data)[2] <- "Prep.medio"
      Lodi_prep <- rbind(Lodi_prep, data)
    }
    
    #Mantova
    sensors_prep <- sensors %>% filter(Tipo == "Precipitazione" & Provincia == "Mantova")
    Mantova_prep <- read.table(file = paste0(path, "ARPA/", filenames[grepl(sensors_prep$Sensore, filenames)])) %>% distinct()
    colnames(Mantova_prep) <- c("Sensor", "Date", "Hour", "Medio")
    Mantova_prep$Province <- rep("Mantova", nrow(Mantova_prep))
    
    Mantova_prep <- Mantova_prep %>%
      filter(Medio != -999) %>%
      dplyr::select(Date, Medio, Province) %>%
      group_by(Date) %>%
      mutate(
        Date = as.Date(Date),
        
        Prep.delta = max(Medio) - min(Medio),
        Medio = mean(Medio)
      ) %>% distinct()
    
    colnames(Mantova_prep)[2] <- "Prep.medio"
    
    
    Date_max <- max(Mantova_prep$Date)
    res =  httr::GET(paste0("https://www.dati.lombardia.it/resource/647i-nhxk.json?idsensore=",sensors_prep$Sensore))
    data = jsonlite::fromJSON(rawToChar(res$content))
    data$Date <- as.Date(data$data)
    data$Province <- rep("Mantova", nrow(data))
    data$valore <- as.numeric(data$valore)
    data <- data %>% filter(Date >Date_max)
    if(nrow(data)!= 0){
      
      data <- data %>%
        filter(valore != -999) %>%
        dplyr::select(Date, valore, Province) %>%
        group_by(Date) %>%
        mutate(
          Prep.delta = max(valore) - min(valore),
          valore = mean(valore)
        ) %>% distinct()
      
      colnames(data)[2] <- "Prep.medio"
      Mantova_prep <- rbind(Mantova_prep, data)
    }
    
    #Milano
    sensors_prep <- sensors %>% filter(Tipo == "Precipitazione" & Provincia == "Milano")
    Milano_prep <- read.table(file = paste0(path, "ARPA/", filenames[grepl(sensors_prep$Sensore, filenames)])) %>% distinct()
    colnames(Milano_prep) <- c("Sensor", "Date", "Hour", "Medio")
    Milano_prep$Province <- rep("Milano", nrow(Milano_prep))
    
    Milano_prep <- Milano_prep %>%
      filter(Medio != -999) %>%
      dplyr::select(Date, Medio, Province) %>%
      group_by(Date) %>%
      mutate(
        Date = as.Date(Date),
        
        Prep.delta = max(Medio) - min(Medio),
        Medio = mean(Medio)
      ) %>% distinct()
    
    colnames(Milano_prep)[2] <- "Prep.medio"
    
    
    Date_max <- max(Milano_prep$Date)
    res =  httr::GET(paste0("https://www.dati.lombardia.it/resource/647i-nhxk.json?idsensore=",sensors_prep$Sensore))
    data = jsonlite::fromJSON(rawToChar(res$content))
    data$Date <- as.Date(data$data)
    data$Province <- rep("Milano", nrow(data))
    data$valore <- as.numeric(data$valore)
    data <- data %>% filter(Date >Date_max)
    if(nrow(data)!= 0){
      
      data <- data %>%
        filter(valore != -999) %>%
        dplyr::select(Date, valore, Province) %>%
        group_by(Date) %>%
        mutate(
          Prep.delta = max(valore) - min(valore),
          valore = mean(valore)
        ) %>% distinct()
      
      colnames(data)[2] <- "Prep.medio"
      Milano_prep <- rbind(Milano_prep, data)
    }
    
    #Monza
    sensors_prep <- sensors %>% filter(Tipo == "Precipitazione" & Provincia == "Monza")
    Monza_prep <- read.table(file = paste0(path, "ARPA/", filenames[grepl(sensors_prep$Sensore, filenames)])) %>% distinct()
    colnames(Monza_prep) <- c("Sensor", "Date", "Hour", "Medio")
    Monza_prep$Province <- rep("Monza", nrow(Monza_prep))
    
    Monza_prep <- Monza_prep %>%
      filter(Medio != -999) %>%
      dplyr::select(Date, Medio, Province) %>%
      group_by(Date) %>%
      mutate(
        Date = as.Date(Date),
        
        Prep.delta = max(Medio) - min(Medio),
        Medio = mean(Medio)
      ) %>% distinct()
    
    colnames(Monza_prep)[2] <- "Prep.medio"
    
    Date_max <- max(Monza_prep$Date)
    res =  httr::GET(paste0("https://www.dati.lombardia.it/resource/647i-nhxk.json?idsensore=",sensors_prep$Sensore))
    data = jsonlite::fromJSON(rawToChar(res$content))
    data$Date <- as.Date(data$data)
    data$Province <- rep("Monza", nrow(data))
    data$valore <- as.numeric(data$valore)
    data <- data %>% filter(Date >Date_max)
    if(nrow(data)!= 0){
      
      data <- data %>%
        filter(valore != -999) %>%
        dplyr::select(Date, valore, Province) %>%
        group_by(Date) %>%
        mutate(
          Prep.delta = max(valore) - min(valore),
          valore = mean(valore)
        ) %>% distinct()
      
      colnames(data)[2] <- "Prep.medio"
      Monza_prep <- rbind(Monza_prep, data)
    }
    
    #Pavia
    sensors_prep <- sensors %>% filter(Tipo == "Precipitazione" & Provincia == "Pavia")
    Pavia_prep <- read.table(file = paste0(path, "ARPA/", filenames[grepl(sensors_prep$Sensore, filenames)])) %>% distinct()
    colnames(Pavia_prep) <- c("Sensor", "Date", "Hour", "Medio")
    Pavia_prep$Province <- rep("Pavia", nrow(Pavia_prep))
    
    Pavia_prep <- Pavia_prep %>%
      filter(Medio != -999) %>%
      dplyr::select(Date, Medio, Province) %>%
      group_by(Date) %>%
      mutate(
        Date = as.Date(Date),
        
        Prep.delta = max(Medio) - min(Medio),
        Medio = mean(Medio)
      ) %>% distinct()
    
    colnames(Pavia_prep)[2] <- "Prep.medio"
    
    
    Date_max <- max(Pavia_prep$Date)
    res =  httr::GET(paste0("https://www.dati.lombardia.it/resource/647i-nhxk.json?idsensore=",sensors_prep$Sensore))
    data = jsonlite::fromJSON(rawToChar(res$content))
    data$Date <- as.Date(data$data)
    data$Province <- rep("Pavia", nrow(data))
    data$valore <- as.numeric(data$valore)
    data <- data %>% filter(Date >Date_max)
    if(nrow(data)!= 0){
      
      data <- data %>%
        filter(valore != -999) %>%
        dplyr::select(Date, valore, Province) %>%
        group_by(Date) %>%
        mutate(
          Prep.delta = max(valore) - min(valore),
          valore = mean(valore)
        ) %>% distinct()
      
      colnames(data)[2] <- "Prep.medio"
      Pavia_prep <- rbind(Pavia_prep, data)
    }
    
    #Sondrio
    sensors_prep <- sensors %>% filter(Tipo == "Precipitazione" & Provincia == "Sondrio")
    Sondrio_prep <- read.table(file = paste0(path, "ARPA/", filenames[grepl(sensors_prep$Sensore, filenames)])) %>% distinct()
    colnames(Sondrio_prep) <- c("Sensor", "Date", "Hour", "Medio")
    Sondrio_prep$Province <- rep("Sondrio", nrow(Sondrio_prep))
    
    Sondrio_prep <- Sondrio_prep %>%
      filter(Medio != -999) %>%
      dplyr::select(Date, Medio, Province) %>%
      group_by(Date) %>%
      mutate(
        Date = as.Date(Date),
        
        Prep.delta = max(Medio) - min(Medio),
        Medio = mean(Medio)
      ) %>% distinct()
    
    colnames(Sondrio_prep)[2] <- "Prep.medio"
    
    Date_max <- max(Sondrio_prep$Date)
    res =  httr::GET(paste0("https://www.dati.lombardia.it/resource/647i-nhxk.json?idsensore=",sensors_prep$Sensore))
    data = jsonlite::fromJSON(rawToChar(res$content))
    data$Date <- as.Date(data$data)
    data$Province <- rep("Sondrio", nrow(data))
    data$valore <- as.numeric(data$valore)
    data <- data %>% filter(Date >Date_max)
    if(nrow(data)!= 0){
      
      data <- data %>%
        filter(valore != -999) %>%
        dplyr::select(Date, valore, Province) %>%
        group_by(Date) %>%
        mutate(
          Prep.delta = max(valore) - min(valore),
          valore = mean(valore)
        ) %>% distinct()
      
      colnames(data)[2] <- "Prep.medio"
      Sondrio_prep <- rbind(Sondrio_prep, data)
    }
    
    #Varese
    sensors_prep <- sensors %>% filter(Tipo == "Precipitazione" & Provincia == "Varese")
    Varese_prep <- read.table(file = paste0(path, "ARPA/", filenames[grepl(sensors_prep$Sensore, filenames)])) %>% distinct()
    colnames(Varese_prep) <- c("Sensor", "Date", "Hour", "Medio")
    Varese_prep$Province <- rep("Varese", nrow(Varese_prep))
    
    Varese_prep <- Varese_prep %>%
      filter(Medio != -999) %>%
      dplyr::select(Date, Medio, Province) %>%
      group_by(Date) %>%
      mutate(
        Date = as.Date(Date),
        
        Prep.delta = max(Medio) - min(Medio),
        Medio = mean(Medio)
      ) %>% distinct()
    
    colnames(Varese_prep)[2] <- "Prep.medio"
    
    
    Date_max <- max(Varese_prep$Date)
    res =  httr::GET(paste0("https://www.dati.lombardia.it/resource/647i-nhxk.json?idsensore=",sensors_prep$Sensore))
    data = jsonlite::fromJSON(rawToChar(res$content))
    data$Date <- as.Date(data$data)
    data$Province <- rep("Varese", nrow(data))
    data$valore <- as.numeric(data$valore)
    data <- data %>% filter(Date >Date_max)
    if(nrow(data)!= 0){
      
      data <- data %>%
        filter(valore != -999) %>%
        dplyr::select(Date, valore, Province) %>%
        group_by(Date) %>%
        mutate(
          Prep.delta = max(valore) - min(valore),
          valore = mean(valore)
        ) %>% distinct()
      
      colnames(data)[2] <- "Prep.medio"
      Varese_prep <- rbind(Varese_prep, data)
    }
    
    
    Prep_db <- rbind(Bergamo_prep,
                     Brescia_prep,
                     Como_prep,
                     Cremona_prep,
                     Lecco_prep,
                     Lodi_prep,
                     Mantova_prep,
                     Milano_prep,
                     Monza_prep,
                     Pavia_prep,
                     Sondrio_prep,
                     Varese_prep)
    
    DB <- left_join(DB, Prep_db, by = c("Date", "Province"))
    
    
    ########################################Neve########################################
    
    
    #Bergamo
    sensors_snow <- sensors %>% filter(Tipo == "Neve" & Provincia == "Bergamo")
    Bergamo_snow <- read.table(file = paste0(path, "ARPA/", filenames[grepl(sensors_snow$Sensore, filenames)])) %>% distinct()
    colnames(Bergamo_snow) <- c("Sensor", "Date", "Hour", "Medio")
    Bergamo_snow$Province <- rep("Bergamo", nrow(Bergamo_snow))
    
    Bergamo_snow <- Bergamo_snow %>%
      filter(Medio != -999) %>%
      dplyr::select(Date, Medio, Province) %>%
      group_by(Date) %>%
      mutate(
        Date = as.Date(Date),
        
        Snow.delta = max(Medio) - min(Medio),
        Medio = mean(Medio)
      ) %>% distinct()
    
    colnames(Bergamo_snow)[2] <- "Snow.medio"
    
    Date_max <- max(Bergamo_snow$Date)
    res =  httr::GET(paste0("https://www.dati.lombardia.it/resource/647i-nhxk.json?idsensore=",sensors_snow$Sensore))
    data = jsonlite::fromJSON(rawToChar(res$content))
    data$Date <- as.Date(data$data)
    data$Province <- rep("Bergamo", nrow(data))
    data$valore <- as.numeric(data$valore)
    data <- data %>% filter(Date >Date_max)
    if(nrow(data)!= 0){
      
      data <- data %>%
        filter(valore != -999) %>%
        dplyr::select(Date, valore, Province) %>%
        group_by(Date) %>%
        mutate(
          Snow.delta = max(valore) - min(valore),
          valore = mean(valore)
        ) %>% distinct()
      
      colnames(data)[2] <- "Snow.medio"
      Bergamo_snow <- rbind(Bergamo_snow, data)
    }
    
    
    #Brescia
    sensors_snow <- sensors %>% filter(Tipo == "Neve" & Provincia == "Brescia")
    Brescia_snow <- read.table(file = paste0(path, "ARPA/", filenames[grepl(sensors_snow$Sensore, filenames)])) %>% distinct()
    colnames(Brescia_snow) <- c("Sensor", "Date", "Hour", "Medio")
    Brescia_snow$Province <- rep("Brescia", nrow(Brescia_snow))
    
    Brescia_snow <- Brescia_snow %>%
      filter(Medio != -999) %>%
      dplyr::select(Date, Medio, Province) %>%
      group_by(Date) %>%
      mutate(
        Date = as.Date(Date),
        
        Snow.delta = max(Medio) - min(Medio),
        Medio = mean(Medio)
      ) %>% distinct()
    
    colnames(Brescia_snow)[2] <- "Snow.medio"
    
    Date_max <- max(Brescia_snow$Date)
    res =  httr::GET(paste0("https://www.dati.lombardia.it/resource/647i-nhxk.json?idsensore=",sensors_snow$Sensore))
    data = jsonlite::fromJSON(rawToChar(res$content))
    data$Date <- as.Date(data$data)
    data$Province <- rep("Brescia", nrow(data))
    data$valore <- as.numeric(data$valore)
    data <- data %>% filter(Date >Date_max)
    if(nrow(data)!= 0){
      
      data <- data %>%
        filter(valore != -999) %>%
        dplyr::select(Date, valore, Province) %>%
        group_by(Date) %>%
        mutate(
          Snow.delta = max(valore) - min(valore),
          valore = mean(valore)
        ) %>% distinct()
      
      colnames(data)[2] <- "Snow.medio"
      Brescia_snow <- rbind(Brescia_snow, data)
    }
    
    #Como
    sensors_snow <- sensors %>% filter(Tipo == "Neve" & Provincia == "Como")
    Como_snow <- read.table(file = paste0(path, "ARPA/", filenames[grepl(sensors_snow$Sensore, filenames)])) %>% distinct()
    colnames(Como_snow) <- c("Sensor", "Date", "Hour", "Medio")
    Como_snow$Province <- rep("Como", nrow(Como_snow))
    
    Como_snow <- Como_snow %>%
      filter(Medio != -999) %>%
      dplyr::select(Date, Medio, Province) %>%
      group_by(Date) %>%
      mutate(
        Date = as.Date(Date),
        
        Snow.delta = max(Medio) - min(Medio),
        Medio = mean(Medio)
      ) %>% distinct()
    
    colnames(Como_snow)[2] <- "Snow.medio"
    
    Date_max <- max(Como_snow$Date)
    res =  httr::GET(paste0("https://www.dati.lombardia.it/resource/647i-nhxk.json?idsensore=",sensors_snow$Sensore))
    data = jsonlite::fromJSON(rawToChar(res$content))
    data$Date <- as.Date(data$data)
    data$Province <- rep("Como", nrow(data))
    data$valore <- as.numeric(data$valore)
    data <- data %>% filter(Date >Date_max)
    if(nrow(data)!= 0){
      
      data <- data %>%
        filter(valore != -999) %>%
        dplyr::select(Date, valore, Province) %>%
        group_by(Date) %>%
        mutate(
          Snow.delta = max(valore) - min(valore),
          valore = mean(valore)
        ) %>% distinct()
      
      colnames(data)[2] <- "Snow.medio"
      Como_snow <- rbind(Como_snow, data)
    }
    
    #Lecco
    sensors_snow <- sensors %>% filter(Tipo == "Neve" & Provincia == "Lecco")
    Lecco_snow <- read.table(file = paste0(path, "ARPA/", filenames[grepl(sensors_snow$Sensore, filenames)])) %>% distinct()
    colnames(Lecco_snow) <- c("Sensor", "Date", "Hour", "Medio")
    Lecco_snow$Province <- rep("Lecco", nrow(Lecco_snow))
    
    Lecco_snow <- Lecco_snow %>%
      filter(Medio != -999) %>%
      dplyr::select(Date, Medio, Province) %>%
      group_by(Date) %>%
      mutate(
        Date = as.Date(Date),
        
        Snow.delta = max(Medio) - min(Medio),
        Medio = mean(Medio)
      ) %>% distinct()
    
    colnames(Lecco_snow)[2] <- "Snow.medio"
    
    Date_max <- max(Lecco_snow$Date)
    res =  httr::GET(paste0("https://www.dati.lombardia.it/resource/647i-nhxk.json?idsensore=",sensors_snow$Sensore))
    data = jsonlite::fromJSON(rawToChar(res$content))
    data$Date <- as.Date(data$data)
    data$Province <- rep("Lecco", nrow(data))
    data$valore <- as.numeric(data$valore)
    data <- data %>% filter(Date >Date_max)
    if(nrow(data)!= 0){
      
      data <- data %>%
        filter(valore != -999) %>%
        dplyr::select(Date, valore, Province) %>%
        group_by(Date) %>%
        mutate(
          Snow.delta = max(valore) - min(valore),
          valore = mean(valore)
        ) %>% distinct()
      
      colnames(data)[2] <- "Snow.medio"
      Lecco_snow <- rbind(Lecco_snow, data)
    }
    
    #Sondrio
    sensors_snow <- sensors %>% filter(Tipo == "Neve" & Provincia == "Sondrio")
    Sondrio_snow <- read.table(file = paste0(path, "ARPA/", filenames[grepl(sensors_snow$Sensore, filenames)])) %>% distinct()
    colnames(Sondrio_snow) <- c("Sensor", "Date", "Hour", "Medio")
    Sondrio_snow$Province <- rep("Sondrio", nrow(Sondrio_snow))
    
    Sondrio_snow <- Sondrio_snow %>%
      filter(Medio != -999) %>%
      dplyr::select(Date, Medio, Province) %>%
      group_by(Date) %>%
      mutate(
        Date = as.Date(Date),
        
        Snow.delta = max(Medio) - min(Medio),
        Medio = mean(Medio)
      ) %>% distinct()
    
    colnames(Sondrio_snow)[2] <- "Snow.medio"
    
    Date_max <- max(Sondrio_snow$Date)
    res =  httr::GET(paste0("https://www.dati.lombardia.it/resource/647i-nhxk.json?idsensore=",sensors_snow$Sensore))
    data = jsonlite::fromJSON(rawToChar(res$content))
    data$Date <- as.Date(data$data)
    data$Province <- rep("Sondrio", nrow(data))
    data$valore <- as.numeric(data$valore)
    data <- data %>% filter(Date >Date_max)
    if(nrow(data)!= 0){
      
      data <- data %>%
        filter(valore != -999) %>%
        dplyr::select(Date, valore, Province) %>%
        group_by(Date) %>%
        mutate(
          Snow.delta = max(valore) - min(valore),
          valore = mean(valore)
        ) %>% distinct()
      
      colnames(data)[2] <- "Snow.medio"
      Sondrio_snow <- rbind(Sondrio_snow, data)
    }
    
    #Varese
    sensors_snow <- sensors %>% filter(Tipo == "Neve" & Provincia == "Varese")
    Varese_snow <- read.table(file = paste0(path, "ARPA/", filenames[grepl(sensors_snow$Sensore, filenames)])) %>% distinct()
    colnames(Varese_snow) <- c("Sensor", "Date", "Hour", "Medio")
    Varese_snow$Province <- rep("Varese", nrow(Varese_snow))
    
    Varese_snow <- Varese_snow %>%
      filter(Medio != -999) %>%
      dplyr::select(Date, Medio, Province) %>%
      group_by(Date) %>%
      mutate(
        Date = as.Date(Date),
        
        Snow.delta = max(Medio) - min(Medio),
        Medio = mean(Medio)
      ) %>% distinct()
    
    colnames(Varese_snow)[2] <- "Snow.medio"
    
    Date_max <- max(Varese_snow$Date)
    res =  httr::GET(paste0("https://www.dati.lombardia.it/resource/647i-nhxk.json?idsensore=",sensors_snow$Sensore))
    data = jsonlite::fromJSON(rawToChar(res$content))
    data$Date <- as.Date(data$data)
    data$Province <- rep("Varese", nrow(data))
    data$valore <- as.numeric(data$valore)
    data <- data %>% filter(Date >Date_max)
    if(nrow(data)!= 0){
      
      data <- data %>%
        filter(valore != -999) %>%
        dplyr::select(Date, valore, Province) %>%
        group_by(Date) %>%
        mutate(
          Snow.delta = max(valore) - min(valore),
          valore = mean(valore)
        ) %>% distinct()
      
      colnames(data)[2] <- "Snow.medio"
      Varese_snow <- rbind(Varese_snow, data)
    }
    
    
    Snow_db <- rbind(Bergamo_snow,
                     Brescia_snow,
                     Como_snow,
                     Lecco_snow,
                     Sondrio_snow,
                     Varese_snow)
    
    DB <- left_join(DB, Snow_db, by = c("Date", "Province"))
    
    DB$Snow.medio[is.na(DB$Snow.medio)] <- 0
    DB$Snow.delta[is.na(DB$Snow.delta)] <- 0
    
    #Totali positivi
    
    Italy_provinces <- read.csv("https://raw.githubusercontent.com/pcm-dpc/COVID-19/master/dati-province/dpc-covid19-ita-province.csv")
    
    
    Italy_provinces <- Italy_provinces %>%
      filter(denominazione_regione == "Lombardia") %>%
      mutate(denominazione_provincia = ifelse(denominazione_provincia == "Monza e della Brianza", "Monza", denominazione_provincia)) %>%
      filter(denominazione_provincia %notin% c("In fase di definizione/aggiornamento","Fuori Regione / Provincia Autonoma"))
    
    tot_casi <- Italy_provinces %>%
      dplyr::select(denominazione_provincia, data, totale_casi)
    
    colnames(tot_casi) <- c("Province", "Date", "Tot_casi")
    
    tot_casi$Date <- as.Date(ymd_hms(tot_casi$Date))
    
    tot_casi <- tot_casi %>%
      arrange(Tot_casi) %>%
      group_by(Province) %>%
      mutate(Tot_casiLag5 = dplyr::lag(Tot_casi, n = 5, default = NA, order_by = Date))
    
    DB$Date <- as.Date(DB$Date)
    DB <- left_join(DB, tot_casi, by=c("Province", "Date"))
    DB <- DB %>% mutate(Tot_casi = ifelse(is.na(Tot_casi), 0, Tot_casi))
    
    ###RT totale casi by province
    
    
    load(paste0(path, "Rt_TC_Province.RData"))
    df <- df[,c(1:2,5)]
    colnames(df)[2] <- "Rt_TC.medio"
    DB <- left_join(DB, df, by=c("Date", "Province")) 
    
  }
  
  ####################################################################################
  ######################################Rt index######################################
  ####################################################################################
  shape.stimato <- 1.87
  rate.stimato <- 0.28
  
  ## massimo numero di giorni dell'intervallo seriale
  N <- 300
  
  ## definisco la distribuzione dell'intervallo seriale
  intervallo.seriale <- dgamma(0:N, shape=shape.stimato, rate=rate.stimato) 
  
  ## normalizzo la distribuzione dell'intervallo seriale in modo che la somma faccia 1
  SI <- (intervallo.seriale/sum(intervallo.seriale)) 
  
  ## leggo la curva epidemica da un file con 3 colonne separate da spazi: data, numero di casi trasmessi localmente, numero di casi importati
  
  curva.epidemica <- read.table(paste0(path,"curva_epidemica_Italia_2021-04-14"))
  curva.epidemica[,1] <- as.Date(curva.epidemica[,1])
  names(curva.epidemica) <- c("dates", "local", "imported") ## assegno i nomi richiesti dal pacchetto EpiEstim
  
  ## calcolo la stima di R applicando la funzione estimate_R del pacchetto EpiEstim
  stima <- estimate_R(incid=curva.epidemica, method="non_parametric_si", config = make_config(list(si_distr = SI, n1=10000, mcmc_control=make_mcmc_control(thin=1, burnin=1000000))))
  
  ## il pacchetto avvisa che la stima di Rt viene fornita con una media mobile settimanale ("Default config will estimate R on weekly sliding windows"), eventualmente personalizzabile
  ## avvisa inoltre che la parte iniziale della curva non e' sufficiente alla stima corretta della variabilita' di Rt ("You're estimating R too early in the epidemic to  httr::GET the desired posterior CV")
  
  ###################
  ### Attenzione! ###
  ###################
  
  ## La stima e' calcolata su tutta la curva epidemica specificata, ma il pacchetto non puo' tenere conto dei ritardi di inserimento nel dato
  ## Le stime di Rt varieranno man mano che vengono inseriti nuovi casi con data di inizio sintomi indietro nel tempo
  ## Per questo motivo ISS considera valide le stime fino a 14 giorni prima della data in cui viene effettuata la stima.
  ## Questo ritardo puo' cambiare nel tempo
  
  ## estraggo i risultati di interesse
  R.medio <- stima$R$`Mean(R)` ## valore medio
  R.lowerCI <- stima$R$`Quantile.0.025(R)` ## estremo inferiore dell'intervallo di confidenza
  R.upperCI <- stima$R$`Quantile.0.975(R)` ## estremo superiore dell'intervallo di confidenza
  
  ## estraggo le date di riferimento per la stima di R
  ## la data rappresenta il giorno centrale intorno a cui e' calcolata la media mobile di Rt (con finestra di ampiezza pari a una settimana)
  sel.date <- stima$R[, "t_end"]
  Date <- curva.epidemica[sel.date,1] 
  
  R.medio <- data.frame(R.medio, Date)
  R.medio <- R.medio %>% dplyr::select(R.medio, Date)
  DB <- left_join(DB, R.medio, by = "Date")
  DB <- DB %>% mutate(R.medio = ifelse(is.na(R.medio), 0, R.medio))
  
  #Incidenza influenza (to update every week https://www.epicentro.iss.it/influenza/flunews#epi)
  
  flu <- read.csv(paste0(path,"incidenza-delle-sindromi.csv"))
  colnames(flu) <- c("Week", "2020", "2019", "2018", "2017", "2016", "2015", "2014")
  
  db_flu <- data.frame(Week = rep(flu$Week, 7),
                       Flu = c(c(rep(NA, length(flu$Week)-length(flu$`2020`[12:28])),flu$`2020`[12:28]), 
                               c(flu$`2020`[1:11],flu$`2019`[12:28]), 
                               c(flu$`2019`[1:11],flu$`2018`[12:28]), 
                               c(flu$`2018`[1:11],flu$`2017`[12:28]), 
                               c(flu$`2017`[1:11],flu$`2016`[12:28]), 
                               c(flu$`2016`[1:11],flu$`2015`[12:28]),
                               c(flu$`2015`[1:11],flu$`2014`[12:28])),
                       Year = c(rep(2021, length(flu$Week)),
                                rep(2020, length(flu$Week)), 
                                rep(2019, length(flu$Week)), 
                                rep(2018, length(flu$Week)), 
                                rep(2017, length(flu$Week)),
                                rep(2016, length(flu$Week)),
                                rep(2015, length(flu$Week))) )
  
  
  DB <- left_join(DB, db_flu, by=c("Year", "Week")) 
  
  DB$Flu <- ifelse(is.na(DB$Flu), 0, DB$Flu)
  
  DB <- DB %>% mutate(DayWeekend = ifelse(Day %in% c("Saturday", "Sunday"), "Weekend", "Weekdays"),
                      Year.dummy = ifelse(Year == 2020, 1, 0))
  
  #RT Lombardy level
  load(paste0(path, "Rt_hosp_BN.RData"))
  
  DB <- left_join(DB, R2, by=c("Date")) 
  
  load(paste0(path, "Rt_NP_BN.RData"))
  
  DB <- left_join(DB, R2, by=c("Date")) 
  
  load(paste0(path, "Rt_oneweek_TP.RData"))
  df <- df[,c(1,2)]
  colnames(df)[2] <- "Rt_oneweek_TP"
  
  DB <- left_join(DB, df, by=c("Date")) 
  
  load(paste0(path, "Rt_oneweek_Hosp.RData"))
  df <- df[,c(1,2)]
  colnames(df)[2] <- "Rt_oneweek_Hosp"
  
  DB <- left_join(DB, df, by=c("Date")) 
  
  load(paste0(path, "Rt_NP_BN_ISS.RData"))
  df <- R2_ISS[,c(1,2)]
  colnames(df)[1] <- "Rt_ISS"
  df <- df[1:(nrow(df)-1),]
  
  DB <- left_join(DB, df, by=c("Date")) 
  
  DB <- DB %>% mutate(Rt_TC.medio = ifelse(is.na(Rt_TC.medio), 0, Rt_TC.medio),
                      Rt_hosp = ifelse(is.na(Rt_hosp), 0, Rt_hosp),
                      Rt_np = ifelse(is.na(Rt_np), 0, Rt_np),
                      Rt_ISS = ifelse(is.na(Rt_ISS), 0, Rt_ISS),
                      Rt_oneweek_TP = ifelse(is.na(Rt_oneweek_TP), 0, Rt_oneweek_TP),
                      Rt_oneweek_Hosp = ifelse(is.na(Rt_oneweek_Hosp), 0, Rt_oneweek_Hosp))
  
  
  
  #COVID variables Lombardy level
  
  dati = read.csv("https://raw.githubusercontent.com/pcm-dpc/COVID-19/master/dati-regioni/dpc-covid19-ita-regioni.csv")
  
  
  dati <- dati %>%
    filter(denominazione_regione == "Lombardia")
  
  dati <- dati %>%
    dplyr::select(data, 
                  totale_ospedalizzati, 
                  terapia_intensiva, 
                  ricoverati_con_sintomi, 
                  isolamento_domiciliare,
                  ingressi_terapia_intensiva,
                  dimessi_guariti,
                  deceduti,
                  totale_positivi,
                  variazione_totale_positivi,
                  nuovi_positivi,
                  tamponi,
                  casi_testati,
                  totale_positivi_test_molecolare,
                  totale_positivi_test_antigenico_rapido,
                  tamponi_test_molecolare,
                  tamponi_test_antigenico_rapido
    )
  
  colnames(dati)[1] <- c("Date")
  
  dati$Date <- as.Date(ymd_hms(dati$Date))
  
  DB <- left_join(DB, dati, by=c("Date"))
  DB <- DB %>% mutate(totale_ospedalizzati = ifelse(is.na(totale_ospedalizzati), 0, totale_ospedalizzati),
                      terapia_intensiva = ifelse(is.na(terapia_intensiva), 0, terapia_intensiva),
                      ricoverati_con_sintomi = ifelse(is.na(ricoverati_con_sintomi), 0, ricoverati_con_sintomi),
                      isolamento_domiciliare = ifelse(is.na(isolamento_domiciliare), 0, isolamento_domiciliare),
                      ingressi_terapia_intensiva = ifelse(is.na(ingressi_terapia_intensiva), 0, ingressi_terapia_intensiva),
                      dimessi_guariti_cumsum = ifelse(is.na(dimessi_guariti), 0, dimessi_guariti),
                      dimessi_guariti = diff(c(0, dimessi_guariti_cumsum)),
                      deceduti_cumsum = ifelse(is.na(deceduti), 0, deceduti),
                      deceduti = diff(c(0, deceduti_cumsum)),
                      tamponi_test_antigenico_rapido = ifelse(is.na(tamponi_test_antigenico_rapido), 0, tamponi_test_antigenico_rapido),
                      tamponi_test_antigenico_rapido = ifelse(is.na(tamponi_test_antigenico_rapido), 0, tamponi_test_antigenico_rapido),
                      totale_positivi = ifelse(is.na(totale_positivi), 0, totale_positivi),
                      variazione_totale_positivi = ifelse(is.na(variazione_totale_positivi), 0, variazione_totale_positivi),
                      nuovi_positivi = ifelse(is.na(nuovi_positivi), 0, nuovi_positivi),
                      tamponi_cumsum = ifelse(is.na(tamponi), 0, tamponi),
                      tamponi = diff(c(0,tamponi_cumsum)),
                      casi_testati_cumsum = ifelse(is.na(casi_testati), 0, casi_testati),
                      casi_testati = diff(c(0, casi_testati_cumsum)),
                      totale_positivi_test_antigenico_rapido = ifelse(is.na(totale_positivi_test_antigenico_rapido), 0, totale_positivi_test_antigenico_rapido))
  
  
  DB <- DB %>%
    mutate(NevePiu = as.factor(ifelse(Snow.medio > 5, 1, 0))) %>%
    mutate(Eventi_piu = as.factor(ifelse(NevePiu == 1 | Eventi_piu == "Evento", 1, 0)))
  
  return(DB)
}
