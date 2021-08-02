
##Create TEST SET

#DB: dataset
#nday: how many day ahead predictions
#level: soreu or province
#zona: inside the level

create_newdataHour <- function(DB, nday, level, zona, full = TRUE){
  
  level_set <- c("soreu", "province")
  zona_set <- c("pianura", "laghi", "como", "cremona", "lecco", "lodi", "mantova", "pavia", "varese", "monza")
  
  level <- match.arg(tolower(level), level_set)
  
  #check consistency zona-level
  if(level =="soreu" & zona %in% c("como", "cremona", "lecco", "lodi", "mantova", "pavia", "varese")){
    stop("Please insert laghi or pianura")}
  if(level =="province" & zona %in% c("pianura", "laghi")){
    stop("Please insert a province")}
  
  if(level == "soreu") {zona <- toupper(zona)}
  if(level == "province") {zona <- firstup(zona)}
  
  Date_newdata = max(DB$Date) + nday
  Time_newdata = max(DB$Time) + nday
  newdata <- data.frame(Date = rep(Date_newdata,24),
                        Time = rep(Time_newdata,24),
                        Hour = c(0:23),
                        Day = rep(factor(weekdays(as.Date(Date_newdata)),
                                         levels = c("Monday","Tuesday","Wednesday", "Thursday", "Friday","Saturday","Sunday")),24))
  
  newdata <- newdata %>% 
    mutate(Week = lubridate::week(ymd(Date))) %>%
    mutate(Month = lubridate::month(ymd(Date)),
           Quarter = lubridate::quarter(ymd(Date)),
           Day_num = lubridate::day(ymd(Date)),
           Month_Day = as.factor(paste0(Month,"-", Day_num)),
           Year = lubridate::year(ymd(Date)),
           Date = as.Date(Date),
           Day_year = lubridate::yday(ymd(Date)),
           Day_month = lubridate::mday(ymd(Date)),
           Day_week = lubridate::wday(ymd(Date)),
           Day_quarter = lubridate::qday(ymd(Date)))
  
  newdata <- newdata %>% 
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
  
  newdata$Eventi_piu <- as.factor(newdata$Eventi_piu)
  levels(newdata$Eventi_piu) <- c(0,1)
  newdata <- newdata %>% mutate(DayWeekend = ifelse(Day %in% c("Saturday", "Sunday"), "Weekend", "Weekdays"),
                                Year.dummy = ifelse(Year == 2020, 1, 0),
                                Month_n = as.numeric(Month),
                                Day_n = as.numeric(Day),
                                Year_n = as.numeric(Year))
  newdata$Week <- ifelse(newdata$Week == max(newdata$Week), 
                         52, newdata$Week)
  newdata$Month_f <- factor(newdata$Month, ordered = T)
  newdata$Day_f <- factor(newdata$Day, ordered = T)
  newdata$Year_f <- factor(newdata$Year, ordered = T)
  newdata <- newdata %>%
    mutate(DayP = as.factor(ifelse(Day %in% c("Tuesday", "Wednesday", "Thursday", "Friday"),1,0)))
  
  #Forecast to updated with arpa data
  if(level == "soreu"){
    DB <- DB %>% filter(Soreu == zona)
  }else{
    DB <- DB %>%
      dplyr::select(-c("Soreu")) %>%
      group_by(Date, Province) %>%
      mutate(calls = sum(calls)) %>% distinct()
    
    DB <- DB %>% filter(Province == zona)
  }
  
  if(full){
    fit <- forecast::baggedModel(na.omit(DB$R.medio))
    fcast <- forecast(fit,h = nday)
    newdata$R.medio <- fcast$mean[nday]
    
    fit <- forecast::baggedModel(na.omit(DB$Temp.delta))
    fcast <- forecast(fit,h = nday)
    newdata$Temp.delta <- fcast$mean[nday]
    
    fit <- forecast::baggedModel(na.omit(DB$Prep.medio))
    fcast <- forecast(fit,h = nday)
    newdata$Prep.medio <- fcast$mean[nday]
    
    fit <- forecast::baggedModel(na.omit(DB$Temp.medio))
    fcast <- forecast(fit,h = nday)
    newdata$Temp.medio <- fcast$mean[nday]
    
    fit <- forecast::baggedModel(na.omit(DB$Temp.delta16))
    fcast <- forecast(fit,h = nday)
    newdata$Temp.delta16 <- fcast$mean[nday]
    
    fit <- forecast::baggedModel(na.omit(DB$Rt_hosp_lead1))
    fcast <- forecast(fit,h = nday)
    newdata$Rt_hosp_lead1 <- fcast$mean[nday]
    
    fit <- forecast::baggedModel(na.omit(DB$Rt_hosp_lead2))
    fcast <- forecast(fit,h = nday)
    newdata$Rt_hosp_lead2 <- fcast$mean[nday]
    
    fit <- forecast::baggedModel(na.omit(DB$totale_ospedalizzati))
    fcast <- forecast(fit,h = nday)
    newdata$totale_ospedalizzati <- fcast$mean[nday]
    #newdata$totale_ospedalizzati_lag1 <- DB$totale_ospedalizzati_lag1[which.max(DB$Date)]
    fit <- forecast::baggedModel(na.omit(DB$Tot_casi))
    fcast <- forecast(fit,h = nday)
    newdata$Tot_casi <- fcast$mean[nday]
  }
  
  newdata$Tot_casiLag5 <- DB$Tot_casi[which(DB$Date == max(DB$Date)-4)]
  newdata$totale_ospedalizzati_lag1 <- DB$totale_ospedalizzati[which(DB$Date == max(DB$Date))]
  
  newdata$Flu <- DB$Flu[which(DB$Date == max(DB$Date))]
  newdata$Flu1 <- DB$Flu[which(DB$Date == max(DB$Date))]
  newdata$Flu2 <- DB$Flu[which(DB$Date == max(DB$Date)-1)]
  newdata$R.medio1 <- DB$R.medio[which(DB$Date == max(DB$Date))]
  newdata$R.medio2 <- DB$R.medio[which(DB$Date == max(DB$Date)-1)]
  
  newdata$Y_lagDay1 <- DB$Y[which(DB$Date == max(DB$Date))]
  newdata$Y_lagDay2 <- DB$Y[which(DB$Date == max(DB$Date)-1)]
  newdata$Y_lagDay3 <- DB$Y[which(DB$Date == max(DB$Date)-2)]
  newdata$Y_lagDay4 <- DB$Y[which(DB$Date == max(DB$Date)-3)]
  newdata$Y_lagDay5 <- DB$Y[which(DB$Date == max(DB$Date)-4)]
  newdata$Y_lagDay6 <- DB$Y[which(DB$Date == max(DB$Date)-5)]
  newdata$Y_lagDay7 <- DB$Y[which(DB$Date == max(DB$Date)-6)]
  newdata$Y_lagDay18 <- DB$Y[which(DB$Date == max(DB$Date)-17)]
  newdata$Y_lagDay30 <- DB$Y[which(DB$Date == max(DB$Date)-29)]
  newdata$Y_lagDay21 <- DB$Y[which(DB$Date == max(DB$Date)-20)]
  
  newdata$tamponi <- DB$tamponi[which(DB$Date == max(DB$Date))]
  
  newdata$Y_roll2_mean <- DB$Y_roll2_mean[which(DB$Date == max(DB$Date))]
  newdata$Y_roll3_mean <- DB$Y_roll3_mean[which(DB$Date == max(DB$Date))]
  newdata$Y_roll4_mean <- DB$Y_roll4_mean[which(DB$Date == max(DB$Date))]
  newdata$Y_roll5_mean <- DB$Y_roll5_mean[which(DB$Date == max(DB$Date))]
  newdata$Y_roll6_mean <- DB$Y_roll6_mean[which(DB$Date == max(DB$Date))]
  newdata$Y_roll8_mean <- DB$Y_roll8_mean[which(DB$Date == max(DB$Date))]
  newdata$Y_roll7_mean <- DB$Y_roll7_mean[which(DB$Date == max(DB$Date))]
  newdata$Y_roll9_mean <- DB$Y_roll9_mean[which(DB$Date == max(DB$Date))]
  newdata$Y_roll10_mean <- DB$Y_roll10_mean[which(DB$Date == max(DB$Date))]
  newdata$Y_roll11_mean <- DB$Y_roll11_mean[which(DB$Date == max(DB$Date))]
  newdata$Y_roll12_mean <- DB$Y_roll12_mean[which(DB$Date == max(DB$Date))]
  newdata$Y_roll13_mean <- DB$Y_roll13_mean[which(DB$Date == max(DB$Date))]
  newdata$Y_roll14_mean <- DB$Y_roll14_mean[which(DB$Date == max(DB$Date))]
  newdata$Y_roll30_mean <- DB$Y_roll30_mean[which(DB$Date == max(DB$Date))]
  
  newdata$Tot_casi_lead1 <- DB$Tot_casi_lead1[which(DB$Date == max(DB$Date))]
  newdata$Tot_casi_lead2 <- DB$Tot_casi_lead2[which(DB$Date == max(DB$Date))]
  newdata$Tot_casi_lead3 <- DB$Tot_casi_lead3[which(DB$Date == max(DB$Date))]
  newdata$Tot_casi_lead4 <- DB$Tot_casi_lead4[which(DB$Date == max(DB$Date))]
  newdata$Tot_casi_lead5 <- DB$Tot_casi_lead5[which(DB$Date == max(DB$Date))]
  
  newdata$Y_roll7_lagDay1 <- DB$Y_roll7_lagDay1[which(DB$Date == max(DB$Date))]
  newdata$Y_roll14_lagDay1 <- DB$Y_roll14_lagDay1[which(DB$Date == max(DB$Date))]
  newdata$Y_roll30_lagDay1 <- DB$Y_roll30_lagDay1[which(DB$Date == max(DB$Date))]
  newdata$Y_roll7_lagDay2 <- DB$Y_roll7_lagDay2[which(DB$Date == max(DB$Date))]
  newdata$Y_roll14_lagDay2 <- DB$Y_roll14_lagDay2[which(DB$Date == max(DB$Date))]
  newdata$Y_roll30_lagDay2 <- DB$Y_roll30_lagDay2[which(DB$Date == max(DB$Date))]
  
  newdata$Temp.delta16_lead1 <- DB$Temp.delta16_lead1[which(DB$Date == max(DB$Date))]
  newdata$Temp.delta16_lead2 <- DB$Temp.delta16_lead2[which(DB$Date == max(DB$Date))]
  
  newdata$delta161 <- DB$delta161[which(DB$Date == max(DB$Date))]
  newdata$Temp.medio2 <- DB$Temp.medio2[which(DB$Date == max(DB$Date))]
  newdata$Temp.delta161 <- DB$Temp.delta16[which(DB$Date == max(DB$Date))]
  newdata$Temp.medio1 <- DB$Temp.medio1[which(DB$Date == max(DB$Date))]
  newdata$Prep.medio1 <- DB$Prep.medio[which(DB$Date == max(DB$Date))]
  
  newdata <- newdata %>%
    group_by(Date) %>%
    mutate(Y_lagDayTot1 = sum(Y_lagDay1),
           Y_lagDayTot2 = sum(Y_lagDay2),
           Y_lagDayTot3 = sum(Y_lagDay3),
           Y_lagDayTot4 = sum(Y_lagDay4),
           Y_lagDayTot5 = sum(Y_lagDay5),
           Y_lagDayTot6 = sum(Y_lagDay6),
           Y_lagDayTot7 = sum(Y_lagDay7)) %>% ungroup()
  newdata$Month <-   factor(newdata$Month,levels = levels(DB$Month))
  
  newdata$Temp.delta161 <- ifelse(is.na(newdata$Temp.delta161),
                                  na.locf(DB$Temp.delta16)[which.max(DB$Date)],
                                  newdata$Temp.delta161)
  
  return(newdata)
}