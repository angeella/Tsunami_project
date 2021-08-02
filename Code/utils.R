`%notin%` <- Negate(`%in%`)

firstup <- function(x) {
  substr(x, 1, 1) <- toupper(substr(x, 1, 1))
  x
}

split_slots <- function(Slots){
  
  # transform into date time column (if it is not already one)
  # create breaks
  breaks <- c("0", "4", "7", "10", "12")
  # labels for the breaks
  labels <- c("Night", "Morning", "Afternoon", "Evening")
  
  Time_of_day <- cut(x=as.numeric(Slots), breaks = breaks, labels = labels, include.lowest=TRUE)
  
  return(Time_of_day)
}

split_hours <- function(Slots){
  
  # transform into date time column (if it is not already one)
  # create breaks
  breaks <- c("0", "7", "13", "19", "24")
  # labels for the breaks
  labels <- c("Night", "Morning", "Afternoon", "Evening")
  
  Time_of_day <- cut(x=as.numeric(Slots), breaks = breaks, labels = labels, include.lowest=TRUE)
  
  return(Time_of_day)
}

ff <- function() theme(panel.background = element_blank(),
                       panel.grid.major = element_blank(), 
                       panel.grid.minor = element_blank(),
                       axis.line = element_line(colour = "black"),
                       panel.border = element_rect(colour = "black", fill=NA, size=5))

trim <- function (x) gsub("^\\s+|\\s+$", "", x)


aggregateResultsHour <- function(new_data){
  
  new_data$Y <- ifelse(new_data$Y >1500, mean(new_data$Y[new_data$Y <1500]), new_data$Y)
  
  out = data.frame(Day = new_data$Day, 
                   Calls_Pred = round(new_data$Y),
                   TestDay = as.Date(new_data$Date)) %>% 
    group_by(TestDay) %>%
    mutate(Calls_Pred = sum(Calls_Pred)) %>% distinct() 
  
  
  return(out)
}

