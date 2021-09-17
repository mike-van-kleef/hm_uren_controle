CombineMourikData <- function(pers, mint){
  # This function combines the declaration data of Mourik 
  #
  # Args:
  # - pers    : data frame with declaration data of Mourik Personeel, in the form of output function CleanMourikData
  # - mint    : data frame with declaration data of Mourik International, in the form of output function CleanMourikData
  #
  # Returns:
  # - data: data frame with declaration data of Mourik 
  #

  
# combine data of personeel and international
  data           <- rbind(pers, mint) %>% arrange(common_id, full_name)

# aggregate data to common_id and date_work    
  data_agg           <- data %>% 
    group_by(common_id) %>%
    mutate(
      full_name = max(full_name)
    ) %>% ungroup() %>%
    group_by(common_id, full_name, date_work, job_function_type, double_decl_same_day, double_name_same_common_id) %>%
    summarise(
      contractor_decl     = if_else( max(contractor_decl) == min(contractor_decl), max(contractor_decl) , 'Beide'),
      decl_working_hours  = sum(decl_working_hours) 
    ) %>% as.data.frame() 
  
  
  double_name_same_day <- plyr::count(unique(data[,c('date_work','common_id','full_name')]),c('date_work','common_id'))
  double_name_same_day <- double_name_same_day[!(double_name_same_day$common_id %in% c('XXX','xxx')),]
  cat("\n")
  cat('Employees with double declarations on same day for both departments:', '\n')
  cat("\n")
  print(double_name_same_day[double_name_same_day$freq >1,])
  cat("\n")
  
  double_name_same_common_id <- plyr::count(unique(data[,c('common_id','full_name')]),c('common_id'))
  double_name_same_common_id <- double_name_same_common_id[double_name_same_common_id$freq >1,]
  cat('Employees with same common_id and different names:', '\n')
  print(unique(data[data$common_id %in% double_name_same_common_id$common_id,c('common_id','full_name')]))
  cat("\n")

  
return(data_agg)  
  
}
  
  