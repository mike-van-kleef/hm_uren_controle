CleanBisData <- function(data, job_function = ""){
  # This function cleans the declaration data 
  #
  # Args:
  # - data: data frame with declaration data, in the form of output function import_files
  # - func: data frame with job_function data, in the form of output function import_files
  #
  # Returns:
  # - data: data frame with clean BIS data
  #

# delete rows with no names  
  data <- data[is.na(data$full_name) == FALSE, ] 
  
# declaration working hours 
# delete rows with no declarations  
  data <- data %>%
    mutate(
      decl_hours_dayshift   = if_else(is.na(decl_hours_dayshift)  ==TRUE, 0, decl_hours_dayshift),
      decl_hours_nightshift = if_else(is.na(decl_hours_nightshift)==TRUE, 0, decl_hours_nightshift),
      decl_hours_overtime   = if_else(is.na(decl_hours_overtime)  ==TRUE, 0, decl_hours_overtime),
      decl_working_hours    = decl_hours_dayshift + decl_hours_nightshift + decl_hours_overtime
  ) %>%
    filter(decl_working_hours > 0)
  
  



# Clean data
  data <- data %>%
    mutate(
      common_id = case_when(
        is.na(common_id) == TRUE              ~ 'XXXX',
        TRUE                                  ~ toupper(common_id)
      ),
      
      # NOG AANPASSEN IN BESTAND
      full_name = case_when(
        full_name == 'vrijdag, M. (menno)'            ~ 'Vrijdag, M. (Menno)',
        full_name == 'Steen, M.M. van der (martin)'   ~ 'Steen, M.M. van der (Martin)',
        full_name == 'Kuczkowski, A.P. (Andrzej'      ~ 'Kuczkowski, A.P. (Andrzej)',
        full_name == 'Sochaj, A.(Andrzej)'            ~ 'Sochaj, A. (Andrzej)',
        full_name == 'Grzebieniarz, P.Z. (Pawel'      ~ 'Grzebieniarz, P.Z. (Pawel)',
        full_name == 'Wiczynski, g. (Grzegorz)'       ~ 'Wiczynski, G. (Grzegorz)',
        TRUE                                          ~ full_name
        ),
      
      job_function_type = case_when(
        job_function_type == 'direct'                   ~ 'Direct',  
        job_function_type == 'indirect'                 ~ 'Indirect'  ,
        TRUE                                            ~ job_function_type
      ),
      
      week_nr      = sub("21-*|20-*", "", tolower(week_nr))
      )

# delete rows without common_id
  cat('\n','Employee without common_id', '\n')
  teller <- nrow(data[is.na(data$common_id) == TRUE,])
  if(teller > 0 ){
  print(data %>% filter(is.na(common_id) == TRUE))}

    

 data_agg <- data %>%
   group_by(common_id, full_name, week_nr,date_work) %>%
   summarise(
     counter                   = n(),
     different_plants_same_day = n_distinct(plant),
     double_decl_same_day      = if_else(counter > different_plants_same_day, 1, 0),
     decl_total_working_days   = -1,
     decl_working_hours        = sum(decl_working_hours),
     job_function              = 'Onbekend',
     job_function_type         = max(job_function_type)
   ) %>% as.data.frame()
   
 

# No correction on early arrival and late departed. Hereby all job_function_type are indirect
# Add attributes
 data_agg <- data_agg %>%
    mutate(
      contractor_decl   = 'BIS'
    ) %>%
    # double name same common_id
    group_by(common_id) %>%
    mutate(
      double_name_same_common_id = n_distinct(full_name),
      job_function_type          = max(job_function_type, na.rm = TRUE) # if person has multiple job_function_types then Indirect is Taken before Direct.
    ) %>%
    ungroup() %>% 
    arrange(common_id, full_name, week_nr, date_work) %>% as.data.frame()




    
# Employee has two or more declaration on same day
  cat("\n")
  cat("Employee has two or more declaration on same day","\n")
  print(data_agg[data_agg$double_decl_same_day > 1,])
  cat("\n")
  
# print total number of double declaration
  cat('number of double declaration on same day:', sum(data_agg[data_agg$double_decl_same_day > 1,]$double_decl_same_day -1), '\n')
  
  
  
# Employee has two names with same common_id
  cat('\n','Employee with different name for same common_id', '\n')
  employee = unique(data_agg[,c('common_id','full_name')])
  id_not_unique = plyr::count(employee[,c('common_id')]) 
  id_not_unique = id_not_unique[id_not_unique$freq >1,]
  print(employee[employee$common_id %in% id_not_unique$x & employee$common_i!= 'XXXX', ])    
  cat("\n")

  
  
return(data_agg)  
  
}
  
  