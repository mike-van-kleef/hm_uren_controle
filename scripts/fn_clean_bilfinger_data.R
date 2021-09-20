CleanBilfingerData <- function(data, job_function = ""){
  # This function cleans the declaration data 
  #
  # Args:
  # - data: data frame with declaration data, in the form of output function import_files
  # - func: data frame with job_function data, in the form of output function import_files
  #
  # Returns:
  # - data: data frame with clean bilfinger data
  #

# delete rows with no names  
  data <- data[is.na(data$full_name) == FALSE, ]  
  
# columns with a date
index  = grepl("[0-9]",colnames(data))

# rename columns to date
colnames(data)[index] <- as.Date(as.numeric(colnames(data)[index]),origin = '1899-12-30')

# sort data by employee (common_id)
  data <- data %>%
    pivot_longer(
      cols = ,colnames(data)[index],
      names_to  = 'date_work',
      values_to = 'decl_working_hours' 
    ) %>% 
    mutate(
      date_work           = as.Date(as.numeric(date_work), origin = '1970-01-01'),
      common_id           = trimws(toupper(common_id)),
      decl_working_hours  = as.numeric(decl_working_hours),
      job_function        = tolower(trimws(job_function))
    ) %>%
    as.data.frame()

# remove rows with no declaration for working hours    
  data <- data[is.na(data$decl_working_hours) == FALSE,]


# clean job function
  job_function <- job_function %>%
    mutate(
      job_function      = tolower(trimws(job_function)),
      job_function_type = trimws(job_function_type)
    )
  
  
  
# Add job function type (direct vs indirect)    
  data <- left_join(data, job_function, by = c('job_function' = 'job_function'))

# remove columns
  data <- data %>%
    select(-c(decl_night_working_days,decl_night_working_hours,decl_night_working_hours_critic,
              decl_day_working_days,decl_day_working_hours,decl_day_working_hours_critic,decl_total_working_hours_critic
               )
           )

# Name contractor  
  data$contractor_decl = 'Bilfinger Maintenance'
  
  # aantal unieke common_id - full_name
  #nrow(unique(data[,c('common_id','full_name','job_function','site','night_shift')]))      

# NOG AANPASSEN IN BESTAND
  data <- data %>%
    mutate(
      
      common_id = case_when(
        full_name == 'Rodriquez, Pieter Jose Frederik Jan'~ '27021967RODR',
        full_name == 'Wisniewski, Marcin'                 ~ '11041981WISN',
        full_name == 'Mijnhals, Wesley'                   ~ '16101991MIJN',
        TRUE                                              ~  common_id 
        ),
      
      job_function = case_when(
        common_id == '01111978ORCU'                       ~ 'foreman',
        TRUE                                              ~ job_function
       )
    )

    
# Employee has two or more declaration on same day
  double_decl = plyr::count(data[,c('common_id','full_name','date_work')])
  print(double_decl[double_decl$freq > 1,])
  cat("\n")
  
# print total number of double declaration
  cat('number of double declaration on same day:', sum(double_decl[double_decl$freq > 1,]$freq - 1), '\n')
  cat('\n','Employee with different name for same common_id', '\n')
  
# Employee has two names with same common_id
  employee = unique(data[,c('common_id','full_name')])
  id_not_unique = plyr::count(employee[,c('common_id')]) 
  id_not_unique = id_not_unique[id_not_unique$freq >1,]
  print(employee[employee$common_id %in% id_not_unique$x & employee$common_i!= 'XXXX', ])    
  cat("\n")

  data <- data %>% 
    
    # double declaration same day
    group_by(common_id, full_name, date_work) %>%
    mutate(
      double_decl_same_day = if_else( n() >1, 'Ja', 'Nee'),
    ) %>% ungroup() %>%
    
    # double name same common_id
    group_by(common_id) %>%
    mutate(
      double_name_same_common_id = n_distinct(full_name)
    ) %>%
    ungroup() %>% as.data.frame()
  
  
# Aggregate to employee and date at work  
  data <- data %>%
    group_by(common_id, full_name, date_work, firma, night_shift, site, assignment, land, job_function, job_function_type, contractor_decl, double_decl_same_day, double_name_same_common_id) %>%
    summarise(
      tarif                    = max(tarif),
      decl_total_working_days  = sum(decl_total_working_days),
      decl_total_working_hours = sum(decl_total_working_hours),
      decl_working_hours       = sum(decl_working_hours)
    ) %>% as.data.frame()
  
return(data)  
  
}
  
  