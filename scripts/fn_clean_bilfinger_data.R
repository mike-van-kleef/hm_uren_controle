CleanBilfingerData <- function(data, job_function = ""){
  # This function cleans the gate data 
  #
  # Args:
  # - data: data frame with gate data, in the form of output function import_files
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
      common_id           = toupper(common_id),
      decl_working_hours  = as.numeric(decl_working_hours)
    ) %>%
    as.data.frame()

# remove rows with no declaration for working hours    
  data <- data[is.na(data$decl_working_hours) == FALSE,]

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
  
# NOG AANPASSEN IN BESTAND
  data <- data %>%
    mutate(
      common_id = case_when(
        full_name == 'Rodriquez, Pieter Jose Frederik Jan'~ '27021967RODR',
        full_name == 'Wisniewski, Marcin'                 ~ '11041981WISN',
        TRUE                                              ~  common_id)
    )
    
return(data)  
  
}
  
  