SelectEmployee <- function(contractors){
  
  # This function determine the employees from all contractors
  #
  # Args:
  # - employee            : data frame with employee data, in the form of output function .....
  #
  # Returns:
  # - data: data frame with unique employees. The key on an employee is common_id 
  #


# select employees
  df.employee              <- unique(contractors[, c('full_name','common_id','job_function_type', 'contractor_decl')])   


# determine number of function_type for the same common_id  
# select only valid common_ids
# sort by job_function_type. If someone is direct and indirect employee, then indirect is selected
  
  df.employee  <- df.employee %>%
    
    filter(common_id != 'XXXX') %>%
    group_by(common_id) %>%
    mutate(
      duplicate_function_type  = n()
      ) %>%
    arrange(common_id, desc(job_function_type)) %>%
    ungroup() %>% as.data.frame()
  
# print duplicates  
  print(df.employee[df.employee$duplicate_function_type >1 & df.employee$common_id != 'XXXX',])
  
# print number of unique common_ids  
  cat('number of unique common_ids:',length(unique(df.employee$common_id)))

# remove duplicates.   
  df.employee <- df.employee[!duplicated(df.employee$common_id),]
  
return(df.employee)  
  
}