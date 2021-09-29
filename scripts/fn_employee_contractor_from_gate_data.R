EmployeeContractorFromGate <- function(gate){
  
  # This function determine the employees from the gate data
  #
  # Args:
  # - gate            : data frame with employee data, in the form of output function CleanGateData
  #
  # Returns:
  # - data: data frame with employees. The key on an employee is 'last_name','first_name','common_id','contractor'
  #

  employee <- unique(gate[, c('last_name','first_name','common_id','contractor')])
  employee <- employee %>% 
    filter(! tolower(last_name) %in% c('chauffeur','unknown')) %>%
    arrange(last_name,first_name,common_id,contractor) %>%
    group_by(last_name, first_name) %>%
    mutate(
      n_common_id  = n_distinct(common_id),
      n_contractor = n_distinct(contractor)
    ) %>%
    ungroup() %>% as.data.frame()
  
  contractor_agg <- employee %>%
    group_by(contractor) %>%
    summarise(
      n_employee           = n(),
      n_common_id          = n_distinct(common_id),
      n_lastname_firstname = n_distinct(paste0(last_name,first_name))
    ) %>% arrange(desc(n_employee)) %>% as.data.frame()
  
  employee_contractor = list(employee,contractor_agg)
  
return(employee_contractor)  
  
}