EmployeeCheckAgg <- function(hours_check_employee_working_day){
  # This function aggregate the hours to employee level
  #
  # Args:
  # - employee_period : data frame with hours from contractors and gate data, in the form of output function CombineContractorGateAgg
  #
  # Returns:
  # - employee_check: data frame with hours at employee level
  #

  

  
  # employee check
  employee_check <- hours_check_employee_working_day %>%
    group_by(common_id, full_name, job_function_type, contractor_decl) %>%
    summarise(
      #contractor                            = max(if_else(is.na(contractor) ==TRUE, "",contractor)),
      job_function_type                     = max(if_else(is.na(job_function_type) ==TRUE, "",job_function_type)),
      tot_decl_working_days                 = sum(if_else(decl_working_hours > 0, 1, 0)),
      tot_decl_working_hours                = sum(decl_working_hours),
      tot_working_days                      = sum(if_else(bruto_working_hours > 0, 1, 0)),
      tot_bruto_working_hours               = sum(bruto_working_hours),
      tot_correction_early_arrival          = sum(tot_correction_early_arrival),
      tot_work_break                        = sum(work_break),
      tot_netto_working_hours               = sum(netto_working_hours),
      tot_delta_decl_vs_bruto_hours         = sum(delta_decl_vs_bruto_hours),
      tot_delta_decl_vs_netto_hours         = sum(delta_decl_vs_netto_hours)
    ) %>% 
    group_by(common_id) %>%
    mutate(
      duplicated_person                     = n_distinct(full_name)
    ) %>%
    ungroup() %>% as.data.frame()
  

    
return(employee_check)
  
}