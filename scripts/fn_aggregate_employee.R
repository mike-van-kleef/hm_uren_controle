EmployeeCheckAgg <- function(hours_check_employee_working_day){
  # This function aggregate the hours to employee level
  #
  # Args:
  # - employee_period : data frame with hours from contractors and gate data, in the form of output function CombineContractorGateAgg
  #
  # Returns:
  # - employee_check: data frame with hours at employee level
  #

  

  
  # employee with known common id. Replace NAs from attributes
  employee_check_known_id <- hours_check_employee_working_day %>%
    filter(common_id != 'XXXX') %>%
    group_by(common_id) %>%
    mutate(
      full_name                             = max(full_name, na.rm = TRUE),
      contractor_decl                       = max(contractor_decl, na.rm = TRUE)
    ) %>% ungroup() %>% as.data.frame()
    
  
  # employee with unknown common id. 
  employee_check_unknown_id <- hours_check_employee_working_day %>%
    filter(common_id == 'XXXX') 
  
  
  # bind data frames  
  employee_check = rbind(employee_check_known_id,employee_check_unknown_id)

  employee_check <- employee_check %>%    
    group_by(common_id, full_name, job_function_type, contractor_decl) %>%
    summarise(
      double_decl_same_day                    = max(double_decl_same_day, na.rm = TRUE),
      double_name_same_common_id              = max(double_name_same_common_id, na.rm = TRUE),
      tot_decl_working_days                   = sum(if_else(decl_working_hours > 0, 1, 0)),
      tot_decl_working_hours                  = sum(decl_working_hours),
      tot_working_days                        = sum(if_else(bruto_working_hours > 0, 1, 0)),
      tot_hours_on_site                       = sum(tot_hours_on_site),
      tot_correction_no_check_out             = sum(tot_correction_no_check_out),
      bruto_working_hours                     = sum(bruto_working_hours),
      tot_correction_early_arrival            = sum(tot_correction_early_arrival),
      tot_correction_late_departed            = sum(tot_correction_late_departed),
      tot_work_break                          = sum(work_break_cor_off_site),
      tot_change_of_dress_time                = sum(change_of_dress_time),
      tot_netto_working_hours                 = sum(netto_working_hours),
      tot_netto_working_hours_cor             = sum(netto_working_hours_cor),
      tot_delta_decl_vs_bruto_hours           = sum(delta_decl_vs_bruto_hours),
      tot_delta_decl_vs_netto_hours           = sum(delta_decl_vs_netto_hours),
      tot_delta_decl_vs_netto_hours_cor       = sum(delta_decl_vs_netto_hours_cor)
    ) %>% 
    mutate(
      tot_delta_decl_vs_netto_hours_pos       = if_else(tot_delta_decl_vs_netto_hours_cor < 0, 0, tot_delta_decl_vs_netto_hours_cor),
      remark = case_when(
        common_id               == 'XXXX'                                        ~ 'Employee not found',
        tot_netto_working_hours == 0                                             ~ 'Employee found - no gate clocking for period',
        TRUE                                                                     ~ ''
        )
      ) %>% as.data.frame()
  
  
  
  

    
return(employee_check)
  
}