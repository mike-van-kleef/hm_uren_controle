CombineContractorGateAgg <- function(contractor, gate_agg, employee){
  # This function combines the contractor data with the gata data on workday level
  #
  # Args:
  # - contractors : data frame with contractors data, in the form of output function .........
  # - gate_agg    : data frame with aggregated gate data on workday level, in the form of output function AggregateGate
  # - employee    : data frame with employee data from the different contractors, in the form of output function ......
  #
  # Returns:
  # - hours_check_employee_working_day: data frame with contractors data and gate data at the level employee, working day
  #

  # remove rows with unknown common_id (XXXX)
  employee <- employee[employee$common_id != 'XXXX', ]
  
  # Keep only gate data which matches the employees from the different contractors
  gate_agg <- gate_agg[gate_agg$common_id %in% employee$common_id,]
  
  
  # Last Date at Work
  contractor <- contractor %>%
    group_by(common_id) %>%
    mutate(
      first_work_day_contractor         = min(date_work), 
      last_work_day_contractor          = max(date_work),
      job_function_type_direct_indirect = max(job_function_type)  # Indirect gaat voor Direct
    ) %>%
    select(-c(job_function_type)) %>%
    ungroup %>% as.data.frame()
  

  hours_check_employee_working_day  <- contractor %>%
    full_join(gate_agg, by = c('common_id' = 'common_id', 'date_work' = 'working_day', 'job_function_type_direct_indirect' = 'job_function_type')) %>%
    
    # Include only gate data in the period of declaration
    filter(date_work >= first_work_day_contractor -1 & date_work <= last_work_day_contractor + 1) %>%
    
    mutate(
      decl_working_hours               = if_else(is.na(decl_working_hours)           ==TRUE, 0, decl_working_hours),
      bruto_working_hours              = if_else(is.na(bruto_working_hours)          ==TRUE, 0, bruto_working_hours),
      tot_correction_early_arrival     = if_else(is.na(tot_correction_early_arrival) ==TRUE, 0, tot_correction_early_arrival),
      work_break                       = if_else(is.na(work_break)                   ==TRUE, 0, work_break),
      netto_working_hours              = if_else(is.na(netto_working_hours)          ==TRUE, 0, netto_working_hours),
    ) %>%
    
    mutate(
      delta_declaration_vs_bruto_hours = round(decl_working_hours - bruto_working_hours,2),
      delta_declaration_vs_netto_hours = round(decl_working_hours - netto_working_hours,2)
    ) %>%

     select(
       common_id,
       full_name,
       last_name_gate                      = last_name,
       first_name_gate                     = first_name,
       double_decl_same_day,
       double_name_same_common_id,
       contractor_decl,
       job_function,
       job_function_type                   = job_function_type_direct_indirect,
       duplicate_function_type,
       date_work,
       decl_total_working_days,
       decl_working_hours,
       correction_start_shift_ind,
       bruto_working_hours,
       tot_correction_early_arrival,
       work_break,
       netto_working_hours,
       delta_declaration_vs_bruto_hours,
       delta_declaration_vs_netto_hours,
       first_clock,
       last_clock_buiten_site,
       working_days_without_checkout_correction_ind
       #remark
       
     )
    
  
    
return(hours_check_employee_working_day)
  
}