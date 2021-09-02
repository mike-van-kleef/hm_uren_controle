CombineContractorGateAgg <- function(contractor, gate_agg, employee){
  # This function combines the contractor data with the gata data on workday level
  #
  # Args:
  # - contractors : data frame with contractors data, in the form of output function .........
  # - gate_agg    : data frame with aggregated gate data on workday level, in the form of output function AggregateGate
  # - employee    : data frame with employee data from the different contractors, in the form of output function ......
  #
  # Returns:
  # - uren_controle_person: data frame with contractors data and gate data 
  #

  # remove rows with unknown common_id (XXXX)
  employee <- employee[employee$common_id != 'XXXX', ]
  
  # Keep only gate data which matches the employees from the different contractors
  gate_agg <- gate_agg[gate_agg$common_id %in% employee$common_id,]
    

  
  uren_controle_person  <- contractor %>%
    full_join(gate_agg, by = c('common_id' = 'common_id', 'date_work' = 'working_day', 'job_function_type' = 'job_function_type')) %>%
    
    mutate(
      decl_working_hours               = if_else(is.na(decl_working_hours)  ==TRUE, 0, decl_working_hours),
      bruto_working_hours              = if_else(is.na(bruto_working_hours) ==TRUE, 0, bruto_working_hours),
      netto_working_hours              = if_else(is.na(netto_working_hours) ==TRUE, 0, netto_working_hours),
    ) %>%
    
    mutate(
      delta_declaration_vs_bruto_hours = round(decl_working_hours - bruto_working_hours,2),
      delta_declaration_vs_netto_hours = round(decl_working_hours - netto_working_hours,2)
    )
  
  
    
return(uren_controle_person)
  
}