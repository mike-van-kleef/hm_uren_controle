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
      delta_declaratie_vs_bruto_uren = round(as.numeric(decl_working_hours) - bruto_working_hours,1)
    )
  
  
    
return(uren_controle_person)
  
}