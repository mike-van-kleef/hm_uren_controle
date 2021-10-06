CombineGateEmployee <- function(gate, employee){
  # This function combines the gate data with all employees of different contractors to extract the job_function_type of the employee
  #
  # Args:
  # - gate:     data frame with gate data, in the form of output function CleanGateData
  # - employee: data frame with employee data, in the form of output function ......
  #
  # Returns:
  # - data: data frame with gate data and employee data
 
  # remove rows with unknown common_id (XXXX)
  employee <- employee[employee$common_id != 'XXXX', ]
  
  # combine gate with employee. Gate is leading
  gate <- gate %>%
    left_join(employee, by = 'common_id') %>%
    mutate(
      job_function_type = if_else(is.na(job_function_type) == TRUE, 'UNKNOWN', job_function_type),
      personnel_type    = if_else(is.na(personnel_type) == TRUE, 'UNKNOWN', personnel_type)
    )

return(gate)  
  
}
  