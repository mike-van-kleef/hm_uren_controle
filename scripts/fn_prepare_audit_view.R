PrepareAuditView <- function(gate, employee){
  # his function selects the columns for the audit view
  #
  # Args:
  # - gate_correction     : data frame with gate data and the derived attributes in the form of output function CorrectionHours
  # - employee            : data frame with employee data from the different contractors, in the form of output function ......
  #
  # Returns:
  # - audit_view      : data frame with selected columns for the hours control of contractors
  #

 
  audit <- gate %>%
    select(
      common_id,
      last_name,
      first_name,
      job_function_type,
      location,
      site_ind_gate_ind,
      working_day,
      shift_type,
      first_clock,
      last_clock_buiten_site,
      datetime_check_in_out,
      time_check_in_out,
      workhours,
      working_days_without_checkout_correction_ind,
      correction_workhours_no_check_out,
      datetime_check_in_out_correction,
      correction_early_arrival,
      datetime_check_in_out_correction_end,
      correction_late_departed
    )

  # contractors
  contractors <- c('All',unique(employee$contractor_decl))
  
  # loop through all contractors and assign gate data to specific contractor 
  audit_list <- list()
  for (i in contractors){
    
    if (i == 'All'){ 
      audit_list[[i]] <- audit 
    } else{
      df <- audit %>% 
        inner_join(employee[employee$contractor_decl %in% i, ])
      
      i = gsub(" ", "_",trimws(i))
      audit_list[[i]] <- df
      #assign(paste0('audit_',i),df)     
      }
   }
  
    
return(audit_list)
  
}