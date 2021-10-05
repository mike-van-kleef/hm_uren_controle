ContractorCheckAgg <- function(employee){
  # This function aggregate the hours to contractor level
  #
  # Args:
  # - employee : data frame with hours at employee level, in the form of output function EmployeeCheckAgg
  #
  # Returns:
  # - contractor_check: data frame with hours at employee level
  #

  

  
  # summary on contractor level
  contractor_check <- employee %>%    
    group_by(contractor_decl) %>%
    summarise(
      decl_working_days                   = round(sum(tot_decl_working_days),0),
      decl_working_hours                  = round(sum(tot_decl_working_hours),0),
      working_days                        = round(sum(tot_working_days),0),
      hours_on_site                       = round(sum(tot_hours_on_site),0),
      correction_no_check_out             = round(sum(tot_correction_no_check_out),0),
      gross_working_hours                 = round(sum(gross_working_hours),0),
      correction_early_arrival            = round(sum(tot_correction_early_arrival),0),
      correction_late_departed            = round(sum(tot_correction_late_departed),0),
      work_break                          = round(sum(tot_work_break),0),
      change_of_dress_time                = round(sum(tot_change_of_dress_time),0),
      workday_gate_not_in_invoice         = round(sum(tot_net_working_hours) - sum(tot_net_working_hours_cor),0),
      net_working_hours                   = round(sum(tot_net_working_hours),0),
      delta_decl_vs_net_hours             = round(sum(tot_delta_decl_vs_net_hours),0),
      net_working_hours_cor               = round(sum(tot_net_working_hours_cor),0),
      delta_decl_vs_net_hours_cor         = round(sum(tot_delta_decl_vs_net_hours_cor),0),
      delta_decl_vs_net_hours_pos         = round(sum(tot_delta_decl_vs_net_hours_pos),0)
    ) %>% t() %>% as.data.frame()

    
return(contractor_check)
  
}