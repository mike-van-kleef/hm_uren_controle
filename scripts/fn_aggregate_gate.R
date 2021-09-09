AggregateGate <- function(gate, p_shift_start_day, p_shift_start_night, p_shift_end_day, p_shift_end_night, p_hour, 
                          p_work_break_threshhold, p_work_break_small, p_work_break_normal, p_work_break_min_threshhold, p_change_of_dress_time){
  # This function aggregate the gate data to the level person, workday
  #
  # Args:
  # - gate : data frame with aggregated gate data, in the form of output function CombineGateEmployee
  # - p_shift_start_day           : parameter specifies the start of day shift
  # - p_shift_start_night         : parameter specifies the start of night shift
  # - p_shift_end_day             : parameter specifies the end of day shift
  # - p_shift_end_night           : parameter specifies the end of night shift
  # - p_hour                      : parameter specifies the range for correction of workinghours for being early or late.
  # - p_work_break_threshhold     : parameter specifies the threshhold for small or normal work_break
  # - p_work_break_small          : parameter specifies time for small work break
  # - p_work_break_normal         : parameter specifies time for normal work break
  # - p_work_break_min_threshhold : parameter specifies the minimum threshhold. Below this threshold there is no work_break
  # - p_change_of_dress_time      : parameter specifies time for changing of dress
  #
  # Returns:
  # - gate_agg: data frame with aggregated gate data
  #
  

 
  
# Aggregate gate data to level workingday
  gate_agg <- gate %>%
    filter(site_ind_gate_ind != 'Buiten site_UIT') %>%
    group_by(common_id, common_id_unique_ind, job_function_type, shift_type, working_day, duplicate_function_type, 
             first_clock, first_clock_time, last_clock, last_clock_buiten_site, deviating_start_shift, number_of_clocks) %>%
    summarise(
      tot_hours_on_site                            = sum(workhours),
      tot_correction_no_check_out                  = sum(correction_no_check_out),
      bruto_working_hours                          = sum(workhours - correction_no_check_out),
      tot_correction_early_arrival                 = sum(correction_early_arrival), 
      correction_start_shift_ind                   = max(correction_start_ind, na.rm = TRUE),
      tot_correction_late_departed                 = sum(correction_late_departed), 
      correction_end_shift_ind                     = max(correction_end_ind, na.rm = TRUE),
      working_days_without_checkout_correction_ind = max(working_days_without_checkout_correction_ind),   
      first_name                                   = min(first_name),  
      last_name                                    = min(last_name),   
      contractor                                   = min(contractor)   
      ) %>%
    mutate(
      
      # Determine Work Break
      work_break            = case_when(
        bruto_working_hours <= p_work_break_min_threshhold                                                               ~ 0,
        bruto_working_hours > p_work_break_min_threshhold & bruto_working_hours <= p_work_break_threshhold               ~ p_work_break_small,
        TRUE                                                                                                             ~ p_work_break_normal,
        ),
      
      # change_of_dress_time
      change_of_dress_time  = case_when(
        toupper(job_function_type) == "DIRECT"                                   ~ p_change_of_dress_time,
        TRUE                                                                     ~ 0
      ),
      
      # Determine Netto Working Hours
      netto_working_hours          = bruto_working_hours - tot_correction_early_arrival - tot_correction_late_departed - work_break - change_of_dress_time
      
      ) %>% as.data.frame()

# Check records
   df_controle  <- gate %>%  filter(site_ind_gate_ind != 'Buiten site_UIT')
   n_unique_ids <- nrow(unique(df_controle[,c('common_id','working_day')]))
   
   cat('Is number of unique ids equal to number of records data frame?', nrow(gate_agg) == n_unique_ids, '\n')
   cat('number of records:', nrow(gate_agg), '\n')
   cat('number of unique ids:', n_unique_ids, '\n')
   

    

return(gate_agg)

}
