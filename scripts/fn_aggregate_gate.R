AggregateGate <- function(gate, p_shift_start_day, p_shift_start_night, p_shift_end_day, p_shift_end_night, p_hour){
  # This function aggregate the gate data to the level person, workday
  #
  # Args:
  # - gate : data frame with aggregated gate data, in the form of output function CombineGateEmployee
  # - p_shift_start_day   : parameter specifies the start of day shift
  # - p_shift_start_night : parameter specifies the start of night shift
  # - p_shift_end_day     : parameter specifies the end of day shift
  # - p_shift_end_night   : parameter specifies the end of night shift
  # - p_hour              : parameter specifies the range for correction of workinghours for being early or late.
  #
  # Returns:
  # - gate_agg: data frame with aggregated gate data
  #
  
  
# Aggregate gate data to level workingday
  gate_agg <- gate %>%
    filter(site_ind_gate_ind != 'Buiten site_UIT') %>%
    group_by(common_id, first_name, last_name, contractor, job_function_type, shift_type, working_day, duplicate_function_type, first_clock, last_clock, last_clock_buiten_site, number_of_clocks, check_first_before_last_ind) %>%
    summarise(
      bruto_working_hours = sum(workhours)
      ) %>%
    as.data.frame()

  
# determine correction_early_arrival
  gate_agg$start_dayshift     <- p_shift_start_day    # add shift_start_day
  gate_agg$start_nightshift   <- p_shift_start_night  # add shift_start_night
  
  gate_agg <- gate_agg %>%
    mutate(
      
      first_clock_date             = format(gate_agg$first_clock, format = "%Y-%m-%d"),
      first_clock_time             = hms(format(gate_agg$first_clock, format = "%H:%M:%S")),
      
      
      date_check_in_out_correction_agg = case_when(
        
        toupper(job_function_type) == "DIRECT" & shift_type == "dagshift"
        & first_clock_time > (hms(p_shift_start_day) - hours(p_hour)) 
        & first_clock_time < hms(p_shift_start_day)                         ~ as.POSIXct(paste0(first_clock_date, " ", start_dayshift), format = "%Y-%m-%d %H:%M:%S", tz = 'UTC'),
        
        toupper(job_function_type) == "DIRECT" & shift_type == "nachtshift"
        & first_clock_time > (hms(p_shift_start_night) - hours(p_hour)) 
        & first_clock_time < hms(p_shift_start_night)                       ~ as.POSIXct(paste0(first_clock_date, " ", start_nightshift), format = "%Y-%m-%d %H:%M:%S", tz = 'UTC'),
        
        TRUE                                                                ~ first_clock
      ),
      
      correction_early_arrival     =  round(difftime( date_check_in_out_correction, first_clock , units = "hours"),3)   
    ) 
  
  
    
    

return(gate_agg)

}
