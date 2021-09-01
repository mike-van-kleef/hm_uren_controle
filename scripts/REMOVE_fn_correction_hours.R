CorrectionHours <- function(data, p_shift_start_day, p_shift_start_night, p_shift_end_day, p_shift_end_night, p_hour ){
  # This function corrects workhours for direct employees for being early. Employees can only start when shift starts
  #
  # Args:
  # - data                : data frame with gate data, in the form of output function import_files
  # - p_shift_start_day   : parameter specifies the start of day shift
  # - p_shift_start_night : parameter specifies the start of night shift
  # - p_shift_end_day     : parameter specifies the end of day shift
  # - p_shift_end_night   : parameter specifies the end of night shift
  #
  # Returns:
  # - data: data frame with gate data with the correction for early arrival
  #

# determine correction_early_arrival
  data$start_dayshift     <- p_shift_start_day    # add shift_start_day
  data$start_nightshift   <- p_shift_start_night  # add shift_start_night
  
  correction_start <- data %>%
    select(common_id, first_name, last_name, job_function_type, shift_type, datetime_check_in_out, date_check_in_out, working_day, time_check_in_out, start_dayshift, start_nightshift, site_ind_gate_ind, workhours) %>%
    mutate(
      
      date_check_in_out_correction = case_when(

          toupper(job_function_type) == "DIRECT" & shift_type == "dagshift"
        & hms(time_check_in_out) > (hms(p_shift_start_day) - hours(p_hour)) 
        & hms(time_check_in_out) < hms(p_shift_start_day)                         ~ as.POSIXct(paste0(date_check_in_out, " ", start_dayshift), format = "%Y-%m-%d %H:%M:%S", tz = 'UTC'),

          toupper(job_function_type) == "DIRECT" & shift_type == "nachtshift"
        & hms(time_check_in_out) > (hms(p_shift_start_night) - hours(p_hour)) 
        & hms(time_check_in_out) < hms(p_shift_start_night)                       ~ as.POSIXct(paste0(date_check_in_out, " ", start_nightshift), format = "%Y-%m-%d %H:%M:%S", tz = 'UTC'),

        TRUE                                                                      ~ datetime_check_in_out
        )
    ) %>%
    
    
    group_by(common_id) %>%
    mutate(
      
      correction_delta     = round(difftime( lead(date_check_in_out_correction), date_check_in_out_correction , units = "hours"),3),
      correction_workhours = 
        case_when(
          site_ind_gate_ind == 'Buiten site_UIT'     ~ 0,
          is.na(correction_delta) == TRUE            ~ 0,
          TRUE                                       ~ as.numeric(correction_delta)
        ),
      
      correction_early_arrival = if_else(workhours - correction_workhours >= 0, workhours - correction_workhours, 0)
      ) %>%
    ungroup() %>% as.data.frame()
  
  
# determine correctie_vroeg_aanwezig  
  data$end_dayshift       <- p_shift_end_day      # add shift_end_day
  data$end_nightshift     <- p_shift_end_night    # add shift_end_night    

     
return(data)

}