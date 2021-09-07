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


  
# Correction Arealy Arrival  -------------------------------------------------------------------------------------------------------------
  
# determine correction_early_arrival. Only direct employee is corrected. Employee is correct when in clocks before the shift start time.
  data$start_dayshift     <- p_shift_start_day    # add shift_start_day
  data$start_nightshift   <- p_shift_start_night  # add shift_start_night

  data <- data %>%
    # Selection is only for test purpose.
    #select(common_id, job_function_type, shift_type, datetime_check_in_out, date_check_in_out, working_day,
    #       time_check_in_out, first_clock, first_clock_time, start_dayshift, start_nightshift, site_ind_gate_ind, workhours) %>%
    mutate(

      datetime_check_in_out_correction = case_when(

        # correction for direct personal - dayshift
        toupper(job_function_type) == "DIRECT" & shift_type == "dagshift"
        & first_clock_time >= (hms(p_shift_start_day) - hours(p_hour))
        & first_clock_time <   hms(p_shift_start_day)
        & hms(time_check_in_out) >=(hms(p_shift_start_day) - hours(p_hour))
        & hms(time_check_in_out) <  hms(p_shift_start_day)                         ~ as.POSIXct(paste0(date_check_in_out, " ", start_dayshift), format = "%Y-%m-%d %H:%M:%S", tz = 'UTC'),

        # correction for direct personal nightshift
          toupper(job_function_type) == "DIRECT" & shift_type == "nachtshift"
        & first_clock_time >= (hms(p_shift_start_night) - hours(p_hour))
        & first_clock_time <   hms(p_shift_start_night)
        & hms(time_check_in_out) >= (hms(p_shift_start_night) - hours(p_hour))
        & hms(time_check_in_out) <   hms(p_shift_start_night)                      ~ as.POSIXct(paste0(date_check_in_out, " ", start_nightshift), format = "%Y-%m-%d %H:%M:%S", tz = 'UTC'),

        TRUE                                                                       ~ datetime_check_in_out
        ),

      # determine correction_start_ind
      correction_start_ind = if_else(datetime_check_in_out_correction != datetime_check_in_out, 1, 0),

      # Determine deviating start shift
      deviating_start_shift        = case_when(
            shift_type == "dagshift"
        & ( first_clock_time <= (hms(p_shift_start_day) - hours(p_hour))
          | first_clock_time >= (hms(p_shift_start_day) + hours(p_hour)))     ~ 'afwijkende shift',

            shift_type == "nachtshift"
        & ( first_clock_time <= (hms(p_shift_start_night) - hours(p_hour))
            | first_clock_time >= (hms(p_shift_start_night) + hours(p_hour))) ~ 'afwijkende shift',

        TRUE                                                                  ~ 'normale shift'
      )


    ) %>%

    group_by(common_id) %>%
    mutate(

      correction_delta     = round(difftime( lead(datetime_check_in_out_correction), datetime_check_in_out_correction , units = "hours"),3),
      correction_workhours = case_when(

          site_ind_gate_ind == 'Buiten site_UIT'     ~ 0,
          is.na(correction_delta) == TRUE            ~ 0,
          TRUE                                       ~ as.numeric(correction_delta)
        ),

      correction_early_arrival = if_else(workhours - correction_workhours >= 0, workhours - correction_workhours, 0)
      ) %>%
    ungroup() %>% as.data.frame()


# Correction Late Departed  -------------------------------------------------------------------------------------------------------------

# determine correction late departed
  data$end_dayshift       <- p_shift_end_day      # add shift_end_day
  data$end_nightshift     <- p_shift_end_night    # add shift_end_night    
  
  
  data <- data %>%
    # Selection is only for test purpose. 
    #select(common_id, job_function_type, shift_type, datetime_check_in_out, date_check_in_out, working_day, 
    #       time_check_in_out, last_clock_buiten_site, last_clock_time_buiten_site, end_dayshift, end_nightshift, site_ind_gate_ind, workhours) %>%
    mutate(
      
      datetime_check_in_out_correction_end = case_when(
        
        # correction for direct personal - dayshift
        toupper(job_function_type) == "DIRECT" & shift_type == "dagshift"
        & last_clock_time_buiten_site >    hms(p_shift_end_day) 
        & last_clock_time_buiten_site <= ( hms(p_shift_end_day) + hours(p_hour)) 
        & hms(time_check_in_out) >     hms(p_shift_end_day)
        & hms(time_check_in_out) <=  ( hms(p_shift_end_day) + hours(p_hour))                      ~ as.POSIXct(paste0(date_check_in_out, " ", end_dayshift), format = "%Y-%m-%d %H:%M:%S", tz = 'UTC'),
        
        # correction for direct personal - nightshift
        toupper(job_function_type) == "DIRECT" & shift_type == "nachtshift"
        & last_clock_time_buiten_site >    hms(p_shift_end_night) 
        & last_clock_time_buiten_site <= ( hms(p_shift_end_night) + hours(p_hour)) 
        & hms(time_check_in_out) >    hms(p_shift_end_night) 
        & hms(time_check_in_out) <= ( hms(p_shift_end_night) + hours(p_hour))                     ~ as.POSIXct(paste0(date_check_in_out, " ", end_nightshift), format = "%Y-%m-%d %H:%M:%S", tz = 'UTC'),
        
        TRUE                                                                                      ~ datetime_check_in_out
      ),
      
      # determine correction_start_ind
      correction_end_ind = if_else(datetime_check_in_out_correction_end != datetime_check_in_out, 1, 0)
      
      
    ) %>%
    
    group_by(common_id) %>%
    mutate(
      
      correction_delta_end_shift     = round(difftime( lead(datetime_check_in_out_correction_end), datetime_check_in_out_correction_end , units = "hours"),3),
      correction_workhours_end_shift = case_when(
        
        site_ind_gate_ind == 'Buiten site_UIT'     ~ 0,
        is.na(correction_delta_end_shift) == TRUE  ~ 0,
        TRUE                                       ~ as.numeric(correction_delta_end_shift)
      ),
      
      correction_late_departed = if_else(workhours - correction_workhours_end_shift >= 0, workhours - correction_workhours_end_shift, 0)
    ) %>%
    ungroup() %>% as.data.frame() 
  
  
# Correction Working_days without Buiten site_UIT  -----------------------------------------------------------------------------------------    
  
  # working_days_without_checkout_correction_ind = case_when(
  #   check_first_before_last_ind == 'UNKNOWN' & workhours >= 8 & lead(workhours) >= 12 ~ 1,
  #   workhours >= p_workhour_threshold                                                 ~ 1
  #   TRUE                                                                              ~ 0
  # )  
   
return(data)

}