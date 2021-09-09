CorrectionHours <- function(data, p_shift_start_day, p_shift_start_night, p_shift_end_day, p_shift_end_night, p_hour, p_hour_threshold, p_hour_threshold_after_eight){
  # This function corrects workhours for direct employees for being early. Employees can only start when shift starts
  #               corrects workhours for direct employees for departing lately. Employees can only work during shift
  #               corrects workhours for employees who didn't check out at the gate.
  #
  # Args:
  # - data                         : data frame with gate data, in the form of output function CombineGateEmployee
  # - p_shift_start_day            : parameter specifies the start of day shift
  # - p_shift_start_night          : parameter specifies the start of night shift
  # - p_shift_end_day              : parameter specifies the end of day shift
  # - p_shift_end_night            : parameter specifies the end of night shift
  # - p_hour                       : parameter specifies the range for correction of working hours for being early or late.
  # - p_hour_threshold             : parameter specifies maximum hours in one shift. If this exceeds there is a suspicion of no Clocking out from site (Buiten site_UIT)
  # - p_hour_threshold_after_eight : parameter specifies maximum hours after a clocking of 8 workhours. If this exceeds there is a suspicion of no Clocking out from site (Buiten site_UIT)
  #
  # Returns:
  # - data: data frame with gate data with the correction for early arrival
  #


  
# Correction Early Arrival  -------------------------------------------------------------------------------------------------------------
  
# determine correction_early_arrival. Only direct employee is corrected. Employee is correct when in clocks before the shift start time.
  data$start_dayshift     <- p_shift_start_day    # add shift_start_day
  data$start_nightshift   <- p_shift_start_night  # add shift_start_night

  data <- data %>%
    # Selection is only for test purpose.
    # select(common_id, job_function_type, shift_type, datetime_check_in_out, date_check_in_out, working_day,
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
  data <- data %>%
    # group_by(common_id) %>%
    # mutate(
    #   last_position          = if_else(is.na(lead(time_check_in_out)) == TRUE, 1, 0),
    #   next_time_check_in_out = if_else(is.na(lead(time_check_in_out)) == TRUE, time_check_in_out, lead(time_check_in_out))
    # ) %>% ungroup() %>%
    mutate(
      working_days_without_checkout_correction_ind = case_when(
        # shift_type == "dagshift"
        # & first_clock_time >= (hms(p_shift_start_day) - hours(p_hour))
        # & first_clock_time <   hms(p_shift_start_day)
        # & last_position == 0
        # & hms(time_check_in_out)       <  hms(p_shift_end_day)
        # & hms(next_time_check_in_out)  >  (hms(p_shift_end_day)  + hours(p_hour))
        # & workhours >= 10                                                                        ~ 1,
        # 
        # shift_type == "nachtshift"
        # & first_clock_time >= (hms(p_shift_start_night) - hours(p_hour))
        # & first_clock_time <   hms(p_shift_start_night)
        # & is.na(next_time_check_in_out) == FALSE
        # & last_position == 0
        # & hms(time_check_in_out)      <  hms(p_shift_end_night)
        # & hms(next_time_check_in_out) >  (hms(p_shift_end_night)  + hours(p_hour))
        # & workhours >= 10                                                                        ~ 2, 
        
        
        lag(workhours) >= 6 & lag(workhours) <= 12 & workhours > p_hour_threshold_after_eight    ~ 1,
        workhours > p_hour_threshold                                                             ~ 2,
        workhours > p_hour_threshold_after_eight & lead(workhours) >= 6 & lead(workhours) <= 12  ~ 3,
        

        TRUE                                                                                     ~ 0
        ),
       
      
      
      datetime_correction_no_check_out = case_when(
          working_days_without_checkout_correction_ind >= 1 & shift_type == 'dagshift'             ~ as.POSIXct(paste0(date_check_in_out, " ", end_dayshift),   format = "%Y-%m-%d %H:%M:%S", tz = 'UTC'),
          
          working_days_without_checkout_correction_ind >= 1 & shift_type == 'nachtshift'  
        & hms(time_check_in_out)  <= hms(format("23:59:59", format = "%H:%M:%S"))
        & hms(time_check_in_out)  >= hms(format("16:00:00", format = "%H:%M:%S"))                  ~ as.POSIXct(paste0(date_check_in_out+1, " ", end_nightshift), format = "%Y-%m-%d %H:%M:%S", tz = 'UTC'),
        
        working_days_without_checkout_correction_ind >= 1 & shift_type == 'nachtshift'             ~ as.POSIXct(paste0(date_check_in_out, " ", end_nightshift), format = "%Y-%m-%d %H:%M:%S", tz = 'UTC'),
        
        working_days_without_checkout_correction_ind == 0                                          ~ datetime_check_in_out
      ),
      
      
      workhours_no_check_out            = as.numeric(round(difftime( datetime_correction_no_check_out, datetime_check_in_out , units = "hours"),3)),
      correction_workhours_no_check_out = if_else(workhours_no_check_out == 0, 0, workhours - workhours_no_check_out),
  ) %>% as.data.frame()
   
return(data)

}