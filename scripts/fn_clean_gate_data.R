CleanGateData <- function(data, site, p_workday_split, p_select_period, p_gate_data_start, p_gate_data_end){
  # This function cleans the gate data 
  #
  # Args:
  # - data                : data frame with gate data, in the form of output function import_files
  # - site                : data frame with site data, in the form of output function import_files
  # - p_workday_split     : paramemer specifies the split. work hours for the split are counted on the previous day
  # - p_select_period     : parameter specifies whether data is selected over a period of time
  # - p_gate_data_start   : parameter specifies the start date of the period
  # - p_gate_data_end     : parameter specifies the end date of the period
  #
  # Returns:
  # - data: data frame with clean gate data
  #

# select port data for given time horizon. Only if 
  if (p_select_period == TRUE){
    data <- data[data$datetime_check_in_out >= p_gate_data_start & data$datetime_check_in_out <= p_gate_data_end,]
  }
      
# remove duplicates
  data <- unique(data)
  
# add parameters
  data$workday_split      <- p_workday_split      # add workday_split

# sort data by employee (common_id)
  data <- data %>%
    arrange(common_id, datetime_check_in_out)   
     
# combine with site data
  data <- data %>%
    left_join(site[,c('site','gate_in_out','site_op_buiten','site_entrance', 'remark')], by = c('location' = 'site'))  
        
# determine in out
  data <- data %>% 
    
    mutate(
      date_check_in_out  = as.Date(datetime_check_in_out, format = "%Y-%m-%d"),
      time_check_in_out  = format(datetime_check_in_out,  format = "%H:%M:%S"),
      hour_check_in_out  = format(datetime_check_in_out,  format = "%H"),
      time_dummy         = difftime(datetime_check_in_out,date_check_in_out, units = "hours"),
      working_day        = if_else(time_dummy <= workday_split, date_check_in_out - 1, date_check_in_out),
      site_ind_gate_ind  = paste0(site_op_buiten, "_", gate_in_out),
      common_id          = if_else(is.na(common_id) == TRUE, 'Unknown', toupper(common_id))
      ) %>%
    
    group_by(common_id) %>% 
    mutate(
      current_next_in_out            = paste0(gate_in_out, "_", lead(gate_in_out)),
      current_next_site_ind_gate_ind = paste0(site_ind_gate_ind, "_", lead(site_ind_gate_ind)),
      delta                          = round(difftime( lead(datetime_check_in_out), datetime_check_in_out , units = "hours"),3),
      workhours                      = 
        case_when(
          site_ind_gate_ind == 'Buiten site_UIT' ~ 0,
          is.na(delta) == TRUE                   ~ 0,
          TRUE                                   ~ as.numeric(delta)
          )
      ) %>% 
    select(-c(time_dummy)) %>%
    ungroup() %>% as.data.frame() 
    
# determine shift based on clocking on site
  data_agg <- data %>%
    filter(site_op_buiten == 'Op site') %>%
    group_by(common_id,working_day) %>%
    summarise(
      first_clock        = min(datetime_check_in_out),
      first_clock_hour   = as.numeric(format(first_clock, format = "%H")),
      last_clock         = max(datetime_check_in_out),
      last_clock_hour    = as.numeric(format(last_clock, format = "%H")),
      number_of_clocks   = n(),
      shift_type         = case_when(
        first_clock_hour  >= 6  & first_clock_hour  <= 14    ~ 'dagshift',
        first_clock_hour  >= 15 & first_clock_hour  <= 23    ~ 'nachtshift',
        first_clock_hour  >= 0  & first_clock_hour  <= 4     ~ 'nachtshift',
        first_clock_hour  >= 5                               ~ 'dagshift',     
        #first_clock_hour  >= 5  & last_clock_hour   <= 7     ~ 'nachtshift',   
        TRUE                                                 ~ 'geen'
        )
    ) %>%
    #select(-c(first_clock_hour,last_clock_hour)) %>%
    #ungroup() %>% 
    as.data.frame() 

  data <- data %>%
    left_join(data_agg, by = c('common_id','working_day')) %>%
    mutate(
      shift_type = if_else(is.na(shift_type) == TRUE,'UNKNOWN',shift_type)
    )

# determine last clock hour based on clocking buiten site
  data_agg_buiten_site <- data %>%
    filter(site_ind_gate_ind == 'Buiten site_UIT') %>%
    group_by(common_id,working_day) %>%
    summarise(
      last_clock_buiten_site  = max(datetime_check_in_out)
    )
  
  data <- data %>%
    left_join(data_agg_buiten_site, by = c('common_id','working_day')) %>%
    mutate(
      check_first_before_last_ind = if_else(difftime(last_clock_buiten_site,first_clock, units = "hours") < 0, 'Fout','Goed')
    )

      
return(data)  
  
}
  
  