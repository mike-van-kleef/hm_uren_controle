CombineContractorGateAgg <- function(contractor, gate_agg, employee){
  # This function combines the contractor data with the gata data on workday level
  #
  # Args:
  # - contractors         : data frame with contractors data, in the form of output function .........
  # - gate_agg            : data frame with aggregated gate data on workday level, in the form of output function AggregateGate
  # - employee            : data frame with employee data from the different contractors, in the form of output function ......
  #
  # Returns:
  # - hours_check_employee_working_day: data frame with contractors data and gate data at the level employee, working day
  #

  # remove rows with unknown common_id (XXXX)
  employee <- employee[employee$common_id != 'XXXX', ]
  
  # Keep only gate data which matches the employees from the different contractors
  gate_agg <- gate_agg[gate_agg$common_id %in% employee$common_id,]
  

### Add the declaration hours for different contractors  -----------------------------------------------------------------
#   when employee is active for different contractors on the same day. Add the declaration hours  
  
  # employee with common_id  
  # One job_function_type and one name for each common_id
  contractor_with_id <- contractor %>%
    filter(common_id != 'XXXX') %>%
    group_by(common_id) %>%
    mutate(
      job_function_type_direct_indirect = max(job_function_type),  # Indirect gaat voor Direct,
      full_name                         = max(full_name, na.rm = TRUE)
    ) %>%
    select(-c(job_function_type)) %>%
    ungroup() %>%
    group_by(common_id, full_name, date_work, job_function_type_direct_indirect) %>%
    summarise(
      job_function               = min(job_function),
      double_decl_same_day       = min(double_decl_same_day),
      double_name_same_common_id = max(double_name_same_common_id),
      decl_total_working_days    = sum(decl_total_working_days),
      decl_working_hours         = sum(decl_working_hours),
      number_of_contractors      = n_distinct(contractor_decl),
      contractor_decl            = if_else( max(contractor_decl) == min(contractor_decl), max(contractor_decl) , 'Multiple Contractors'),
    ) %>% as.data.frame()
  

  # employee without common_id
  contractor_no_id <- contractor %>%    
    filter(common_id == 'XXXX') %>%
    group_by(common_id) %>%
    mutate(
      job_function_type_direct_indirect = max(job_function_type),  # Indirect gaat voor Direct,
      number_of_contractors             = 1
    ) %>% ungroup() %>%
    select(common_id, full_name, date_work, job_function_type_direct_indirect,job_function, double_decl_same_day, double_name_same_common_id, 
           decl_total_working_days, decl_working_hours, number_of_contractors, contractor_decl) %>%
    as.data.frame()

 
  # combine both data frames  
  contractor_agg <- rbind(contractor_no_id, contractor_with_id)

  hours_check_employee_working_day  <- contractor_agg %>%
    full_join(gate_agg, by = c('common_id' = 'common_id', 'date_work' = 'working_day', 'job_function_type_direct_indirect' = 'job_function_type')) %>%
    group_by(common_id) %>%
    mutate(
      first_work_day_contractor         = min(date_work, na.rm = TRUE), 
      last_work_day_contractor          = max(date_work, na.rm = TRUE)
    ) %>%
    arrange(common_id, date_work) %>%
    ungroup %>% as.data.frame() %>%
    
    # Include only gate data in the period of declaration
    filter(date_work >= first_work_day_contractor -1 & date_work <= last_work_day_contractor + 1) %>%
    
    mutate(
      decl_working_hours                    = if_else(is.na(decl_working_hours)                    == TRUE, 0, decl_working_hours),
      tot_hours_on_site                     = if_else(is.na(tot_hours_on_site)                     == TRUE, 0, tot_hours_on_site),
      tot_correction_no_check_out           = if_else(is.na(tot_correction_no_check_out)           == TRUE, 0, tot_correction_no_check_out),
      bruto_working_hours                   = if_else(is.na(bruto_working_hours)                   == TRUE, 0, bruto_working_hours),
      tot_correction_early_arrival          = if_else(is.na(tot_correction_early_arrival)          == TRUE, 0, tot_correction_early_arrival),
      tot_correction_late_departed          = if_else(is.na(tot_correction_late_departed)          == TRUE, 0, tot_correction_late_departed),
      work_break                            = if_else(is.na(work_break)                            == TRUE, 0, work_break),
      work_break_cor_off_site               = if_else(is.na(work_break_cor_off_site)               == TRUE, 0, work_break_cor_off_site),
      change_of_dress_time                  = if_else(is.na(change_of_dress_time)                  == TRUE, 0, change_of_dress_time),
      netto_working_hours                   = if_else(is.na(netto_working_hours)                   == TRUE, 0, netto_working_hours),
    ) %>%
    
    mutate(
      delta_decl_vs_bruto_hours = round(decl_working_hours - bruto_working_hours,2),
      delta_decl_vs_netto_hours = round(decl_working_hours - netto_working_hours,2)
      ) %>%
   
    group_by(common_id) %>%
    mutate(
      row_id                         = row_number(),
      tot_netto_working_hours        = sum(netto_working_hours),
      employee_clocking_without_decl = if_else(sum(is.na(double_decl_same_day) == TRUE) > 0 & tot_netto_working_hours > 0,1,0)
    ) %>% ungroup() %>%
    
    mutate(
      remark = case_when(
        common_id               == 'XXXX'                                        ~ 'Employee not found',
        tot_netto_working_hours == 0                                             ~ 'Employee found - no gate clocking for period',
        netto_working_hours     == 0                                             ~ 'No gate clockings for workday',
        deviating_start_shift   == 'afwijkende shift'                            ~ 'Deviating shift', 
        TRUE                                                                     ~ '-'
        )
      ) %>%
    
    group_by(common_id) %>%
    mutate(
      netto_cor_ind           = case_when(
        is.na(double_decl_same_day)  == TRUE                                                            # (persoon komt niet voor in declaratieoverzicht)
        &  decl_working_hours == 0
        &  remark != 'Deviating shift'                                                        ~ 1,
        
        is.na(double_decl_same_day)  == TRUE                                                            # (persoon komt niet voor in declaratieoverzicht)
        &  remark == 'Deviating shift'                                           
        &  delta_decl_vs_netto_hours < 0
        &  !(
              (if_else(is.na(lag(delta_decl_vs_netto_hours)) ,FALSE, lag(delta_decl_vs_netto_hours)  > 2)
              & lag(date_work) == (date_work - 1)
              )
            | (if_else(is.na(lead(delta_decl_vs_netto_hours)),FALSE, lead(delta_decl_vs_netto_hours) > 2)
              & lead(date_work) == (date_work + 1)
              )
        )                                                                                     ~ 2,

           delta_decl_vs_netto_hours < -6
        &  !(
             (if_else(is.na(lag(delta_decl_vs_netto_hours)) ,FALSE, lag(delta_decl_vs_netto_hours)  > 3)
              & lag(date_work) == (date_work - 1)
             )
          | (if_else(is.na(lead(delta_decl_vs_netto_hours)),FALSE, lead(delta_decl_vs_netto_hours) > 3)
             & lead(date_work) == (date_work + 1)
            )
          )                                                                                    ~ 3,
        
        delta_decl_vs_netto_hours < -1
        &  !(
          (if_else(is.na(lag(delta_decl_vs_netto_hours)) ,FALSE, lag(delta_decl_vs_netto_hours)  > 0)
           & lag(date_work) == (date_work - 1)
          )
          | (if_else(is.na(lead(delta_decl_vs_netto_hours)),FALSE, lead(delta_decl_vs_netto_hours) > 0)
             & lead(date_work) == (date_work + 1)
          )
        )                                                                                    ~ 4,
        
        TRUE                                                                                  ~ 0
      ),
      
      check_neg_decl             = max(if_else(netto_cor_ind == 0 & delta_decl_vs_netto_hours < -3, 1,0))
    )  %>% ungroup() %>%
    
    mutate(
       netto_working_hours_cor = case_when(
         netto_cor_ind %in% c(1,2)                                                            ~ 0,
         netto_cor_ind == 3                                                                   ~ decl_working_hours,
         TRUE                                                                                 ~ netto_working_hours
      ),
      
      delta_decl_vs_netto_hours_cor   = round(decl_working_hours - netto_working_hours_cor,2)
    ) %>%
    
     select(
       common_id,
       full_name,
       last_name_gate                      = last_name,
       first_name_gate                     = first_name,
       remark,
       double_decl_same_day,
       double_name_same_common_id,
       contractor_decl,
       job_function,
       job_function_type                   = job_function_type_direct_indirect,
       duplicate_function_type,
       date_work,
       deviating_start_shift,
       shift_type,
       decl_total_working_days,
       decl_working_hours,
       delta_last_first_clock,
       tot_hours_on_site,
       tot_hours_off_site,
       working_days_without_checkout_correction_ind,
       tot_correction_no_check_out,
       gross_working_hours                 = bruto_working_hours,
       correction_start_shift_ind,
       tot_correction_early_arrival,
       correction_end_shift_ind,
       tot_correction_late_departed,
       work_break,
       work_break_cor_off_site,
       change_of_dress_time,
       net_working_hours                   = netto_working_hours,
       net_working_hours_cor               = netto_working_hours_cor,
       delta_decl_vs_gross_hours           = delta_decl_vs_bruto_hours,
       delta_decl_vs_net_hours             = delta_decl_vs_netto_hours,
       delta_decl_vs_net_hours_cor         = delta_decl_vs_netto_hours_cor,
       first_clock,
       first_clock_hour,
       last_clock_buiten_site,
       employee_clocking_without_decl,
       net_cor_ind                         = netto_cor_ind,
       commissioning_ind,
       check_neg_decl
       
     ) %>% as.data.frame()
    
  
    
return(hours_check_employee_working_day)
  
}