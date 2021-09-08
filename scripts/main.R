
# INITIAL ----------------------------------------------------------------------

# Clean R environment
  rm(list = ls(all = TRUE))          # Environment
  if(!is.null(dev.list())) dev.off() # Plots
  cat("\014")                        # Console


  
# PARAMETERS -------------------------------------------------------------------  

# Parameters
p_select_period             <- TRUE                                    # specifies whether data is selected over a period of time
p_period_start              <- as.Date('2021-03-14')                   # specifies the start date of the period
p_period_end                <- as.Date('2021-06-02')                   # specifies the end date of the period
p_workday_split             <- 6                                       # specifies the split. work hours for the split are counted on the previous day
p_shift_start_day           <- format("07:00:00", format = "%H:%M:%S") # specifies the start of day shift
p_shift_start_night         <- format("18:00:00", format = "%H:%M:%S") # specifies the start of night shift
p_shift_end_day             <- format("17:15:00", format = "%H:%M:%S") # specifies the start of day shift
p_shift_end_night           <- format("04:15:00", format = "%H:%M:%S") # specifies the start of night shift
p_hour                      <- 2                                       # specifies the range for correction of workinghours for being early or late.
p_work_break_min_threshhold <- 2                                       # specifies the minimum threshhold. Below this threshold there is no work_break
p_work_break_threshhold     <- 4                                       # specifies the threshhold for small or normal work break
p_work_break_small          <- 0.25                                    # specifies time for small work break
p_work_break_normal         <- 0.75                                    # specifies time for normal work break
p_change_of_dress_time      <- 0.25                                    # specifies time for changing of dress
p_hour_threshold            <- 15                                      # specifies maximum hours between two clockings. This exceeds if there is no Clocking out from sote (Buiten site_UIT)




# Packages and Functions--------------------------------------------------------  

# load packages
library(tidyverse)
library(lubridate)

# import all functions from map 'scripts'
  invisible( # suppress output
    sapply(
      list.files(pattern     = 'fn_.*\\.R$',
                 path       = './scripts', 
                 full.names = TRUE
      ), # collect all .R-files from specific map
      source # apply source-function to all files
    )
  )


  
# Import Data   ----------------------------------------------------------------    

# load port data (staging)
  file_map                <-  'data/raw/gate/'
  meta_file               <- paste0(file_map,"metadata_poort.xlsx")
  file_name               <- paste0(file_map,"totaallijst_zonder_eigen_personeel.xlsx")
  df.gate_staging         <- import_files(p.meta = TRUE, meta_file, file_name, sheet.nr =1, skip_rows = 0 )


# load site data (staging)  
  file_map                <- 'data/raw/reference/'
  file_name               <- paste0(file_map,"site_toegangen.xlsx")
  df.site_staging         <- import_files(p.meta = FALSE, filename = file_name, sheet.nr =1, skip_rows = 0 )  

  
# load function data (staging)  
  file_map                <- 'data/raw/reference/'
  file_name               <- paste0(file_map,"job_function.xlsx")
  df.job_function_staging <- import_files(p.meta = FALSE, filename = file_name, sheet.nr =1, skip_rows = 0 )  
  
    
# load contractor data (staging)
  file_map                <-  'data/raw/contractors/'
  meta_file               <- paste0(file_map,"meta_bilfinger.xlsx")
  file_name               <- paste0(file_map,"Bilfinger Maintenance.xlsm")
  df.bf_staging           <- import_files(p.meta = TRUE, filemeta = meta_file, filename = file_name, sheet.nr =1, skip_rows = 2)
  

  
# Clean Data   ----------------------------------------------------------------      
# clean gate data
  df.gate_clean             <- CleanGateData(data = df.gate_staging, site = df.site_staging, p_workday_split = p_workday_split,
                                             p_select_period = p_select_period, p_gate_data_start = p_period_start, p_gate_data_end = p_period_end)

# clean bilfinger data
  df.bilfinger_clean        <- CleanBilfingerData(data = df.bf_staging, job_function = df.job_function_staging)  
  
  # select bilfinger data for given time horizon. Only if (p_select_period == TRUE
  if (p_select_period == TRUE){
    df.bilfinger_clean      <- df.bilfinger_clean[df.bilfinger_clean$date_work >= p_period_start & df.bilfinger_clean$date_work <= p_period_end,]
  }

  
# combine contractors  ####### Nog andere contractors toevoegen #####
  df.contractor            <- df.bilfinger_clean
  

# select employees
  df.employee              <- SelectEmployee(contractors = df.contractor)


# combine gate data with employees
  df.gate_clean_employee   <- CombineGateEmployee(gate = df.gate_clean, employee = df.employee)


# correction on hours for early arrival and the hours after end shift   
  df.gate_correction       <- CorrectionHours(data = df.gate_clean_employee,
                                              p_shift_start_day = p_shift_start_day, p_shift_start_night = p_shift_start_night,
                                              p_shift_end_day   = p_shift_end_day  , p_shift_end_night   = p_shift_end_night,
                                              p_hour = p_hour)
  

# aggregate gate data to day 
  df_agg.gate              <- AggregateGate(gate = df.gate_correction,
                                            p_shift_start_day  = p_shift_start_day                    , p_shift_start_night     = p_shift_start_night,
                                            p_shift_end_day    = p_shift_end_day                      , p_shift_end_night       = p_shift_end_night,
                                            p_hour             = p_hour                               , p_work_break_threshhold = p_work_break_threshhold, 
                                            p_work_break_small = p_work_break_small                   , p_work_break_normal     = p_work_break_normal,
                                            p_work_break_min_threshhold = p_work_break_min_threshhold , p_change_of_dress_time  = p_change_of_dress_time)


  
# combine gate data with contractors 
  df_agg.hours_check_employee_working_day <- CombineContractorGateAgg( contractor = df.contractor, gate_agg = df_agg.gate, employee = df.employee)  
  
  
# aggregate to level employee
# Bilfinger has 628 unique common_ids. 1 common_id with a double full_name and common_id = XXXX with 7 full_names. Together 636 unique common_id - full_names
  df_agg.hours_check_employee             <- EmployeeCheckAgg(hours_check_employee_working_day = df_agg.hours_check_employee_working_day)  
  
  
# prepare view for the audit of gate data
  list.audit_gate                           <- PrepareAuditView(gate = df.gate_correction, employee = df.employee)
 
  
# export files --------------------------------------------------------------------------------------------------------------------------------------
  
# path results
  filepath <- "./results/"
    
# export urencontrole_werkdag 
  filename <- paste0(format(Sys.Date(),'%Y%m%d'),'_urencontrole_werkdag.csv')
  write.csv2(df_agg.hours_check_employee_working_day, paste0(filepath,filename), row.names = FALSE)      
        
# export urencontrole_medewerker  
  filename <- paste0(format(Sys.Date(),'%Y%m%d'),'_urencontrole_medewerker.csv')
  write.csv2(df_agg.hours_check_employee, paste0(filepath,filename), row.names = FALSE)        

# export audit_view_gate_Bilfinger_Maintenance  
  filename <- paste0(format(Sys.Date(),'%Y%m%d'),'_audit_view_gate_Bilfinger_Maintenance.csv')
  write.csv2(list.audit_gate$Bilfinger_Maintenance, paste0(filepath,filename), row.names = FALSE)        
  

# export audit_view_gate_all
  filename <- paste0(format(Sys.Date(),'%Y%m%d'),'_audit_view_gate_all.csv')
  write.csv2(list.audit_gate$All, paste0(filepath,filename))        
  
    
# test
  med = unique(df.bf_staging[,c('common_id','job_function')])
  med = med[is.na(med$common_id) == TRUE,]
  x = df.gate_clean[df.gate_clean$common_id %in% med$common_id,]
  
  
  df.gate_clean[df.gate_clean$common_id == '03081988AAZZ', !colnames(df.gate_clean) %in% c('remark')]
  df.bf_staging[df.bf_staging$Code_ID == '03081988AAZZ',]
  
  m =unique(df.bilfinger_clean[,c('full_name','firma','common_id','decl_total_working_hours','tarif')])
  m = m[is.na(m$full_name) == FALSE,]
  p = unique(df.gate_clean[,c('common_id','first_name','last_name','contractor','date_birth')])

  z = left_join(m,p, by = c('common_id'='common_id'))  
  z2 = z[is.na(z$first_name) == TRUE,] %>%
    select(-c('first_name','last_name','contractor','date_birth')) %>%
    mutate(
      total_cost = decl_total_working_hours * tarif
    )
  
  z3 = z[is.na(z$first_name) == FALSE,]  
  
# write.csv2(z2, "analyse/Bilfinger_unknown_persons.csv")  

# write.csv2(df_agg.hours_check_employee, "results/urencontrole_medewerker.csv")      
# write.csv2(list.audit_gate$Bilfinger_Maintenance, "results/audit_view_gate_Bilfinger_Maintenance.csv")      

  
  