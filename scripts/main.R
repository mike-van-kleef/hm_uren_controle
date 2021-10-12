
# INITIAL ----------------------------------------------------------------------

# Clean R environment
  rm(list = ls(all = TRUE))          # Environment
  if(!is.null(dev.list())) dev.off() # Plots
  cat("\014")                        # Console


  
# PARAMETERS -------------------------------------------------------------------  

# Parameters
p_select_period              <- TRUE                                    # specifies whether data is selected over a period of time
p_period_start               <- as.Date('2021-03-14')                   # specifies the start date of the period
p_period_end                 <- as.Date('2021-06-02')                   # specifies the end date of the period (30 mei 2021 + 2 days for marge)
#p_period_end                 <- as.Date('2021-07-11')                   # specifies the end date of the period for BIS (09 july 2021 + 2 days for marge)
p_period_end                 <- as.Date('2021-08-04')                   # specifies the end date of the period for Mourik (04 Aug 2021)
p_workday_split              <- 5                                       # specifies the split. work hours for the split are counted on the previous day
p_shift_start_day            <- format("07:00:00", format = "%H:%M:%S") # specifies the start of day shift
p_shift_start_night          <- format("18:00:00", format = "%H:%M:%S") # specifies the start of night shift
p_shift_end_day              <- format("17:15:00", format = "%H:%M:%S") # specifies the start of day shift
p_shift_end_night            <- format("04:15:00", format = "%H:%M:%S") # specifies the start of night shift
p_hour                       <- 2                                       # specifies the range for correction of workinghours for being early or late.
p_work_break_min_threshhold  <- 2                                       # specifies the minimum threshhold. Below this threshold there is no work_break
p_work_break_threshhold      <- 4                                       # specifies the threshhold for small or normal work break
p_work_break_small           <- 0.25                                    # specifies time for small work break
p_work_break_normal          <- 0.75                                    # specifies time for normal work break
p_change_of_dress_time       <- 0.25                                    # specifies time for changing of dress
p_hour_threshold             <- 20                                      # specifies maximum hours in one shift. If this exceeds there is a suspicion of no Clocking out from site (Buiten site_UIT)
p_hour_threshold_after_eight <- 12                                      # specifies maximum hours after a clocking of 8 workhours. If this exceeds there is a suspicion of no Clocking out from site (Buiten site_UIT)
p_start_commissioning        <- as.Date('2021-05-05')                   # specifies the start date of the commissioning. Between this period no corrections because of standby of employees
p_end_commissioning          <- as.Date('2021-05-10')                   # specifies the end date of the commissioning. Between this period no corrections because of standby of employees




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
  file_map                   <- 'data/raw/reference/'
  file_name                  <- paste0(file_map,"job_function_bilfinger.xlsx")
  df.job_function_bf_staging <- import_files(p.meta = FALSE, filename = file_name, sheet.nr =1, skip_rows = 0 )  
  
  file_map                   <- 'data/raw/reference/'
  file_name                  <- paste0(file_map,"job_function_mourik.xlsx")
  df.job_function_mourik_staging <- import_files(p.meta = FALSE, filename = file_name, sheet.nr =1, skip_rows = 0 )  
    
# load contractor data (staging)
  file_map                <-  'data/raw/contractors/'
  meta_file               <- paste0(file_map,"meta_bilfinger.xlsx")
  file_name               <- paste0(file_map,"Bilfinger Maintenance.xlsm")
  df.bf_staging           <- import_files(p.meta = TRUE, filemeta = meta_file, filename = file_name, sheet.nr =1, skip_rows = 2)
  
  # file_map                <-  'data/raw/contractors/'
  # meta_file               <- paste0(file_map,"meta_mourik_personeel.xlsx")
  # file_name               <- paste0(file_map,"Mourik.xlsx")
  # df.mourik_pers_staging  <- import_files(p.meta = TRUE, filemeta = meta_file, filename = file_name, sheet.nr =1, skip_rows = 1)
  
  file_map                <-  'data/raw/contractors/'
  meta_file               <- paste0(file_map,"meta_mourik.xlsx")
  file_name               <- paste0(file_map,"Mourik.xlsx")
  df.mourik_staging       <- import_files(p.meta = TRUE, filemeta = meta_file, filename = file_name, sheet.nr =6, skip_rows = 0)
  
  file_map                <-  'data/raw/contractors/'
  meta_file               <- paste0(file_map,"meta_mammoet.xlsx")
  file_name               <- paste0(file_map,"Mammoet.xlsx")
  df.mammoet_staging      <- import_files(p.meta = TRUE, filemeta = meta_file, filename = file_name, sheet.nr =1, skip_rows = 0)
  
  file_map                <-  'data/raw/contractors/'
  meta_file               <- paste0(file_map,"meta_bis.xlsx")
  file_name               <- paste0(file_map,"BIS.xlsx")
  df.bis_staging          <- import_files(p.meta = TRUE, filemeta = meta_file, filename = file_name, sheet.nr =1, skip_rows = 3)
  
# Clean Data   ----------------------------------------------------------------      
# clean gate data
  df.gate_clean             <- CleanGateData(data = df.gate_staging, site = df.site_staging, p_workday_split = p_workday_split,
                                             p_select_period = p_select_period, p_gate_data_start = p_period_start, p_gate_data_end = p_period_end)

# clean bilfinger data
  df.bilfinger_clean        <- CleanBilfingerData(data = df.bf_staging, job_function = df.job_function_bf_staging)  

# clean Mourik data
  df.mourik_clean           <- CleanMourikData(data = df.mourik_staging, job_function = df.job_function_mourik_staging)  
  # df.mourik_pers_clean      <- CleanMourikData(data = df.mourik_pers_staging, job_function = df.job_function_mourik_staging, p_department = 'Mourik_Personeel')  
  # df.mourik_mint_clean      <- CleanMourikData(data = df.mourik_mint_staging, job_function = df.job_function_mourik_staging, p_department = 'Mourik_International' )  

# combine both data frames and aggregate 
  # df.mourik_clean           <- CombineMourikData(pers = df.mourik_pers_clean, mint = df.mourik_mint_clean)

  
# clean Mammoet data  
  df.mammoet_clean          <- CleanMammoetData(data = df.mammoet_staging)    


# clean BIS data    
  df.bis_clean              <- CleanBisData(data = df.bis_staging)    
      
# select data for given time horizon. Only if (p_select_period == TRUE
  if (p_select_period == TRUE){
    df.bilfinger_clean      <- df.bilfinger_clean[df.bilfinger_clean$date_work >= p_period_start & df.bilfinger_clean$date_work <= p_period_end,]
    df.mourik_clean         <- df.mourik_clean[df.mourik_clean$date_work       >= p_period_start & df.mourik_clean$date_work    <= p_period_end,]
    df.mammoet_clean        <- df.mammoet_clean[df.mammoet_clean$date_work     >= p_period_start & df.mammoet_clean$date_work   <= p_period_end,]
    df.bis_clean            <- df.bis_clean[df.bis_clean$date_work             >= p_period_start & df.bis_clean$date_work       <= p_period_end,]
  }



    
# combine contractors  ####### Nog andere contractors toevoegen #####
  #df.contractor            <- CombineContractors(bilfinger = df.bilfinger_clean, mourik = df.mourik_clean)
  #df.contractor            <- df.bilfinger_clean
  #df.contractor            <- df.mammoet_clean
  #df.contractor            <- df.bis_clean
  df.contractor             <- df.mourik_clean
  

# select employees
  df.employee              <- SelectEmployee(contractors = df.contractor)


# combine gate data with employees
  df.gate_clean_employee   <- CombineGateEmployee(gate = df.gate_clean, employee = df.employee)


# correction on hours for early arrival and the hours after end shift   
  df.gate_correction       <- CorrectionHours(data = df.gate_clean_employee,
                                              p_shift_start_day = p_shift_start_day         , p_shift_start_night   = p_shift_start_night,
                                              p_shift_end_day   = p_shift_end_day           , p_shift_end_night     = p_shift_end_night,
                                              p_hour            = p_hour                    , p_hour_threshold      = p_hour_threshold,
                                              p_hour_threshold_after_eight = p_hour_threshold_after_eight,
                                              p_start_commissioning = p_start_commissioning , p_end_commissioning   = p_end_commissioning
                                              )
  

# aggregate gate data to day 
  df_agg.gate              <- AggregateGate(gate = df.gate_correction,
                                            p_shift_start_day  = p_shift_start_day                    , p_shift_start_night     = p_shift_start_night,
                                            p_shift_end_day    = p_shift_end_day                      , p_shift_end_night       = p_shift_end_night,
                                            p_hour             = p_hour                               , p_work_break_threshhold = p_work_break_threshhold, 
                                            p_work_break_small = p_work_break_small                   , p_work_break_normal     = p_work_break_normal,
                                            p_work_break_min_threshhold = p_work_break_min_threshhold , p_change_of_dress_time  = p_change_of_dress_time)


  
# combine gate data with contractors 
  df_agg.hours_check_employee_working_day   <- CombineContractorGateAgg( contractor = df.contractor, gate_agg = df_agg.gate, employee = df.employee)  
  
  
# aggregate to level employee
# Bilfinger has 628 unique common_ids. common_id = XXXX with 7 full_names. Together 635 unique common_id - full_names
  df_agg.hours_check_employee               <- EmployeeCheckAgg(hours_check_employee_working_day = df_agg.hours_check_employee_working_day)  
  
  
# prepare view for the audit of gate data
  list.audit_gate                           <- PrepareAuditView(gate = df.gate_correction, employee = df.employee)
  

# prepare view to level contractor
  df_agg.hours_check_contractor             <- ContractorCheckAgg(employee = df_agg.hours_check_employee)

  
# prepare overview with employees and contractors according to the gate data
  employee_contractor_gate                  <- EmployeeContractorFromGate(gate = df.gate_clean)
  employee_gate                             <- employee_contractor_gate$employee
  contractor_gate                           <- employee_contractor_gate$contractor_agg
  
# export files --------------------------------------------------------------------------------------------------------------------------------------
  
# path results
  filepath   <- "./results/"
  contractor <- unique(df_agg.hours_check_contractor[1:1,])
    
# export urencontrole_werkdag 
  filename <- paste0(format(Sys.Date(),'%Y%m%d'),'_urencontrole_werkdag_',contractor,'.csv')
  write.csv2(df_agg.hours_check_employee_working_day, paste0(filepath,filename), row.names = FALSE)      
        
# export urencontrole_medewerker  
  filename <- paste0(format(Sys.Date(),'%Y%m%d'),'_urencontrole_medewerker_',contractor,'.csv')
  write.csv2(df_agg.hours_check_employee, paste0(filepath,filename), row.names = FALSE)        
  
# export urencontrole_contractor
  filename <- paste0(format(Sys.Date(),'%Y%m%d'),'_urencontrole_contractor_',contractor,'.csv')
  write.csv2(df_agg.hours_check_contractor, paste0(filepath,filename), row.names = TRUE)          

# export audit_view_gate_Bilfinger_Maintenance  
  filename <- paste0(format(Sys.Date(),'%Y%m%d'),'_audit_view_gate_Bilfinger_Maintenance.csv')
  write.csv2(list.audit_gate$Bilfinger_Maintenance, paste0(filepath,filename), row.names = FALSE)        

# export audit_view_gate_Mammoet
  filename <- paste0(format(Sys.Date(),'%Y%m%d'),'_audit_view_gate_Mammoet.csv')
  write.csv2(list.audit_gate$Mammoet, paste0(filepath,filename), row.names = FALSE)        
  
# export audit_view_gate_Bis
  filename <- paste0(format(Sys.Date(),'%Y%m%d'),'_audit_view_gate_BIS.csv')
  write.csv2(list.audit_gate$BIS, paste0(filepath,filename), row.names = FALSE)     

# export audit_view_gate_Mourik
  filename <- paste0(format(Sys.Date(),'%Y%m%d'),'_audit_view_gate_Mourik.csv')
  write.csv2(list.audit_gate$Mourik, paste0(filepath,filename), row.names = FALSE)      

# export audit_view_gate_all
  filename <- paste0(format(Sys.Date(),'%Y%m%d'),'_audit_view_gate_all.csv')
  write.csv2(list.audit_gate$All, paste0(filepath,filename))  
  
#   
# # export employee_gate
#   filename <- paste0(format(Sys.Date(),'%Y%m%d'),'_employee_gate.csv')
#   write.csv2(employee_gate, paste0(filepath,filename), row.names = FALSE)        
#   
# 
# # export contractor_gate
#   filename <- paste0(format(Sys.Date(),'%Y%m%d'),'_contractor_gate.csv')
#   write.csv2(contractor_gate, paste0(filepath,filename), row.names = FALSE)             
#   
    
# test
  med = unique(df.bf_staging[,c('common_id','job_function')])
  med = med[is.na(med$common_id) == TRUE,]
  x = df.gate_clean[df.gate_clean$common_id %in% med$common_id,]
  
  
  df.gate_clean[df.gate_clean$common_id == '03081988AAZZ', !colnames(df.gate_clean) %in% c('remark')]
  df.bf_staging[df.bf_staging$Code_ID == '03081988AAZZ',]
  
  #m =unique(df.bilfinger_clean[,c('full_name','firma','common_id','decl_total_working_hours','tarif')])
  #m =unique(df.mammoet_clean[,c('full_name','common_id')])
  #m =unique(df.bis_clean[,c('full_name','common_id')])
  m =unique(df.mourik_clean[,c('full_name','common_id')])
  m = m[is.na(m$full_name) == FALSE,]
  p = unique(df.gate_clean[,c('common_id','first_name','last_name','contractor','date_birth')])

  z = left_join(m,p, by = c('common_id'='common_id'))  
  z2 = z[is.na(z$first_name) == TRUE,] 
  # %>%
  #   select(-c('first_name','last_name','contractor','date_birth')) %>%
  #   mutate(
  #     total_cost = decl_total_working_hours * tarif
  #   )
  
  z3 = z[is.na(z$first_name) == FALSE,]  
  
# write.csv2(z2, "analyse/Bilfinger_unknown_persons.csv")  
# write.csv2(z2, "Mourik_unknown_persons.csv")  
# write.csv2(z2, "Mammoet_unknown_persons.csv")  
# write.csv2(z2, "BIS_unknown_persons.csv")  
# write.csv2(z2, "Mourik_unknown_persons.csv")  

# write.csv2(df_agg.hours_check_employee, "results/urencontrole_medewerker.csv")      
# write.csv2(list.audit_gate$Bilfinger_Maintenance, "results/audit_view_gate_Bilfinger_Maintenance.csv")      

  
  