CleanMammoetData <- function(data, job_function = ""){
  # This function cleans the declaration data 
  #
  # Args:
  # - data: data frame with declaration data, in the form of output function import_files
  # - func: data frame with job_function data, in the form of output function import_files
  #
  # Returns:
  # - data: data frame with clean mammoet data
  #

# delete rows with no names  
  data <- data[is.na(data$full_name) == FALSE, ] 
  
# sort data by common_id, week_nr
  data <- data %>%
    #filter(labour_equipment_ind == 'Labour Costs') %>%
    arrange(common_id, week_nr, cost_center_code)


# NOG AANPASSEN IN BESTAND
  data <- data %>%
    mutate(
      common_id = case_when(
        full_name             == 'Creemers, J.'    ~ '20011971CREM',
        full_name             == 'Kok'             ~ '23121998KOK',
        full_name             == 'Vryssier, M.'    ~ '27101974VAYS',
        is.na(common_id)      == TRUE              ~ 'XXXX',
        substr(common_id,1,1) == '?'               ~ 'XXXX',
        TRUE                                       ~ toupper(common_id)
      ),
      
      
      full_name = case_when(
        full_name == 'Bal, A,.'               ~ 'Bal, A.',
        full_name == 'Vette N de.'            ~ 'Vette, N de.',
        full_name == 'Bruin, S. de.'          ~ 'Bruin, S de.',
        full_name == 'Bie, R de.'             ~ 'Bie, R. de',
        full_name == 'Brooks, A.'             ~ 'Brooks, A',
        TRUE                                  ~ full_name
        ),
      
      week_nr = sub("week *", "", tolower(week_nr))
      )

# delete rows without common_id
  cat('\n','Employee without common_id', '\n')
  teller <- nrow(data[is.na(data$common_id) == TRUE,])
  if(teller > 0 ){
  print(data %>% filter(is.na(common_id) == TRUE))}

    
#----------------------------------------------------------------------------------------
# pivot columns with start, eind and pauze to rows  -------------------------------------

# 1) select columns not to pivot 
# 2) pivot workday_start  
# 3) pivot workday_eind
# 4) pivot workday_pauze
#----------------------------------------------------------------------------------------  

  pivot_df <- function(data_key, data, p_pattern = ""){    
    
    # helper function for step 2), 3) and 4)  
    # This function selects columns with specific pattern from data frame and combines with key colomns
    #
    # Args:
    # - data_key  : data frame with key columns
    # - data      : data frame whose columns are pivoted that satisfy pattern 
    # - p_pattern : pattern to which the column names conform
    
    # create index with columns with pattern  
    index  = grepl(pattern = p_pattern,colnames(data))  
    
    # combine columns not to pivot with columns with a workday start
    data_pattern <- cbind(data_key, data[,colnames(data)[index]])
    index        <- grepl(pattern = p_pattern,colnames(data_pattern))  
    
    # days of the to rows
    data_pattern <- data_pattern %>%
      pivot_longer(
        cols = colnames(data_pattern)[index],
        names_to  = paste0('decl_day_',p_pattern),
        values_to = paste0('decl_time_',p_pattern)
      ) %>% 
      as.data.frame()
    
    return(data_pattern)  
  }
  
  

# 1) select columns not to pivot
  data_all <- data %>%
    select(full_name, common_id, week_nr, decl_total_working_hours, tarif) %>%
    group_by(full_name, common_id, week_nr) %>%
    summarise(
      decl_total_working_hours = sum(difftime(decl_total_working_hours, as.POSIXct('1899-12-31 00:00:00',  format = "%Y-%m-%d %H:%M:%S", tz = 'UTC'), unit = 'hours'))
    )

  x = plyr::count(data,c('full_name', 'common_id', 'cost_center_code', 'week_nr'))
  x[x$freq>1,]

  y = unique(data[,c('full_name', 'common_id')])
  z = plyr::count(y,'common_id')
  z = z[z$freq>1,]
  y[y$common_id %in% z$common_id,]
    
# 2) data_start 3) data_eind and 4) data_pauze
 data_start  <- pivot_df(data_key = data[,c('common_id', 'full_name','week_nr','cost_center_code')], data = data, p_pattern = 'start')  %>% 
   mutate(
     day = substr(decl_day_start,1,2),
     ) %>%  filter(is.na(decl_time_start) == FALSE)
 
 data_eind   <- pivot_df(data_key = data[,c('common_id', 'full_name','week_nr','cost_center_code')], data = data, p_pattern = 'eind')   %>%
   mutate(
     day              = substr(decl_day_eind,1,2),
     ) %>%  filter(is.na(decl_time_eind) == FALSE)
 
 data_pauze  <- pivot_df(data_key = data[,c('common_id', 'full_name','week_nr','cost_center_code')], data = data, p_pattern = 'pauze')  %>%
   mutate(
     day              = substr(decl_day_pauze,1,2),
     decl_time_pauze  = format(decl_time_pauze,  format = "%H:%M:%S")
     ) %>%  filter(is.na(decl_time_pauze) == FALSE)

 data_samen <- data_start %>%
   left_join(data_eind,  by = c('common_id', 'full_name' ,'week_nr','cost_center_code','day')) %>%
   left_join(data_pauze, by = c('common_id', 'full_name' ,'week_nr','cost_center_code','day')) %>%
   mutate(
     decl_time_eind_dummy      = if_else(decl_time_eind < decl_time_start, decl_time_eind + 3600*24, decl_time_eind),
     decl_time_pauze           = if_else(is.na(decl_time_pauze) == TRUE, 0 , minute(hms(decl_time_pauze)) / 60),
     decl_working_hours_bruto  = as.numeric(difftime(decl_time_eind_dummy, decl_time_start, unit = 'hours')),
     decl_working_hours        = decl_working_hours_bruto - decl_time_pauze,
     
     day_nr                    = case_when(
       day == 'ma' ~ 1,
       day == 'di' ~ 2,
       day == 'wo' ~ 3,
       day == 'do' ~ 4,
       day == 'vr' ~ 5,
       day == 'za' ~ 6,
       day == 'zo' ~ 7
       
     ) 
   ) 
 
 data_samen_agg <- data_samen %>%
   group_by(common_id, full_name, week_nr,day, day_nr) %>%
   summarise(
     double_decl_same_day = n(),
     decl_total_working_days = -1,
     decl_working_hours   = sum(decl_working_hours)
   ) %>% as.data.frame()
   
 
# combine data frame with data_all
  decl <- data_samen_agg %>%
   inner_join(data_all,  by = c('common_id','full_name','week_nr')) 

# No correction on early arrival and late departed. Hereby all job_function_type are indirect
# Add attributes
  decl <- decl %>%
    mutate(
      job_function_type = 'Indirect',
      job_function      = 'Onbekend',
      date_work         = if_else(day == 'zo',as.Date(paste(2021, week_nr, day_nr, sep="-"), "%Y-%W-%u") + 7, as.Date(paste(2021, week_nr, day_nr, sep="-"), "%Y-%W-%u")),
      contractor_decl   = 'Mammoet'
    ) %>%
    # double name same common_id
    group_by(common_id) %>%
    mutate(
      double_name_same_common_id = n_distinct(full_name)
    ) %>%
    ungroup() %>% 
    arrange(common_id, full_name, week_nr, day_nr) %>% as.data.frame()




    
# Employee has two or more declaration on same day
  cat("\n")
  cat("Employee has two or more declaration on same day","\n")
  print(decl[decl$double_decl_same_day > 1,])
  cat("\n")
  
# print total number of double declaration
  cat('number of double declaration on same day:', sum(decl[decl$double_decl_same_day > 1,]$double_decl_same_day -1), '\n')
  
  
  
# Employee has two names with same common_id
  cat('\n','Employee with different name for same common_id', '\n')
  employee = unique(decl[,c('common_id','full_name')])
  id_not_unique = plyr::count(employee[,c('common_id')]) 
  id_not_unique = id_not_unique[id_not_unique$freq >1,]
  print(employee[employee$common_id %in% id_not_unique$x & employee$common_id != 'XXXX', ])    
  cat("\n")

  
  
return(decl)  
  
}
  
  