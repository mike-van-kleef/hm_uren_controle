CleanMourikData <- function(data, job_function = "", p_department = ""){
  # This function cleans the declaration data  
  #
  # Args:
  # - data         : data frame with declaration data, in the form of output function import_files
  # - job_function : data frame with job_function data, in the form of output function import_files
  # - p_contractor : p_contractor specifies the department of Mourik
  #
  # Returns:
  # - data: data frame with clean Mourik data
  #

# delete rows with no names  
  data <- data[is.na(data$full_name) == FALSE, ]  

# remove rows with no declaration for working hours    
  data <- data[is.na(data$decl_working_hours) == FALSE,]

# transformormation date_work to date
# remove spaces and to lower  
  data <- data %>%
    mutate(
      date_work               = as.Date(date_work),
      job_function_uitgebreid = tolower(trimws(job_function)),
      job_function            = trimws(gsub('[0-9]+%','',job_function))
    )
  
# remove slaapuren
  data <- data[!grepl(pattern = 'slaap', tolower(data$job_function)),]
  
# remove ademluchtvergoeding
  data <- data[tolower(data$job_function) != 'ademluchtvergoeding',]  
  
# remove punctuations
  data$full_name         = str_replace_all(data$full_name, "[[:punct:]]", "")  

  
  
  
# clean job function
  df.job_function <- job_function %>%
    mutate(
      job_function      = trimws(gsub('[0-9]+%','',job_function)),
      job_function_type = trimws(job_function_type)
    )
  df.job_function <- unique(df.job_function[,c('job_function','job_function_type')])
  
  
# Add job function type (direct vs indirect)    
  data <- left_join(data, df.job_function, by = c('job_function' = 'job_function')) %>%
    arrange(common_id, date_work)


# Name contractor  
  data$contractor_decl = 'Mourik'
  

# NOG AANPASSEN IN BESTAND
  data <- data %>%
    mutate(

      common_id = case_when(
        common_id %in% c('XXX','NVT','Niet te vinden')  ~ 'XXXX',
        TRUE                                            ~ common_id
      ),
      
      full_name = case_when(
        full_name == 'Agelista'                 ~ 'Angelista SAM',
        full_name == 'Anjos'                    ~ 'Vitorio Dos Anjos Samuel',
        full_name == 'Averink  D'               ~ 'Averink D',
        full_name == 'Averin k D'               ~ 'Averink D',
        full_name == 'A Ouadi'                  ~ 'Ouadi A',
        full_name == 'Balijn  EG'               ~ 'Balijn EG',
        full_name == 'C Bektas'                 ~ 'Bektas C',
        full_name == 'DA Silva'                 ~ 'Silva DA',
        full_name == 'Ferreira Macedo Alegria'  ~ 'Ferreira Marcedo Alegria DJ',
        full_name == 'Ferreira Macedo Alegria D'~ 'Ferreira Marcedo Alegria DJ',
        full_name == 'Filipovoc I'              ~ 'Filipovic I',
        full_name == 'Finix  S'                 ~ 'Finix S',
        full_name == 'Halasowki M'              ~ 'Halasowski M',
        full_name == 'Jagë'                     ~ 'Jäge',
        full_name == 'Kdami A'                  ~ 'Mkadmi A',
        full_name == 'KJ Nederhand Ramires'     ~ 'Nederhand Ramirez KJ',
        full_name == 'Kramer D'                 ~ 'Kramer G',
        full_name == 'Lange  HM de'             ~ 'Lange HM de',
        full_name == 'LC Afonso'                ~ 'Afonso LC',
        full_name == 'Lopez Harteveldt'         ~ 'Lopez Harteveld',
        full_name == 'Marques'                  ~ 'Pereira Marques C',
        full_name == 'Mathoera AA'              ~ 'Mathoera A',
        full_name == 'Mathoera'                 ~ 'Mathoera A',
        full_name == 'OzturkH'                  ~ 'Ozturk H',
        full_name == 'OzturkM'                  ~ 'Ozturk M',
        full_name == 'OzturkS'                  ~ 'Ozturk S',
        full_name == 'Paap'                     ~ 'Riet Paap K van',
        full_name == 'Riet Paap KA'             ~ 'Riet Paap K van',
        full_name == 'RR Badal'                 ~ 'Badal RR',
        full_name == 'Vasile RA'                ~ 'Vasile R',
        TRUE                                    ~  full_name
        ),
      
      dienst = case_when(
        toupper(shift_type) == 'D'          ~ 'Dag',
        toupper(shift_type) == 'N'          ~ 'Nacht',
        TRUE                                ~ 'Onbekend'
      )
    ) %>%
    select(-c(job_function_uitgebreid, job_function, tarif, shift_type))

# Aggregate to common_id,full_name,date_work, week_nr,personnel_type, job_function_type, contractor_decl
  data_agg <- data %>%
    group_by(common_id,full_name,date_work, week_nr,personnel_type, job_function_type, contractor_decl) %>%
    summarise(
      counter               = n(),
      counter_key           = n_distinct(paste0(scopenummer,costcenter)),
      same_scope_costcenter = if_else(counter > counter_key, 'Ja', 'Nee'),
      decl_working_hours    = sum(decl_working_hours)
    ) %>% as.data.frame()

    
# Employee has two or more declaration on same day
  double_decl = plyr::count(data_agg[,c('common_id','full_name','date_work')])
  print(double_decl[double_decl$freq > 1,])
  cat("\n")
  
# print total number of double declaration
  cat('number of double declaration on same day:', sum(double_decl[double_decl$freq > 1,]$freq - 1), '\n')
  cat('\n','Employee with different name for same common_id', '\n')
  
# Employee has two names with same common_id
  employee = unique(data_agg[,c('common_id','full_name')])
  id_not_unique = plyr::count(employee[,c('common_id')]) 
  id_not_unique = id_not_unique[id_not_unique$freq >1,]
  print(employee[employee$common_id %in% id_not_unique$x & employee$common_id != 'XXXX', ])    
  cat("\n")

  data_agg <- data_agg %>% 
    
    # double declaration same day
    group_by(common_id, full_name, date_work) %>%
    mutate(
      double_decl_same_day = n()
    ) %>% ungroup() %>%
    
    # double name same common_id
    group_by(common_id) %>%
    mutate(
      double_name_same_common_id = n_distinct(full_name)
    ) %>%
    ungroup() %>% as.data.frame()
  
  
# aggregate data to common_id and date_work    
  data_agg        <- data_agg %>% 
    mutate(
      personnel_type_dummy = gsub(' ','_',personnel_type)
    ) %>%
    pivot_wider(
      id_cols     = c(common_id, full_name, date_work, job_function_type, double_decl_same_day, double_name_same_common_id, contractor_decl),
      names_from  = personnel_type_dummy,
      values_from = decl_working_hours,
      values_fill = 0
      ) %>% 
    
    mutate(
      decl_working_hours = Personeel + Personeel_MINT + Personeel_staf
    ) %>%  as.data.frame()
  
return(data_agg)  
  
}
  
  