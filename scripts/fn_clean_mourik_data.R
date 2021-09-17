CleanMourikData <- function(data, job_function = "", p_department = ""){
  # This function cleans the declaration data  
  #
  # Args:
  # - data         : data frame with declaration data, in the form of output function import_files
  # - job_function : data frame with job_function data, in the form of output function import_files
  # - p_contractor : p_contractor specifies the department of Mourik
  #
  # Returns:
  # - data: data frame with clean bilfinger data
  #

# delete rows with no names  
  data <- data[is.na(data$full_name) == FALSE, ]  

# remove rows with no declaration for working hours    
  data <- data[is.na(data$decl_working_hours) == FALSE,]

# transformormation date_work to date
# remove spaces and to lower  
  data <- data %>%
    mutate(
      date_work         = as.Date(as.numeric(date_work),origin = '1899-12-30'),
      job_function      = tolower(trimws(job_function)),
    )

  # remove punctuations
  data$full_name         = str_replace_all(data$full_name, "[[:punct:]]", "")  
  

  
  
# clean job function
  job_function <- job_function %>%
    mutate(
      job_function      = tolower(trimws(job_function)),
      job_function_type = trimws(job_function_type)
    )
  
  
  
# Add job function type (direct vs indirect)    
  data <- left_join(data, job_function, by = c('job_function' = 'job_function')) %>%
    arrange(common_id, date_work)


# Name contractor  
  data$contractor_decl = p_department
  

# NOG AANPASSEN IN BESTAND
  data <- data %>%
    mutate(

      full_name = case_when(
        full_name == 'Averink  D'               ~ 'Averink D',
        full_name == 'Averin k D'               ~ 'Averink D',
        full_name == 'A Ouadi'                  ~ 'Ouadi A',
        full_name == 'Balijn  EG'               ~ 'Balijn EG',
        full_name == 'C Bektas'                 ~ 'Bektas C',
        full_name == 'DA Silva'                 ~ 'Silva DA',
        full_name == 'Ferreira Macedo Alegria'  ~ 'Ferreira Marcedo Alegria DJ',
        full_name == 'Filipovoc I'              ~ 'Filipovic I',
        full_name == 'Finix  S'                 ~ 'Finix S',
        full_name == 'Halasowki M'              ~ 'Halasowski M',
        full_name == 'KJ Nederhand Ramires'     ~ 'Nederhand Ramirez KJ',
        full_name == 'Lange  HM de'             ~ 'Lange HM de',
        full_name == 'LC Afonso'                ~ 'Afonso LC',
        full_name == 'Mathoera AA'              ~ 'Mathoera A',
        full_name == 'OzturkH'                  ~ 'Ozturk H',
        full_name == 'OzturkM'                  ~ 'Ozturk M',
        full_name == 'OzturkS'                  ~ 'Ozturk S',
        full_name == 'Riet Paap KA'             ~ 'Riet Paap K van',
        full_name == 'RR Badal'                 ~ 'Badal RR',
        TRUE                                    ~  full_name
        ),
      
      dienst = case_when(
        toupper(night_shift) == 'D'          ~ 'Dag',
        toupper(night_shift) == 'N'          ~ 'Nacht',
        TRUE                                 ~ 'Onbekend'
      )
    ) %>%
    select(-c(job_function, tarif))


    
# Employee has two or more declaration on same day
  double_decl = plyr::count(data[,c('common_id','full_name','date_work')])
  print(double_decl[double_decl$freq > 1,])
  cat("\n")
  
# print total number of double declaration
  cat('number of double declaration on same day:', sum(double_decl[double_decl$freq > 1,]$freq - 1), '\n')
  cat('\n','Employee with different name for same common_id', '\n')
  
# Employee has two names with same common_id
  employee = unique(data[,c('common_id','full_name')])
  id_not_unique = plyr::count(employee[,c('common_id')]) 
  id_not_unique = id_not_unique[id_not_unique$freq >1,]
  print(employee[employee$common_id %in% id_not_unique$x & employee$common_i!= 'XXXX', ])    
  cat("\n")

  data <- data %>% 
    
    # double declaration same day
    group_by(common_id, full_name, date_work) %>%
    mutate(
      double_decl_same_day = if_else( n() >1, 'Niet te bepalen', 'Niet te bepalen'),
    ) %>% ungroup() %>%
    
    # double name same common_id
    group_by(common_id) %>%
    mutate(
      double_name_same_common_id = n_distinct(full_name)
    ) %>%
    ungroup() %>% as.data.frame()
  
  
# Aggregate to employee and date at work  
  data <- data %>%
    group_by(common_id, full_name, date_work, job_function_type, contractor_decl, double_decl_same_day, double_name_same_common_id) %>%
    summarise(
      decl_working_hours       = sum(decl_working_hours)
    ) %>% as.data.frame()
  
return(data)  
  
}
  
  