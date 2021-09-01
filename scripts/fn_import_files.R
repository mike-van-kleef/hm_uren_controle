
import_files <- function(p.meta = FALSE, filemeta = "", filename, sheet.nr, skip_rows ){
# read csv
# df2 <- read.csv("data/raw/poort/totaallijst_zonder_eigen_personeel.csv", sep = ',')
# colnames(df2)[1] <- 'first_name'
# df2$date_time    <- as.POSIXct(df2$date_time, format = '%y-%m-%d %H:%M:%S', tz = 'UTC') --> seconden weg


# List of all Excel sheets
excel.sheets <- readxl::excel_sheets(filename)
length(excel.sheets)


if(p.meta == TRUE){
  
  # meta
  meta             <- readxl::read_excel(filemeta)  
  meta$column_name <- if_else(is.na(meta$column_name) == TRUE, meta$column_letter, meta$column_name)
    
  # Create data frame from Microsoft Excel sheet
  df               <- readxl::read_excel(filename, sheet = sheet.nr, col_types = meta$data_type, skip = skip_rows)
  colnames(df)     <- meta[meta$delete_ind == 'N',]$column_name

} else {
  
  # Create data frame from Microsoft Excel sheet
  df <- readxl::read_excel(filename, sheet = sheet.nr, skip = skip_rows)
  
}

df <- as.data.frame(df)
  
return(df)

}