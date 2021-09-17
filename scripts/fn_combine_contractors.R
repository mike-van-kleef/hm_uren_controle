CombineContractorsData <- function(bilfinger, mourik){
  # This function combines the declaration data of Mourik 
  #
  # Args:
  # - bilfinger  : data frame with declaration data of Bilfinger, in the form of output function CleanBilfingerData
  # - mourik     : data frame with declaration data of Mourik, in the form of output function CombineMourikData
  #
  # Returns:
  # - data: data frame with declaration data of Mourik 
  #

  
# combine data of personeel and international
  data           <- rbind(pers, mint) %>% arrange(common_id, full_name)


  
return(data)  
  
}
  
  