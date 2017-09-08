#'@title clean and validate alias
#'@description checks wheter a provided alias is valid and cleans what can be cleaned
#'@param alias the alias to check
#'@return the cleaned alias or NULL if the provided alias is invalid
#'@export 
cleanAndValidateAlias <- function(alias){
  #Trim whitespace
  alias <- trimws(alias)
  
  #Convert to lowercase
  alias <- tolower(alias)
  
  #Check for invalid characters
  alias <- ifelse(grepl('^[a-z0-9-]+$', alias), alias, "")
  
  #Check for valid length
  if(nchar(alias) > 0){
    return(alias)
  } else {
    return(NULL)
  }
}