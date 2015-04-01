#'@title authenticate a user to CCH portal
#'@description returns a token that can be used in POST or PUT methods
#'@param username an active directory username (e.g., \code{'bbadger'})\
#'@param password a character password for the active directory username. 
#'Will be prompted for password if missing and in interactive mode
#'@return a character token
#'@importFrom httr POST accept_json
#'@export 
authenticateUser <- function(username, password){
  
  
  if(missing(username)){
    stop('username required for authentication')
  }
  
  if(!interactive() & missing(password)){
    stop('No password supplied to authenticateUser in a non-interactive session.')
  }else{
    password = ifelse(missing(password), readPassword('Please enter your Active Directory password:'), password)
  }
  

  ## authenticate
  resp = POST(pkg.env$auth_token, accept_json(),
              body = list(username=username, password=password), encode='form', 
              config=list(ssl.verifypeer = FALSE))
  return(content(resp)$tokenId)
}

readPassword <- function(prompt) {
  if (exists(".rs.askForPassword")) {
    pass <- .rs.askForPassword(prompt)
  } else {
    pass <- readline(prompt)
  }
  return (pass)
}

