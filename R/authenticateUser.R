#'@title authenticate a user to CCH portal
#'@description returns a token that can be used in POST or PUT methods
#'@param username an active directory username (e.g., \code{'bbadger'})
#'@param password a character password for the active directory username. 
#'Will be prompted for password if missing and in interactive mode. 
#'@details if \code{authenticateUser} is called with a \code{username} argument, 
#'\code{username} is stored in the R session environment, and future calls to 
#'\code{authenticateUser} within the same R session can use the password argument 
#'only (e.g., \code{authenticateUser(password = '12345')})
#'@return a character token, or the status code if not 200
#'@importFrom httr POST accept_json content
#'@export 
authenticateUser <- function(username, password){
  
  
  if (missing(username)) {
    username <- pkg.env$username
  }
  if (is.null(username)) {
    stop('username required for authentication')
  }
  
  if(!interactive() & missing(password)){
    stop('No password supplied to authenticateUser in a non-interactive session.')
  }else{
    password = ifelse(missing(password), readPassword('Please enter your Active Directory password:'), password)
  }
  

  ## authenticate
  resp = POST(pkg.env$auth_token, accept_json(),
              body = list(username=username, password=password), encode='form')
  if (resp$status_code == 200){
    pkg.env$username <- username
    return(content(resp)$tokenId)
  } else {
    return(stop('authentication for ',username,' failed ',resp$status_code))
  }
  
}

readPassword <- function(prompt) {
  if (exists(".rs.askForPassword")) {
    pass <- .rs.askForPassword(prompt)
  } else {
    pass <- readline(prompt)
  }
  return (pass)
}

