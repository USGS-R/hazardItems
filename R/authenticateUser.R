#'@title authenticate a user to CCH portal
#'@description returns a token that can be used in POST or PUT methods
#'@param username an active directory username (e.g., \code{'bbadger'})
#'@param password a character password for the active directory username. 
#'Will be prompted for password if missing and in interactive mode. 
#'@param verbose boolean for verbose output. Default FALSE
#'@details if \code{authenticateUser} is called with a \code{username} argument, 
#'\code{username} is stored in the R session environment, and future calls to 
#'\code{authenticateUser} within the same R session can use the password argument 
#'only (e.g., \code{authenticateUser(password = '12345')})
#'The username and password can also be specified in the config file
#'@return a character token, or the status code if not 200
#'@importFrom httr POST accept_json content timeout verbose
#'@export 
authenticateUser <- function(username, password, verbose=FALSE){
  
  
  if (is.null(getUsername(username)) & interactive()) {
    username <- readPassword('Please enter your Active Directory username:')
  } else if (missing(username)) {
    username <- getUsername()
  } 
  if (is.null(username)) {
    stop('username required for authentication')
  }
  
  if(!interactive() & is.null(getPassword(password))){
    stop('No password supplied to authenticateUser in a non-interactive session.')
  }else{
    password = ifelse(is.null(getPassword(password)), readPassword('Please enter your Active Directory password:'), getPassword(password))
  }
  

  ## authenticate

  resp = POST(pkg.env$auth_token, accept_json(),
              body = list(username=username, password=password), 
              encode='form', timeout(5), config(verbose=verbose))
  
  if (resp$status_code == 200){

    pkg.env$username <- username
    pkg.env$authToken <- content(resp)$tokenId
    invisible(pkg.env$authToken)
  } else {
    stop('authentication for ',username,' failed ',resp$status_code)
  }
  
}

readPassword <- function(prompt) {
  .rs.askForPassword = "_private_global"
  if (exists(".rs.askForPassword")) {
    pass <- .rs.askForPassword(prompt)
  } else {
    pass <- readline(prompt)
  }
  return (pass)
}

getUsername <- function(username) {
  result <- NULL
  if (missing(username)) {
    if (is.null(pkg.env$username)) {
      configUser = getOption("hazardItems")$username
      if (!is.null(configUser) & nzchar(configUser)) {
        result = configUser
      }
    } else {
      result = pkg.env$username
    }
  } else {
    result = username
  }
  return(result)
}

getPassword <- function(password) {
  result = NULL
  if (missing(password)) {
    configPass = getOption("hazardItems")$password
    if (!is.null(configPass) & nzchar(configPass)) {
      result = configPass
    }
  }
  return(result)
}