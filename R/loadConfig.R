#'@title Load CCH config
#'
#'@description Loads the config file into options which are
#'used elsewhere in the application
#'
#'@author Jordan Walker
#'
#'@examples
#'loadConfig()
#'@importFrom yaml yaml.load_file
#'@export
loadConfig = function(filename) {
  if (missing(filename)) {
    filename <- file.path(Sys.getenv("HOME"), ".R", "hazardItems.yaml")
  }
  if (!file.exists(filename)) {
    copyDefaultConfig(filename)
  }
  
  config <- yaml.load_file(filename)
  options("hazardItems"=config)
}

copyDefaultConfig <- function(filename) {
 
  configDir <- dirname(filename)
  if (!file.exists(configDir)) {
    dir.create(configDir, recursive = TRUE)
  }
  file.copy(system.file("extdata", "hazardItems.yaml", package="hazardItems"), filename, overwrite=FALSE)
}