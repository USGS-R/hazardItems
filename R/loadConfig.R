#'@title Load CCH config
#'
#'@description Loads the config file into options which are
#'used elsewhere in the application
#'
#'@author Jordan Walker
#'
#'@examples
#'loadConfig()
#'@importFrom yaml as.yaml yaml.load_file
#'@export
loadConfig = function(filename) {
  if (missing(filename)) {
    filename <- file.path(Sys.getenv("HOME"), ".R", "hazardItems.yaml")
  }
  if (!file.exists(filename)) {
    createDefaultConfig(filename)
  }
  
  config <- yaml.load_file(filename)
  options("hazardItems"=config)
}

createDefaultConfig <- function(filename) {
  defaults = list("username"="", "password"="", 
                  "realtime.storms"=list("trackId"="DvDJ6Vcg"))
  defaultYaml <- as.yaml(defaults)
  configDir <- dirname(filename)
  if (!file.exists(configDir)) {
    dir.create(configDir, recursive = TRUE)
  }
  file.create(filename)
  
  fh <- file(filename)
  cat(defaultYaml, file=fh)
  close(fh)
}