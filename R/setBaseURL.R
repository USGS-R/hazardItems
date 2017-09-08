#'@title Set CCHP endpoint
#'
#'@param endpoint Indicate which CCHP endpoint 
#' you want to use options: \code{c('prod','qa','dev')}.
#' Fully qualified urls are also allowed
#' (i.e. \code{https://localhost:8080/coastal-hazards-portal/})
#'
#'@description Sets the internal URLS used to either the production, QA, or dev server. 
#'URLS are stored internally to the package
#'
#'@author Luke Winslow, Jordan S Read
#'
#'@examples
#'\dontrun{
#'setBaseURL('prod')
#'setBaseURL('qa')
#'setBaseURL('dev')
#'}
#'@export
setBaseURL = function(endpoint="prod"){
  
  endpoint = tolower(endpoint)
  
  if (endpoint=="prod"){
    pkg.env$url_base = "https://cida.usgs.gov/coastalchangehazardsportal/"
    cat('Setting endpoint to ',pkg.env$url_base,'\n')
  } else if (endpoint=="qa"){
    pkg.env$url_base = "https://cida-test.er.usgs.gov/coastalchangehazardsportal/"
    cat('Setting endpoint to ',pkg.env$url_base,'\n')
  } else if (endpoint=="dev"){
    pkg.env$url_base = "https://cida-test.er.usgs.gov/dev/coastalchangehazardsportal/"
    cat('Setting endpoint to ',pkg.env$url_base,'\n')
  } else {
    pkg.env$url_base = endpoint
    cat('Setting endpoint to ',pkg.env$url_base,'\n')
  }

  pkg.env$alias_json = paste0(pkg.env$url_base, "data/alias/")
  pkg.env$item_json = paste0(pkg.env$url_base, "data/item/")
  pkg.env$item_sld = paste0(pkg.env$url_base, "data/sld/")
  pkg.env$item_layer = paste0(pkg.env$url_base, 'data/layer/')
  pkg.env$tile_cache = paste0(pkg.env$url_base,'data/cache/')
  pkg.env$item_template = paste0(pkg.env$url_base, 'data/template/item/')
  pkg.env$item_download = paste0(pkg.env$url_base, 'data/download/item/')
  pkg.env$item_thumbnail = paste0(pkg.env$url_base, "data/thumbnail/item/")
  pkg.env$auth_token = paste0(pkg.env$url_base, "authentication/auth/authenticate/")
  pkg.env$auth_check = paste0(pkg.env$url_base, "security/auth/check/")
  pkg.env$item_thumbnail = paste0(pkg.env$url_base, "data/thumbnail/item/")
  pkg.env$authToken = NULL
  pkg.env$username = NULL

}
