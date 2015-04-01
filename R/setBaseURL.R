#'@title Set CCHP endpoint
#'
#'@param endpoint Indicate which CCHP endpoint 
#' you want to use options: \code{c('prod','dev','qa')}
#'
#'@description Sets the internal URLS used to either the production, dev or QA server. 
#'URLS are stored internally to the package
#'
#'@author Luke Winslow, Jordan S Read
#'
#'@examples
#'\dontrun{
#'set_endpoint('prod')
#'set_endpoint('dev')
#'}
#'@export
setBaseURL = function(endpoint="prod"){
  
  endpoint = tolower(endpoint)
  
  if (endpoint=="prod"){
    pkg.env$url_base = "https://marine.usgs.gov/coastalchangehazardsportal/"
    pkg.env$url_auth = "https://cida.usgs.gov/coastalchangehazardsportal/"
    cat('Setting endpoint to ',pkg.env$url_base,'\n')
  } else if (endpoint=="qa"){
    pkg.env$url_base = "https://cida-test.er.usgs.gov/coastalchangehazardsportal/"
    pkg.env$url_auth = "https://cida-test.er.usgs.gov/coastalchangehazardsportal/"
    cat('Setting endpoint to ',pkg.env$url_base,'\n')
  } else if (endpoint=="dev"){
    pkg.env$url_base = "https://cida-test.er.usgs.gov/dev/coastalchangehazardsportal/"
    pkg.env$url_auth = "https://cida-test.er.usgs.gov/dev/coastalchangehazardsportal/"
    cat('Setting endpoint to ',pkg.env$url_base,'\n')
  } else if (endpoint=="qa"){
    pkg.env$url_base = "https://cida-test.er.usgs.gov/coastalchangehazardsportal/"
    pkg.env$url_auth = "https://cida-test.er.usgs.gov/coastalchangehazardsportal/"
    cat('Setting endpoint to ',pkg.env$url_base,'\n')
  } else {
    error('Unsupported endpoint option')
  }

  pkg.env$item_json = paste0(pkg.env$url_base, "data/item/")
  pkg.env$item_sld = paste0(pkg.env$url_base, "data/sld/")
  pkg.env$item_layer = paste0(pkg.env$url_base, 'data/layer/')
  pkg.env$item_template = paste0(pkg.env$url_base, 'data/template/item/')
  pkg.env$auth_token = paste0(pkg.env$url_auth, "authentication/auth/authenticate/")
  pkg.env$username = NULL
}
