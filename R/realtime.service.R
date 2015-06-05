#'@title create summary json for an individual storm item
#'@description This service parses the metadata record and creates a summary specific to the STORM theme, 
#'with subtype being defined by the attribute name
#'@param serviceEndpoint valid xml, either local or a url
#'@param attribute an attribute to use form the shapefile corresponding to 
#'\code{serviceEndpoint}
#'@return Serialized JSON for summary
#'@importFrom jsonlite toJSON
#'@import XML
#'@examples
#'serviceEndpoint  <-	'http://olga.er.usgs.gov/data/NACCH/GOM_erosion_hazards_metadata.xml'
#'attribute	<-	'PCOL'
#'summary	<-	realtime.service(serviceEndpoint,attribute)
#'print(summary)
#'@export
realtime.service = function(serviceEndpoint,attribute=NULL){
	subType	<-	tolower(attribute)

	subTypeDataSrc <- names(sourceSynonyms)

	doc <- xmlInternalTreeParse(serviceEndpoint)
	title <- xmlValue(getNodeSet(doc,'//citation/citeinfo/title')[[1]])
  titleParts <- extractInfoFromTitle(title)
  
	abstract	<-	xmlValue(getNodeSet(doc,'//descript/abstract')[[1]])
	dataSrc <-  sapply(getNodeSet(doc,'//dataqual/lineage/srcinfo/srccite/citeinfo/title'),xmlValue)
  sourceContent <-  paste(dataSrc,collapse='|')
  
  # NULL if it is for the whole storm
  if (length(subType) == 0) {
    tiny.text <- paste(titleParts$name, "Assessment of Potential Coastal-Change Impacts: NHC Adv.", titleParts$advNum)
    medium.title <- titleParts$name
    medium.text <- paste0("Potential coastal change impacts during a direct landfall of ",
                         titleParts$name, ": ", titleParts$advFull)
    full.title <- title
    full.text <- paste("This dataset contains a coastal erosion hazards analysis for",
                        paste0(titleParts$name, "."), "The analysis is based on a storm-impact scaling model",
                        "that combines observations of beach morphology with hydrodynamic models to predict how sandy beaches,",
                        "the first line of defense for many coasts exposed to tropical storms and hurricanes, will respond during",
                        "a direct landfall. Storm-induced total water levels, due to both surge and waves, are compared to beach",
                        "and dune elevations to determine the probabilities of three types of coastal change - collision (dune erosion),",
                        "overwash, and inundation.")
    full.text <- paste(full.text, getSurgeDescription(doc))
    full.text <- paste(full.text, "Maximum wave heights in 20-m water depth, obtained from the NOAA WaveWatch3",
                       "model 7-day forecast, were used to compute wave runup elevations at the shoreline.",
                       "Dune elevations were extracted from lidar topographic surveys.",
                       "\n\nDisclaimer: This product is based on published research results of the USGS National",
                       "Assessment of Coastal Change Hazards Project and is intended to indicate the potential",
                       "for coastal change caused by storm surge and wave runup. This product is based on an",
                       "analysis that simplifies complex coastal change processes to two important aspects -",
                       "measured dune elevations and predicted total water levels. As such, the actual changes",
                       "that occur during extreme storms may be different than what is described here. Results",
                       "apply to open coast environments and do not consider potential coastal change along",
                       "inland waters. The public should not base evacuation decisions on this product. Citizens",
                       "should follow the evacuation advice of local emergency management authorities.")
    full.publications <- list(data=list(),publications=list(),resources=list())
    keywords <- ""
  } else {
    tiny.text <- paste(titleMap$medium[[subType]], "during", paste0(titleParts$name, ":"), "NHC Adv.", titleParts$advNum)
    medium.title <- paste(titleMap$medium[[subType]])
    
    if (subType == "pcol" | subType == "povw" | subType == "pind") {
      description <- getPCOIDescription(subType, titleParts, doc)
      full.title <- paste(titleMap$medium[[subType]], "during", paste0(titleParts$name, ":"), titleParts$advFull)
      medium.text <- description$medium
      full.text <- description$full
    } else if (subType == "dhigh" | subType == "dlow") {
      collectionDate <- extractCollectionDate(doc, subType)
      attrDef <- ifelse(subType == "dhigh", 
        "The elevation of the dune crest, or top of the foredune,",
        "The elevation of the dune toe, or ocean-side base of the foredune,")
      full.title <- paste(titleMap$medium[[subType]], "prior to", titleParts$name)
      medium.text <- paste(titleMap$medium[[subType]], "(m, NAVD88) for open coast sandy beaches every 1 km alongshore.")
      full.text <- paste("This dataset contains", tolower(titleMap$medium[[subType]]),
                         "(m, NAVD88) for the United States coastline.", attrDef,
                         "was extracted for open coast sandy beaches from gridded",
                         "lidar topography every 10 m alongshore and then averaged",
                         "in 1-km bins. Lidar surveys were collected from",
                         paste0(collectionDate, "."))
    } else if (subType == "mean" | subType == "extreme") {
      surge <- getSurgeDescription(doc)
      full.title <- paste("Modeled", tolower(titleMap$medium[[subType]]), "during",
                          paste0(titleParts$name), titleParts$advFull)
      medium.text <- paste("The storm-induced", ifelse(subType == "mean",
                           "mean water levels,", 
                           "extreme (98% exceedance) water levels,"),
                           "at the shoreline for", paste0(titleParts$name, ": ", titleParts$advFull, "."))
      full.text <- paste("This dataset contains modeled storm-induced", ifelse(subType == "mean",
                             "mean water levels", 
                             "extreme (98% exceedance) water levels"),
                         "at the shoreline during", paste0(titleParts$name, "."),
                         "Values were computed by summing modeled storm",
                         paste0("surge and parameterized wave", ifelse(subType == "mean",
                         "setup, the increase in mean water level at the shoreline due to breaking waves.", "runup.")),
                         surge, "Maximum wave heights in 20-m water depth, obtained from the NOAA",
                         "WaveWatch3 model 7-day forecast, were used to compute wave setup at the shoreline.")
      tiny.text <- paste("Modeled", gsub("mean", "Mean", gsub("extreme", "Probability", tiny.text)))
    }
    
    full.publications  <-	getPublications(doc)
    keywords	<-	getKeywords (doc,subType)
  }
	
	summaryJSON	<- toJSON(list(
		'version'=as.character(packageVersion(getPackageName())),
		'tiny'=list('text'=tiny.text),
		'medium'=list('title'=medium.title,'text'=medium.text),
		'full'=list('title'=full.title,'text'=full.text,'publications'=full.publications),
		'keywords'=keywords), auto_unbox = TRUE )
	summaryJSON	<- sub('NULL. ','',summaryJSON)
	return(summaryJSON)
}

extractInfoFromTitle = function(title) {
  # Hurricane Sandy Assessment of Potential Coastal-Change Impacts: NHC Advisory 29, 1100 AM EDT MON OCT 29 2012
  match <- regexec("(\\w+ \\w+).*(NHC Advisory (\\d+), (.*))", title)
  parts <- as.list(regmatches(title, match)[[1]][c(2, 3, 4, 5)])
  names(parts)<- c("name", "advFull", "advNum", "time")
  return(parts)
}

extractCollectionDate = function(doc, attr) {
  attrDescs <- sapply(getNodeSet(doc,paste0('//eainfo/detailed/attr')), xmlChildren)
  matchingAttr <- Filter(function(f){
    grepl(attr, xmlValue(f$attrlabl), ignore.case = TRUE)
  }, attrDescs)
  defn <- xmlValue(matchingAttr[[1]]$attrdef)
  match <- regexec(".*?(\\w+ \\d{4} to \\w+ \\d{4})\\.?", defn)
  collected <- regmatches(defn, match)[[1]][2]
  return(collected)
}

getSurgeDescription = function(doc) {
  text <- paste("The storm surge elevations along the open coast were obtained from the",
                "National Oceanic and Atmospheric Administration's (NOAA)")
  srcUsed <- sapply(getNodeSet(doc, '//dataqual/lineage/procstep/srcused'), xmlValue)
  if (has(srcUsed, "psurge", ignore.case=TRUE)) {
    text <- paste(text, "probabilistic surge forecast (psurge), which is based on conditions specific to the",
                       "landfalling storm. Errors in hurricane forecasts are included in order to identify probable surge levels.",
                       "The 10% exceedance surge level was used to represent the worst-case scenario.")
  } else if (has(srcUsed, "estofs", ignore.case=TRUE)) {
    text <- paste(text, "ESTOFS (Extratropical Surge and Tide Operational Forecast System).")
  } else if (has(srcUsed, "moms", ignore.case=TRUE)) {
    text <- paste(text, "Sea, Lake, and Overland Surges from Hurricanes (SLOSH) model, maximum of the maximum (MOM).")
  } else {
    # this shouldn't happen
    text <- paste(text, ".")
  }
  return(text)
}

getPCOIDescription = function(subType, titleParts, ...) {
  definition <- paste("probability of",
                  paste0(titleMap$full[[subType]], ","), "or the likelihood",
                  "that wave runup and storm surge will",
                  ifelse(subType == "pcol", "reach the dune toe,", 
                         ifelse(subType == "povw", "overtop the dune crest,",
                                "submerge the beach and dune crest,")))
  
  full <- paste("These data represent the", definition, "during",
                 paste0(titleParts$name, "."),
                  "Estimates were based on observations of dune morphology and modeled",
                  "storm surge and wave runup.",
                 getSurgeDescription(...),
                 "Maximum wave heights in 20-m water depth, obtained from the NOAA WaveWatch3",
                 "model 7-day forecast, were used to compute wave runup elevations at the",
                 "shoreline. Dune elevations were extracted from lidar surveys.")
  medium <- paste(gsub("probability", "Probability", definition), "during", 
                  paste0(titleParts$name, ":"), titleParts$advFull)
  return(list("medium"=medium, "full"=full))
}

has = function(x, pattern, ignore.case=FALSE) {
  has <- grepl(pattern=pattern, x=x, ignore.case=ignore.case)
  has <- Reduce("|", has)
  return(has)
}