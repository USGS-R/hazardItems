hazardItems
==============
[![Build Status](https://travis-ci.org/USGS-R/hazardItems.svg)](https://travis-ci.org/USGS-R/hazardItems)  
[![Coverage Status](https://coveralls.io/repos/USGS-R/hazardItems/badge.svg)](https://coveralls.io/r/USGS-R/hazardItems)

services for coastal change hazards items

Getting started in R is easy! Check out how to set up your local environment with <a href="http://owi.usgs.gov/R/resources.html">this helpful set of resources from the USGS Office of Water Information R Community</a>. At a minimum, you'll need to have <a href="http://cran.rstudio.com/">R installed on your machine</a>.

#Installation
To install **hazardItems** 

Install package with dependencies:
```{r, eval=FALSE}
install.packages("hazardItems",
repos = c("http://owi.usgs.gov/R"),
dependencies = TRUE, type = 'both')
```

#Updating 
To update **hazardItems**

```{r, eval=FALSE}
update.packages(repos = c("http://owi.usgs.gov/R", "http://cran.rstudio.com/"))
```

##Real Time Storm Item Creation
A real-time storms event item is established in a special way. When it needs to be updated, the parent aggregation is dropped and re-created each time a model update is run and NACCH scientists want to update the data by using the `createStorm()` function. The parent item represents the storm. For example, "Hurricane Sandy" could be the title of a parent aggregation item for a real-time storm. The NOAA NHC Hurricane cone, track and points are all added automatically when the `createStorm()` function is run. 

<br />
<br />
An example workflow to create a new real time storm would involve the following:

```{r,eval=FALSE}
library(hazardItems)
setBaseURL("dev") #setBaseURL("prod") in the event of actual storm. setBaseURL("qa") in the event of testing on the qa server.
createStorm(templateId = NULL, "D:\\mkhdata\\model_output\\Sandy_CIDA060520150800.zip") #don't forget to double escape slashes if you are running this command in a Windows environment.
```

Where the model output shapefile is zipped into a zip file and pointed through in the arguments above.

When this function completes, the user must record the ID returned in the console (e.g. CAQw7M1) and use that argument in the function `templateId = "CAQw7M1")` for future updates to the same active storm.

##Updating a Real Time Storm Item
If a user wants to update the model run output on an existing real time storm item, they must have the templateId handy, and follow the example workflow to update the real time storm:

```{r,eval=FALSE}
library(hazardItems)
setBaseURL("dev") #setBaseURL("prod") in the event of actual storm.
createStorm(templateId = "CAQw7M1", "D:\\mkhdata\\model_output\\Sandy_CIDA060520151400.zip")
```

When this function is run to replace the existing out of date item, the previously posted parent and children are orphaned. Record the new templateId returned when the function is successful in order to replace with future updates.

Disclaimer
----------
This software is in the public domain because it contains materials that originally came from the U.S. Geological Survey, an agency of the United States Department of Interior. For more information, see the official USGS copyright policy at [http://www.usgs.gov/visual-id/credit_usgs.html#copyright](http://www.usgs.gov/visual-id/credit_usgs.html#copyright)

This information is preliminary or provisional and is subject to revision. It is being provided to meet the need for timely best science. The information has not received final approval by the U.S. Geological Survey (USGS) and is provided on the condition that neither the USGS nor the U.S. Government shall be held liable for any damages resulting from the authorized or unauthorized use of the information. Although this software program has been used by the USGS, no warranty, expressed or implied, is made by the USGS or the U.S. Government as to the accuracy and functioning of the program and related program material nor shall the fact of distribution constitute any such warranty, and no responsibility is assumed by the USGS in connection therewith.

This software is provided "AS IS."


 [
    ![CC0](http://i.creativecommons.org/p/zero/1.0/88x31.png)
  ](http://creativecommons.org/publicdomain/zero/1.0/)
