\name{thumb.service}
\alias{thumb.service}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{create thumbnail for an item}
\description{takes json url and creates summary image for item}
\usage{
thumb.service(json.url)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{json.url}{ a valid JSON url}
}
\value{
A strong location for the created png image
}
\author{
Jordan S. Read
}
\seealso{
\code{historical.service}
\code{storm.service}
\code{vulnerability.service}
}
\examples{
	json.url	<-	'http://cida-test.er.usgs.gov/coastalchangehazardsportal/data/item/C67pzz9'
	thumb.service(json.url)

}


\keyword{coastal}
\keyword{hazards}
\keyword{thumbnail}


