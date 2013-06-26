\name{storm.service}
\alias{storm.service}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{create various levels of summary for an individual storm item}
\description{This service parses the metadata record and creates a summary specific to the \emph{STORM} theme, with subtype being defined by the attribute name}
\usage{
storm.service(serviceEndpoint,attribute)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{serviceEndpoint}{ a valid WFS endpoint}
  \item{attribute}{ a valid and supported attribute name}
}
\value{
A JSON object as summary holder
}
\author{
Jordan S. Read
}
\seealso{
\code{historical.service}
\code{vulnerability.service}
}


\keyword{coastal}
\keyword{hazards}
\keyword{storm}

