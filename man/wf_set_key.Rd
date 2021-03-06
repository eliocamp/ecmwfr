% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/wf_set_key.R
\name{wf_set_key}
\alias{wf_set_key}
\title{Set secret ECMWF token}
\usage{
wf_set_key(user, key, service)
}
\arguments{
\item{user}{user (email address) used to sign up for the ECMWF data service}

\item{key}{token provided by ECMWF}

\item{service}{service associated with credentials ("webapi" or "cds")}
}
\value{
It invisibly returns the user.
}
\description{
Saves the token to your local keychain under
a service called "ecmwfr".
}
\examples{

\dontrun{
# set key
wf_set_key(user = "test@mail.com", key = "123")

# get key
wf_get_key(user = "test@mail.com")

# leave user and key empty to open a browser window to the service's website
# and type the key interactively
wf_get_key()

}
}
\seealso{
\code{\link[ecmwfr]{wf_get_key}}
}
\author{
Koen Kufkens
}
\keyword{key}
\keyword{management}
