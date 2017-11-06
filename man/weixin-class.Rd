\name{weixin-class}
\docType{class}
\alias{weixin-class}
\alias{weixin}
\alias{expiresIn}
\alias{authorize}

\title{Class "weixin" }
\description{To use the APIs of Weixin open platform, you have to register a application first. 
The open platform will provide a unique App ID and App Secret for every application. You can use these
ID and secret to invoke the authorize interface of OAuth2. All the authorization information will be 
stored in this object of weixin.}
\section{Objects from the Class}{
Objects can be created by calls of the function \code{\link{createWeixin}}. 
}
\section{Fields}{
	\describe{
	\item{\code{appPath}:}{Object of class \code{"character"}. Path of the application folder.}
	\item{\code{appName}:}{Object of class \code{"character"}. Name of the application.}
    \item{\code{appID}:}{Object of class \code{"character"}. ID of the application.}
    \item{\code{appSecret}:}{Object of class \code{"character"}. Secret of the application. }
    \item{\code{appLatestday}:}{Object of class \code{"character"}. Latest day of the application. }
    \item{\code{oauthToken}:}{Object of class \code{"character"}. Token of the oauth.}
    \item{\code{oauthTime}:}{Object of class \code{"character"}. Created time.}
    \item{\code{oauthLife}:}{Object of class \code{"numeric"}. Expected life (in sec) of the oauth.}
    \item{\code{authURL}:}{Object of class \code{"character"}. URL of authorization.}
    \item{\code{oauthLimits}:}{Object of class \code{"numeric"}. Number of oauth limits.}
    \item{\code{oauthMsg}:}{Object of class \code{"character"}. oauth massage.}
    }
}

\section{Methods}{
  \describe{
    \item{\code{expiresIn(byAPI = FALSE)}:}{Will return the expired
     time in seconds.}
    \item{\code{authorize(forcelogin = FALSE)}:}{Will do the authorization.}
    \item{\code{save()}:}{Save the authorization information to file.}
    \item{\code{list()}:}{List the fields of the object.}
	\item{\code{show()}:}{Show all the fields of the object.}
  }
}

\author{Jian li <rweibo@sina.com>}

\examples{
	showClass("weixin")
}
\keyword{classes}
