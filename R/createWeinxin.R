

##' Create an authorized OAuth object
##' 
##' @title create an OAuth object
##' @param app_name name of the application.
##' @return An reference object of \code{\link{weixin}}.
##' @note There is only one OAuth object needed.
##' @author Jian Li <\email{rweibo@@sina.com}>
##' @seealso \code{\link{registerAccounts}}
##' @references \url{https://mp.weixin.qq.com}
##' @keywords authorization
##' @examples \dontrun{
##' 
##' w1 <- createOAuth("test")
##' }
createWeixin <- function(app_name) {
	oauthobj <- new("weixin", appName = app_name)
	oauthobj$authorize()
	return(oauthobj)
}



