

##' Create an authorized Weixin object
##' 
##' @title create an Weixin object
##' @param app_name name of the application.
##' @return An reference object of \code{\link{weixin}}.
##' @note There is only one Weixin object needed.
##' @author Jian Li <\email{rweibo@@sina.com}>
##' @seealso \code{\link{registerAccounts}}
##' @references \url{https://mp.weixin.qq.com}
##' @keywords authorization
##' @examples \dontrun{
##' 
##' w1 <- createWeixin("test")
##' }
createWeixin <- function(app_name) {
	oauthobj <- new("weixin", appName = app_name)
	oauthobj$authorize()
	return(oauthobj)
}



