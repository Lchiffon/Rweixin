
getUserList <- function(obj, ...) {
	if (!inherits(obj, "weixin")) stop("A weixin object is required!")
	requestURL <- "https://api.weixin.qq.com/cgi-bin/user/get"
	s0 <- paste0(requestURL, "?access_token=", obj$oauthToken, "&next_openid=")
	
	u0 <- .getURL(s0, ...)
	OUT <- u0$data$openid
	if (u0$total > u0$count) {
		tmp.nextid <- u0$next_openid
		while (length(OUT) < u0$total) {

			s1 <- paste0(requestURL, "?access_token=", obj$oauthToken, "&next_openid=", tmp.nextid)
			u1 <- fromJSON(getURL(s1, ...))
			tmp.nextid <- u1$next_openid
			OUT <- c(OUT, u1$data$openid)
		}
	}
	return(OUT)
}
