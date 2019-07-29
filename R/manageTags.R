getTagsList <- function(obj,...){
	if (!inherits(obj, "weixin")) stop("A weixin object is required!")
	url = "https://api.weixin.qq.com/cgi-bin/tags/get"
	requestURL = paste0(url, "?access_token=", obj$oauthToken)
  
	t0 = .getURL(requestURL, ...)
	return(t0)
}

getTagUsers <- function(obj, tagid, ...){
	if (!inherits(obj, "weixin")) stop("A weixin object is required!")
	url = "https://api.weixin.qq.com/cgi-bin/user/tag/get"
	requestURL = paste0(url, "?access_token=", obj$oauthToken)
	
	params <- list(tagid = tagid)
	out <- .postURL(requestURL, params, ...)
	OUT <- out$data$openid
	
	if (tail(out$data$openid, n=1) != out$next_openid) {
		while (tail(out$data$openid, n=1) != out$next_openid) {
			params <- list(tagid = tagid, next_openid = out$next_openid)
			out <- .postURL(requestURL, params, ...)
			OUT <- c(OUT, out)
		}
	}
	
	return(OUT)
}


postTags <- function(obj, tagid, openids, ...) {
	if (!inherits(obj, "weixin")) stop("A weixin object is required!")
	requestURL = paste0("https://api.weixin.qq.com/cgi-bin/tags/members/batchtagging",
			"?access_token=", obj$oauthToken)
	openidlist <- split(openids, f = rep(1:(length(openids) %/% 50 + 1), each = 50)[1:length(openids)])
	for (i in 1:length(openidlist)) {
		params <- list(openid_list = openidlist[[i]], tagid = tagid)
		out = .postURL(requestURL, params, ...)
	}
	return(out)
}

postUnTags <- function(obj, tagid, openids, ...) {
	if (!inherits(obj, "weixin")) stop("A weixin object is required!")
	requestURL = paste0("https://api.weixin.qq.com/cgi-bin/tags/members/batchuntagging",
			"?access_token=", obj$oauthToken)
	openidlist <- split(openids, f = rep(1:(length(openids) %/% 50 + 1), each = 50)[1:length(openids)])
	for (i in 1:length(openidlist)) {
		params <- list(openid_list = openidlist[[i]], tagid = tagid)
		out = .postURL(requestURL, params, ...)
	}
	return(out)
}




