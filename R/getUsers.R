
getUsers <- function(obj, userlist = "") {
	userlist <- .verifyChar(userlist)
	if (!inherits(obj, "weixin")) stop("A weixin object is required!")
	if (!nzchar(userlist)) {
		userlist <- getUserList(obj)
	}
	
	outlist <- list()
	for (i in 1:length(userlist)) {
		requestURL <- "https://api.weixin.qq.com/cgi-bin/user/info"
		s1 <- paste0(requestURL, "?access_token=", obj$oauthToken, "&openid=", userlist[i], "&lang=zh_CN")
		u1 <- try(.getURL(s1), silent = TRUE)
		if (inherits(u1, "try-error")) {
			outlist[[i]] <- data.frame()
		} else {
			tmp.time <- .transChar(u1$subscribe_time)
			if (nchar(tmp.time)) {
				tmp.time <- strftime(as.POSIXct(as.numeric(tmp.time), origin="1970-01-01"), format = "%Y-%m-%d %H:%M:%S")
			}
			outlist[[i]] <- data.frame(nickname = .transChar(u1$nickname), 
					sex = .transChar(u1$sex), 
					city = .transChar(u1$city), 
					province = .transChar(u1$province), 
					country = .transChar(u1$country), 
					language = .transChar(u1$language), 
					subscribe = .transChar(u1$subscribe), 
					subscribe_time = tmp.time, 
					openid = .transChar(u1$openid), 
					unionid = .transChar(u1$unionid), 
					remark = .transChar(u1$remark), 
					headimgurl = .transChar(u1$headimgurl), 
					stringsAsFactors = FALSE)	
		}
		
	}
	OUT <- do.call("rbind", outlist)
	
	return(OUT)
}
