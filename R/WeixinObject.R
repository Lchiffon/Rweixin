
setRefClass("weixin", 
		fields = list(
				appPath = "character", 
				appName = "character", 
				appID = "character", 
				appSecret = "character", 	
				appLatestday = "character",
				oauthToken = "character", 
				oauthTime = "character", 
				oauthLife = "numeric",
				authURL = "character", 
				oauthLimits = "numeric",
				oauthMsg = "character"
		),
		
		methods = list(
			initialize = function(appName) {
				.self$appPath <- file.path(Sys.getenv("APPDATA"), "Rweixin", "oauth")
				.self$appName <- appName
				applist <- listAccount(appName)
				.self$appID <- applist$app_id
				.self$appSecret <- applist$app_secret
				.self$appLatestday <- applist$app_latestday
				.self$authURL <- "https://api.weixin.qq.com/cgi-bin/token"
				.self$oauthToken <- applist$app_token[["token_key"]]
				.self$oauthTime <- applist$app_token[["token_time"]]
				.self$oauthLife <- applist$app_token[["token_expires"]]
				.self$oauthLimits <- applist$app_token[["token_limit"]]
				.self$oauthMsg <- ""
				
			},	
			expiresIn = function() {
				if (as.numeric(.self$oauthLife) < 0) {
					OUT <- -1
				} else {
					oauthtimediff <- difftime(Sys.time(), as.POSIXlt(.self$oauthTime, format = "%Y-%m-%d %H:%M:%S"), units = "secs")
					OUT <- as.numeric(.self$oauthLife) - floor(as.numeric(oauthtimediff))
				}
				return(OUT)
			}, 
			authorize = function(forcelogin = FALSE,...) {
				time.expr <- as.POSIXlt(.self$oauthTime, format = "%Y-%m-%d %H:%M:%S") + .self$oauthLife
				is.expr <- is.na(time.expr) || time.expr < Sys.time()
				if (is.expr || forcelogin) {
					verifyURL <- paste0(.self$authURL, "?grant_type=client_credential&appid=", .self$appID, "&secret=", .self$appSecret)
					tokenList <- fromJSON(getURL(verifyURL,...))
					curr.time <- Sys.time()
					if ("errcode" %in% names(tokenList)) {
						.self$oauthMsg = tokenList$errmsg
						#stop(tokenList$errmsg)
						cat(tokenList$errmsg)
						cat("\n")
						invisible(FALSE)
					} else {
						.self$oauthToken <- tokenList$access_token
						.self$oauthTime <- format(curr.time, format = "%Y-%m-%d %H:%M:%S")
						.self$oauthLife <- as.numeric(tokenList$expires_in)
						.self$oauthMsg <- ""
						if (.self$appLatestday == substr(.self$oauthTime, 1, 10)) {
							.self$oauthLimits <- .self$oauthLimits - 1
						} else {
							.self$appLatestday <- substr(.self$oauthTime, 1, 10)
							.self$oauthLimits <- 2000
						}
						.self$save()
					}	
				}
				invisible(TRUE)
			},
			save = function() {
				apppath <- file.path(Sys.getenv("APPDATA"), "Rweixin", "oauth")
				if (!file.exists(apppath)) dir.create(apppath, recursive = TRUE)
				if (.self$appName %in% list.files(apppath)) {
					applist <- fromJSON(file.path(apppath, .self$appName))
					applist$app_latestday <- .self$appLatestday
					applist$app_token$token_key = .self$oauthToken
					applist$app_token$token_time = .self$oauthTime
					applist$app_token$token_expires = .self$oauthLife
					applist$app_token$token_limit = .self$oauthLimits
					appfile <- file(file.path(apppath, .self$appName) , open = "w" )
					writeLines(toJSON(applist), appfile)
					close(appfile)
				} else {
					stop(paste(.self$appName, "doesn't exist, please use 'registerApp' to create"))
				}
				invisible(TRUE)
				cat("Saved!\n")
			},
			list = function() {
				OUT <- base::list(
						"appPath" = .self$appPath, 
						"appName" = .self$appName, 
						"appID" = .self$appID, 
						"appSecret" = .self$appSecret, 
						"oauthToken" = .self$oauthToken
				)
				return(OUT)
			}
		)
)


setMethod("show", signature="weixin", 
		function(object) {
			cat(paste0("Application: ", object$appName, " (", object$appID, ")\n"))
			nex <- object$expiresIn()
			if (nex > 0) {
				nh <- floor(nex/ 3600)
				nm <- floor((nex - nh * 3600) / 60)
				ns <- nex - nh * 3600 - nm * 60
				cat(paste0("Expires In: ", nh, " HOUR ", nm, " MIN ", ns, " SEC\n"))
			} else {
				cat("oauth was expired, please use '$authorize()' to authorize.\n")
			}
			if (nzchar(object$oauthMsg)) {
				cat(object$oauthMsg)
				cat("\n")
			}
		}
)






