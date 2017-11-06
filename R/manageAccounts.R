


##' Reigster, modify and delete application.
##' 
##' Use the key and secret you get from "mp.weixin.qq.com"
##' 
##' @title Manage application
##' @rdname registerAccounts
##' @param app_name name of an application
##' @param app_id key of an application
##' @param app_secret secret of an application
##' @return a logical value 
##' @note You should register an application on weixin firstly.
##' @author Jian Li <\email{rweibo@@sina.com}>
##' @seealso See Also as \code{\link{createOAuth}}
##' @references \url{https://mp.weixin.qq.com}
##' @keywords authorization
##' @examples \dontrun{
##' registerAccounts(app_name = "test", "your_app_id", "your_app_secret")
##' listAccounts()
##' modifyAccounts("test", "t1", "t2")
##' deleteAccounts("test")
##' }
registerAccounts <- function(app_name, app_id, app_secret) {
	apppath <- file.path(Sys.getenv("APPDATA"), "Rweixin", "oauth")
	if (!file.exists(apppath)) dir.create(apppath, recursive = TRUE)
	if (file.exists(file.path(apppath, app_name))) {
		warning(paste("The App", app_name, "has been registered, please use 'modifyAccounts' to make change."))
		invisible(FALSE)
	} else {
		applist <- list(app_id = app_id, app_secret = app_secret, app_latestday = "",
				app_token = list(token_key = "", token_time = "", token_expires = -1, token_limit = 2000))
		appfile <- file(file.path(apppath, app_name) , open = "w" )
		writeLines(toJSON(applist), appfile)
		close(appfile)
		invisible(TRUE)
	}
}

##' @rdname registerAccounts
##' @return a logical value 	
modifyAccounts <- function(app_name, app_id, app_secret) {
	apppath <- file.path(Sys.getenv("APPDATA"), "Rweixin", "oauth")
	if (!file.exists(apppath)) dir.create(apppath, recursive = TRUE)
	if (app_name %in% list.files(apppath)) {
		applist <- fromJSON(file.path(apppath, app_name))
		applist$app_id <- app_id
		applist$app_secret <- app_secret
		applist$app_latestday <- ""
		applist$app_token <- list(token_key = "", token_time = "", token_expires = -1, token_limit = 2000)
		appfile <- file(file.path(apppath, app_name) , open = "w" )
		writeLines(toJSON(applist), appfile)
		close(appfile)
	} else {
		stop(paste(app_name, "doesn't exist, please use 'registerAccounts' to create"))
	}
	return(TRUE)
}

##' @rdname registerAccounts
##' @return a logical value 
deleteAccounts <- function(app_name) {
	apppath <- file.path(Sys.getenv("APPDATA"), "Rweixin", "oauth")
	if (!file.exists(apppath)) dir.create(apppath, recursive = TRUE)
	if (file.exists(file.path(apppath, app_name))) {
		unlink(file.path(apppath, app_name))
	} else {
		stop(paste(app_name, "doesn't exist"))
	}
	return(TRUE)
}

##' @rdname registerAccounts
##' @return a list of all accounts
listAccounts <- function(detail = FALSE) {
	apppath <- file.path(Sys.getenv("APPDATA"), "Rweixin", "oauth")
	if (!file.exists(apppath)) dir.create(apppath, recursive = TRUE)
	all.app <- list.files(apppath, full.names = FALSE)
	if (length(all.app) == 0) stop("There is no apps, please use 'registerAccounts' to create")
	if (identical(detail, TRUE)) {
		OUT <- list()
		for (app_name in all.app) {
			OUT[[app_name]] <- fromJSON(file.path(apppath, app_name))
		}
	} else {
		OUT <- all.app
	}
	return(OUT)
}

##' @rdname registerAccounts
##' @return a list with components as below
##'   \item{app_id }{key of the application} 
##'   \item{app_secret }{secret of the application} 
##'   \item{app_token }{authorization information of OAuth} 
listAccount <- function(app_name) {
	apppath <- file.path(Sys.getenv("APPDATA"), "Rweixin", "oauth")
	if (!file.exists(apppath)) dir.create(apppath, recursive = TRUE)
	if (file.exists(file.path(apppath, app_name))) {
		return(fromJSON(file.path(apppath, app_name)))
	} else {
		stop(paste(app_name, "doesn't exist, please use 'registerAccounts' to create"))
	}
}


