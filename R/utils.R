
.verifyChar <- function(vec) {
	if (is.null(vec)) stop("Must be an atomic character vector!", call. = FALSE)
	if (length(vec) == 0) stop("Must be an atomic character vector!", call. = FALSE)
	if (!is.atomic(vec)) stop("Must be an atomic character vector!", call. = FALSE)
	OUT <- as.character(vec)
	return(OUT)
}

.transChar <- function(vec) {
	if (is.null(vec)||is.na(vec)||length(vec) == 0||!is.atomic(vec)) {
		OUT <- ""
	} else {
		OUT <- as.character(vec)
	}
	return(OUT)
}

.getURL <- function(strurl, errormsg = "") {
	OUT <- fromJSON(getURL(strurl, .encoding = "UTF-8"))
	if ("errcode" %in% names(OUT)) {	
		stop(paste0(errormsg, "\nServer response: ", OUT$errmsg))
	} else {
		return(OUT)
	}
}


