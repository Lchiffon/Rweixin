
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

.transTime <- function(num) {
	strftime(as.POSIXct(as.numeric(num), origin="1970-01-01"), format = "%Y-%m-%d %H:%M:%S")
}

.getURL <- function(strurl, errormsg = "",...) {
	OUT <- fromJSON(getURL(strurl, .encoding = "UTF-8",...))
	if ("errcode" %in% names(OUT)) {	
		stop(paste0(errormsg, "\nServer response: ", OUT$errmsg))
	} else {
		return(OUT)
	}
}


.tojson = function(x)
  jsonlite::toJSON(x,auto_unbox = T)

.postURL = function(strurl, postList, errormsg = "",file = NULL,...){
  OUT = postForm(strurl, 
           binary = F,
           style='POST',
           .opts = curlOptions(postfields=.tojson(postList), ...))
  # OUT = try(fromJSON(originResponse),silent = T)
  
  if(class(OUT) == 'raw'){
    message('It seems a voice/image file.')
    if(is.null(file)) file = tempfile()
    writeBin(OUT, file)
    message(sprintf('Saving to %s...', file))
    return(NULL)
  }else{
    OUT = fromJSON(OUT)
  }
    
  if ("errcode" %in% names(OUT)) {
    if(OUT$errcode==0) return(OUT)
    
    stop(paste0(errormsg, "\nServer response: ", OUT$errmsg))
  } else {
    return(OUT)
  }
}

