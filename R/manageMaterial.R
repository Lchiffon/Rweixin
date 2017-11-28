getMaterialNum = function(obj,...){
  if (!inherits(obj, "weixin")) stop("A weixin object is required!")
  url = "https://api.weixin.qq.com/cgi-bin/material/get_materialcount"
  requestURL = paste0(url, "?access_token=", obj$oauthToken)
  
  u0 = .getURL(requestURL, ...)
  return(u0)
}

getMaterialList = function(obj, materialType, ...){
  if (!inherits(obj, "weixin")) stop("A weixin object is required!")
  
  message('Getting number of material info from api.weixin.qq.com...')
  materialNum = getMaterialNum(obj, ...)
  names(materialNum) = gsub("_count", "", names(materialNum))
  
  if(!materialType %in% names(materialNum))
    stop('materialType should be one of "voice","video","image","news"')
  
  requestURL = paste0("https://api.weixin.qq.com/cgi-bin/material/batchget_material",
                       "?access_token=", obj$oauthToken)
  params = list(type = materialType,
                      offset = 0,
                      count = 25)
  
  u0 = .postURL(requestURL, params, ...)
  OUT = u0$item
  if(u0$total_count==0){
    warning(sprintf('There are no %s materials....', materialType))
    return(OUT)
  }
  
  if (u0$total_count > u0$item_count) {
    params$offset = params$offset + 25
    while (length(OUT) < u0$total_count) {
      u1 = .postURL(requestURL, params, ...)
      params$offset = params$offset + 25
      OUT = cbind(OUT, u1$item)
    }
  }
  return(OUT)
}

downloadMaterial = function(obj, media_id, ...){
  if (!inherits(obj, "weixin")) stop("A weixin object is required!")
  
  requestURL = paste0("https://api.weixin.qq.com/cgi-bin/material/get_material",
                      "?access_token=", obj$oauthToken)
  params = list(media_id = media_id)
  out = .postURL(requestURL, params, ...)
  return(out)
}