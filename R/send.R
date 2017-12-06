sendNews = function(obj, mediaID,...){
  if (!inherits(obj, "weixin")) stop("A weixin object is required!")
  
  url = 'https://api.weixin.qq.com/cgi-bin/message/mass/sendall'
  requestURL = paste0(url, "?access_token=", obj$oauthToken)
  
  # response = list(media_id='U3FGv3J0zDb6Bg2_oR9U90IZAjeDlBGX1PVRRT84iE8')
  lis = list(
    "filter"=list(
      "is_to_all" = T,
      "tag_id" = NULL
      
    ),
    "mpnews"=list(
      "media_id"=mediaID
    ),
    "msgtype"="mpnews",
    "send_ignore_reprint"=0
  )
  
  response = .postURL(requestURL, lis, ...)
  response
}

sendMessage = function(obj, openid, message,...){
  if (!inherits(obj, "weixin")) stop("A weixin object is required!")
  
  url = 'https://api.weixin.qq.com/cgi-bin/message/custom/send'
  requestURL = paste0(url, "?access_token=", obj$oauthToken)
  lis = list(
    "touser" = openid,
    "msgtype" = "text",
    "text" = list(content = message)
  )
  
  response = .postURL(requestURL, lis, ...)
  response
}