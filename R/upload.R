uploadImage = function(obj, fileName,...){
  if (!inherits(obj, "weixin")) stop("A weixin object is required!")
  if (!file.exists(fileName)) stop(sprintf("This file (%s) does not exist!", fileName))
  if (file.size(fileName)>9e5) stop(sprintf("This file (%s) is too large, please compress it!", fileName))
  url = 'https://api.weixin.qq.com/cgi-bin/media/uploadimg'
  requestURL = paste0(url, "?access_token=", obj$oauthToken)
  if(grepl('\\.png$',fileName)){
    type = 'application/png'
  }else{
    type = 'application/jpg'
  }
  
  response = postForm(requestURL, file=fileUpload(fileName, contentType = type),
                   .opts = curlOptions(...))
  fromJSON(response)
}



uploadNews = function(obj,
                      title,
                      thumb_media_id,
                      author,
                      digest = NULL,
                      show_cover_pic = 1,
                      content,
                      content_source_url = NULL, ...){
  if (!inherits(obj, "weixin")) stop("A weixin object is required!")
  url = 'https://api.weixin.qq.com/cgi-bin/material/add_news'
  requestURL = paste0(url, "?access_token=", obj$oauthToken)
  lis = list(articles=list(list(
        title = title,
        thumb_media_id = thumb_media_id,
        author = author,
        digest = digest,
        show_cover_pic = show_cover_pic,
        content = content,
        content_source_url = content_source_url
    )))

  response = .postURL(requestURL,lis,ssl.verifypeer=F)

  response
}



uploadDraft = function(obj,
                      title,
                      author,
                      digest = NULL,
                      content,
                      content_source_url = NULL
                      thumb_media_id,
                      need_open_comment=0,
                      only_fans_can_comment=0, ...){
  if (!inherits(obj, "weixin")) stop("A weixin object is required!")
  url = 'https://api.weixin.qq.com/cgi-bin/draft/add'
  requestURL = paste0(url, "?access_token=", obj$oauthToken)
  lis = list(articles=list(list(
        title = title,
        author = author,
        digest = digest,        
        content = content,
        content_source_url = content_source_url,
        thumb_media_id = thumb_media_id,
        need_open_comment = need_open_comment,
        only_fans_can_comment = only_fans_can_comment
        
    )))

  response = .postURL(requestURL,lis,ssl.verifypeer=F)

  response
}



