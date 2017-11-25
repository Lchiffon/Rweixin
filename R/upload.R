uploadImage = function(obj, fileName,...){
  if (!inherits(obj, "weixin")) stop("A weixin object is required!")
  if (!file.exists(fileName)) stop(sprintf("This file (%s) does not exist!", fileName))
  if (file.size(fileName)>9e5) stop(sprintf("This file (%s) is too large, please compress it!", fileName))
  
  postForm(url, file=fileUpload("1.jpg", contentType = "application/jpg"),
                   .opts = curlOptions(ssl.verifypeer = F))
}