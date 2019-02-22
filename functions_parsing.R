url_expander = function(x){
  tryCatch({
    get_object = withTimeout(GET(x), timeout = 60, onTimeout = "error")
    return(get_object$url)
  }, error=function(e){
    return(paste("ERROR", conditionMessage(e)))})
}

url_extractor = function(x){
  z = pbsapply(x, function(y) url_expander(y))
  df = as.data.frame(cbind(x,z))
  colnames(df) = c("url", "expanded_url")
  rownames(df) = c(1:nrow(df))
  return(df)
}
               
domain = function(x) {
  if (str_detect(x, '^htt.*://(www.facebook.com/events/|facebook.com/|twitter.com/|www.facebook.com/|www.instagram.com/|vk.com/|www.youtube.com/).*')){
    extr = str_extract(x, '(www.facebook.com/events/|facebook.com/|twitter.com/|www.facebook.com/|www.instagram.com/|vk.com/|www.youtube.com/).*?($|/|\\?)')[[1]]
    return(extr %>% str_replace('\\/$|\\/p$|\\?$', '') %>% str_replace('\\/$', ''))
  } else {
    tryCatch({
      return(strsplit(gsub("http://|https://|www\\.", "", x), "/")[[c(1, 1)]])
    }, error=function(e){
      return(paste("ERROR", conditionMessage(e)))})
  }
}

