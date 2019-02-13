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
  tryCatch({
    return(strsplit(gsub("http://|https://|www\\.", "", x), "/")[[c(1, 1)]])
  }, error=function(e){
    return(paste("ERROR", conditionMessage(e)))})
}
