url_expander = function(x){
  tryCatch({
    get_object = withTimeout(GET(x, add_headers("user-agent" = "Mozilla/5.0")), 
                             timeout = 60, onTimeout = "error")
    if(str_detect(get_object$url, 'unsupportedbrowser')){
      return(get_object$all_headers[[1]]$headers$location)
    } else {
      return(get_object$url)
    }  
  }, error=function(e){
    return(paste("ERROR", conditionMessage(e)))})
}

url_extractor = function(x){
  library(httr)
  library(R.utils)
  library(pbapply)
  z = pbsapply(x, function(y) url_expander(y))
  df = as.data.frame(cbind(x,z))
  colnames(df) = c("url", "expanded_url")
  rownames(df) = c(1:nrow(df))
  return(df)
}

split_gather = function(df, col, sep = ','){
    #' the function takes column of the dataframe in which values are separated strings (for instance, one of the values of the column can be 
    #' "george michael, george clooney etc"). the function separates these values and rewrites them as a new variable, where each value 
    #' makes new row. after that, the function gathers (check package "tidyr") these values.
    #' inputs: df (required), column (required), separator (optional, comma is taken as a default)
    #' output: dataframe with new variable containing separated values; all other columns are duplicated
  x = str_split(as.character(df[,col]), sep) 
  x = bind_rows(lapply(x, function(x) as.data.frame(unlist(t(x)))))
  colnames(x) = str_c('variable_agg', c(1:ncol(x)))
  new_df = cbind(df, x) 
  new_df = gather(new_df, identificator,link_splitted, colnames(new_df)[str_detect(colnames(new_df), 'variable_agg')])
  new_df$identificator = NULL
  return(new_df)
}

clean_links = function(df, col){
  new_df = df[str_length(df[,col]) > 10 &
                !is.na(df[,col]) &
                str_detect(df[,col], 'htt.+') &
                !str_detect(df[,col], 'mailto|\\=htt.+|share=|whatsapp|viber|app_scoped_user_id|vk.com/share.php|^www.instagram.com/p$|www.instagram.com/explore|photo.php|www.facebook.com/about|www.facebook.com/help'), ]
  new_df[, col] = str_replace_all(new_df[, col], 'de-de.facebook|sv-se.facebook|en-gb.facebook', 'facebook')
  new_df[, col] = str_replace_all(new_df[, col], 'de-de.facebook|sv-se.facebook|en-gb.facebook', 'facebook')
  new_df[, col] = str_replace_all(new_df[, col], 'hashtag/|groups/', '')
  return(new_df)
}                      
