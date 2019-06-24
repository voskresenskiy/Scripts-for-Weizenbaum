domain = function(x){
  
  library(httr)
  library(tidyverse)
  
  # simple preprocessing
  x = str_trim(as.character(x))
  
  tryCatch({ # errors handling
  
    if(str_detect(x, 'https?://(www.|m.)facebook.com/people/|https?://(www.|m.){0,1}facebook.com/pg/|https?://(www.|m.){0,1}facebook.com/pages/biz/|es-la.facebook.com')){
      x = str_replace_all(x, 'https?://(www.|m.)facebook.com/people/|https?://(www.|m.){0,1}facebook.com/pg/|https://(www.|m.){0,1}facebook.com/pages/biz/|https://es-la.facebook.com', 'https://www.facebook.com/')
    }
      
    if(str_detect(x, '=https://.+$')){
      x = str_replace(x, '=https://.+$', '')
    }
    
  
    # for the instagram post, returns the name of the account
    if (str_detect(x, '^https?://www.instagram.com/p/.+')){
      extr = str_extract(x, '^https?://www.instagram.com/p/.+\\/')
      x = GET(extr)
      y = str_extract(rawToChar(as.raw(strtoi(x$content, 16L))), 'www.instagram.com/.+/p') %>%
        str_replace_all('https?://|www.|/p$', '')
      if(is.na(y)){
        return('not_available')
      } else {
        return(y)}
  
    # for twitter post, returns name of the account  
    } else if(str_detect(x, 'twitter.com/i/web/status/')){
      y = GET(as.character(x))
      y = str_extract(rawToChar(as.raw(strtoi(y$content, 16L))), 'twitter.com/.+(?=/status)')
      if(is.na(y)){
        return('not_available')
      } else {
        return(y)}
    
    # marks suspended twitter accounts
    } else if(str_detect(x, 'account/suspended')){
        return('twitter_suspended')
    
    # marks shares and intents as 'twitter action'  
    } else if(str_detect(x, 'https?://twitter.com/intent/|https?://twitter.com/share')){
        return('twitter_action')
    
    # for youtube videos, returns youtube channels  
    } else if(str_detect(x, 'https?://www.youtube.com/watch|https?://www.youtube.com/c/|https?://youtu.be/')){
      y = GET(as.character(x))
      y = str_trim(str_replace(str_extract(rawToChar(as.raw(strtoi(y$content, 16L))), 
                                       'channel/.+(?=class)'), '\"', ''))
      if(is.na(y)){
        return('not_available')
      } else {
        return(str_c('youtube.com/', y))}
    
  # for facebook stories, photos, searches and posts returns the name of account
  } else if (str_detect(x, 'facebook.com/story|facebook.com/permalink.php|facebook.com/profile.php')){
    new_url = str_c('https://www.facebook.com/', str_replace(str_extract(x, '(\\?|&)id=[0-9]+'), '(\\?|&)id=', ''))
    x = GET(new_url, add_headers("user-agent" = "Mozilla/5.0")) # FB requires adding a header
    if (str_detect(str_replace_all(x$url, 'https?://|www.|\\/$', ''), 'facebook.com/people/')){
      return(str_extract(str_replace_all(x$url, 'https?://|www.|\\/$|people/', ''), 'facebook.com/.+(?=/)'))
    } else {
      return(str_replace_all(x$url, 'https?://|www.|\\/$', ''))
    }
    
  } else if (str_detect(x, 'facebook.com/[0-9]+/posts/[0-9]+')){
    new_url = str_c('https://www.', str_extract(x, 'facebook.com/[0-9]+'))
    x = GET(new_url, add_headers("user-agent" = "Mozilla/5.0"))
    return(str_replace_all(x$url, 'https?://|www.|\\/$', ''))  
    
  } else if (str_detect(x, 'facebook.com/photo.php')){
    y = GET(as.character(x), add_headers("user-agent" = "Mozilla/5.0"))
    y = str_extract(rawToChar(as.raw(strtoi(y$content, 16L))), 'www.facebook.com%2F.+%2F') %>%
      str_replace_all('%2F', '/') %>%
      str_replace('posts/', '') %>%
      str_replace_all('https?://|www.|\\/$', '')
    return(y)
    
  } else if (str_detect(x, 'www.facebook.com/search')){
    search = str_extract(x, 'q=.+%') %>%
      str_replace_all('q=|\\%', '')
    return(str_c('www.facebook.com/', search))
    
  # reduces social media accounts to domains
  } else if (str_detect(x, '^https?://(m.facebook.com/|www.facebook.com/events/|facebook.com/|business.facebook.com/|www.twitter.com|twitter.com/|www.facebook.com/|www.instagram.com/|vk.com/|mobile.twitter.com).+')){
    extr = str_extract(x, '(facebook.com/|www.facebook.com/events/|facebook.com/|twitter.com/|www.facebook.com/|www.instagram.com/|vk.com/).*?($|/|\\?)')[[1]]
    return(extr %>% str_replace_all('https?://|www.|\\/$|\\/p$|\\?$', ''))
    
  } else if (str_detect(x, '^https?://(www.youtube.com/channel/|www.youtube.com/user/).+')){
    extr = str_extract(x, '(www.youtube.com/channel/|www.youtube.com/user/|www.youtube.com/).*?($|/|&|\\?)')[[1]]
    return(extr %>% str_replace_all('https?://|www.|\\/$|&|\\?', ''))
    
  # reduces media to domains
  } else {
    return(strsplit(gsub("https?://|www\\.", "", x), "/")[[c(1, 1)]])
  }  
    
    }, error=function(e){
      return(paste("ERROR", conditionMessage(e)))})

}


