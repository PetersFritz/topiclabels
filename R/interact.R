interact_with_token = function(model, params, prompt, token){
  headers <- c(
    "Authorization" = paste("Bearer", token)
  )
  payload <- list(
    "messages" = list(
      list(
        "role" = "user",
        "content" = prompt
      )
    ),
    "model" = model
  )
  for(i in 1 : length(params)) payload[names(params)[i]] = params[i] 
  response <- POST(
    url = "https://router.huggingface.co/v1/chat/completions",
    httr::add_headers(.headers = headers),
    body = payload,
    encode = "json"
  )  
  if (http_error(response)) {
    stop(
      paste("Hugging Face API request failed with status:", status_code(response)),
      content(response, "text", encoding = "UTF-8")
    )
  }
  result <- content(response, as = "parsed")
  if(length(result)>1) result <- result$choices[[1]]$message$content
  return(result)
}
