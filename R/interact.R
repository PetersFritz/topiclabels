interact_with_token = function(model, params, prompt, token){
  url = paste0("https://api-inference.huggingface.co/models/", model)
  post = POST(url = url,
              body = list("inputs" = prompt,
                          "parameters" = params),
              httr::add_headers(.headers = c(
                "Authorization" = paste("Bearer",token),
                "X-Wait-For-Model" = "true",
                "X-Use-Cache" = "false")),
              encode = "json")
  content(post)
}

interact = function(model, params, prompt, token){
  if(!is.na(token)) return(interact_with_token(model = model, params = params,
                                             prompt = prompt, token = token))
  url = paste0("https://api-inference.huggingface.co/models/", model)
  post = POST(url = url,
              body = list("inputs" = prompt,
                          "parameters" = params),
              httr::add_headers(.headers = c(
                "X-Wait-For-Model" = "true",
                "X-Use-Cache" = "false")),
              encode = "json")
  content(post)
}
