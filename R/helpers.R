.default_model_params = function(model){
  if(model == "tiiuae/falcon-7b-instruct")
    return(list("max_new_tokens" = 300, return_full_text = FALSE))
  return(list())
}