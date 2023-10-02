.default_model_params = function(model){
  if(model == "tiiuae/falcon-7b-instruct")
    return(list("max_new_tokens" = 300, return_full_text = FALSE))
  return(list())
}

.make_progress_bar = function(progress, ...) {
  if (progress && getOption("width") >= 20L){
    progress_bar$new(...)
  }else{
    list(tick = function(len = 1, tokens = list()) NULL, update = function(ratio, tokens) NULL)
  }
}
