.default_model_params = function(model){
  if(model == "tiiuae/falcon-7b-instruct")
    return(list("max_new_tokens" = 300, return_full_text = FALSE))
  return(list())
}

.extract_labels = function(model_output){
  model_output_processed = sapply(
    strsplit(model_output, "\""), function(x)
      ifelse(length(x) == 3, x[2], NA_character_))
  ind = is.na(model_output_processed)
  if(!any(ind))
    return(model_output_processed)
  # hier steht aktuell nochmal der gleiche Code. Einfach entsprechend anpassen
  # z.B. "topic is (related to)" als identifier (regex formulieren)
  model_output_processed[ind] = sapply(
    strsplit(model_output[ind], "\""), function(x)
      ifelse(length(x) == 3, x[2], NA_character_))
  # + ggf. dritte "Ebene", falls immer noch NAs
  ind = is.na(model_output_processed)
  if(!any(ind))
    return(model_output_processed)
  # ...

  model_output_processed
}

.make_progress_bar = function(progress, ...){
  if (progress && getOption("width") >= 20L){
    progress_bar$new(...)
  }else{
    list(tick = function(len = 1, tokens = list()) NULL, update = function(ratio, tokens) NULL)
  }
}
