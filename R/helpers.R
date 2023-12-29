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
    strsplit(model_output[ind], "topic is( related to)* "), function(x)
      ifelse(length(x) == 2, x[2], NA_character_))
  # + ggf. dritte "Ebene", falls immer noch NAs
  ind = is.na(model_output_processed)
  if(!any(ind))
    return(model_output_processed)
  model_output_processed[ind] = sapply(
    strsplit(model_output[ind], "best label( for the topic)* is "), function(x)
      ifelse(length(x) == 2, x[2], NA_character_))
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

.ask_user = function(){
  continue = menu(c("Yes", "No, quit and output already labeled topics"),
                  title = paste0("\nThe predefined waiting time if the rate ",
                                 "limit is reached has been exceeded. Would ",
                                 "you like to continue waiting ",
                                 "for all topics to be labeled?"))
  if(continue == 2) return(0L)
  repeat{
    max_wait = readline(
      prompt = paste0("After how many minutes would you like to be\n",
                      "asked the next time whether you would like to\n",
                      "wait any longer if the rate limit is reached?\n",
                      "(insert integer): "))
    max_wait = suppressWarnings(as.numeric(max_wait))
    if(!is.na(max_wait) && test_integerish(max_wait)) break
    message("Input is not an integer.")
  }
  as.integer(max_wait)
}
