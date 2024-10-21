.default_model_params = function(model){
  if(model == "HuggingFaceH4/zephyr-7b-beta" |
     model == "tiiuae/falcon-7b-instruct" |
     model == "mistralai/Mixtral-8x7B-Instruct-v0.1")
    return(list("max_new_tokens" = 300, return_full_text = FALSE))
  return(list())
}

.extract_labels = function(model_output, type = "json"){
  if (type == "json") f = .extract_labels_from_json
  else if (type == "plain") f = .extract_labels_from_plaintext
  else if (type == "json-roles") f = "not implemented yet!"
  f(model_output)
}

.extract_labels_from_plaintext = function(model_output){
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

.extract_labels_from_json = function(model_output) {
  model_output_processed = sapply(model_output, function(llm_response){
    # extract JSON part using regex, handling multiline JSON structures
    json_part = regmatches(llm_response, regexpr("(?s)\\{.*?\\}", llm_response, perl = TRUE))
    if (length(json_part) == 0){
      return(NA_character_)
    }
    # correct escaped quotes for proper JSON parsing
    json_part = gsub("\\\\\"", "\"", json_part)
    # parse JSON, returning NA on error
    parsed_json = tryCatch(fromJSON(json_part), error = function(e) return(NA_character_))
    # extract and return 'label' field, if present
    if ("label" %in% names(parsed_json)){
      return(parsed_json$label)
    }else{
      return(NA_character_)
    }
  })
  unname(model_output_processed)
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
                                 "for all topics to be labeled?",
                                 "\nThis is not recommended/effective if the ",
                                 "model signals that you should log in or ",
                                 "use an access token."))
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
