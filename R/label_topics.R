#' @title
#' TITLE TBA
#'
#' @description
#' DESCRIPTION TBA
#'
#' @details
#' DETAILS TBA
#'
#' @param terms [\code{list (k) of character}]\cr
#' List (each list entry represents one topic) of \code{character} vectors
#' containing the top terms representing the topics that are to be labeled.
#' If a single \code{character} vector is passed, this is interpreted as
#' the top terms of a single topic. If a \code{character} matrix is passed,
#' each column is interpreted as the top terms of a topic.
#' @param model [\code{character(1)}]\cr
#' Optional. Default is \code{tiiuae/falcon-7b-instruct}.\cr
#' The language model to use for labeling the topics.
#' The model must be accessible via the Huggingface API.
#' @param params [\code{list}]\cr
#' Optional.\cr
#' Model parameters to pass. Default parameters for common models are
#' given in the details section.
#' @param token [\code{character(1)}]\cr
#' Optional.\cr
#' TBA
#' @param context description example
#' @param sep_terms description
#' @param max_length_label description
#' @param prompt_type description
#' @param max_wait [\code{integer(1)}]\cr
#' time in minutes after which the user is ask whether to proceed if rate limit
#' is reached (default 0 -> user is askes every time the rate limit is reached)
#' @param progress description
#'
#' @return [\code{character(k)}] Labels for all \code{k} topics.
#'
#' @examples
#' # EXAMPLES TBA
#'
#' @export label_topics

label_topics = function(
    terms,
    model = "tiiuae/falcon-7b-instruct",
    params = list(),
    token,
    context = "",
    sep_terms = "; ",
    max_length_label = 5L,
    prompt_type = 1L,
    max_wait = 0L,
    progress = TRUE){

  params = c(params, .default_model_params(model))
  params = params[!duplicated(names(params))]
  if(missing(token)) token = ""
  if(!is.list(terms)){
    if(is.matrix(terms)) terms = as.list(as.data.frame(terms))
    else terms = list(terms)
  }
  k = length(terms)
  # checkmate terms, params, token, ...

  model_output = character(k)
  prompts = sapply(terms, function(x)
    generate_standard_prompt(
      terms = x,
      context = context,
      sep_terms = sep_terms,
      max_length_label = max_length_label))
  message(paste0(
    sprintf("Labeling %s topic(s) using the language model %s", k, model),
    ifelse(!(token == ""), " and a Huggingface API token.", ".")))
  pb = .make_progress_bar(
    progress = progress,
    callback = function(x) message("Labeling process finished"),
    total = k,
    format = "Label topic :current/:total  [:bar] :percent elapsed: :elapsed eta: :eta")
  time_start = waited = Sys.time()

  for(i in seq_len(k)){
    model_output[i] = interact(model = model,
                               params = params,
                               prompt = prompts[i],
                               token = token)[[1]][[1]]
    while(grepl("rate limit reached", model_output[i], ignore.case = TRUE)){
      if(as.numeric(difftime(Sys.time(), waited, units = "mins")) > max_wait){
        max_wait = .ask_user()
        if(max_wait == 0L){
          time_end = Sys.time()
          return(as.lm_topic_labels(
            prompts = prompts, model = model, params = params,
            with_token = !(token == ""),
            time = as.numeric(difftime(time_end, time_start, units = "mins")),
            model_output = model_output, labels = .extract_labels(model_output)))
        }
        waited = Sys.time()
        message("Wait for five minutes", appendLF = FALSE)
        Sys.sleep(5*60) #sleep 5 minutes if rate limit is reached
      }else{
        message("\nRate limit reached - wait for one minute", appendLF = FALSE)
        Sys.sleep(60) #sleep 1 minute after each unsuccessful query
      }
      message(" - try to continue (total minutes elapsed: ",
              round(as.numeric(difftime(Sys.time(), time_start, units = "mins")), 2),
              ")", appendLF = FALSE)
      model_output[i] = interact(model = model,
                                 params = params,
                                 prompt = prompts[i],
                                 token = token)[[1]][[1]]
    }
    pb$tick()
  }
  time_end = Sys.time()

  as.lm_topic_labels(
    prompts = prompts, model = model, params = params, with_token = !(token == ""),
    time = as.numeric(difftime(time_end, time_start, units = "mins")),
    model_output = model_output, labels = .extract_labels(model_output))
}

as.lm_topic_labels = function(prompts, model, params, with_token, time,
                              model_output, labels){
  res = list(
    prompts = prompts,
    model = model,
    params = params,
    with_token = with_token,
    time = time,
    model_output = model_output,
    labels = labels
  )
  class(res) = "lm_topic_labels"
  res
}

#' @export
print.lm_topic_labels = function(x, ...){
  # insert alignment for topic ids
  cat(
    "lm_topic_labels Object generated using ", x$model, "\n ",
    paste0(seq_along(x$labels), ": ", x$labels, collapse = "\n "),
    sep = ""
  )
}
