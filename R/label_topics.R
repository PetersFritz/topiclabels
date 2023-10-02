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
#' @param context description
#' @param sep_terms description
#' @param max_length_label description
#' @param prompt_type description
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
    progress = TRUE){

  params = c(params, .default_model_params(model))
  params = params[!duplicated(names(params))]
  with_token = FALSE
  if(!missing(token)) with_token = TRUE
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
    ifelse(with_token, " and a Huggingface API token.", ".")))
  pb = .make_progress_bar(
    progress = progress,
    callback = function(x) message("Labeling process finished"),
    total = k,
    format = "Label topic :current/:total  [:bar] :percent elapsed: :elapsed eta: :eta")

  if(with_token){
    for(i in seq_len(k)){
      # gibt's hier auch ratelimits? Wie hoch?
      model_output[i] = interact_with_token(model = model,
                                            params = params,
                                            prompt = prompts[i],
                                            token = token)[[1]][[1]]
      pb$tick()
    }
  }else{
    # falls zu viele Topics zu labeln -> könnte etwas Zeit kosten (ja/nein)
    # hängt vllt auch vom Model ab, ratelimits und sleeping times in Abhängigkeit
    # der Modelle abspeichern (kann man das ggf. auch per GET abfragen?)
    # ratelimit für falcon relativ niedrig: in etwa 15-20
    for(i in seq_len(k)){
      model_output[i] = interact(model = model,
                                 params = params,
                                 prompt = prompts[i])[[1]][[1]]
      pb$tick()
    }
  }
  model_output_processed = sapply(
    strsplit(model_output, "\""), function(x)
      ifelse(length(x) == 3, x[2], NA_character_))
  list(
    prompts = prompts,
    model_output = model_output,
    labels = model_output_processed)
}
