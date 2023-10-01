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
    prompt_type = 1L){

  params = c(params, .default_model_params(model))
  params = params[!duplicated(names(params))]
  with_token = FALSE
  if(!missing(token)){
    with_token = TRUE
  }
  if(!is.list(terms)){
    if(is.matrix(terms)){
      terms = as.list(as.data.frame(terms))
    }else{
      terms = list(terms)
    }
  }
  k = length(terms)
  # checkmate terms, params, token, ...

  if(with_token){
    # message! mit token; model; k topics
    # progress bar.
    for(i in seq_len(k)){
      prompt = generate_standard_prompt(terms = terms[[i]],
                                        context = context,
                                        sep_terms = sep_terms,
                                        max_length_label = max_length_label)
      res = interact_with_token(model = model,
                                params = params,
                                prompt = prompt,
                                token = token)
      # res bearbeiten und zusammenfassen!
    }
  }else{
    # message! mit token; model; k topics
    # falls zu viele Topics zu labeln -> könnte etwas Zeit kosten (ja/nein)
    # progress bar.
    message(sprintf("Labeling %s topic(s) using the language model %s.",
                    k, model))
    for(i in seq_len(k)){
      prompt = generate_standard_prompt(terms = terms[[i]],
                                        context = context,
                                        sep_terms = sep_terms,
                                        max_length_label = max_length_label)
      res = interact(model = model,
                     params = params,
                     prompt = prompt)
      # res bearbeiten und zusammenfassen!
    }
  }
  res
}
