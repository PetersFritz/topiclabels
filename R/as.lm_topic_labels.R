#' @title lm_topic_labels object
#'
#' @description Constructor for lm_topic_labels objects used in this package.
#'
#' @details
#' If you call \code{as.lm_topic_labels} on an object \code{x} which already is of
#' the structure of a \code{lm_topic_labels} object (in particular a \code{lm_topic_labels}
#' object itself), the additional arguments \code{id, param, ...}
#' may be used to override the specific elements.
#'
#' @param x [\code{named list}]\cr
#' \code{\link[topiclabels:as.lm_topic_labels]{lm_topic_labels}} object. Alternatively each element can be passed for
#' individual results. Individually set elements overwrite elements from \code{x}.
#' @param terms [\code{list(n) of character}]\cr
#' List of \code{character} vectors, whereas each vector represents the top terms
#' of a topic. Topics may consist of different numbers of top terms.
#' @param prompts [\code{character(n)}]\cr
#' Optional.\cr
#' Each entry of the \code{character} vector contains the original prompt that
#' was used to obtain the corresponding entry of \code{model_output}.
#' @param model [\code{character(1)}]\cr
#' The language model used for labeling the topics.
#' @param params [\code{named list}]\cr
#' Optional.\cr
#' Model parameters passed.
#' @param with_token [\code{logical(1)}]\cr
#' Optional.\cr
#' Was the labeling executed using a Huggingface token?
#' @param time [\code{numeric(1)}]\cr
#' Optional.\cr
#' Time needed for the labeling.
#' @param model_output [\code{character(n)}]\cr
#' Optional.\cr
#' Each entry of the \code{character} vector contains the original model output
#' obtained using the corresponding prompt from \code{prompts}.
#' @param labels [\code{character(n)}]\cr
#' The extracted labels from \code{model_output}.
#' @param obj [\code{R} object]\cr
#' Object to test.
#' @param verbose [\code{logical(1)}]\cr
#' Should test information be given in the console?
#' @return [\code{named list}] \code{\link[topiclabels:as.lm_topic_labels]{lm_topic_labels}} object.
#'
#' @examples
#' \dontrun{
#' token = "" # please insert your hf token here
#' topwords_matrix = matrix(c("zidane", "figo", "kroos",
#'                            "gas", "power", "wind"), ncol = 2)
#' obj = label_topics(topwords_matrix, token = token)
#' obj$model
#' obj_modified = as.lm_topic_labels(obj, model = "It is possible to modify individual entries")
#' obj_modified$model
#'
#' obj_modified$model = 3.5 # example for an invalid modification
#' is.lm_topic_labels(obj_modified, verbose = TRUE)
#'
#' obj_manual = as.lm_topic_labels(terms = list(c("zidane", "figo", "kroos"),
#'                                              c("gas", "power", "wind")),
#'                                 model = "manual labels",
#'                                 labels = c("Football Players", "Energy Supply"))
#' }
#' @export as.lm_topic_labels
as.lm_topic_labels = function(x, terms, prompts, model, params, with_token, time,
                              model_output, labels){
  if (!missing(x)){
    if (!is.lm_topic_labels(x)){
      is.lm_topic_labels(x, verbose = TRUE)
      stop("\"x\" is not a lm_topic_labels object")
    }
    if (missing(terms)) terms = x$terms
    if (missing(prompts)) prompts = x$prompts
    if (missing(model)) model = x$model
    if (missing(params)) params = x$params
    if (missing(with_token)) with_token = x$with_token
    if (missing(time)) time = x$time
    if (missing(model_output)) model_output = x$model_output
    if (missing(labels)) labels = x$labels
  }
  if (missing(terms)) stop("\"terms\" must be specified")
  if (missing(model)) stop("\"model\" must be specified")
  if (missing(labels)) stop("\"labels\" must be specified")
  if (missing(prompts)) prompts = rep(NA_character_, length(labels))
  if (missing(model_output)) model_output = rep(NA_character_, length(labels))
  if (missing(params)) params = list()
  if (missing(with_token)) with_token = NA
  if (missing(time)) time = NA_real_

  res = list(
    terms = terms,
    prompts = prompts,
    model = model,
    params = params,
    with_token = with_token,
    time = time,
    model_output = model_output,
    labels = labels
  )
  class(res) = "lm_topic_labels"
  if (!is.lm_topic_labels(res)){
    is.lm_topic_labels(res, verbose = TRUE)
    stop("input arguments do not create a lm_topic_labels object")
  }
  res
}

#' @rdname as.lm_topic_labels
#' @export
is.lm_topic_labels = function(obj, verbose = FALSE){
  assert_flag(verbose)

  if (!inherits(obj, "lm_topic_labels")){
    if (verbose) message("object is not of class \"lm_topic_labels\"")
    return(FALSE)
  }

  if (!is.list(obj)){
    if (verbose) message("object is not a list")
    return(FALSE)
  }
  testNames = c("terms", "prompts", "model", "params", "with_token", "time",
                "model_output", "labels")

  if (!test_list(
    obj,
    types = c("list", "character", "character", "list", "logical", "numeric", "character", "character"),
    names = "named", any.missing = FALSE)){
    if (verbose) message(check_list(
      obj,
      types = c("list", "character", "character", "list", "logical", "numeric", "character", "character"),
      names = "named", any.missing = FALSE))
    return(FALSE)
  }
  if (!test_set_equal(names(obj), testNames)){
    if (verbose) message("Names of object: ", check_set_equal(names(obj), testNames))
    return(FALSE)
  }
  if (!test_list(obj$terms, any.missing = FALSE, types = "character")){
    if (verbose) message("terms: ", check_list(obj$terms, any.missing = FALSE, types = "character"))
    return(FALSE)
  }
  n = length(obj$terms)
  for(i in seq_along(obj$terms)){
    if (!test_character(obj$terms[[i]], any.missing = FALSE)){
      if (verbose) message("terms[[",i, "]]: ", check_character(obj$terms[[i]], any.missing = FALSE))
      return(FALSE)
    }
  }
  if (!test_character(obj$prompts, len = n)){
    if (verbose) message("prompts: ", check_character(obj$prompts, len = n))
    return(FALSE)
  }
  if (!test_character(obj$model, any.missing = FALSE, len = 1)){
    if (verbose) message("model: ", check_character(obj$model, any.missing = FALSE, len = 1))
    return(FALSE)
  }
  if (!test_list(obj$params, names = "unique")){
    if (verbose) message("params: ", check_list(obj$params, names = "unique"))
    return(FALSE)
  }
  if (!test_logical(obj$with_token, len = 1)){
    if (verbose) message("with_token: ", check_logical(obj$with_token, len = 1))
    return(FALSE)
  }
  if (!test_numeric(obj$time, len = 1, lower = 0)){
    if (verbose) message("time: ", check_numeric(obj$time, len = 1, lower = 0))
    return(FALSE)
  }
  if (!test_character(obj$model_output, len = n)){
    if (verbose) message("model_output: ", check_character(obj$model_output, len = n))
    return(FALSE)
  }
  if (!test_character(obj$labels, len = n)){
    if (verbose) message("labels: ", check_character(obj$labels, len = n))
    return(FALSE)
  }
  return(TRUE)
}

#' @export
print.lm_topic_labels = function(x, nchars = 45, ...){
  # insert alignment for topic ids
  topic_terms = ""
  if (nchars > 0){
    topic_terms = sapply(x$terms, paste, collapse = ", ")
    topic_terms[nchar(topic_terms) > nchars] =
      paste0(substr(topic_terms[nchar(topic_terms) > nchars], 1, nchars-3), "...")
    topic_terms = paste0(" [", topic_terms, "]")
  }
  cat(
    "lm_topic_labels object generated using ", x$model, "\n ",
    paste0(seq_along(x$labels), ": ", x$labels, topic_terms, collapse = "\n "),
    sep = ""
  )
}
