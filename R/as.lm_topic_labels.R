#' @title lm_topic_labels Object
#'
#' @description Constructor for lm_topic_labels objects used in this package.
#'
#' @details
#' If you call \code{as.lm_topic_labels} on an object \code{x} which already is of
#' the structure of an \code{lm_topic_labels} object (in particular a \code{lm_topic_labels}
#' object itself), the additional arguments \code{id, param, ...}
#' may be used to override the specific elements.
#'
#' @param x [\code{named list}]\cr
#' \code{\link{RollingLDA}} object. Alternatively each element can be passed for
#' individual results. Individually set elements overwrite elements from \code{x}.
#' @param terms [\code{list(n) of character}]\cr
#' TBA
#' @param prompts [\code{character(n)}]\cr
#' TBA
#' @param model [\code{character(1)}]\cr
#' TBA
#' @param params [\code{named list}]\cr
#' TBA
#' @param with_token [\code{logical(1)}]\cr
#' TBA
#' @param time [\code{numeric(1)}]\cr
#' TBA
#' @param model_output [\code{character(n)}]\cr
#' TBA
#' @param labels [\code{character(n)}]\cr
#' TBA
#' @param obj [\code{R} object]\cr
#' Object to test.
#' @param verbose [\code{logical(1)}]\cr
#' Should test information be given in the console?
#' @return [\code{named list}] \code{\link{lm_topic_labels}} object.
#'
#' @examples
#' # EXAMPLES TBA
#'
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
    if (verbose) message(check_set_equal(names(obj), testNames))
    return(FALSE)
  }

  # hier noch die Einzeltests analog hinzufügen

  if (FALSE){
    #id
    if (verbose) message("id: ", appendLF = FALSE)
    id = getID(obj)
    if (!is.character(id) || !(length(id) == 1)){
      if (verbose) message("not a character of length 1")
      return(FALSE)
    }
    if (verbose) message("checked")

    #lda
    if (verbose) message("lda: ", appendLF = FALSE)
    lda = try(getLDA(obj), silent = !verbose)
    if(inherits(lda, "try-error")){
      # should not happen
      return(FALSE)
    }
    if(!is.LDA(lda)){
      if (verbose) message("not an \"LDA\" object")
      return(FALSE)
    }
    if (verbose) message("checked")

    #docs
    if (verbose) message("docs: ", appendLF = FALSE)
    docs = getDocs(obj)
    if (!test_list(docs, min.len = 1, names = "unique", types = "matrix", any.missing = FALSE)){
      if (verbose) message(check_list(docs, min.len = 1, names = "unique", types = "matrix", any.missing = FALSE))
      return(FALSE)
    }
    if (!all(sapply(docs, nrow) == 2)){
      if (verbose) message("not all elements have two rows")
      return(FALSE)
    }
    if (!all(sapply(docs, function(x) all(x[2,] == 1)))){
      if (verbose) message("not all values in the second row equal 1")
      return(FALSE)
    }
    if (verbose) message("checked")

    #dates
    if (verbose) message("dates: ", appendLF = FALSE)
    dates = getDates(obj)
    if (!test_date(dates, any.missing = FALSE)){
      if (verbose) message(check_date(dates, any.missing = FALSE))
      return(FALSE)
    }
    if (!all(names(dates) %in% names(docs)) || !all(names(docs) %in% names(dates))){
      if (verbose) message("not same names as \"docs\"")
      return(FALSE)
    }
    if (length(dates) != length(docs)){
      # should not happen
      if (verbose) message("not same length as \"docs\"")
      return(FALSE)
    }
    if (verbose) message("checked")

    #vocab
    if (verbose) message("vocab: ", appendLF = FALSE)
    vocab = getVocab(obj)
    if (!test_character(vocab, any.missing = FALSE, unique = TRUE)){
      if (verbose) message(check_character(vocab, any.missing = FALSE, unique = TRUE))
      return(FALSE)
    }
    if (verbose) message("checked")

    #chunks
    if (verbose) message("chunks: ", appendLF = FALSE)
    chunks = getChunks(obj)
    if (!is.data.table(chunks) ||
        !all(c("chunk.id", "start.date", "end.date", "memory", "n", "n.discarded",
               "n.memory", "n.vocab") %in% colnames(chunks))){
      if (verbose) message("not a data.table with standard parameters")
      return(FALSE)
    }
    if (anyDuplicated(chunks$chunk.id)){
      if (verbose) message("duplicated \"chunk.id\"")
      return(FALSE)
    }
    if (!is.integer(chunks$chunk.id)){
      if (verbose) message("\"chunk.id\" is not an integer")
      return(FALSE)
    }
    if (!is.integer(chunks$n)){
      if (verbose) message("\"n\" is not an integer")
      return(FALSE)
    }
    if (!is.integer(chunks$n.discarded)){
      if (verbose) message("\"n.discarded\" is not an integer")
      return(FALSE)
    }
    if (!is.integer(chunks$n.memory)){
      if (verbose) message("\"n.memory\" is not an integer")
      return(FALSE)
    }
    if (!is.integer(chunks$n.vocab)){
      if (verbose) message("\"n.vocab\" is not an integer")
      return(FALSE)
    }
    if (!is.Date(chunks$start.date)){
      if (verbose) message("\"start.date\" is not a Date object")
      return(FALSE)
    }
    if (!is.Date(chunks$end.date)){
      if (verbose) message("\"end.date\" is not a Date object")
      return(FALSE)
    }
    if (!is.Date(chunks$memory)){
      if (verbose) message("\"memory\" is not a Date object")
      return(FALSE)
    }
    if (any(is.na(chunks$chunk.id))){
      if (verbose) message("NA(s) in \"chunk.id\"")
      return(FALSE)
    }
    if (any(is.na(chunks$n))){
      if (verbose) message("NA(s) in \"n\"")
      return(FALSE)
    }
    if (any(is.na(chunks$n.vocab))){
      if (verbose) message("NA(s) in \"n.vocab\"")
      return(FALSE)
    }
    if (any(is.na(chunks$start.date))){
      if (verbose) message("NA(s) in \"start.date\"")
      return(FALSE)
    }
    if (any(is.na(chunks$end.date))){
      if (verbose) message("NA(s) in \"end.date\"")
      return(FALSE)
    }
    if (length(dates) != sum(chunks$n)){
      if (verbose) message("sum of \"n\" does not match number of texts")
      return(FALSE)
    }
    if (length(vocab) != max(chunks$n.vocab)){
      if (verbose) message("max of \"n.vocab\" does not match number of vocabularies")
      return(FALSE)
    }
    if (is.unsorted(chunks$n.vocab)){
      if (verbose) message("\"n.vocab\" is not monotonously increasing")
      return(FALSE)
    }
    if (min(dates) < min(chunks$start.date)){
      if (verbose) message("minimum of \"start.date\" is larger than minimum of text's dates")
      return(FALSE)
    }
    if (max(dates) > max(chunks$end.date)){
      if (verbose) message("maximum of \"end.date\" is smaller than maximum of text's dates")
      return(FALSE)
    }
    if (verbose) message("checked")

    #param
    if (verbose) message("param: ", appendLF = FALSE)
    param = getParam(obj)
    testNames = c("vocab.abs", "vocab.rel", "vocab.fallback", "doc.abs")
    if (!test_list(param, types = c("numeric", "integer"), names = "named", any.missing = FALSE)){
      if (verbose) message(check_list(param, types = c("numeric", "integer"), names = "named", any.missing = FALSE))
      return(FALSE)
    }
    if (!test_set_equal(names(param), testNames)){
      if (verbose) message(check_set_equal(names(param), testNames))
      return(FALSE)
    }
    if (param$vocab.abs < 0){
      if (verbose) message("\"vocab.abs\" is smaller than 0")
      return(FALSE)
    }
    if (param$vocab.rel < 0){
      if (verbose) message("\"vocab.rel\" is smaller than 0")
      return(FALSE)
    }
    if (param$vocab.rel > 1){
      if (verbose) message("\"vocab.rel\" is greater than 0")
      return(FALSE)
    }
    if (param$vocab.fallback < 0){
      if (verbose) message("\"vocab.fallback\" is smaller than 0")
      return(FALSE)
    }
    if (param$doc.abs < 0){
      if (verbose) message("\"doc.abs\" is smaller than 0")
      return(FALSE)
    }
    if (verbose) message("checked")
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
    "lm_topic_labels Object generated using ", x$model, "\n ",
    paste0(seq_along(x$labels), ": ", x$labels, topic_terms, collapse = "\n "),
    sep = ""
  )
}
