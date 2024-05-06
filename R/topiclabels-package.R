#' @title Automated Topic Labeling with Language Models
#'
#' @description Leveraging (large) language models for automatic topic labeling.
#' The main function converts a list of top terms into a label for each topic.
#' Hence, it is complementary to any topic modeling package that produces a list
#' of top terms for each topic. While human judgement is indispensable for
#' topic validation (i.e., inspecting top terms and most representative documents),
#' automatic topic labeling can be a valuable tool for researchers in various scenarios.
#'
#' @section Labeling function:
#' \code{\link{label_topics}}
#'
#' @section Constructor:
#' \code{\link{as.lm_topic_labels}}
#'
#'
#' @import stats
#' @import checkmate
#' @import progress
#' @import httr
#' @import jsonlite
"_PACKAGE"
