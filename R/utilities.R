library(data.table)
library(mltools)

#' One-hot encode categorical variables.
#'
#' This function one-hot encodes categorical variables and binds the entire frame.
#'
#' @param data frame
#' @param feature to encode
#' @return hot-encoded data frame
#'
#' @export
encode_and_bind <- function(frame, feature_to_encode) {
    res <- cbind(iris, mltools::one_hot(data.table::as.data.table(frame[[feature_to_encode]])))
    return(res)
}

#' Remove specified features from data frame.
#'
#' This function removes specified features from data frame.
#'
#' @param data frame
#' @param features to remove
#' @return original data frame less removed features
#'
#' @export
remove_features <- function(frame, features) {
  rem_vec <- unlist(strsplit(features, ', '))
  res <- frame[,!(names(frame) %in% rem_vec)]
  return(res)
}

#' Apply function to multiple columns.
#'
#' This function enables the application of a function to multiple columns.
#'
#' @param data frame
#' @param list of columns to apply function to
#' @param name of new column
#' @param function to apply
#' @return original data frame with new column attached 
#'
#' @export
apply_function_to_column <- function(frame, list_of_columns, new_col, funct) {
    use_cols <- unlist(strsplit(list_of_columns, ', '))
    new_cols <- unlist(strsplit(new_col, ', '))
    frame[new_cols] <- apply(frame[use_cols], 2, function(x) {eval(parse(text=funct))})
    return(frame)
}

#' Discover closest matching string from set.
#'
#' This function discovers the closest matching string from vector of strings.
#'
#' @param vector of strings
#' @param search string
#' @return closest matching string
#'
#' @export
get_closest_string <- function(vector_of_strings, search_string) {
    all_dists <- adist(vector_of_strings, search_string)
    closest <- min(all_dists)
    res <- vector_of_strings[which(all_dists == closest)]
    return(res)
}