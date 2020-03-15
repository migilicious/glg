#' @title Grades Creators
#'
#' @description This is a function named 'grade' which creates a discontinuous linear grade scale
#'
#' @param total The total points
#' @param pass The min points to pass the exam
#' @param six The min points to score a six
#' @param cfactor The correction factor allowing for a discontinuous linear grade scale
#' @return The grade
#' @export
grade <- function (total, pass, six, cfactor) {
  if (total>=pass) {
    if(any(is.na(total))) warning("Please drop your NAs")
    round((((total-pass)*2.25)/((six-pass)*cfactor)+3.75)/0.5, digits = 0)*0.5
  } else {
    if(any(is.na(total))) warning("Please drop your NAs")
    round(((total*2.75)/(pass)+1)/0.5, digits = 0)*0.5
  }
}
