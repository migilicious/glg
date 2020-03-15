#' @title Grades Creators
#'
#' @description This is a function named 'grade' which creates a discontinuous linear scale of grades for the Swiss school grading system. It allows to distinguish the slope of your grading scale for passes and fails.
#'
#' @param total The total points
#' @param pass The min points to pass the exam
#' @param six The min points to score a six
#' @param cfactor The correction factor allowing for a discontinuous linear grade scale
#' @param rnd Rounds the grades
#' @return The grade
#' @export
grade <- function (total, pass, six, cfactor=1, rnd=0.5) {
  if(any(is.na(total))) stop("Please drop your NAs")
  if(pass>=six) warning("Your scale is bullshit")
    if (total>=pass) {
    round((((total-pass)*2.25)/((six-pass)*cfactor)+3.75)/rnd)*rnd
  } else {
    round(((total*2.75)/(pass)+1)/rnd)*rnd
  }
}
