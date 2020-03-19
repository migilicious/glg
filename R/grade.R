#' @title Grades Creator
#'
#' @description This is a function named 'grade' which creates a discontinuous linear scale of grades for the Swiss school grading system. It allows to distinguish the slope of your grading scale for passes and fails.
#'
#' @param total The total points scored.
#' @param pass The minimum number of points to pass the exam.
#' @param maxp The maximum number of points possible.
#' @param cfactor The correction factor allows for a steeper slope of the grading scale for passing grades. The smaller the \code{cfator}, the steeper the slope (default=1).
#' @param rnd Rounds the grades.
#' @return Rounds the grades to whatever value you specify (default =0.5).
#' @export
grade <- function (total, pass, maxp, cfactor=1, rnd=0.5) {
  if(pass>=maxp) stop("Your scale is bullshit")
  f=round(((total*2.75)/(pass)+1)/rnd)*rnd
  p=round((((total-pass)*2.25)/((maxp-pass)*cfactor)+3.75)/rnd)*rnd
  ifelse(total>=pass,f,p)
}
