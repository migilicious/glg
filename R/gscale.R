#' @title Grading Scale
#'
#' @description This is a function named 'gscale' which creates the scale of grades for the Swiss school grading system.
#'
#' @param pass The minimum number of points to pass the exam.
#' @param maxp The maximum number of points possible.
#' @param cfactor The correction factor allows for a steeper slope of the grading scale for passing grades. The smaller the \code{cfator}, the steeper the slope (default=1).
#' @return The grading scale.
#' @export
gscale <- function (pass, maxp, cfactor=1, gr=seq(0.75,6,0.5)) {
  if(cfactor>1 | cfactor<0) warning("Choose a better cfactor")
  f=(gr-1)*(pass)/2.75
  p=(gr-3.75)*((maxp-pass)*cfactor)/2.25+pass
  points=ifelse(gr>4,p,f)
  points=ifelse(points<0,0,points)
  grade=gr+0.25
  print(cbind(grade,points))
}
