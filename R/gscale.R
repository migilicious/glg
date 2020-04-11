#' @title Grading Scale
#'
#' @description This is a function named 'gscale' which creates the scale of grades for the Swiss school grading system.
#'
#' @param pass The minimum number of points to pass the exam.
#' @param maxp The maximum number of points possible.
#' @param cfactor The correction factor allows for a steeper slope of the grading scale for passing grades. The smaller the \code{cfator}, the steeper the slope (default=1).
#' @param rnd Rounds the grades.
#' @return The grading scale.
#' @export
gscale <- function (pass, maxp, cfactor=1, rnd=0.5) {
  Grade=seq(1,6,rnd)
  if(cfactor>1 | cfactor<0) warning("Choose a better cfactor")
  f=(Grade-1)*(pass)/2.75
  p=(Grade-3.75)*((maxp-pass)*cfactor)/2.25+pass
  Points=ifelse(Grade>=3.75,p,f)
  Points=ifelse(Points<0,0,Points)
  print(cbind(Grade,Points))
  plot(Points,Grade,type="o",main = "Grading Scale")
  abline(h=(round(3.75/rnd))*rnd,col="red",lty = 2)
  grid()
}
