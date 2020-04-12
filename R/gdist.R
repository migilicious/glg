#' @title Grades Creator
#'
#' @description The function \code{gdist()} creates a distribution of your grades for the Swiss school grading system.
#'
#' @param gr A vector of grades.
#' @param scale You can choose either quarter or half grades.
#' @return Generates a distribution of your grades.
#' @export
gdist <- function (gr, scale=0.5) {
  if (scale==0.5) {
  hist(gr, main = "Distribution of Grades", xlab = "Grade",
     breaks = seq(1,6,by=0.1),col = "grey", xaxt="n")
  axis(1, at=c(0.95,1.45,1.95,2.45,2.95,3.45,3.95,4.45,4.95,5.45,5.95),
     labels = c("1","1.5","2","2.5","3","3.5","4","4.5","5","5.5","6"))
  grid(NA,NULL)
  } else if (scale==0.25) {
  hist(gr, main = "Distribution of Grades", xlab = "Grade",
     breaks = seq(1,6,by=0.1),col = "grey", xaxt="n")

  axis(1, at=c(0.95,1.2,1.45,1.7,1.95,2.2,2.45,2.7,2.95,3.2,3.45,3.7,3.95,4.2,4.45,4.7,4.95,5.2,5.45,5.7,5.95),
     labels = c("1","1.25","1.5","1.75","2","2.25","2.5","2.75","3","3.25","3.5","3.75","4","4.25","4.5","4.75","5","5.25","5.5","5.75","6"))
  grid(NA,NULL)
  } else {
    stop("Choose a scale of either 0.25 or 0.5.")
  }
}
