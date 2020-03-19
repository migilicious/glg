\name{grade}
\alias{grade}
\title{Grades Creator}
\usage{
grade(total,pass,maxp,cfactor=1,rnd=0.5)
}
\description{
The function \code{grade()} creates a discontinuous linear scale of grades for the Swiss school grading system. It allows to distinguish the slope of your grading scale for passes and fails.
}
\arguments{
  \item{total}{
The total points scored.
}
  \item{pass}{
The minimum number of points to pass the exam.
}
  \item{maxp}{
The maximum number of points possible.
}
  \item{cfactor}{
The correction factor allows for a steeper slope of the grading scale for passing grades. The smaller the \code{cfator}, the steeper the slope (default=1).
}
  \item{rnd}{
Rounds the grades to whatever value you specify (default =0.5).
}
}
\examples{
grade(45,34,90,0.8)
}
