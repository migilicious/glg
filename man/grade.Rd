\name{grade}
\alias{grade}
\title{Grades Creator}
\usage{
grade(total,pass,six,cfactor=1,rnd=0.5)
}
\description{
The function \code{grade()} creates a discontinuous linear scale of grades for the Swiss school grading system. It allows to distinguish the slope of your grading scale for passes and fails.
}
\arguments{
  \item{total}{
The total points
}
  \item{pass}{
The min points to pass the exam
}
  \item{six}{
The min points to score a six
}
  \item{cfactor}{
The correction factor, to make sure that grades above a six are no possible (default =1)
}
  \item{rnd}{
Rounds the grades to whatever value you specify (default =0.5)
}
}
\examples{
grade(45,34,67,2)
}
