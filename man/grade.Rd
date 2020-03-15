\name{grade}
\alias{grade}
\title{Grades Creator}
\usage{
grade(total,pass,six,cfactor=1,rnd=0.5)
}
\description{
This is a function named 'grade' which creates a discontinuous linear scale of grades for the Swiss school grading system. It allows to distinguish the slope of your grading scale for passes and fails. For a cfactor greater than one, the slope for grades between 1 and 3.5 will be less steep than the slope between 4 and 6.
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
The correction factor allowing for a discontinuous linear grade scale (default =1)
}
  \item{rnd}{
Rounds the grades to whatever value you specify (default =0.5)
}
}
\examples{
grade(45,34,67,2)
}
