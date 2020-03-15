\name{grade}
\alias{grade}
\title{Grades Creator}
\usage{
grade(total,pass,six,cfactor)
}
\description{
This is a function named 'grade' which creates a discontinuous linear grade scale
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
grade(45,34,67,1)
}
