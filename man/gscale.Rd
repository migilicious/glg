\name{gscale}
\alias{gscale}
\title{Grading Scale}
\usage{
gscale(pass,maxp,cfactor=1)
}
\description{
This is a function named 'gscale' which creates the scale of grades for the Swiss school grading system.
}
\arguments{

  \item{pass}{
The minimum number of points to pass the exam.
}
  \item{maxp}{
The maximum number of points possible.
}
  \item{cfactor}{
The correction factor allows for a steeper slope of the grading scale for passing grades. The smaller the \code{cfator}, the steeper the slope (default=1).
}
}
\examples{
gscale(34,90,0.8)
}
