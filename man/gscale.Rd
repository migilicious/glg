\name{gscale}
\alias{gscale}
\title{Grading Scale}
\usage{
gscale(pass,maxp,cfactor=1,rnd=0.5)
}
\description{
The function \code{gscale()} creates grading scales for the Swiss school grading system.
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
  \item{rnd}{
Rounds the grades to whatever value you specify (default =0.5).
}
}
\examples{
gscale(pass=34,maxp=90,cfactor=0.8,rnd=0.25)
}
