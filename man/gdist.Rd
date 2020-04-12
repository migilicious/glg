\name{gdist}
\alias{gdist}
\title{Grade Distribution}
\usage{
gdist(gr,scale=0.5)
}
\description{
The function \code{gdist()} creates a distribution of your grades for the Swiss school grading system.
}
\arguments{
  \item{gr}{
A vector of grades.
}
  \item{scale}{
You can choose either quarter or half grades (default =0.5).
}
}
\examples{
gdist(gr=c(2,2,3,4,4.5,6,5,2,3.5,3.5),scale=0.5)
}
