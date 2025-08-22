\name{round_near}
\alias{round_near}
\title{
Rounds a Variable to the Nearest Pretty Number
}
\description{
For some graphs, \pkg{Base} \code{pretty} function may not provide the correct rounding. This is a different algorithm suitable for the graphs produced in 'EQUAL-STATS' software.
}
\usage{
round_near(x)
}
\arguments{
\item{x}{A numeric variable.}
}
\value{
A "pretty number" suitable for use in graphs.
}
\references{
\url{https://sites.google.com/view/equal-group/home}
}
\author{
Kurinchi Gurusamy
}
\note{
This is part of a suite of functions required to allow 'EQUAL-STATS' program to run. This is unlikely to be used as a stand alone function.
}
\seealso{
\code{\link[base:pretty]{base::pretty()}}
}
\concept{ EQUAL-STATS }
\examples{
x <- 7
round_near(x)

x <- 754
round_near(x)
}
