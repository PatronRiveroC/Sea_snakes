\name{jackk_sea_snakes}
\alias{jackk_sea_snakes}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Jackknife partitioning for multiple species%%  ~~function to do ... ~~
}
\description{Partitioning and save all data by jackkniffe method and save all species with all posibilities of train and test data
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
jackk_sea_snakes(csv_dir, train_dir, test_dir)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{csv_dir}{ directory containing all csv for all species to do this method
%%     ~~Describe \code{x} here~~
}
\item{train_dir}{ empty directory when all training data will be contained
%%     ~~Describe \code{x} here~~
}
\item{csv_dir}{ empty directory when all testing data will be contained
%%     ~~Describe \code{x} here~~
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{ all csv for train and test by species
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
\references{ Shcheglovitova, M., & Anderson, R. P. (2013). Estimating optimal complexity for ecological niche models: a jackknife approach for species with small sample sizes. Ecological Modelling, 269, 9-17.
%% ~put references to the literature/web site here ~
}
\author{ Carlos Patron-Rivero
%%  ~~who you are~~
}
\note{ for intallation
library(devtools)
install_github("PatronRiveroC/Ssnakes")
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{



% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory (show via RShowDoc("KEYWORDS")):
% \keyword{ ~kwd1 }
% \keyword{ ~kwd2 }
% Use only one keyword per line.
% For non-standard keywords, use \concept instead of \keyword:
% \concept{ ~cpt1 }
% \concept{ ~cpt2 }
% Use only one concept per line.
