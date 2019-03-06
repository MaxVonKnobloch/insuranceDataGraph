\name{insuranceDurationPlot}
\alias{insuranceDurationPlot}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Insurance Duration Plot
}
\description{
Plots a histogram that shows the insurance duration distribution in the data set.
}
\usage{
insuranceDurationPlot(data)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{data}{data set with features "INSURANCE_START" and "INSURANCE_END"
%%     ~~Describe \code{data} here~~
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
Maximlian von Knobloch
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

## The function is currently defined as
function (data) 
{
    headlineTheme = theme(legend.title = element_blank(), plot.title = element_text(size = 20, 
        face = "bold", hjust = 0.5))
    chart = ggplot(data = data, aes(data$INSURANCE_TIME)) + geom_histogram(binwidth = 5, 
        fill = "Blue", col = "Black") + headlineTheme + scale_fill_brewer(palette = "Set1") + 
        scale_x_continuous(breaks = seq(0, 30, 5)) + labs(title = "Length of Insurance Duration", 
        x = "Insurance Duration in Years", y = "Number of Insured People")
    return(chart)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
