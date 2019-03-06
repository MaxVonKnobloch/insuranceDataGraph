\name{ageGenderPlot}
\alias{ageGenderPlot}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Age and Gender Plot
}
\description{
Plots a histogram that shows the age and gender distribution in the data set.
}
\usage{
ageGenderPlot(data)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{data}{data set with features "AGE" and "GENDER"
}
}
\details{
}
\value{
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

## The function is currently defined as
function (data) 
{
    headlineTheme = theme(legend.title = element_blank(), plot.title = element_text(size = 20, 
        face = "bold", hjust = 0.5))
    chart = ggplot(data = data, aes(x = AGE, fill = GENDER)) + 
        headlineTheme + geom_histogram(binwidth = 10, position = position_dodge(width = 7)) + 
        headlineTheme + labs(title = "Insured People", x = "Age", 
        y = "Number of People") + scale_fill_brewer(palette = "Set1", 
        breaks = c("F", "M"), labels = c("Female", "Male")) + 
        scale_x_continuous(breaks = seq(0, 80, 10)) + scale_y_continuous(breaks = seq(0, 
        13, 1))
    return(chart)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
