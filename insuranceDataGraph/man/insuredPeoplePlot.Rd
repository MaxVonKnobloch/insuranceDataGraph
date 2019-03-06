\name{insuredPeoplePlot}
\alias{insuredPeoplePlot}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Insured People Plot
}
\description{
Plots a histogram that shows the number of insured people at the same time.
}
\usage{
insuredPeoplePlot(data)
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
Maximilian von Knobloch
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
    insurance_year = format(data$INSURANCE_START, "\%Y")
    years = as.numeric(min(format(data$INSURANCE_START, "\%Y"))):as.numeric(max(format(data$INSURANCE_END, 
        "\%Y")))
    insured_people = rep(0, length(years))
    for (i in 1:nrow(data)) {
        years_person = as.numeric(min(format(data$INSURANCE_START[i], 
            "\%Y"))):as.numeric(max(format(data$INSURANCE_END[i], 
            "\%Y")))
        insured_people[years_person - years[1] + 1] = insured_people[years_person - 
            years[1] + 1] + 1
    }
    insured = data.frame(years, insured_people)
    chart = ggplot(data = insured, aes(x = years, y = insured_people)) + 
        geom_line(color = "red", size = 1) + labs(title = "Number of People that have been Insured", 
        x = "Years", y = "Number of People") + theme(plot.title = element_text(size = 20, 
        face = "bold", hjust = 0.5))
    return(chart)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
