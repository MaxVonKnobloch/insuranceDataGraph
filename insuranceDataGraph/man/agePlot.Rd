\name{agePlot}
\alias{agePlot}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Age Plot
}
\description{
Plots a histogram that shows the age and gender distribution in the data set.
}
\usage{
agePlot(data)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{data}{data set with feature "AGE".
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

## The function is currently defined as
function (data) 
{
    at <- length(data$agegroup) - as.numeric(cumsum(table(data$agegroup)) - 
        0.5 * table(data$agegroup))
    label = paste0(round(table(data$agegroup)/sum(table(data$agegroup)), 
        2) * 100, "\%")
    headlineTheme = theme(legend.title = element_blank(), plot.title = element_text(size = 20, 
        face = "bold", hjust = 0.5))
    chart = ggplot(data = data, aes(x = factor(1), fill = agegroup)) + 
        geom_bar(width = 1) + labs(title = "Age Distribution of People") + 
        theme_minimal() + headlineTheme + theme(axis.title = element_blank(), 
        panel.grid = element_blank(), axis.text = element_blank()) + 
        scale_fill_brewer(palette = "Set1", breaks = as.character(0:4), 
            labels = c("< 20 years", "20-40 years", "40-60 years", 
                "60-80 years", "> 80 years")) + annotate(geom = "text", 
        y = at, x = 1, label = label, size = 5) + coord_polar("y")
    return(chart)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
