\name{createAPI}
\alias{createAPI}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Create API
}
\description{
Creates the REST endpoint http-methods 
GET info()
and
POST Graphs(dataset).
info() will return a json-file with a short description of the package.
Graphs(dataset) will return a pdf-file with visualizations of the input data set
}
\usage{
createAPI()
}
%- maybe also 'usage' for other objects documented here.
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
function () 
{
    library(plumber)
    library(ggplot2)
    pr <- plumber$new()
    pr$handle("GET", "/info", function() {
        list(Name = "Insurance Graphics", Description = "Returns graphics to help interpreting the input dataset. \n         The inputed file must have the features AGE, GENDER, INSURANCE_START \n         and INSURANCE_END.", 
            POST.method = "Returns pdf file with visualization of the gender distribution, age and gender distribution, distribution of the insurance durations and number of insured people")
    })
    pr$handle("POST", "/Graphs", serializer = plumber::serializer_content_type(type = "application/pdf"), 
        function(dataset) {
            tmp <- tempfile()
            pdf(tmp)
            library(ggplot2)
            t <- textConnection(dataset)
            data <- read.csv(t, header = TRUE, sep = ";")
            data$agegroup = as.character(round(data$AGE/20))
            print(agePlot(data = data))
            print(ageGenderPlot(data = data))
            data$INSURANCE_START = as.Date(levels(data$INSURANCE_START))
            data$INSURANCE_END = as.Date(levels(data$INSURANCE_END))
            data$INSURANCE_TIME = round(as.numeric(data$INSURANCE_END - 
                data$INSURANCE_START)/356, 2)
            print(insuranceDurationPlot(data = data))
            print(insuredPeoplePlot(data = data))
            dev.off()
            readBin(tmp, "raw", n = file.info(tmp)$size)
        })
    pr$run()
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
