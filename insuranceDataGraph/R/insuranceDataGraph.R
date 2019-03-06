

#' createing REST-Endpoints
#'
#' Runs the swagger UI for the GET endpoint info and the POST endpoint Graphs(dataset)
#' @export
createAPI <- function(){
  library(plumber)
  library(ggplot2)
  pr <- plumber$new()
  
  pr$handle("GET", "/info", function(){
    list(Name= "Insurance Graphics",
         Description = "Returns graphics to help interpreting the input dataset. 
         The inputed file must have the features AGE, GENDER, INSURANCE_START 
         and INSURANCE_END. The POST method Graphs(dataset) returns a pdf file 
         with visualization of the gender distribution, age and gender distribution, 
         distribution of the insurance durations and number of insured people"
    )
  })
  
  pr$handle("POST","/Graphs",serializer = plumber::serializer_content_type(type="application/pdf"), function(dataset){
    
    #Create pdf which contains graphs
    tmp <- tempfile()
    pdf(tmp)
    
    library(ggplot2)
    t <- textConnection(dataset)
    data <- read.csv(t,header = TRUE,sep = ";")
    
    # Create Plots and print them into pdf file
    
    # Group ages in five groups
    data$agegroup = as.character(round(data$AGE/20))
    
    # Graph 1: Age distribution
    print(agePlot(data = data))
    
    #Graph 2: Age and Gender Distribution
    print(ageGenderPlot(data = data))
    
    # modifying data set
    data$INSURANCE_START = as.Date(levels(data$INSURANCE_START))
    data$INSURANCE_END = as.Date(levels(data$INSURANCE_END))
    data$INSURANCE_TIME = round(as.numeric(data$INSURANCE_END-data$INSURANCE_START)/356,2)
    
    #Graph 3: Distribution of Insurance Duration
    print(insuranceDurationPlot(data = data))
    
    # Graph 4: Number of people with active insurance per year
    print(insuredPeoplePlot(data = data))
    
    dev.off()
    
    readBin(tmp, "raw", n=file.info(tmp)$size)
  })
  
  pr$run()
}

#' agePlot
#'
#' Function to Plot the distribution of Age
#' @param data dataset with feature AGE
#' @export
agePlot = function(data){
  
  # Create coordinates and labels for the percentage annotation in the pie chart
  at <-  length(data$agegroup) - as.numeric(cumsum(table(data$agegroup))-0.5*table(data$agegroup))
  label=paste0(round(table(data$agegroup)/sum(table(data$agegroup)),2) * 100,"%")
  
  headlineTheme = theme(legend.title=element_blank(),
                        plot.title=element_text(size=20, face="bold",hjust = 0.5))
  
  chart = ggplot(data=data, aes(x=factor(1),fill=agegroup))+
    geom_bar(width=1)+
    labs(title = "Age Distribution of People")+
    theme_minimal()+
    headlineTheme+
    theme(
      axis.title = element_blank(),
      panel.grid=element_blank(),
      axis.text = element_blank()
    )+
    scale_fill_brewer(palette = "Set1",breaks =as.character(0:4),
                      labels=c("< 20 years",
                               "20-40 years",
                               "40-60 years",
                               "60-80 years",
                               "> 80 years"))+
    annotate(geom = "text", y = at, x = 1,label=label,size=5)+
    coord_polar("y")
  
  return(chart)
}

#' ageGenderPlot
#'
#' Function to Plot the distribution of Age and Gender
#' @param data dataset with feature AGE and GENDER
#' @export
ageGenderPlot = function(data){
  headlineTheme = theme(legend.title=element_blank(),
                        plot.title=element_text(size=20, face="bold",hjust = 0.5))
  
  chart = ggplot(data=data,aes(x = AGE, fill = GENDER))+
    headlineTheme+
    geom_histogram(binwidth = 10,position = position_dodge(width = 7))+
    headlineTheme+
    labs(title="Insured People",x = "Age", y = "Number of People")+
    scale_fill_brewer(palette = "Set1",breaks=c("F", "M"),labels=c("Female", "Male"))+
    scale_x_continuous(breaks = seq(0,80,10))+
    scale_y_continuous(breaks = seq(0,13,1))
  
  return(chart)
}

#' insuranceDurationPlot
#'
#' Function to Plot the distribution of insurance durations
#' @param data dataset with feature INSURANCE_START and INSURANCE_END
#' @export
insuranceDurationPlot = function(data){
  
  headlineTheme = theme(legend.title=element_blank(),
                        plot.title=element_text(size=20, face="bold",hjust = 0.5))
  
  chart = ggplot(data = data, aes(data$INSURANCE_TIME))+
    geom_histogram(
      binwidth = 5,
      fill = "Blue",
      col = "Black"
    )+
    headlineTheme+
    scale_fill_brewer(palette = "Set1")+
    scale_x_continuous(breaks = seq(0,30,5))+
    labs(title="Length of Insurance Duration",x = "Insurance Duration in Years",y="Number of Insured People")
 
  return(chart) 
}

#' insuredPeoplePlot
#'
#' Function to Plot the number of simultaniously insured people
#' @param data dataset with feature INSURANCE_START and INSURANCE_END
#' @export
insuredPeoplePlot = function(data){
  
  # Get years for x axis
  insurance_year = format(data$INSURANCE_START,"%Y")
  years = as.numeric(min(format(data$INSURANCE_START,"%Y"))):as.numeric(max(format(data$INSURANCE_END,"%Y")))
  
  # Count for each year, how many people were insured
  insured_people = rep(0,length(years))
  for (i in 1:nrow(data)){
    years_person = as.numeric(min(format(data$INSURANCE_START[i],"%Y"))):as.numeric(max(format(data$INSURANCE_END[i],"%Y")))
    insured_people[years_person-years[1]+1] = insured_people[years_person-years[1]+1]+1
  }
  
  #Plot
  insured = data.frame(years,insured_people)
  chart = ggplot(data = insured, aes(x = years,y=insured_people))+
    geom_line(color="red",size=1)+
    labs(title = "Number of People that have been Insured",x = "Years",y = "Number of People")+
    theme(plot.title=element_text(size=20, face="bold",hjust = 0.5))
  
  return(chart)
  
}
