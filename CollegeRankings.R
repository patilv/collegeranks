library(ggplot2)
library(reshape2)
library(rCharts)
suppressPackageStartupMessages(library(googleVis))


CompRank=read.csv("OverallRankings.csv", stringsAsFactors=FALSE)
BestValue=read.csv("2013BestValues.csv",stringsAsFactors=FALSE)
# Merge both
TotalRank=merge(CompRank,BestValue,all=T)
TotalRank$Name=paste(TotalRank$Name,TotalRank$State,sep=", ")
TotalRank=TotalRank[-c(2)]

# Replacing factor value and converting to numeric

for (i in 1:18){TotalRank[,i]<-gsub("600-650", "625", TotalRank[,i])}
for (i in 1:18){TotalRank[,i]<-gsub("#NA", "", TotalRank[,i])}

for (i in 4:18){TotalRank[,i] = as.numeric(TotalRank[,i])}
for (i in 1:3){TotalRank[,i] = factor(TotalRank[,i])}

TotalRank$Carnegie.Classification=factor(TotalRank$Carnegie.Classification,
                                        c("Baccalaureate Colleges--Arts & Sciences",
                                          "Baccalaureate Colleges--Diverse Fields",
                                          "Master's Colleges and Universities (smaller programs)",
                                          "Master's Colleges and Universities (medium programs)",
                                          "Master's Colleges and Universities (larger programs)",
                                          "Doctoral/Research Universities",
                                          "Research Universities (high research activity)",
                                          "Research Universities (very high research activity)"))

save(TotalRank,file="TotalRank.rda")

########## Saving table to display

tab1=dTable(TotalRank, sPaginationType = "full_numbers")
tab1$save("TotalRanktable.html", cdn=T)
tab1$publish('University Rankings Data',host='gist')
#tab1

### Plot all schools and best value schools on map
allschool=gvisGeoChart(TotalRank,locationvar="Name",colorvar="Overall.Rank",hovervar="Name", 
                  sizevar="Total.Score",
                  options=list(region="US",displayMode="markers",resolution="provinces",
                               colorAxis="{colors:['green','yellow','orange','red']}"))

cat(allschool$html$chart, file="allschoolgeolocation.html")
bestvalschool=gvisGeoChart(subset(TotalRank,Best.Value.Rank>0),locationvar="Name",colorvar="Best.Value.Rank",hovervar="Name", 
                                     sizevar="Total.Score",options=list(region="US",displayMode="markers",resolution="provinces",
                                                                        colorAxis="{colors:['green','yellow','orange','red']}"))
cat(bestvalschool$html$chart, file="bestvalschoolgeolocation.html")

# Overall Rankings of Colleges for those ranked as Best Value

BvalandOrankmore=TotalRank[c(1,2,3,17,18)]
BvalandOrankmore=subset(BvalandOrankmore,BvalandOrankmore$Best.Value.Rank!="NA")

BvalandOrankmore$Name=reorder(BvalandOrankmore$Name,BvalandOrankmore$Best.Value.Rank)
Bvalmelt=melt(BvalandOrankmore,id=c("Name","Carnegie.Classification","Control"))
names(Bvalmelt)=c("Name","Carnegie.Classification","Control","Type","value")                  

overallversustotal=ggplot(Bvalmelt,aes(x=value,y=Name,color=Type, size=Carnegie.Classification))+
  geom_point(alpha=.7)+ theme(axis.text.y = element_text(color="black"))+theme(axis.text.x = element_text(color="black"))+
  xlab("Rank")+ ylab("University")+ggtitle("Rankings of 100 Best value universities")
print(overallversustotal)

############ Plotting colleges with Overall Rankings and Best Value Rankings as axes

d1 <- dPlot(
  Best.Value.Rank ~ Overall.Rank,
  groups = c("Name","Carnegie.Classification","Control"),
  data = BvalandOrankmore,
  type = "bubble",height=800,
  width=1000,
  bounds = list(x=60, y=25, width=400, height=350)
)
d1$xAxis( type = "addMeasureAxis" )
d1$yAxis( type = "addMeasureAxis" )
d1$legend(
  x = 465,
  y = 0,
  width = 50,
  height = 200,
  horizontalAlign = "left"
)
d1
d1$save("bestval.html", cdn=T)
d1$publish()

############## Motion Chart to play around with individual colleges or a set of colleges

# Convenience interface to gvisMotionChart that allows to set default columns: Courtesy: Sebastian Kranz: http://stackoverflow.com/questions/10258970/default-variables-for-a-googlevis-motionchart
myMotionChart = function(df,idvar=colnames(df)[1],timevar=colnames(df)[2],xvar=colnames(df)[3],yvar=colnames(df)[4], colorvar=colnames(df)[5], sizevar = colnames(df)[6],...) {
  
  
  # Generate a constant variable as column for time if not provided
  # Unfortunately the motion plot still shows 1900...
  if (is.null(timevar)) {
    .TIME.VAR = rep(0,NROW(df))
    df = cbind(df,.TIME.VAR)
    timevar=".TIME.VAR"
  }
  
  # Transform booleans into 0 and 1 since otherwise an error will be thrown
  for (i in  1:NCOL(df)) {
    if (is.logical(df [,i])[1])
      df[,i] = df[,i]*1
  }
  
  # Rearrange columns in order to have the desired default values for
  # xvar, yvar, colorvar and sizevar
  firstcols = c(idvar,timevar,xvar,yvar,colorvar,sizevar)
  colorder = c(firstcols, setdiff(colnames(df),firstcols))
  df = df[,colorder]
  
  gvisMotionChart(df,idvar=idvar,timevar=timevar,...)
}

#  creating temp dataset with two new variables
#Adding a column for the year: Why? see tab 2 discussion below
TotalRank$Year=2013

motion = myMotionChart(TotalRank, idvar="Name", xvar= "Rate.My.Prof.Rank", yvar="Overall.Rank",timevar="Year",colorvar="Carnegie.Classification",
                options=list(showSidePanel=TRUE,showXScalePicker=FALSE,
                             showYScalePicker=FALSE
                ))
plot(motion)

cat(motion$html$chart, file="hellomotion.html")
####################
