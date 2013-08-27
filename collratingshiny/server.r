library(ggplot2)
library(ggthemes)
shinyServer(function(input, output) {
  #loading stored data file
  load("TotalRank.rda")
  TotalRank$Control=factor(TotalRank$Control,
                          c("Private",
                            "Public","#NA"))
  
  output$scatter1plot=renderPlot({scatter1plot<-ggplot(TotalRank,aes_string(x=input$char1, y=input$char2))+
                                   geom_point(aes_string(color=input$colorvar),size=3)+ geom_smooth(method='loess',level=.95,size=1)+
                                   ggtitle("Scatter Plot")+theme_solarized()
                                 print(scatter1plot)})
   })