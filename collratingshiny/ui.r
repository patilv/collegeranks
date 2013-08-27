shinyUI(bootstrapPage(
  
  div(class="row",
      div(class="span3",
          selectInput("char1", "Characteristic 1 (Horizontal axis):",
                      choices =c(
                        "Overall.Rank","Rate.My.Prof.Rank","Actual.Retention.Rate.Rank","Actual.Vs.Pred.Retention.Rank","Payscale.Salary.Rank",
                        "American.Leaders.Rank","Federal.Student.Debt.Rank","Debt.Default.Rate.Rank","Actual.Vs.Pred.Prop.Of.Students.Borrowing.Rank",
                        "Actual.Graduation.Rate.Rank","Actual.Vs.Pred.Graduation.Rate.Rank","Student.Awards.Rank","Alumni.Receiving.PhD.Rank",
                        "Total.Score","Best.Value.Rank"))),
      
      div(class="row",
          div(class="span3",
              selectInput("char2", "Characteristic 2 (Vertical axis):",
                          choices =c(
                            "Federal.Student.Debt.Rank","Rate.My.Prof.Rank","Actual.Retention.Rate.Rank","Actual.Vs.Pred.Retention.Rank","Payscale.Salary.Rank",
                            "American.Leaders.Rank","Overall.Rank","Debt.Default.Rate.Rank","Actual.Vs.Pred.Prop.Of.Students.Borrowing.Rank",
                            "Actual.Graduation.Rate.Rank","Actual.Vs.Pred.Graduation.Rate.Rank","Student.Awards.Rank","Alumni.Receiving.PhD.Rank",
                            "Total.Score","Best.Value.Rank"))))),
  div(class="row",
      div(class="span3",selectInput("colorvar", "Color Dots by:",
                                    choices =c("Control","Carnegie.Classification")))),  
  div(class="row",
      div(class="span7",plotOutput("scatter1plot")))
  
  
  
))

