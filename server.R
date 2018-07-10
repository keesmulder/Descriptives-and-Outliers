# Copyright statement:
# This shiny apllication is developed by Milica Miocevic to be used for educational purposes.
# Is is part of a program sponsered by the Education Incentive Funds of Utrecht University. 
# The lay-out for the shiny applications for this program is developed by Kimberley Lek. 
# The application is licensed under the ?? GNU General Public License V3.0 - decision on this? ?? 

# Author Comment:
# Do not delete any lines of code that are commented out; the app is still under development
# and these might be useful in the next version.
# For any quenstions or comments you can contact me at m.miocevic@uu.nl.

# File description:
# This file contains the server for the application related to correlation.

# Loading libraries 
library(shiny)

# server design
server <- function(input, output) {
  
# set.seed(2)
#data<-reactive({rnorm(input$samplesize, 7,3)})

# The user needs to click on the action button labeled "Draw Sample" for the plot and descriptives to appear    
  data<-eventReactive(input$do_sample,
                      {rnorm(input$samplesize, 7,3)})
# Draws boxplot in the first row of the app  
    output$plot1 <- renderPlot({

      y=data() 
      
      if(input$transform=="Add Outlier"){
        
        y_n<-sort(y, decreasing=TRUE)
        y_m<-y_n[2:length(y_n)]
        y<-c(29,y_m) # The outlier will always be equal to 29
             } 
      if(input$transform=="Add 5 points to each score"){
        y=data()+5
              } 

       if(input$transform=="Multiply all scores by 2"){
      y=data()*2
              } 
      if(input$transform=="Divide all scores by 2"){
        y=data()/2
              } 
      if(input$transform=="Calculate z-scores"){
        y=(data()-mean(data()))/sd(data())
              } 
    boxplot(y,xlab="x",ylab="", xlim =c(0,2) , ylim=c(-2,30), main = "Boxplot ", horizontal = TRUE)

       abline(v=mean(y),col="blue") 
       abline(v=median(y), col="red")

  })

  # Table 1, table containing the mean, standard deviation, variance, minimum, first quartile, median, third quartile, and maximum
   output$descriptives <- renderTable({
        y=data()
        
        if(input$transform=="Add Outlier"){

          y_n<-sort(y, decreasing=TRUE)
          y_m<-y_n[2:length(y_n)]
          y<-c(29,y_m)
                } else
          
     if(input$transform=="Add 5 points to each score"){
        y=data()+5
          } else 
     if(input$transform=="Multiply all scores by 2"){
       y=2*data()
     } else 
       if(input$transform=="Divide all scores by 2"){
         y=data()/2
       } else
     if(input$transform=="Calculate z-scores"){
       y=(data()-mean(data()))/sd(data())
     } 
# All sample descriptives are rounded to 2 decimal points
     median<-round(median(y),2)
     mean<-round(mean(y),2)
     Std.Deviation<-round(sd(y),2)
     Variance<-round(var(y),2)
     Q1<-round(quantile(y)[2],2)
     Q3<-round(quantile(y)[4],2)
     Minimum<-round(min(y),2)
     Maximum<-round(max(y),2)
     table.desc <- cbind(mean,Std.Deviation, Variance, Minimum, Q1, 
                         median, Q3, Maximum) 
       table.desc.trans.1<-t(table.desc)
       names<-rbind("Mean","Standard Deviation","Variance",
                    "Minimum", "Q1", "Median", "Q3", "Maximum")
       table.desc.trans<-cbind(names,table.desc.trans.1)
       colnames(table.desc.trans)<-c(" ", " ")
       print(table.desc.trans, colnames = TRUE)
      
  })

}