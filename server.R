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
library(stringr)
library(markdown)
library(pander)




# function derived from the highlightHTMLcells() function of the highlightHTML package
colortable <- function(htmltab, css, style="table-condensed table-bordered"){
  tmp <- str_split(htmltab, "\n")[[1]]
  CSSid <- gsub("\\{.+", "", css)
  CSSid <- gsub("^[\\s+]|\\s+$", "", CSSid)
  CSSidPaste <- gsub("#", "", CSSid)
  CSSid2 <- paste(" ", CSSid, sep = "")
  ids <- paste0("<td id='", CSSidPaste, "'")
  for (i in 1:length(CSSid)) {
    locations <- grep(CSSid[i], tmp)
    tmp[locations] <- gsub("<td", ids[i], tmp[locations])
    tmp[locations] <- gsub(CSSid2[i], "", tmp[locations],
                           fixed = TRUE)
  }
  htmltab <- paste(tmp, collapse="\n")
  Encoding(htmltab) <- "UTF-8"
  list(
    tags$style(type="text/css", paste(css, collapse="\n")),
    tags$script(sprintf(
      '$( "table" ).addClass( "table %s" );', style
    )),
    HTML(htmltab)
  )
}



# server design
server <- function(input, output) {

# set.seed(2)
#data<-reactive({rnorm(input$samplesize, 7,3)})

# The user needs to click on the action button labeled "Draw Sample" for the plot and descriptives to appear
  data<-eventReactive(input$do_sample,
                      {abs(rnorm(input$samplesize, 7,3))})
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
   output$descriptives <- renderUI({
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


       # This part of the code adds colors to some rows.
       # define CSS tags
       css <- c("#meancolor   {color: #0000FF;}",
                "#mediancolor {color: #FF0000;}")
       # example data frame
       # add the tag inside the cells

       table.desc.trans[1, ] <- paste(table.desc.trans[1, ], "#meancolor")
       table.desc.trans[6, ] <- paste(table.desc.trans[6, ], "#mediancolor")

       print(table.desc.trans)

       # generate html table with pander package and markdown package
       htmltab <- markdownToHTML(
         text = pandoc.table.return(
           table.desc.trans,
           style = "rmarkdown",
           split.tables = Inf,
           row.names = FALSE
         ),
         fragment.only = TRUE
       )
       colortable(htmltab, css)

  })

}
