#
# This is a Shiny web application. 
# Simple Shiny App showing cheat probability of a cheat die in a population. Simple demo of Bayes' theorem.
#
#

library(shiny)
library(ggplot2)

ui <- fluidPage(
  
  
 
  titlePanel("Cheat Die Selected?"),
  
 
  #UI Layout
  
  sidebarPanel(
    h3("User Input For Cheat Parameters:"),
    h4("Cheat Effect:"),
    h6("Probability of 6 on cheat die/dice (prob. values= 0 - 1)"),
    numericInput("cheatEffect", " ",value = 0.5, min=0, max=1, step=.05),
    h4("Dice Mix:"),
    numericInput("diceNum", h6("# of Cheat Dice"), value = 1),
    numericInput("diceDen", h6("# of Total Dice"), value = 100),
    h4("Consecutive Rolls:"),
    h6("# of consecutive rolls of 6"),
    numericInput("consec"," " , value = 3, min=1, max=10)
   ),   
  mainPanel(
    h4(htmlOutput("txt")),
    plotOutput("myplot", height=500))
    

)

server <- function(input, output) {

  
    plotInput <- reactive({
        
      #INPUT FROM UI: forcing parameters to min and max since not being enforced on UI  
      numOfConsec<-ifelse(input$consec>0, as.integer(input$consec), 1)
      
      #INPUTS FROM UI:
      cheatrollProb<-input$cheatEffect
      numCheat<-input$diceNum
      numDice<-input$diceDen
      ProbOfCheat<-input$diceNum/input$diceDen
      
      #Using binomial distribution to find probability: bayes theorem  
      cheatroll<-function(numOfConsec){
            cheatdie<-dbinom(numOfConsec, size=numOfConsec, prob=cheatrollProb)
            cheatdie*ProbOfCheat/(cheatdie*ProbOfCheat+dbinom(numOfConsec, size=numOfConsec, prob=1/6)*(1-ProbOfCheat))
        }
      
      #Y axis  
      probOfCheat<-cheatroll(c(1,2,3,4,5,6,7,8,9,10))
      # X axis
      x <- c(1:10)
      #X and Y  
      data <- data.frame(x, probOfCheat)
        
      #Plot formatting
      p<-ggplot(data, aes(x, probOfCheat)) + 
          labs(title="Cheat Die Prob.\n By \n Consecutive Rolls" , x= "Consecutive Rolls", y="Probability Cheat Die Selected" ) +
          geom_point(color=ifelse(x==numOfConsec,"red","black"), size=2) +
          geom_text(label=ifelse(x==numOfConsec,sprintf("%.3f", probOfCheat),''),fontface = "bold",color="red",hjust=.35,vjust=2 ) + 
          scale_x_continuous(breaks=x, minor_breaks = seq(1, 10, 1)) + 
          scale_y_continuous(breaks=seq(0,1,by=.1), limit=c(0,1)) +
          theme(plot.title = element_text(color = "black", size = 18, face = "bold", hjust=.5,margin = unit(c(5, 5, 5, 5), "mm"))) +
          theme(axis.title.x  = element_text(color = "black", size = 16, face = "bold",margin = unit(c(5, 5, 5, 5), "mm"))) +
          theme(axis.title.y  = element_text(color = "black", size = 16, face = "bold", vjust=.5,margin = unit(c(3, 3, 3, 3), "mm"))) + 
          theme(axis.text.y  = element_text(color = "black", size = 12, vjust=.5,margin = unit(c(2, 2, 2, 2), "mm"))) + 
          theme(panel.background = element_rect(fill = "#D7EDF9",size = 1, linetype = "solid")) +
          theme(legend.position="none")
        p
        
   
        
        txt<-paste("There is a ","<font color=\"#FF0000\"><b>", sprintf("%.3f", probOfCheat[numOfConsec]), "</b></font> probability that the gambler chose 
                   the \"cheat\" die given the following inputs:<ul></ul>",
                   "<ul><li>A cheat die is modified to create a cheat probability of <font color=\"#FF0000\"><b>", cheatrollProb, "</b></font> for rolling a six",
                   "<li>Rolled a six on <font color=\"#FF0000\"><b>", numOfConsec, "</b></font> consecutive rolls",
                   "<li>There is <font color=\"#FF0000\"><b>", numCheat, "</b></font> cheat dice mixed in a box of <font color=\"#FF0000\"><b>", numDice, "</b></font></ul>"
                   
        )   
                
        #To Make reactive
        list(plot = p ,
             txt = txt)
    })
    
    
   
    output$myplot <- renderPlot({ plotInput()$plot })
    output$txt <- renderText({ plotInput()$txt })


}

# Run the application 
shinyApp(ui = ui, server = server)
