#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(ggplot2)

dataset <- diamonds

ui <- fluidPage(
    
    # Application title
    titlePanel("Cheat Die Selected?"),
    
    fluidRow(# Show a plot of the generated distribution
        column(3),
        column(6,plotOutput("myplot")),
        column(3)
    ),
    hr(),
    fluidRow(
        column(3),
        column(6,htmlOutput("txt")),
        column(3)),
    hr(),
    
    fluidRow(
        column(2),column(3),column(3,h2("Cheat Parameters")),column(3), column(1)
        ),
    fluidRow(
        column(2),
        column(3,h3("Cheat Effect:"),
               h5("Probability of 6 on cheat die/dice (prob. values= 0 - 1)"),
               #br(),
               column(4, ""),
               column(4, " ",
                      numericInput("cheatEffect", " ",
                                   value = 1, min=0, max=1, step=.05)
                      ),
               column(4, "")
        ),
        column(3, h3("Dice Mix:"),
               column(6, 
                      numericInput("diceNum", h5("# of Cheat Dice"), value = 1)
                      ),
               column(6,
                      numericInput("diceDen", h5("# of Total Dice"), value = 8)
                      )
    
        ),
        column(3, h3("Consecutive Rolls:"),
               h5("# of consecutive rolls of 6"),
               #column(4, ""),
               column(4, " ",
                numericInput("consec"," " , value = 2, min=1, max=10)),
               column(4, "")
        ),
        column(1)
    )
)

# Define server logic required to draw a histogram







server <- function(input, output) {

  
    plotInput <- reactive({
        
        #numOfConsect<-1
        numOfConsect<-ifelse(input$consec>0, as.integer(input$consec), 1)
        
        cheatrollProb<-input$cheatEffect
        
        numCheat<-input$diceNum
        numDice<-input$diceDen
        ProbOfCheat<-input$diceNum/input$diceDen
        
        #print(numOfConsect)
        cheatroll<-function(numOfConsect){
            
            #cheatrollProb<-1
            
            #ProbOfCheat<-1/8
            
            
            cheatdie<-dbinom(numOfConsect, size=numOfConsect, prob=cheatrollProb)
            
            cheatdie*ProbOfCheat/(cheatdie*ProbOfCheat+dbinom(numOfConsect, size=numOfConsect, prob=1/6)*(1-ProbOfCheat))
            
            
        }
        
        
        
        trace_0<-cheatroll(c(1,2,3,4,5,6,7,8,9,10))
        TracePass<- reactive(trace_0)
        #trace_0 <- rnorm(100, mean = 5)
        trace_1 <- rnorm(100, mean = 0)
        trace_2 <- rnorm(100, mean = -5)
        x <- c(1:10)
        
        #data <- data.frame(x, trace_0, trace_1, trace_2)
        data <- data.frame(x, trace_0)
        
        p<-ggplot(data, aes(x, trace_0)) + 
            labs(title="Cheat Die Prob. By Consecutive Rolls" , x= "Consecutive Rolls", y="Probability Cheat Die Selected" ) +
            geom_point(color=ifelse(x==numOfConsect,"red","black"), size=2) +
            geom_text(label=ifelse(x==numOfConsect,sprintf("%.3f", trace_0),''),fontface = "bold",color="red",hjust=.35,vjust=2 ) + 
            scale_x_continuous(breaks=x, minor_breaks = seq(1, 10, 1)) + 
            scale_y_continuous(breaks=seq(0,1,by=.1), limit=c(0,1)) +
            theme(plot.title = element_text(color = "black", size = 18, face = "bold", hjust=.5,margin = unit(c(5, 5, 5, 5), "mm"))) +
            theme(axis.title.x  = element_text(color = "black", size = 16, face = "bold",margin = unit(c(5, 5, 5, 5), "mm"))) +
            theme(axis.title.y  = element_text(color = "black", size = 16, face = "bold", vjust=.5,margin = unit(c(3, 3, 3, 3), "mm"))) + 
            theme(axis.text.y  = element_text(color = "black", size = 12, vjust=.5,margin = unit(c(2, 2, 2, 2), "mm"))) + 
            theme(panel.background = element_rect(fill = "#D7EDF9",
                                                  
                                                  size = 1, linetype = "solid")) +
            theme(legend.position="none")
        p
        
   
        
        txt<-paste("There is a ","<font color=\"#FF0000\"><b>", sprintf("%.3f", trace_0[numOfConsect]), "</b></font> probability that the gambler chose 
                   the \"cheat\" die given the following inputs:<ul></ul>",
                   
                   "<ul><li>A cheat die is modified to create a cheat probability of <font color=\"#FF0000\"><b>", cheatrollProb, "</b></font> for rolling a six",
                   "<li>Rolled a six on <font color=\"#FF0000\"><b>", numOfConsect, "</b></font> consecutive rolls",
                   "<li>There is <font color=\"#FF0000\"><b>", numCheat, "</b></font> cheat dice mixed in a box of <font color=\"#FF0000\"><b>", numDice, "</b></font></ul>"
                   
        )   
                
        
        list(plot = p ,
             txt = txt)
    })
    
    
   
    output$myplot <- renderPlot({ plotInput()$plot })
    output$txt <- renderText({ plotInput()$txt })


}

# Run the application 
shinyApp(ui = ui, server = server)
