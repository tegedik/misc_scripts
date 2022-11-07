library(shiny)
library(tidyverse)
library(ggplot2)

ui <- fluidPage(
  titlePanel("Collatz Conjecture"),
  
  sidebarLayout(
    sidebarPanel(
      helpText("The Collatz conjecture is a conjecture in mathematics that concerns sequences defined as follows: start with any positive integer n. Then each term is obtained from the previous term as follows: if the previous term is even, the next term is one half of the previous term. If the previous term is odd, the next term is 3 times the previous term plus 1. The conjecture is that no matter what value of n, the sequence will always reach 1.", a(href="https://en.wikipedia.org/wiki/Collatz_conjecture", "Wikipedia")), 
      
      numericInput("num", label = "Pick a (positive integer) number:", value = 3),
      checkboxInput("checkbox", label = "Logarithm", value = FALSE),

br(),            
p("By", a(href="https://twitter.com/tahirenesgedik", "Tahir Enes Gedik"))    
      
    ),
    
    mainPanel(
      plotOutput("collatz")
      
    )
  )
)

server <- function(input, output) {
  
  
  output$collatz <- renderPlot({ 
   
    n <- input$num
    
      collatz <- function(x){
        if((x %% 2) == 0) {
          (x/2)
        } else {
          ((3*x) + 1)
        }} 
      
      n_total <- n
      while (n != 1) {
        n <- collatz(n)
        n_total <- c(n_total, n)
      }
      
      len <- (1:length(n_total))
      df <- data.frame(n_total, log(n_total), len)

if (input$checkbox == FALSE) {      
      df %>% 
        ggplot(aes(len, n_total)) + geom_line() + geom_point() + 
        labs(y="Numbers", x="Iterations", main="Collatz Conjecture") + 
        xlim(0, length(df$n_total)) + 
        ylim(0, max(df$n_total)) +
        theme_minimal()
} else {
  df %>% 
    ggplot(aes(len, log.n_total.)) + geom_line() + geom_point() + 
    labs(y="Numbers (log)", x="Iterations", main="Collatz Conjecture") + 
    xlim(0, length(df$n_total)) + 
    ylim(0, max(df$log.n_total.)) +
    theme_minimal()
}
    
  })
  
}

shinyApp(ui, server)