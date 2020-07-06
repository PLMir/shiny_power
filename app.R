#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(dplyr)
library(ggplot2)


# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Thinking about statistical power"),

    
    
    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            
            sliderInput(inputId = "intercept",
                        label = "Choose a true value for the intercept:",
                        min = -100,
                        max = 100,
                        value = 0,
                        ticks = FALSE),
            sliderInput(inputId = "slope",
                        label = "Choose a true value for the slope:",
                        min = -100,
                        max = 100,
                        value = 0,
                        ticks = FALSE),
            sliderInput(inputId = "res",
                        label = "Choose a true value for the amount of residual variation:",
                        min = 0,
                        max = 1000,
                        value = 500,
                        ticks = FALSE),
            
            actionButton(inputId = "new_truth",
                         label = "Set parameter values"),
            tags$hr(),
            
            sliderInput(inputId = "sample",
                         label = "Choose a sample size:",
                         min = 3,
                         max = 1000,
                         value = 3,
                         step = 1,
                        ticks = FALSE),
            
            actionButton(inputId = "new_sample",
                         label = "New sample")
            
            
        ),

        # Show a plot of the generated distribution
        mainPanel(
            tags$strong("How this app works"),
            tags$p("Start by setting the ground truth and amount of residual variation in the data
           and click on the Set parameter values button. The default values correspond to a null effect on the intercept and on the slope."),
            tags$p("Then choose the size of the sample you wish to collect from the full population (n = 1000) and click on the New sample button.
           Observe how the estimates are more variable with smaller samples."),
            tags$p("If you set the slope to 0, then the null hypothesis is true and any statistically significant result you get arises from your tolerance for a (5%) false positive rate.
           You can also observe how, with lower sample sizes, the p < 0.05 (false positive) results you get will tend to be associated with large estimated effect sizes."),
            tags$p("If you set the slope to a value different than 0, then the null hypothesis is false and you can observe the effect of power, 
           namely your ability to detect the existing effect. Power is impacted by the 0.05 significance threshold, 
           by the magnitude of the effect (how different the slope is from 0), 
           by the sample size, as well as by the amount of noise in your measurement or in the effect (here determined by the amount of residual variation). 
           Here also, you can observe how with low power, you will tend to get statistically significant results that will be associated with overestimates of the effect size 
           (and occasionally even reversals of the direction of the effect!)."),
            
           plotOutput("scatterPlot"),
           textOutput("data_text"),
           textOutput("sample_text"),
           textOutput("note"),
           tags$br(),
           tags$hr(),
           tags$em("This app was created by Patricia Mirabile."),
           tags$br(),
           tags$br()
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {


    data <- eventReactive(input$new_truth, {
        pop_size <- 1000
        
        tibble(x_1 = runif(pop_size, min = 0, max = 100),
                y = input$intercept + 
                   input$slope * x_1 + 
                   rnorm(pop_size, 0, input$res)) 
        
    })
    
    sample_size <- eventReactive(input$new_sample, 
                                 {input$sample})
    
    sample <- eventReactive(c(input$new_truth, input$new_sample), {
        data() %>% 
            sample_n(sample_size())
        })
    
    data_fit <- reactive( {
        lm(y ~ x_1,
           data = data())
    })
    
    sample_fit <- reactive({
        lm(y ~ x_1,
           data = sample())
    })
    
    
    data_p <- reactive({
        round(summary(data_fit())$coefficients[, 4], digits = 4)[2]
    })
    
    sample_p <- reactive({
        round(summary(sample_fit())$coefficients[, 4], digits = 4)[2]
    })
    
    output$scatterPlot <- renderPlot({

         data <- data() %>% 
             mutate(predictions = predict(data_fit()))
         
         sample <- sample() %>% 
             mutate(predictions = predict(sample_fit()))
        
        
        ggplot(data = data, aes(x = x_1, y = y)) +
            labs(x = "x") +
            geom_point(shape = 1, color = "grey70", alpha = 0.8) +
            geom_line(aes(y = predictions), color = "black") +
            
           geom_point(data = sample, color = "red2", alpha = 0.5) +
           geom_line(data = sample, aes(y = predictions),
                     size = 1, color = "red2") +
            theme_bw()
    })
    
    output$data_text <- renderText({
        if (is.null(sample())) {
            
        } else {
    paste0("p-value for the slope on the entire population (n = 1000): ", data_p(),
          ".")
    }})

    output$sample_text <- renderText({
        if (is.null(sample())) {
            
        } else {
        paste0("p-value for the slope on the sample (n = ", sample_size(),")", 
               ifelse(sample_p() <= 0.05,
                      paste0(": ", sample_p(), "*."),
                      paste0(": ", sample_p(), ".")))
               
        }
        
    })
    
    output$note <- renderText({
        if (is.null(sample())) {
            
        } else {
            "* indicates a statistically significant effect."
        }
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
