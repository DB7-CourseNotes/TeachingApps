library(glmnet)
library(MASS)
library(shiny)
library(ggplot2)
library(patchwork)
theme_set(theme_bw())

input <- list(n = 50, max_slope = 10, sigma = 1)

ui <- fluidPage(
    
    # Application title
    titlePanel("Restricting the Slope"),
    
    sidebarLayout(
        sidebarPanel(
            sliderInput("n",
                "n",
                min = 10,
                max = 200,
                value = 50,
                step = 5),
            sliderInput("sigma",
                "SD of Residuals",
                min = 0.1,
                max = 10,
                value = 1,
                step = 0.1),
            sliderInput("max_slope",
                "Max Slope",
                min = 0,
                max = 10,
                value = c(1),
                step = 0.1,
                animate = list(interval = 400)),
            actionButton(inputId = "doit", 
                label = "Click me for new data")
        ),
        
        mainPanel(
            plotOutput("distPlot"),
            tags$div(HTML("When we restrict the slope, the intercept changes accordingly!"))
        )
    )
)

server <- function(input, output) {

    makedata <- reactive({
        input$doit
        X <- cbind(1, runif(input$n, 0, 10))
        colnames(X) <- c("(Intercept)", "x1")

        y <- X %*% c(100, 7) + rnorm(input$n, 0, input$sigma)
        mydf <- data.frame(y = y, x1 = X[, 2])
        mydf
    })
    
    output$distPlot <- renderPlot({
        mydf <- makedata()

        mylm <- lm(y ~ ., data = mydf)
        xslope <- min(mylm$coefficients[2], input$max_slope)
        myglm <- lm(y ~ offset(xslope * x1), data = mydf)
        mylm_coef <- coef(mylm)

        ggplot() +
            geom_point(data = mydf,
                aes(x = x1, y = y)) +
            geom_abline(
                aes(intercept = coef(myglm)[1], slope = xslope)) +
            geom_abline(
                aes(intercept = mylm_coef[1], slope = mylm_coef[2]),
                colour = "red") +
            labs(title = paste0("y vs. x1, slope = ",
                round(xslope, 2),
                " (", round(mylm_coef[2], 2), ")"))
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
