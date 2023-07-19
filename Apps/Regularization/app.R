# pnorm

library(glmnet)
library(MASS)
library(shiny)
library(ggplot2)
library(patchwork)
theme_set(theme_bw())

input <- list(n = 50, corx12 = 0.5, sigma = 1, lambda = 1)

ui <- fluidPage(
    
    # Application title
    titlePanel("Regularization"),
    
    sidebarLayout(
        sidebarPanel(
            sliderInput("n",
                "n",
                min = 10,
                max = 200,
                value = 50,
                step = 5),
            sliderInput("corx12",
                "Correlation btwn x1 and x2",
                min = 0,
                max = 1,
                value = c(0.5),
                step = 0.05),
            sliderInput("sigma",
                "SD of Residuals",
                min = 0.1,
                max = 10,
                value = 1,
                step = 0.1),
            sliderInput("lambda",
                "lambda",
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
            tags$div(HTML("lambda controls the total sum of abs(slope), with the intercept changing accordingly."))
        )
    )
)

server <- function(input, output) {

    makedata <- reactive({
        input$doit
        X <- mvrnorm(input$n, c(5, 10), 
            matrix(c(1, input$corx12, input$corx12, 2), ncol = 2))
        X <- cbind(1, X, runif(input$n, 0, 10))
        colnames(X) <- c("(Intercept)", "x1", "x2", "x3")

        y <- X %*% c(100, 7, -3, 5) + rnorm(input$n, 0, input$sigma)
        mydf <- data.frame(y = y, x1 = X[, 2], x2 = X[, 3], x3 = X[, 4])
        mydf
    })
    
    output$distPlot <- renderPlot({
        mydf <- makedata()

        myglm <- glmnet(mydf[, -1], mydf$y, lambda = input$lambda,
            alpha = 0) # alpha=0 => Ridge
        mylm <- lm(y ~ ., data = mydf)
        mylm_coef <- coef(mylm)

        g1 <- ggplot() +
            geom_point(data = mydf,
                aes(x = x1, y = y)) +
            geom_abline(
                aes(intercept = myglm$a0, slope = myglm$beta[1])) +
            geom_abline(
                aes(intercept = mylm_coef[1], slope = mylm_coef[2]),
                colour = "red") +
            labs(title = paste0("y vs. x1, slope = ",
                round(myglm$beta[1], 2),
                " (", round(mylm_coef[2], 2), ")"))
        g2 <- ggplot() +
            geom_point(data = mydf,
                aes(x = x2, y = y)) +
            geom_abline(
                aes(intercept = myglm$a0, slope = myglm$beta[2])) +
            geom_abline(
                aes(intercept = mylm_coef[1], slope = mylm_coef[3]),
                colour = "red") +
            labs(title = paste0("y vs. x1, slope = ",
                round(myglm$beta[2], 2),
                " (", round(mylm_coef[3], 2), ")"))
        g3 <- ggplot() +
            geom_point(data = mydf,
                aes(x = x3, y = y)) +
            geom_abline(
                aes(intercept = myglm$a0, slope = myglm$beta[3])) +
            geom_abline(
                aes(intercept = mylm_coef[1], slope = mylm_coef[4]),
                colour = "red") +
            labs(title = paste0("y vs. x1, slope = ",
                round(myglm$beta[3], 2),
                " (", round(mylm_coef[4], 2), ")"))
        g1 + g2 + g3 + plot_annotation(
            title = paste0("total absolute slope: ",
                round(sum(abs(myglm$beta)), 4))
        )
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
