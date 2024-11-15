library(shiny)
library(viridisLite)

myseed <- 2112
param_hist <- data.frame(x = NULL, y = NULL, rmse = NULL)

ui <- fluidPage(
  titlePanel("Fitting the parameters of a linear model"),

  sidebarLayout(
    sidebarPanel(
      sliderInput(
        "b0",
        "Intercept",
        min = -10,
        max = 10,
        step = 0.05,
        value = 0
      ),
      sliderInput(
        "b1",
        "Slope",
        min = -10,
        max = 10,
        step = 0.05,
        value = 0
      ),
      actionButton("doit", "Click me for new data"),
      actionButton("doall", "Click me for random params")
    ),

    mainPanel(
      plotOutput("distPlot")
    )
  )
)

server <- function(input, output) {
  newseed <- reactive({
    input$doit
    myseed <<- myseed + 1
    param_hist <<- data.frame(x = NULL, y = NULL, rmse = NULL)
  })


  makedata <- reactive({
    set.seed(myseed)
    input$doit
    beta0 <- runif(1, -9, 9)
    beta1 <- runif(1, -9, 9)
    n <- 60
    x <- runif(n, 0, 10)
    y <- beta0 + beta1 * x + rnorm(n, 0, 4)
    data.frame(x = x, y = y)
  })

  tryparams <- reactive({
    input$doall
    if (nrow(param_hist) > 5) {
      try_vals <- data.frame(x = runif(50, -9, 9), y = runif(50, -9, 9))
      try_vals$rmse <- NA
      for (i in seq_len(nrow(try_vals))) {
        try_vals$rmse[i] <- sqrt(
          mean(
            (xy$y - try_vals$x[i] - try_vals$y[i] * xy$x)^2
          )
        )
      }
      param_hist <<- rbind(
        param_hist,
        try_vals
      )
    }
  })

  output$distPlot <- renderPlot({
    newseed()
    tryparams()
    xy <<- makedata()

    est <- input$b0 + input$b1 * xy$x
    rmse <- sqrt(mean((xy$y - est)^2))

    param_hist <<- rbind(
      param_hist,
      data.frame(x = input$b0, y = input$b1, rmse = rmse)
    )

    par(mfrow = c(1, 2))
    plot(
      xy,
      main = paste0("RMSE: ", round(rmse, 3))
    )
    abline(input$b0, input$b1, col = 2)

    if (nrow(param_hist) > 5) {
      mycols <- viridis(min(15, nrow(param_hist) - 1))
      col_map <- cut(log(param_hist$rmse), breaks = length(mycols) - 1)

      plot(
        param_hist$x, param_hist$y,
        col = mycols[col_map], pch = 19, cex = 2,
        xlab = "b0", ylab = "b1",
        main = paste0(
          "Best: b0=",
          round(param_hist$x[which.min(param_hist$rmse)], 2),
          ", b1=",
          round(param_hist$y[which.min(param_hist$rmse)], 2)
        )
      )
      points(
        param_hist$x[nrow(param_hist)],
        param_hist$y[nrow(param_hist)],
        pch = 1, cex = 2, col = 2
      )
    }
  })
}

shinyApp(ui = ui, server = server)
