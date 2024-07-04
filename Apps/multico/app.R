library(shiny)
library(rgl)
library(pracma)
n <- 200

x1 <- runif(n, 0, 1)
x2 <- x1 + runif(n, -0.25, 0.25)
y <- x1 + x2 + rnorm(n, 0, 1 / 4)
# input <- list(theta = 145)
two_preds <- predict(lm(y ~ x1 + x2),
    newdata = data.frame(x1 = c(0, 1), x2 = c(0, 1)))
p1 <- c(0, 0, two_preds[1])
p2 <- c(1, 1, two_preds[2])

ui <- fluidPage(
    sidebarPanel(
        sliderInput("theta", "Angle of Plane", min = 0, max = 360, value = 0)
    ),
    mainPanel(rglwidgetOutput("plot"), tableOutput("table"))
)

server <- function(input, output) {
    output$plot <- renderRglwidget({
        plot3d(x1, x2, y, bty = "g", colkey = FALSE,
            xlab = "x1", ylab = "x2", zlab = "y",
            xlim = c(-0.25, 1.25), ylim = c(-0.5, 1.5))
        p3 <- c(
            0, 
            0.25 * sin(input$theta * pi / 180), 
            0.25 * cos(input$theta * pi / 180)
        )

        normal <- cross(p2 - p1, p3 - p1)

        planes3d(a = normal[1], b = normal[2], c = normal[3],
            d = -sum(normal * p1), col = "lightblue", alpha = 0.25)
        rglwidget()
    })

    output$table <- renderTable({

        p3 <- c(
            0, 
            0.25 * sin(input$theta * pi / 180), 
            0.25 * cos(input$theta * pi / 180)
        )

        normal <- cross(p2 - p1, p3 - p1)
        a <- normal[1]
        b <- normal[2]
        c <- normal[3]
        d <- -sum(normal * p1)

        data.frame(beta0 = d / c, beta1 = a / c, beta2 = b / c)
    })
}

shinyApp(ui = ui, server = server)
