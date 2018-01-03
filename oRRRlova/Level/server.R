norma = function(x, y, mu, sigma) {
    x = matrix(c(x, y), 1, 2)

    k = 1 / sqrt((2 * pi) ^ 2 * det(sigma))
    e = exp(-0.5 * (x - mu) %*% solve(sigma) %*% t(x - mu))
    k * e
}

server = function(input, output) {
    output$plot = renderPlot({
        mu = matrix(c(0, 0), 1, 2)
        sigma = matrix(c(input$cov11, input$cov12, input$cov12, input$cov22), 2, 2)

        if (det(sigma) <= 0) {
            output$covMessage = renderText({
                "Определитель < 0"
            })
            return()
        }
        output$covMessage = renderText({
            ""
        })

        # Рисуем дискриминантую функцию
        minX = -sigma[1, 1] - 2
        maxX = sigma[1, 1] + 2
        minY = -sigma[2, 2] - 2
        maxY = sigma[2, 2] + 2

        x = seq(minX, maxX, len = 100)
        y = seq(minY, maxY, len = 100)
        z = outer(x, y, function(x, y) {
            sapply(1:length(x), function(i) norma(x[i], y[i], mu, sigma))
        })
        
        add = F
        for (level in seq(from = input$by, to = 0.2, by = input$by)) {
            contour(x, y, z, levels = level, drawlabels = T, lwd = 1, col = "black", add = add, asp = 1)
            add = T
        }
    })
}