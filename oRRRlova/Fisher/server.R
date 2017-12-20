library(MASS)

# Восстановление центра нормального распределения
estimateMu = function(points) {
    rows = dim(points)[1]
    cols = dim(points)[2]
    mu = matrix(NA, 1, cols)
    for (col in 1:cols) {
        mu[1, col] = mean(points[, col])
    }
    return(mu)
}

# Восстановление ковариационной матрицы нормального распределения
estimateCovarianceMatrix = function(points, mu) {
    rows = dim(points)[1]
    cols = dim(points)[2]
    covar = matrix(0, cols, cols)
    for (i in 1:rows) {
        covar = covar + (t(points[i,] - mu) %*% (points[i,] - mu)) / (rows - 1)
    }
    return(covar)
}

# Оценка ковариационной матрицы для ЛДФ
estimateFisherCovarianceMatrix = function(points1, mu1, points2, mu2) {
    rows1 = dim(points1)[1]
    rows2 = dim(points2)[1]
    rows = rows1 + rows2
    cols = dim(points1)[2]
    sigma = matrix(0, cols, cols)

    for (i in 1:rows1)
        sigma = sigma + (t(points1[i,] - mu1) %*% (points1[i,] - mu1))

    for (i in 1:rows2)
        sigma = sigma + (t(points2[i,] - mu2) %*% (points2[i,] - mu2))

    return(sigma / (rows + 2))
}

server = function(input, output) {
    generateData = function() {
        n1 = input$n1
        n2 = input$n2

        covar = matrix(c(input$covInX, 0, 0, input$covInY), 2, 2)
        mu1 = c(input$mu1InX, input$mu1InY)
        mu2 = c(input$mu2InX, input$mu2InY)
        xy1 = mvrnorm(n1, mu1, covar)
        xy2 = mvrnorm(n2, mu2, covar)

        list("xy1" = xy1, "xy2" = xy2)
    }

    drawPoints = function(xy1, xy2) {
        x = rbind(cbind(xy1, 1), cbind(xy2, 2))
        colors = c("red", "blue")
        plot(x[, 1], x[, 2], pch = 21, bg = colors[x[, 3]], asp = 1, xlab = "X", ylab = "Y")
    }

    renderNumber = function(number) {
        number = round(number, 4)
        renderText(number)
    }

    estimateCovarianceMatrix = function(xy1, mu1, xy2, mu2) {
        rows1 = dim(xy1)[1]
        rows2 = dim(xy2)[1]
        rows = rows1 + rows2
        cols = dim(xy1)[2]
        sigma = matrix(0, cols, cols)

        for (i in 1:rows1)
            sigma = sigma + (t(xy1[i,] - mu1) %*% (xy1[i,] - mu1))

        for (i in 1:rows2)
            sigma = sigma + (t(xy2[i,] - mu2) %*% (xy2[i,] - mu2))

        return(sigma / (rows + 2))
    }

    getLDFCoeffs = function(covar, mu1, mu2) {
        invCovar <- solve(covar)
        alpha <- invCovar %*% t(mu1 - mu2)
        beta = (mu1 %*% invCovar %*% t(mu1) - mu2 %*% invCovar %*% t(mu2)) / 2
        list("alpha" = alpha, "beta" = beta)
    }

    output$plot = renderPlot({
        #Создаем тестовые данные
        data = generateData()
        xy1 = data$xy1
        xy2 = data$xy2

        #Рисуем точки
        drawPoints(xy1, xy2)

        # Поиск ковариационной матрицы
        mu1 = estimateMu(xy1)
        mu2 = estimateMu(xy2)
        covar = estimateCovarianceMatrix(xy1, mu1, xy2, mu2)

        # Выводим параметры
        output$mu1OutX = renderNumber(mu1[1])
        output$mu1OutY = renderNumber(mu1[2])
        output$mu2OutX = renderNumber(mu2[1])
        output$mu2OutY = renderNumber(mu2[2])
        output$covOutX = renderNumber(covar[1, 1])
        output$covOutY = renderNumber(covar[2, 2])

        # Рисуем разделяющую поверхность
        data = getLDFCoeffs(covar, mu1, mu2)
        alpha = data$alpha
        beta = data$beta
        abline(beta / alpha[2, 1], - alpha[1, 1] / alpha[2, 1], col = "darkgreen", lwd = 3)
    })
}