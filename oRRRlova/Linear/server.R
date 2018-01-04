library(MASS)

# Нормализация обучающей выборки
normalize = function(xl) {
    cols = dim(xl)[2]
    for (i in 1:cols) {
        xl[, i] = (xl[, i] - min(xl[, i])) / (max(xl[, i]) - min(xl[, i]))
    }
    return(xl)
}

# Добавляем w0 = -1 как третью колонку
prepare = function(xl) {
    rows = dim(xl)[1]
    cols = dim(xl)[2]
    xl = cbind(xl, rep(-1, rows))
}

# Функции потерь
adaline.L = function(m) {
    (m - 1) ^ 2
}

perceptron.L = function(m) {
    max(-m, 0)
}

regression.L = function(m) {
    log2(1 + exp(-m))
}

# Привила обновления весов
adaline.w = function(w, eta, xi, yi) {
    w - eta * (sum(w * xi) - yi) * xi
}

perceptron.w = function(w, eta, xi, yi) {
    w + eta * yi * xi
}

regression.w = function(w, eta, xi, yi) {
    sigmoid = function(z) {
        1 / (1 + exp(-z))
    }

    w + eta * xi * yi * sigmoid(-sum(w * xi) * yi)
}

server = function(input, output) {
    getData = function() {
        n1 = input$n1
        n2 = input$n2

        covar1 = matrix(c(input$cov1X, 0, 0, input$cov1Y), 2, 2)
        covar2 = matrix(c(input$cov2X, 0, 0, input$cov2Y), 2, 2)
        mu1 = c(input$mu1X, input$mu1Y)
        mu2 = c(input$mu2X, input$mu2Y)
        x1 = mvrnorm(n1, mu1, covar1)
        x2 = mvrnorm(n2, mu2, covar2)

        list("x1" = x1, "x2" = x2)
    }

    drawPoints = function(xl, classes) {
        xl1 = xl[classes == -1,]
        xl2 = xl[classes == 1,]
        xl = rbind(xl1, xl2)
        colors = c(rep("red", dim(xl1)[1]), rep("blue", dim(xl2)[1]))

        plot(xl[, 1], xl[, 2], pch = 21, bg = colors, asp = 1, xlab = "X", ylab = "Y")
    }

    # Стохастический градиент
    stoh = function(xl, classes, L, newW, iterId) {
        #изначальная настройка алгоритма
        rows = dim(xl)[1]
        cols = dim(xl)[2]
        w = runif(cols, -1 / (2 * cols), 1 / (2 * cols))
        lambda = 1 / rows

        # initialize Q
        Q = 0
        for (i in 1:rows) {
            margin = sum(w * xl[i,]) * classes[i]
            Q = Q + L(margin)
        }
        Q.prev = Q

        iter = 0
        repeat {
            iter = iter + 1

            # выбрать объекты с ошибкой
            margins = rep(0, rows)
            for (i in 1:rows) {
                xi = xl[i,]
                yi = classes[i]
                margins[i] = sum(w * xi) * yi
            }
            errorIndecies = which(margins <= 0)

            #выходим, если выборки полностью разделены
            if (length(errorIndecies) == 0) {
                break;
            }

            # выбираем случайный ошибочный объект          
            i = sample(errorIndecies, 1)
            xi = xl[i,]
            yi = classes[i]

            # высчитываем ошибку
            margin = sum(w * xi) * yi
            error = L(margin)

            # обновляем веса
            eta = 1 / iter
            w = newW(w, eta, xi, yi)

            # новое Q
            Q = (1 - lambda) * Q + lambda * error

            # выходим, если Q стабилизировалось
            if (abs(Q.prev - Q) / abs(max(Q.prev, Q)) < 1e-5)
                break;

            # выходим, если слишком много итераций
            if (iter == 20000)
                break;

            Q.prev = Q
        }

        # выводим количество итераций
        output[[iterId]] = renderText({
            paste0(iter)
        })

        return(w)
    }

    drawLine = function(w, color) {
        abline(a = w[3] / w[2], b = -w[1] / w[2], lwd = 2, col = color)
    }

    output$plot = renderPlot({
        #Создаем тестовые данные
        data = getData()
        x1 = data$x1
        x2 = data$x2
        y1 = rep(-1, input$n1)
        y2 = rep(+1, input$n2)

        xl = rbind(x1, x2)
        classes = c(y1, y2)

        # Нормализация данных
        xl = normalize(xl)
        xl = prepare(xl)

        #Рисуем точки
        drawPoints(xl, classes)

        # Поиск разделяющей поверхности
        w1 = stoh(xl, classes, adaline.L, adaline.w, iterId = "adaline")
        w2 = stoh(xl, classes, perceptron.L, perceptron.w, iterId = "perseptron")
        w3 = stoh(xl, classes, regression.L, regression.w, iterId = "regression")

        # Рисуем разделяющую поверхность
        drawLine(w1, "darkgreen")
        drawLine(w2, "purple")
        drawLine(w3, "orange")
    })
}