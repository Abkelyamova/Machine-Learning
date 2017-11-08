#вспомогательные функции
#install.packages("plotrix")
require("plotrix") #рисование окружностей
source("help.R")

#PF
mc.PF.kernel = mc.kernel.Q #использовать ЭТО ядро

mc.PF = function(distances, potentials, h) {
    weights = potentials * mc.PF.kernel(distances / h)
    classes = unique(names(distances))

    weightsByClass = sapply(classes, mc.sumByClass, weights)

    if (max(weightsByClass) == 0) return("") #ни одна точка не попала в окно

    return(names(which.max(weightsByClass)))
}

mc.PF.potentials = function(points, classes, h, maxMistakes) {
    n = dim(points)[1]
    potentials = rep(0, n)

    mistakes = maxMistakes + 1
    while (mistakes > maxMistakes) {
        #Подбираем потенциалы
        while (T) {
            #пока потенциалы не обновятся
            i = sample(1:n, 1) #случайный х
            u = points[i,]
            distances = mc.distances(points, u)
            names(distances) = classes

            if (mc.PF(distances, potentials, h) != classes[i]) {
                potentials[i] = potentials[i] + 1
                break
            }
        }

        #Считаем ошибку
        mistakes = 0
        for (i in 1:n) {
            u = points[i,]
            distances = mc.distances(points, u)
            names(distances) = classes

            if (mc.PF(distances, potentials, h) != classes[i])
                mistakes = mistakes + 1
            }

        print(mistakes)
        print(potentials)
    }

    return(potentials)
}

#Отрисовка потенциалов
mc.draw.PF.potentials = function(points, classes, potentials, h, colors) {
    uniqueClasses = unique(classes)
    names(colors) = uniqueClasses

    x = points[, 1]
    y = points[, 2]
    xlim = plot.limits(x, 0.3)
    ylim = plot.limits(y, 0.3)
    plot(points, bg = colors[classes], pch = 21, asp = 1, xlim = xlim, ylim = ylim, main = "Распределение потенциалов", col.lab = "blue") #Рисуем известные точки

    #Рисуем потенциалы
    trans = potentials / max(potentials) #прозрачность
    n = length(potentials)
    for (i in 1:n) {
        if (trans[i] != 0) {
            x = points[i, 1]
            y = points[i, 2]
            color = adjustcolor(colors[classes[i]], trans[i] / 1.5)

            draw.circle(x, y, h[i], 40, border = color, col = color)
        }
    }

    points(points, bg = colors[classes], pch = 21) #Рисуем ирисы сверху
    legend("topright", legend = uniqueClasses, pch = 21, pt.bg = colors[uniqueClasses], xpd = T) #добавим легенду для большей ясности
}

#Отрисовка карты классификации
mc.draw.PF = function(points, classes, potentials, h, colors) {
    uniqueClasses = unique(classes)
    names(colors) = uniqueClasses

    x = points[, 1]
    y = points[, 2]
    xlim = plot.limits(x, 0.3)
    ylim = plot.limits(y, 0.3)
    plot(points, bg = colors[classes], pch = 21, asp = 1, xlim = xlim, ylim = ylim, main = "Карта классификации PF", col.lab = "blue") #Рисуем известные точки

    #Классифицируем точки
    step = 0.1
    ox = seq(xlim[1], xlim[2], step)
    oy = seq(ylim[1], ylim[2], step)

    for (x in ox) {
        for (y in oy) {
            x = round(x, 1) #избегаем случаев 0.1 + 0.2 = 0.3000000004
            y = round(y, 1) #избегаем случаев 0.1 + 0.2 = 0.3000000004
            u = c(x, y)

            if (mc.contains(points, u)) next #не классифицировать известные точки

            distances = mc.distances(points, u)
            names(distances) = classes
            classified = mc.PF(distances, potentials, h)

            #рисуем новую классифицированную точку
            points(u[1], u[2], col = colors[classified], pch = 21) #u
        }
    }

    legend("topright", legend = uniqueClasses, pch = 21, pt.bg = colors[uniqueClasses], xpd = T) #добавим легенду для большей ясности
}

#тестируем программу
test = function() {
    petals = iris[, 3:4]
    petalNames = iris[, 5]
    n = dim(petals)[1]
    h = c(rep(1, 50), rep(0.5, 100))

    par(mfrow = c(1, 2))
    potentials = mc.PF.potentials(petals, petalNames, h, 5)
    colors = c("red", "green3", "blue")

    mc.draw.PF.potentials(petals, petalNames, potentials, h, colors)
    mc.draw.PF(petals, petalNames, potentials, h, colors)
}