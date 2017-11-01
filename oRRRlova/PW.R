#вспомогательные функции
source(file = "help.R", encoding = "UTF-8")

mc.PW.kernel = mc.kernel.E #использовать ЭТО ядро

#PW
mc.PW = function(distances, u, h) {
    weights = mc.PW.kernel(distances / h)
    classes = unique(names(distances))

    weightsByClass = sapply(classes, mc.sumByClass, weights)

    if (max(weightsByClass) == 0) return("") #ни одна точка не попала в окно

    return(names(which.max(weightsByClass)))
}

#LOO
mc.LOO.PW = function(points, classes, hValues) {
    n = dim(points)[1]
    loo = rep(0, length(hValues))

    for (i in 1:n) {
        u = points[i,]
        sample = points[-i,]
        distances = mc.distances(sample, u)
        names(distances) = classes[-i]

        for (j in 1:length(hValues)) {
            h = hValues[j]
            classified = mc.PW(distances, u, h)
            loo[j] = loo[j] + (classified != classes[i])
        }
    }

    loo = loo / n
}

#Отрисовка LOO
mc.draw.LOO.PW = function(points, classes, hValues) {
    loo = mc.LOO.PW(points, classes, hValues)

    x = hValues
    y = loo

    plot(x, y, type = "l", main = "LOO для Парзеновского окна (PW)", xlab = "h", ylab = "LOO", col.lab = "blue")

    h = hValues[which.min(loo)]
    h.loo = round(loo[which.min(loo)], 4)

    points(h, h.loo, pch = 19, col = "blue")
    label = paste("h = ", h, "\n", "LOO = ", h.loo, sep = "")
    text(h, h.loo, labels = label, pos = 3, col = "blue", family = "mono", font = 2)

    return(h)
}

#Отрисовка карты классификации
mc.draw.PW = function(points, classes, colors, h) {
    uniqueClasses = unique(classes)
    names(colors) = uniqueClasses

    x = points[, 1]
    y = points[, 2]
    xlim = plot.limits(x, 0.3)
    ylim = plot.limits(y, 0.3)
    plot(points, bg = colors[classes], pch = 21, asp = 1, xlim = xlim, ylim = ylim, main = "Карта классификации PW", col.lab = "blue") #Рисуем известные точки

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
            classified = mc.PW(distances, u, h)

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

    par(mfrow = c(1, 2))
    h = mc.draw.LOO.PW(petals, petalNames, hValues = seq(0.1, 2, 0.005))
    mc.draw.PW(petals, petalNames, colors = c("red", "green3", "blue"), h = h)
}