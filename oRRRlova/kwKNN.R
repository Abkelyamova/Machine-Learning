#вспомогательные функции
source("help.R")

#kwKNN
mc.kwKNN.w = function(i, k) +(i <= k) * (k + 1 - i) / k

mc.kwKNN = function(sortedDistances, k) {
    orderedDistances = 1:length(sortedDistances)
    names(orderedDistances) = names(sortedDistances)

    weights = mc.kwKNN.w(orderedDistances, k)
    weightsByClass = sapply(unique(names(weights)), mc.sumByClass, weights)

    bestClass = names(which.max(weightsByClass))
}

#LOO
mc.LOO.kwKNN = function(points, classes) {
    n = dim(points)[1]
    loo = rep(0, n-1) #n-1, потому что один элемент всегда будет отсутствовать в выборке

    for (i in 1:n) {
        u = points[i,]
        sample = points[-i,]

        distances = mc.distances(sample, u)
        names(distances) = classes[-i]
        sortedDistances = sort(distances)

        for (k in 1:(n-1)) {
            classified = mc.kwKNN(sortedDistances, k)
            loo[k] = loo[k] + (classified != classes[i])
        }
    }

    loo = loo / n
}

#Отрисовка LOO
mc.draw.LOO.kwKNN = function(points, classes) {
    loo = mc.LOO.kwKNN(points, classes)

    x = 1:length(loo)
    y = loo

    plot(x, y, type = "l", main = "LOO для взвешенного KNN", xlab = "K", ylab = "LOO", col.lab = "blue")

    k = which.min(loo)
    k.loo = round(loo[k], 4)

    points(k, k.loo, pch = 19, col = "blue")
    label = paste("K = ", k, "\n", "LOO = ", k.loo, sep = "")
    text(k, k.loo, labels = label, pos = 3, col = "blue", family = "mono", font = 2, xpd = T)

    return(k)
}

#Отрисовка карты классификации
mc.draw.kwKNN = function(points, classes, colors, k) {
    uniqueClasses = unique(classes)
    names(colors) = uniqueClasses

    x = points[, 1]
    y = points[, 2]
    xlim = plot.limits(x, 0.3)
    ylim = plot.limits(y, 0.3)
    plot(points, bg = colors[classes], pch = 21, asp = 1, xlim = xlim, ylim = ylim, main = "Карта классификации взвешанного KNN", col.lab = "blue") #Рисуем известные точки

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
            classified = mc.kwKNN(sort(distances), k)

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
    k = mc.draw.LOO.kwKNN(petals, petalNames)
    mc.draw.kwKNN(petals, petalNames, colors = c("red", "green3", "blue"), k = k)
}