source("help.R")
source(file = "kwKNN.R", encoding = "UTF-8")

#весовая функция взвешенного KNN
mc.STOLP.w = function(sortedDistances, indecies, k) {
    if (length(indecies) == 0) return(0)

    orderedDistances = 1:length(sortedDistances)
    names(orderedDistances) = names(sortedDistances)
    weights = mc.kwKNN.w(orderedDistances, k)

    weights = weights[indecies]
    if (length(weights) == 0) return(0)

    weightsByClass = sapply(unique(names(weights)), mc.sumByClass, weights)
    max(weightsByClass)[1]
}

#отступ
mc.STOLP.M = function(points, classes, u, class) {
    k = 30
    dist = mc.distances(points, u)
    names(dist) = classes
    sortedDistances = sort(dist)

    #Отсутуп относительно данного класса
    uIndecies = which(sapply(names(sortedDistances), function(v) any(v == class)))
    m1 = mc.STOLP.w(sortedDistances, uIndecies, k)

    #Отступ относительно других классов
    m2 = mc.STOLP.w(sortedDistances, - uIndecies, k)

    #Отступ
    m1 - m2
}

mc.draw.STOLP.noises = function(margins) {
    margins.negative = margins[which(margins < 0)]
    margins.negative = sort(margins.negative)
    n = length(margins.negative)
    margins.diff = margins.negative[1:(n - 1)] - margins.negative[2:n]
    noise.bound = min(margins.diff)[1]
    which(margins < noise.bound)
}

mc.STOLP = function(points, classes, mistakes) {
    n = length(classes)

    #Удаляет все шумовые объекты
    margins = rep(0, n)
    for (i in 1:n)
        margins[i] = mc.STOLP.M(points, classes, points[i,], classes[i])
    noises = mc.draw.STOLP.noises(margins)
    print(noises)
    print(points[noises,])
    n = n - length(noises)
    points = points[-noises,]
    classes = classes[-noises]

    #Выбираем лучшего представителя из каждого класса
    etalone = data.frame()
    etaloneClasses = c()
    for (class in unique(classes)) {
        indecies = which(classes == class) #индексы данного класса
        margins = sapply(indecies, function(i) mc.STOLP.M(points, classes, points[i,], class))
        i = indecies[which.max(margins)]
        print(margins)

        #Добавим точку к эталонным
        etalone = rbind(etalone, points[i,])
        etaloneClasses = c(etaloneClasses, class)
        points = points[-i,]
        classes = classes[-i]
        n = n - 1
    }
    names(etalone) = names(points)

    #Пока ошибка не будет маленькой
    while (n > 0) {
        margins = c()
        margins.i = c()
        for (i in 1:n) {
            m = mc.STOLP.M(etalone, etaloneClasses, points[i,], classes[i])
            if (m <= 0) {
                margins = c(margins, m)
                margins.i = c(margins.i, i)
            }
        }

        print(margins)

        if (length(margins) <= mistakes) break #Нужная точность классификации получена

        #Добавим к эталонным
        i = margins.i[which.min(margins)]
        etalone = rbind(etalone, points[i,])
        etaloneClasses = c(etaloneClasses, classes[i])
        points = points[-i,]
        classes = classes[-i]
        n = n - 1
    }

    #Вернем эталонные
    list("etalone" = etalone, "etaloneClasses" = etaloneClasses)
}

#Рисуем
mc.draw.STOLP = function(points, classes, etalone, etaloneClasses, colors) {
    uniqueClasses = unique(etaloneClasses)
    names(colors) = uniqueClasses

    plot(points, col = colors[classes], pch = 21, asp = 1, main = "STOLP для взвешанного KNN")
    points(etalone, bg = colors[etaloneClasses], pch = 21, col.lab = "blue")
}

mc.draw.STOLP.M = function(points, classes) {
    #посчитаем отступ для каждого объекта
    n = length(classes)
    margins = rep(0, n)
    for (i in 1:n)
        margins[i] = mc.STOLP.M(points, classes, points[i,], classes[i])
    margins = sort(margins)

    #Нарисуем график
    colors = colorRampPalette(c("darkred", "red", "yellow", "green", "darkgreen"))

    plot.polygonGradient(1:n, margins, colors)
    lines(1:n, margins, lwd = 3, col = "blue")
    lines(c(1, n), c(0, 0), col = "grey", lwd = 2)
    title(main = "Отступы для взвешенного KNN при k = 30", ylab = "Отступ (Мargin)", xlab = "Выборка", col.lab = "blue")

    ox = seq(0, 150, 5)
    axis(side = 1, at = ox)
    sapply(ox, function(x) abline(v = x, col = "grey", lty = 3))

    #Отметим выбросы
    noise.index = mc.draw.STOLP.noise(margins)
    noise.len = length(noise.index)
    points(noise.index, margins[noise.index], pch = 21, col = "darkred", bg = "darkred")
}

#Тестируем
test = function() {
    points = iris[, 3:4]
    classes = iris[, 5]
    classes = as.array(levels(classes))[classes] #преобразуем в массив

    res = mc.STOLP(points, classes, 3)

    #draw STOLP
    par(mfrow = c(1, 2), xpd = NA)
    mc.draw.STOLP(points, classes, res$etalone, res$etaloneClasses, c("red", "green3", "blue"))
    mc.draw.kwKNN(res$etalone, res$etaloneClasses, c("red", "green3", "blue"), k = 30)
}

testM = function() {
    points = iris[, 3:4]
    classes = iris[, 5]
    classes = as.array(levels(classes))[classes] #преобразуем в массив

    par(mfrow = c(1, 1), xpd = F)
    mc.draw.STOLP.M(points, classes)
}