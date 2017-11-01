mc.dist = function(p1, p2) sqrt(sum((p1 - p2) ^ 2)) #Евклидово расстояние
mc.distances = function(points, u) apply(points, 1, mc.dist, u) #Расстояние от всех points до точки u
mc.sumByClass = function(class, arr) sum(arr[names(arr) == class]) #Суммирует значения каждого класса
mc.contains = function(points, u) any(apply(points, 1, function(v) all(v == u)))

plot.limits = function(arr, deviation = 0) c(min(arr) - deviation, max(arr) + deviation) #минимальное и максимальное значения с отклонением

#Ядра
mc.kernel.R = function(r) 0.5 * (abs(r) <= 1) #прямоугольное
mc.kernel.T = function(r)(1 - abs(r)) * (abs(r) <= 1) #треугольное
mc.kernel.Q = function(r)(15 / 16) * (1 - r ^ 2) ^ 2 * (abs(r) <= 1) #квартическое
mc.kernel.E = function(r)(3 / 4) * (1 - r ^ 2) * (abs(r) <= 1) #епанечниково
