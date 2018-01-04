# Методы принятия решений

Этот файл содержит описание и результаты всех проделанных нами работ. Работы
подготовили:
_Орлова Татьяна и Юлия Ивлева, 401-И_

## Навигация

- [Метрические алгоритмы классификации](#Метрические-алгоритмы-классификации)
  - [K ближайших соседей (KNN)](#k-ближайших-соседей-knn)
  - [K взвешенных ближайших соседей (kwKNN)](#k-взвешенных-ближайших-соседей-kwknn)
  - [Парзеновское окно (PW)](#Парзеновское-окно-pw)
  - [Потенциальные функции (PF)](#Потенциальные-функции-pf)
  - [STOLP](#stolp)
- [Байесовские классификаторы](#Байесовские-классификаторы)
  - [Нормальный дискриминантный анализ](#Нормальный-дискриминантный-анализ)
  - [Подстановочный алгоритм (Plug-in)](#Подстановочный-алгоритм-plug-in)
  - [Линейный дискриминант Фишера (ЛДФ)](#Линейный-дискриминант-Фишера-ЛДФ)
- [Линейные классификаторы](#Линейные-классификаторы)
  - [Метод стохастического градиента](#Метод-стохастического-градиента)
  - [Адаптивный линейный элемент (adaline)](#Адаптивный-линейный-элемент-adaline)
  - [Персептрон Розенблатта](#Персептрон-розенблатта)
  - [Логистическая регрессия](#Логистическая-регрессия)
  - [Вывод по линейным классификаторам](#Вывод-по-линейным-классификаторам)

## Метрические алгоритмы классификации

__Гипотеза компактности__ утверждает, что схожим объектам соответствуют схожие ответы.

Метрические алгоритмы классификации основаны на __гипотезе компактности__,
которая говорит о том, что <u>схожим объектам соответствуют схожие ответы</u>.

Функцию, определяющую "схожесть" объектов называют __мерой близости__.
Эта функция определяется следующим образом:

![](http://latex.codecogs.com/svg.latex?%5Clarge%20%5Crho%3A%20%28X%20%5Ctimes%20X%29%20%5Crightarrow%20%5Cmathbb%7BR%7D)
(функция расстояния)

_Метрические алгоритмы классификации_ основаны на анализе сходства объектов с помощью
ранее описанной функции _расстояния_. Собственно, чем расстояние меньше, чем больше
объекты похожи друг на друга.

<u>Примечание</u>. Чтобы запустить алгоритм классификации на тестовой выборке необходимо
вызвать функцию `test()`.

### K ближайших соседей (KNN)

Для оценки близости объекта _u_ к классу _y_ алгоритм использует следующую
функцию:
![](http://latex.codecogs.com/svg.latex?%5Clarge%20W%28i%2C%20u%29%20%3D%20%5Bi%20%5Cleq%20k%5D)
, где _i_ обозначает порядок соседа по расстоянию к точке _u_.

Программно, функция веса реализуется следующим образом:
```
mc.KNN.w = function(i, k) +(i <= k)
```

Другими словами, алгоритм выбирает _k_ ближайших соседей и возвращает
тот класс, который среди выбранных встречается большее количество раз.

Программно алгоритм реализуется следующим образом:
```
mc.KNN = function(sortedDistances, k) {
    orderedDistances = 1:length(sortedDistances)
    names(orderedDistances) = names(sortedDistances)

    weights = mc.KNN.w(orderedDistances, k)
    weightsByClass = sapply(unique(names(weights)), mc.sumByClass, weights)

    bestClass = names(which.max(weightsByClass))
}
```
где `sortedDistances` – массив расстояний от неизвестной `u` до всех точек обучающей
выборки. `names(sortedDistances)` – наименование классов для каждой точки выборки.

Реализация алгоритма доступна по
[ссылке](oRRRlova/KNN.R)

#### Оценка алгоритма

Протестируем алгоритм на выборке ирисов Фишера:

![LOO для KNN...](IMG/KNN.png)

Алгоритм дает маленькую погрешность (5 ошибок) при небольшом _k_, однако при росте _k_, начиная
 с _k > 95_, ошибка
стремительно растет. Это объясняется тем, что функция оценки близости
![](http://latex.codecogs.com/svg.latex?%5Clarge%20W%28i%2C%20u%29%20%3D%20%5Bi%20%5Cleq%20k%5D)
никак не учитывает порядок элементов, а учитывает лишь их наличие. Из-за
этого те объекты, что находятся очень далеко от точки _u_ влияют на
классификацию с такой же силой, что и объекты, находящиеся в
непосредственной близости.

__Плюсы:__
- прост в реализации
- неплохие результаты при правильно подобраном _k_

__Минусы:__
- необходимо хранить всю выборку целиком
- классификация точки занимает
![](http://latex.codecogs.com/svg.latex?%5Clarge%20O%28n%20%5Clog%20n%29)
, так как требует сортировку точек по расстоянию
- бедный набор параметров
- примитивная оценка близости
- в случае одинаковых весов классов алгоритм выбирает любой
- не все точки с одинаковым расстоянием будут учитаны

### K взвешенных ближайших соседей (kwKNN)

Для оценки близости объекта _u_ к классу _y_ алгоритм использует следующую
функцию:
![](http://latex.codecogs.com/svg.latex?%5Clarge%20W%28i%2C%20u%29%20%3D%20%5Bi%20%5Cleq%20k%5D%20w%28i%29)
, где _i_ обозначает порядок соседа по расстоянию к точке _u_, а 
![](http://latex.codecogs.com/svg.latex?%5Clarge%20w%28i%29) — строго
убывающая функция веса. Последней алгоритм __взвешенного KNN__ отличается
от __KNN__.

Мы же будем применять следующую функцию веса:
![](http://latex.codecogs.com/svg.latex?%5Clarge%20w%28i%29%20%3D%20%5Cfrac%7Bk%20&plus;%201%20-%20i%7D%7Bk%7D)

Программно функция веса реализуется следующим образом:
```
mc.kwKNN.w = function(i, k) +(i <= k) * (k + 1 - i) / k
```

Сам же алгоритм ничем, кроме функции веса, от __KNN__ не отличается.
Подставив `mc.kwKNN.w` вместо `mc.KNN.w` получим необходимый алгоритм.

Реализация алгоритма доступна по
[ссылке](oRRRlova/kwKNN.R)

#### Оценка алгоритма

Протестируем алгоритм на выборке ирисов Фишера:

![LOO для взвешанного KNN...](IMG/kwKNN.png)

Алгоритм демонстрирует маленькую погрешность при любом _k_, отклонение максимальной ошибки
к минимальной всего лишь в двух объектах.

Алгоритм __взвешенных k соседей__ качественно отличается от обычного
__KNN__ тем, что <u>учитывает порядок объектов</u> при классификации. Тем
самым, ближние к точке _u_ объекты будут влиять на нее гораздо сильнее, чем
дальние. Однако из-за этой особенности, при большом _k_ дальние точки
быстро обесцениваются, поэтому функцию
![](http://latex.codecogs.com/svg.latex?%5Clarge%20w%28i%29)
следует выбирать осторожно.

__Плюсы:__
- прост в реализации
- неплохие результаты при любом _k_

__Минусы:__
- необходимо хранить всю выборку целиком
- классификация точки занимает
![](http://latex.codecogs.com/svg.latex?%5Clarge%20O%28n%20%5Clog%20n%29)
, так как требует сортировку точек по расстоянию
- бедный набор параметров
- в случае одинаковых весов классов алгоритм выбирает любой
(однако стоит заметить, что эти случаи будут встречаться крайне редко)
- не все точки с одинаковым расстоянием будут учитаны

### Парзеновское окно (PW)

Для оценки близости объекта _u_ к классу _y_ алгоритм использует следующую
функцию:

![](http://latex.codecogs.com/svg.latex?%5Clarge%20W%28i%2C%20u%29%20%3D%20K%28%5Cfrac%7B%5Crho%28u%2C%20x%5Ei_u%29%7D%7Bh%7D%29)
, где 
![](http://latex.codecogs.com/svg.latex?%5Clarge%20K%28z%29) — функция ядра.

Чаще всего применяются 5 типов ядер:
- Прямоугольное ![](http://latex.codecogs.com/svg.latex?%5Clarge%20R%28z%29%20%3D%20%5Cfrac%7B1%7D%7B2%7D%20%5B%7Cz%7C%20%5Cleq%201%5D)
- Треугольное ![](http://latex.codecogs.com/svg.latex?%5Clarge%20T%28z%29%20%3D%20%281%20-%20%7Cz%7C%29%20%5Ccdot%20%5B%7Cz%7C%20%5Cleq%201%5D)
- Квартическое ![](http://latex.codecogs.com/svg.latex?%5Clarge%20Q%28z%29%20%3D%20%5Cfrac%7B15%7D%7B16%7D%20%281%20-%20z%5E2%29%5E2%20%5Ccdot%20%5B%7Cz%7C%20%5Cleq%201%5D)
- Епанечниково ![](http://latex.codecogs.com/svg.latex?%5Clarge%20E%28z%29%20%3D%20%5Cfrac%7B3%7D%7B4%7D%20%281%20-%20z%5E2%29%20%5Ccdot%20%5B%7Cz%7C%20%5Cleq%201%5D)
- Гауссовское (нормальное распределение)

Программная реализация ядер:
```
mc.kernel.R = function(r) 0.5 * (abs(r) <= 1) #прямоугольное
mc.kernel.T = function(r)(1 - abs(r)) * (abs(r) <= 1) #треугольное
mc.kernel.Q = function(r)(15 / 16) * (1 - r ^ 2) ^ 2 * (abs(r) <= 1) #квартическое
mc.kernel.E = function(r)(3 / 4) * (1 - r ^ 2) * (abs(r) <= 1) #епанечниково
mc.kernel.G = function(r) dnorm(r) #гауссовское
```

В программе ядра будут применяться поочередно.
Однако на разницу в качестве классификации они влияют слабо.
Выделяется лишь _гауссовское ядро_ (см. ниже).

__Простыми словами:__ алгоритм для классифицируемой точки _u_ строит
окружность, радиусом _h_. Все точки, не попавшие в эту окружность,
отсеиваются (кроме гауссовского). Для остальных, вычисляется вес,
суммируется, и класс с наибольшим весом считается победителем.

Программная реализация алгоритма:
```
mc.PW = function(distances, u, h) {
    weights = mc.PW.kernel(distances / h)
    classes = unique(names(distances))

    weightsByClass = sapply(classes, mc.sumByClass, weights)

    if (max(weightsByClass) == 0) return("") #ни одна точка не попала в окно

    return(names(which.max(weightsByClass)))
}
```
где `distances` – расстояние от точки `u` до всех точек выборки,
`names(distances)` – наименование классов точек выборки.

Реализация алгоритма доступна по
[ссылке](oRRRlova/PW.R)

#### Оценка алгоритма

Протестируем алгоритм на выборке ирисов Фишера:

_Прямоугольное ядро:_
![LOO для PW...](IMG/PW_R.png)

_Треугольное ядро:_
![LOO для PW...](IMG/PW_T.png)

_Квартическое ядро:_
![LOO для PW...](IMG/PW_Q.png)

_Епанечково ядро:_
![LOO для PW...](IMG/PW_E.png)

Алгоритм хорошо себя показывает при _h_ лишь в небольшом диапазоне.
Этот диапазон зависит от плотности классифицируемых точек. Если _h_ сделать
слишком маленьким, количество точек, способных к классификации заметно уменьшится.
Если _h_ сделать слишком большим, он начнет учитывать при
классификации слишком дальние точки. Однако стоит отметить, что в некоторых ядрах
веса дальних точек гораздо меньше, и качество алгоритма при больших _h_ падает не так
сильно. Неудачным ядром (по мнению авторов программной реализации)
является _прямоугольное_, так как веса
всех точек внутри окна одинаковые.

Все вышеописанные ядра имеют весомый недостаток – они не способны
классифицировать точки, не попавшие ни в одно окно. Этот недостаток
устраняет гауссовское ядро.

_Гауссовское ядро:_
![LOO для PW...](IMG/PW_G.png)

Алгоритм Парзеновского окна прост в понимании и реализации (даже проще, чем
__KNN__). Он имеет удовлетворительное качество классификации,
 однако имеет и ряд недостатков, которыми __KNN__ и __взвешанный
KNN__ не обладают.

__Плюсы:__
- прост в реализации
- хорошее качество классификации при правильно подобраном _h_
- все точки с одинаковым расстоянием будут учитаны
- классификация точки занимает
![](http://latex.codecogs.com/svg.latex?%5Clarge%20O%28n%29)
, так как не требует сортировки

__Минусы:__
- необходимо хранить всю выборку целиком
- бедный набор параметров
- в случае одинаковых весов классов алгоритм выбирает любой
(однако стоит заметить, что эти случаи будут встречаться редко)
- диапазон параметра _h_ необходимо подбирать самостоятельно, учитывая
плотность расположения точек
- если ни одна точка не попала в радиус _h_, алгоритм не способен ее
классифицировать (не актуально для гауссовского ядра)

### Потенциальные функции (PF)

Для оценки близости объекта _u_ к классу _y_ алгоритм использует следующую
функцию:

![](http://latex.codecogs.com/svg.latex?W_y%28i%2C%20u%29%20%3D%20%5Cgamma_i%20%5Ccdot%20K%28%5Cfrac%7B%5Crho%28u%2C%20x_u%5Ei%29%7D%7Bh_i%7D%29%2C%20%5Cgamma_i%20%5Cgeq%200%2C%20h_i%20%3E%200)
, где 
![](http://latex.codecogs.com/svg.latex?%5Clarge%20K%28z%29) — функция ядра.

В программной реализации применяется квартическое ядро.
Алгоритм подбирает только силу потенциала
![](http://latex.codecogs.com/svg.latex?%5Cgamma_i), радиусы потенциалов
_h_ известны заранее.

__Простыми словами:__ алгоритм для каждого обучающего объекта _x_ строит
окружность, радиуса _h_ и силы воздействия (потенциала)
![](http://latex.codecogs.com/svg.latex?%5Cgamma_i).

Программная реализация функции классификации:
```
mc.PF = function(distances, potentials, h) {
    weights = potentials * mc.PF.kernel(distances / h)
    classes = unique(names(distances))

    weightsByClass = sapply(classes, mc.sumByClass, weights)

    if (max(weightsByClass) == 0) return("") #ни одна точка не попала в окно

    return(names(which.max(weightsByClass)))
}
```
где `potentials` и `h` – массивы потенциалов и радиусов для каждой точки выборки
соответственно.

Однако, прежде, чем использовать потенциалы, их необходимо подобрать. Этот процесс
также входит в обязанности _PF_.

<u>Описание</u>:
Изачально потенциалы заполняются нулями. Далее, пока количество ошибок классификации
не достигнет нужного предела, выбираем случайно точку _x_ из выборки. Если для нее
классификация выполняется неверно, увеличиваем потенциал на 1 и пересчитываем
общее количество ошибок.

Функция вызывается следующим образом:
```
mc.PF.potentials(points, classes, h, maxMistakes)
```
где `points` и `classes` – массив точек и их классов соответственно,
`h` – массив предпосчитанных радиусов каждой точки,
`maxMistakes` – максимальное количество допустимых ошибок.

Реализация алгоритма доступна по
[ссылке](oRRRlova/PF.R)

#### Оценка алгоритма

Протестируем алгоритм на выборке ирисов Фишера. .Так как целью работы является
подбор
![](http://latex.codecogs.com/svg.latex?%5Cgamma_i), а не _h_, то
выбирем их любым образом (пускай для красных точек _h = 1_, так как они
отделены от остальной обучающей выборки; остальным присвоим _h = 0.25_).

Алгоритм подобрал оптимальные параметры (4 ошибки) за 16 проходов:

![Потенциалы PF...](IMG/PF.png)

Алгоритм заметно сложнее в понимании и реализации, чем предшествующие
алгоритмы. К тому же подбор силы потенциалов занимает долгое время. К
сожалению, это время оценить невозможно, так как алгоритм является
случайным (следующий _x_ в итерации берется случайным образом). При
запуске алгоритма на одной и той же выборке, он показывает разное время
работы, разную итоговую ошибку, а также разное количество итераций.

__Плюсы:__
- результат зависит от _2n_ параметров

__Минусы:__
- необходимо хранить всю выборку целиком
- параметры _h_ необходимо подбирать самостоятельно, алгоритм
в их подборе не принимает участия
- если ни одна точка не попала в радиус _h_, алгоритм не способен ее
классифицировать (не актуально для гауссовского ядра)
- медленно сходится
- слишком грубо настраивает параметры
- неопределенное время работы (при маленьком пороге ошибки может вообще
выполняться бесконечно)

### STOLP

Выделяют несколько выдов объектов обучения:

- _Эталонные_ — типичные представители классов. Если классифицируемый
объект близок к эталону, то, скорее всего, он принадлежит тому же классу.
- _Неинформативные_ — плотно окружены
другими объектами того же класса. Если их удалить из выборки, это практически
не отразится на качестве классификации.
- _Выбросы_ — находятся в окружении объектов чужого класса. Как правило,
- их удаление только улучшает качество классификации.

Алгорим **STOLP** исключает из выборки выбросы и неинформативные
объекты, оставляя лишь нужное количество эталонных. Таким образом
улучшается качество классификации, сокращается объем данных и уменьшается
время классификации объектов. Другими словами **STOLP** — алгоритм
сжатия данных.

Он использует функцию отступа:

![](http://latex.codecogs.com/svg.latex?M%28x_i%29%20%3D%20W_%7By_i%7D%28x_i%29%20-%20%5Cunderset%7By%20%5Cin%20Y%20%5Csetminus%20y_i%7D%7Bmax%7DW_y%28x_i%29%29%29)

![](http://latex.codecogs.com/svg.latex?W_y%28x_i%29) является весовой
функцией и зависит от выбранного алгоритма классификации.

Программно функция веса реализуется следующим образом:
```
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
```
где `indecies` – индексы точек, гдя которых ее необходимо подсчитать, если
`indecies < 0` – индексы точек, которые необходимо выкинуть перед подсчетом.
Функция основана на функции веса __KNN__, поэтому передается дополнительный
параметр `k` и используется `mc.kwKNN.w`. Реализация __STOLP__ для других
алгоритмов классификации будет немного отличаться.

Программная реализация функции отступа:
```
mc.STOLP.M = function(points, classes, u, class) {
    k = 4
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
```
где `class` – класс _Xi_-й точки выборки.

Эти функции используются как вспомогательные для основного алгоритма __STOLP__.

<u>Описание</u>: 
Алгоритм удаляет все выбросы из начальной выборки.
Далее из каждого класса выбирается по этолонному объекту
(с самым большим отступом) и они записываются в массив.
Далее, пока количество ошибок на всей выборке не будет удовлетворять условию,
выбираем из оставшихся объектов точку с самым маленьким отступом и добавляем
в массив.

Алгоритм вызывается следующей функцией:
```
mc.STOLP = function(points, classes, mistakes)
```
где `mistakes` – допустимое количество ошибок.

Выбросы из выборки определяются автоматически и удаляются в начале алгоритма.
Определяются они следующим образом: алгоритм выделяет все объекты с
отрицательным отступом, сортирует и среди всех выбирает тот, с которого
начинается самый большой скачок вниз. Этот объект и определяет
верхнюю грань множества _шумовых объектов_.

Сам алгоритм громоздкий, его реализация доступна по
[ссылке](oRRRlova/STOLP.R)

#### Оценка алгоритма

Протестируем алгоритм на выборке ирисов Фишера. В качестве алгоритма
классификации рассмотрим **взвешанный KNN** и его весовую функцию:
![](http://latex.codecogs.com/svg.latex?W%28i%2C%20u%29%20%3D%20%5Bi%20%5Cleq%20k%5D%20%5Ccdot%20%5Cfrac%7Bk%20&plus;%201%20-%20i%7D%7Bk%7D).

Между соседними типами объектов не существуют четких разграничений, поэтому
границу _выбросов_ установим сами. Будем называть выбросом объект, отступ
которого `< -1`.

Как мы знаем из предыдущих работ, алгоритм **kwKNN** дает лучший результат
(6 ошибок) при практически любом `k` (возьмем, `k = 30`).

Классификация отступов выглядит следующим образом (красная точка - выброс):
![STOLP Margin](IMG/STOLP_M.png)
Выборка содержит огромное количество
_эталонных_ объектов, а это означает, что в выборке очень много "ненужных" классификации
объектов, а, следовательно, их можно удалить не испортив классификатор.

Попытаемся улучшить алгоритм с помощью **STOLP**, сократив
количество ошибок в 2 раза.

С поставленной задачей алгоритм справился за 10 проходов,
допустив всего _2 ошибки_ на эталонной выборке:

![STOLP...](IMG/STOLP.png)

Как можно заметить, выборка сократилась со 150-ти до 8-ти объектов (в 18 раз),
тем самым ускорив время классификации.

__Плюсы:__
- в разы уменьшает размер выборки
- способен улучшить качество классификации

__Минусы:__
- сложный в реализации

## Байесовские классификаторы

Байесовский подход к классификации основан на теореме, утверждающей,
что если плотности распределения каждого из классов известны,
то искомый алгоритм можно выписать в явном виде.
Более того, этот алгоритм оптимален,
то есть обладает минимальной вероятностью ошибок.

Для классифицируемого объекта вычисляются функции правдоподобия каждого
из классов, по ним вычисляются апостериорные вероятности классов.
Объект относится к тому классу, для которого __апостериорная вероятность
максимальна__.

![](https://latex.codecogs.com/svg.latex?a%28x%29%20%3D%20%5Carg%20%5Cmax_%7By%20%5Cin%20Y%7D%20%5Clambda_y%20P_y%20p_y%28x%29)

На практике _плотности распределения классов_, как правило, не известны.
Их приходится оценивать (восстанавливать) по обучающей выборке.
В результате байесовский алгоритм перестаёт быть оптимальным,
так как восстановить плотность по выборке можно только с некоторой погрешностью.

__Разделяющая поверхность__ между классами _t_ и _s_ – это геометрическое
место точек
![](https://latex.codecogs.com/svg.latex?x%20%5Cin%20X)
таких, что _максимум апостериорной вероятности_ достигается одновременно
при _y = s_ и _y = t_.

![](https://latex.codecogs.com/svg.latex?%5Clambda_t%20P_t%20p_t%28x%29%20%3D%20%5Clambda_s%20P_s%20p_s%28x%29).

Следующие алгоритмы по исходным выборкам восстанавливают _плотности распределения
классов_ и отдеяют классы друг от друга при помощи
_разделяющей поверхности_.

### Нормальный дискриминантный анализ

Это специальный случай баесовской классификации, когда предполагается, что плотности
всех классов
![](http://latex.codecogs.com/svg.latex?p_y%28x%29%2C%20y%20%5Cin%20Y)
являются многомерными нормальными. В этом случае задача решается аналитически.
Сами плотности вычисляются по формуле:

![](http://latex.codecogs.com/svg.latex?N%28x%3B%5Cmu%2C%5CSigma%29%20%3D%20%5Cfrac%7B1%7D%7B%5Csqrt%7B%282%5Cpi%29%5En%20%7C%5CSigma%7C%7D%7D%20%5Ccdot%20exp%5Cleft%28-%5Cfrac%7B1%7D%7B2%7D%28x%20-%20%5Cmu%29%5ET%20%5CSigma%5E%7B-1%7D%20%28x%20-%20%5Cmu%29%5Cright%29),
в которой

![](http://latex.codecogs.com/svg.latex?x%20%5Cin%20%5Cmathbb%7BR%7D%5En)
– объект, состоящий из *n* признаков,

![](http://latex.codecogs.com/svg.latex?%5Cmu%20%5Cin%20%5Cmathbb%7BR%7D%5En)
– математическое ожидание,

![](http://latex.codecogs.com/svg.latex?%5CSigma%20%5Cin%20%5Cmathbb%7BR%7D%5E%7Bn%20%5Ctimes%20n%7D)
– ковариационная матрица (положительно определенная, симметричная, невырожденная).

#### Геометрия нормальной плотности

1. Если признаки некореллированы, то есть
![](http://latex.codecogs.com/svg.latex?%5CSigma%20%3D%20%5Cmbox%7Bdiag%7D%28%5Csigma_1%5E2%2C...%2C%5Csigma_n%5E2%29)
, то плотности распределения имеют форму эллипсоидов, параллельных осям координат:

![](IMG/line1.png)

2. Если признаки имеют одинаковые дисперсии
![](http://latex.codecogs.com/svg.latex?%5CSigma%20%3D%20%5Csigma%5E2I_n),
линии уровня имеют форму эллипсоидов:

![](IMG/line2.png)

3. Если матрица не диагональна, то линии уровня – эллипсоиды, повернутые относительно
оси координат:

![](IMG/line3.png)

Саму программу можно посмотреть здесь: [shinyapps.io](https://orlova-tatiana.shinyapps.io/level/)

### Подстановочный алгоритм (Plug-in)

__Подстановочный алгоритм__ относится к нормальному дискриминантному анализу.

Чтобы узнать _плотности распределения классов_, алогритм восстанавливает
неизвестные параметры
![](http://latex.codecogs.com/svg.latex?%5Cmu%2C%20%5CSigma)
по следующим формулам для каждого класса _y_ :

![](http://latex.codecogs.com/svg.latex?%5Chat%7B%5Cmu%7D%20%3D%20%5Cfrac%7B1%7D%7Bl_y%7D%20%24%24%5Csum_%7Bi%20%3D%201%7D%5E%7Bl_y%7D%20x_i%24%24)

```
estimateMu = function(points) {
    rows = dim(points)[1]
    cols = dim(points)[2]
    mu = matrix(NA, 1, cols)
    for (col in 1:cols) {
        mu[1, col] = mean(points[, col])
    }
    return(mu)
}
```

![](http://latex.codecogs.com/svg.latex?%5Chat%7B%5CSigma%7D%20%3D%20%5Cfrac%7B1%7D%7Bl_y%20-%201%7D%20%24%24%5Csum_%7Bi%20%3D%201%7D%5E%7Bl_y%7D%20%28x_i%20-%20%5Chat%7B%5Cmu%7D%29%28x_i%20-%20%5Chat%7B%5Cmu%7D%29%5ET).

```
estimateCovarianceMatrix = function(points, mu) {
    rows = dim(points)[1]
    cols = dim(points)[2]
    covar = matrix(0, cols, cols)
    for (i in 1:rows) {
        covar = covar + (t(points[i,] - mu) %*% (points[i,] - mu)) / (rows - 1)
    }
    return(covar)
}
```

#### Оценка алгоритма

Протестируем алгоритм на двух сгенерированных на основе нормального распределения
выборках, посмотрим, насколько восстановленные значения отличаются от исходных,
и какие разделяющие поверхности между классами он нарисует.

Программа доступна по ссылке:
[shinyapps.io](https://orlova-tatiana.shinyapps.io/plug-in/).

Алгоритм довольно точно восстанавливает _ковариационную матрицу_ и
_мат. ожидание_ объектов выборки.

![](IMG/plug-in-1.png)

Однако при малом количестве количестве объектов, точность падает:

![](IMG/plug-in-2.png)

### Линейный дискриминант Фишера (ЛДФ)

__ЛДФ__ основан на __подстановочном алгоритме__ с предположением,
что ковариационные матрицы классов равны. Отсюда следует, что
разделяющая поверхность вырождается в прямую. Это условие в
__plug-in__ не выполнялось, так как разделяющая поверхность все равно
была квадратичной (хоть и приближенной к прямой). Отсюда следует,
что __ЛДФ__ должен иметь более высокое качество классификации при
одинаковых ковариационных матрицах.

Программно алгоритм отличается от __подстановочного__ пересчетом
_ковариационной матрицы_ и поиском _разделяющей поверхности_.

Так как матрицы равны, можем оценить их все вместе:

![](http://latex.codecogs.com/svg.latex?%5Chat%7B%5CSigma%7D%20%3D%20%5Cfrac%7B1%7D%7Bl%20-%20%7CY%7C%7D%20%5Ccdot%20%5Csum_%7Bi%3D1%7D%5E%7Bl%7D%28x_i%20-%20%5Chat%7B%5Cmu%7D_y_i%29%28x_i%20-%20%5Chat%7B%5Cmu%7D_y_i%29%5ET)

```
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
```

Разделяющая поверхность имеет вид:

![](http://latex.codecogs.com/svg.latex?%5Calpha%20x%5ET%20&plus;%20%5Cbeta%20%3D%200), где

![](https://latex.codecogs.com/svg.latex?%5Calpha%20%3D%20%5CSigma%5E%7B-1%7D%20%5Ccdot%20%28%5Cmu_y_1%20-%20%5Cmu_y_2%29%5ET),

![](https://latex.codecogs.com/svg.latex?%5Cbeta%20%3D%20%5Cfrac%7B1%7D%7B2%7D%20%5Ccdot%20%5Cmu_y_1%20%5Ccdot%20%5CSigma%5E%7B-1%7D%20%5Ccdot%20%5Cmu_y_1%5ET%20-%20%5Cfrac%7B1%7D%7B2%7D%20%5Ccdot%20%5Cmu_y_2%20%5Ccdot%20%5CSigma%5E%7B-1%7D%20%5Ccdot%20%5Cmu_y_2%5ET).

```
getLDFCoeffs = function(covar, mu1, mu2) {
    invCovar = solve(covar)
    alpha = invCovar %*% t(mu1 - mu2)
    beta = (mu1 %*% invCovar %*% t(mu1) - mu2 %*% invCovar %*% t(mu2)) / 2
    list("alpha" = alpha, "beta" = beta)
}
```

#### Оценка алгоритма

Программа доступна по ссылке:
[shinyapps.io](https://orlova-tatiana.shinyapps.io/fisher/)

Сравним результаты двух алгоритмов с одинаковыми параметрами:

![](IMG/fisher_plug-in.png)

![](IMG/fisher.png)

При одинаковых выборках __подстановочный алгоритм__ визуально близок
к __ЛДФ__ и разницу можно увидеть лишь при малом количестве объектов
выборки.

Причем, чем меньше объектов в выборке, тем очевидней превосходство
__ЛДФ__ над __подстановочным алгоритмом__ при одинаковых
ковариационных матрицах.

## Линейные классификаторы

Пусть ![](http://latex.codecogs.com/svg.latex?X%20%3D%20%5Cmathbb%7BR%7D%5En)
и ![](http://latex.codecogs.com/svg.latex?Y%20%3D%20%5C%7B-1%3B&plus;1%5C%7D).
Алгоритм

![](http://latex.codecogs.com/svg.latex?a%28x%2Cw%29%3D%20%5Ctext%7Bsign%7Df%28x%2Cw%29%3D%5Ctext%7Bsign%7D%20%28%5Clangle%20w%2Cx%20%5Crangle-w_0%29%2Cw%20%5Cin%20%5Cmathbb%7BR%7D%5En)

является __линейным алгоритмом классификации__.
Если _f_>0, то алгоритм _a_ относит _x_ к классу +1. Иначе к классу -1.

Отсюда
![](http://latex.codecogs.com/svg.latex?%5Clangle%20w%2Cx%20%5Crangle%3D0)
называется __уравнением разделяющей поверхности__.

Параметр ![](http://latex.codecogs.com/svg.latex?w_0) иногда опускают. Однако в таком
случае разделяющая поверхность (в нашем случае с 2мя признаками – прямая),
соответствующая уравнению
![](http://latex.codecogs.com/svg.latex?%5Clangle%20w%2Cx%20%5Crangle%3D0),
будет всегда проходить через начало координат. Чтобы избежать такого обобщения,
будем полагать, что среди признаков _x_ есть константа
![](http://latex.codecogs.com/svg.latex?f_j%28x%29%20%5Cequiv%20-1),
тогда роль свобоного коэффициента ![](http://latex.codecogs.com/svg.latex?w_0)
играет параметр ![](http://latex.codecogs.com/svg.latex?w_j).
Тогда разделяющая поверхность имеет вид
![](http://latex.codecogs.com/svg.latex?%5Clangle%20w%2Cx%20%5Crangle=w_j).

Величина
![](http://latex.codecogs.com/svg.latex?M_i%28w%29%3Dy_i%5Clangle%20x_i%2Cw%20%5Crangle)
называется __отступом__ объекта относительно алгоритма классификации. Если
![](http://latex.codecogs.com/svg.latex?M_i%28w%29%3C0),
алгоритм совершает на объекте
![](http://latex.codecogs.com/svg.latex?x_i)
ошибку.

![](http://latex.codecogs.com/svg.latex?%5Cmathcal%7BL%7D%28M%29)
– монотонно невозрастающая __функция потерь__, мажорирует пороговую функцию
![](http://latex.codecogs.com/svg.latex?%5BM%3C0%5D%20%5Cleq%20%5Cmathcal%7BL%7D%28M%29).
Тогда __минимизацю суммарных потерь__ можно рассматривать как функцию вида
![](http://latex.codecogs.com/svg.latex?%5Ctilde%7BQ%7D%28w%2CX%5E%5Cell%29%20%3D%20%5Csum_%7Bi%3D1%7D%5E%7B%5Cell%7D%5Cmathcal%28M_i%28w%29%29%5Crightarrow%20%5Cmin_w)

#### Метод стохастического градиента

Для минимизации
![](http://latex.codecogs.com/svg.latex?Q%28w%29)
применяется __метод градиентного спуска__.

В начале выбирается некоторое _начальное приближение вектора весов_ _w_.
Не существует единого способа инициализации весов. Хорошей практикой считается
инициализировать веса случайными малыми значениями:
![](http://latex.codecogs.com/svg.latex?w_j%3A%3D%5Ctext%7Brandom%7D%28-%5Cfrac%7B1%7D%7B2n%7D%2C&plus;%5Cfrac%7B1%7D%7B2n%7D%29)
, где _n_ – количество признаков _x_.

Далее высчитывается _текущая оценка функционала_
![](http://latex.codecogs.com/svg.latex?Q%3A%3D%5Csum_%7Bi%3D1%7D%5E%7B%5Cell%7D%5Cmathcal%7BL%7D%28%5Clangle%20w%2Cx_i%20%5Crangle%20y_i%29)

Затем запускается итерационный процесс, на каждом шаге которого вектор _w_
изменяется в сторону наиболее быстрого убывания _Q_. Это направление противоположно
вектору градиента
![](http://latex.codecogs.com/svg.latex?Q%27%28w%29). Соответственно веса меняются по
правилу:

![](http://latex.codecogs.com/svg.latex?w%3A%3Dw-%5Ceta%20Q%27%28w%29)

или

![](http://latex.codecogs.com/svg.latex?w%3A%3Dw-%5Ceta%5Csum_%7Bi%3D1%7D%5E%7B%5Cell%7D%5Cmathcal%7BL%7D%27%28%5Clangle%20w%2Cx_i%20%5Crangle%20y_i%29x_iy_i),

где
![](http://latex.codecogs.com/svg.latex?%5Ceta%3E0)
– __темп обучения__. Чтобы не проскочить локальный минимум темп обучания принято
полагать небольшим. Однако, при слишком маленьком его значении алгоритм будет
медленно сходится. Хорошей практикой считается его постепенное уменьшение по ходу
итераций. Мы же будем полагать его равным
![](http://latex.codecogs.com/svg.latex?%5Cfrac%7B1%7D%7B%5Ctext%7Biteration%7D%7D).

__Критерий останова__ основан на приблизительной оценке _Q_ методом
_экспоненциальной скользящей средней_:

![](http://latex.codecogs.com/svg.latex?Q%3D%281-%5Clambda%29Q&plus;%5Clambda%20%5Cvarepsilon_i)
,

где
![](http://latex.codecogs.com/svg.latex?%5Cvarepsilon_i%3D%5Cmathcal%7BL%7D%28%5Clangle%20w%2Cx_i%20%5Crangle%20y_i%29)
– __ошибка__ алгоритма на случайном элементе
![](http://latex.codecogs.com/svg.latex?x_i),

![](http://latex.codecogs.com/svg.latex?%5Clambda) – __параметр сглаживания__,
полагаем его равным
![](http://latex.codecogs.com/svg.latex?%5Cfrac%7B1%7D%7B%5Cell%7D).

Алгоритм может остановиться в двух случаях:

1. Он не допускает ошибки ни на каком элементе;
2. Значение _Q_ стабилизировано, то есть
![](http://latex.codecogs.com/svg.latex?%5Cfrac%7B%7CQ_%7Bprev%7D-Q%7C%7D%7B%5Cmax%28Q_%7Bprev%7D%2CQ%29%7D%20%5Cleq%201e-5).

<u>Примечание.</u> Градиентный метод чувствителен к масштабу измерения признаков.
Если норма _x_ большая, итерационный процесс может оказаться парализованным.
Чтобы этого не произошло, рекомендуется _нормализовать_ признаки:

![](http://latex.codecogs.com/svg.latex?x%5Ej%3A%3D%5Cfrac%7Bx%5Ej-x%5Ej_%5Ctext%7Bmin%7D%7D%7Bx%5Ej_%5Ctext%7Bmax%7D-x%5Ej_%5Ctext%7Bmin%7D%7D)
, где
![](http://latex.codecogs.com/svg.latex?x%5Ej)
– _j_-й признак.

В обобщенном виде алгоритм _стохастического градиента_ выглядит следующим образом:

```
stoh = function(xl, classes, L, updateRule) {
    #изначальная настройка алгоритма
    rows = dim(xl)[1]
    cols = dim(xl)[2]
    w = runif(cols, -1 / (2 * cols), 1 / (2 * cols))
    lambda = 1 / rows

    # начальное Q
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
        if (length(errorIndecies) == 0) break

        # выбираем случайный ошибочный объект          
        i = sample(errorIndecies, 1)
        xi = xl[i,]
        yi = classes[i]

        # высчитываем ошибку
        margin = sum(w * xi) * yi
        error = L(margin)

        # обновляем веса
        eta = 1 / iter
        w = updateRule(w, eta, xi, yi)

        # новое Q
        Q = (1 - lambda) * Q + lambda * error

        # выходим, если Q стабилизировалось
        if (abs(Q.prev - Q) / abs(max(Q.prev, Q)) < 1e-5) break

        # выходим, если слишком много итераций (алгоритм парализован)
        if (iter == 20000) break

        Q.prev = Q #запоминаем Q на предыдущем шаге
    }

    return(w)
}
```

Следующие алгоритмы за основу берут _стохастический градиент_, меняя только
функцию потерь
![](http://latex.codecogs.com/svg.latex?%5Cmathcal%7BL%7D)
и правило обновления весов.