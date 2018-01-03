nameWidth = 2
inputWidth = 10

makeSlider = function(id, min, max, value, step = 1, label = NULL) {
    sliderInput(
        inputId = id,
        label = label,
        min = min,
        max = max,
        value = value,
        step = step
    )
}

makeCovMatrixRow = function(id, min, max, values) {
    fluidRow(
        column(nameWidth, h5("Ковар. матрица (разброс)")),
        column(inputWidth,
            fluidRow(
                column(6, makeSlider(paste0(id, "11"), min, max, values[1])),
                column(6, makeSlider(paste0(id, "12"), min, max, values[2])),
                column(6, NULL),
                column(6, makeSlider(paste0(id, "22"), min, max, values[4]))
            )
        )
    )
}

makeOutputRow = function(id) {
    fluidRow(
        column(nameWidth, NULL),
        column(inputWidth, textOutput(id), style = "color: red; text-align: center")
    )
}

makeByRow = function(byLimit) {
    fluidRow(
        column(nameWidth, "Шаг плотности"),
        column(inputWidth,
            fluidRow(
                column(12, makeSlider("by", byLimit[1], byLimit[2], byLimit[3], byLimit[4]))
            )
        )
    )
}

ui <- fluidPage(
    titlePanel("Линии уровня"),

    sidebarLayout(
    
        sidebarPanel(

            makeCovMatrixRow("cov", 0, 10, c(1, 0, 0, 1)),
            makeOutputRow("covMessage"),
            makeByRow(c(0.001, 0.01, 0.005, 0.001))

        ),

        mainPanel(

            plotOutput(outputId = "plot", height = "600px")

        )
    
    )
 
)