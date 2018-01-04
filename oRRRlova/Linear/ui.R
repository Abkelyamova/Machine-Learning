nameWidth = 3
inputWidth = 9

makeSlider = function(id, min, max, value) {
    sliderInput(
        inputId = id,
        label = NULL,
        min = min,
        max = max,
        value = value
    )
}

makeCountRow = function(id, min, max, value) {
    fluidRow(
        column(nameWidth, h5("Количество")),
        column(inputWidth, makeSlider(id, min, max, value))
    )
}

makeCovMatrixRow = function(id, min, max, values) {
    fluidRow(
        column(nameWidth, h5("Ковар. матрица (разброс по X и Y)")),
        column(inputWidth,
            fluidRow(
                column(6, makeSlider(paste0(id, "X"), min, max, values[1])),
                column(6, makeSlider(paste0(id, "Y"), min, max, values[2]))
            )
        )
    )
}

makeExpectedRow = function(id, min, max, values) {
    fluidRow(
        column(nameWidth, h5("Мат. ожидание (отклонение)")),
        column(inputWidth,
            fluidRow(
                column(6, makeSlider(paste0(id, "X"), min, max, values[1])),
                column(6, makeSlider(paste0(id, "Y"), min, max, values[2]))
            )
        )
    )
}

makeColoredText = function(text, color) {
    h5(text, style = paste0("text-align: center; color: ", color))
}

makeColoredOutput = function(id, color) {
    span(textOutput(id), style = paste0("text-align: center; color: ", color))
}

ui <- fluidPage(
    titlePanel("Линейные алгоритмы"),

    sidebarLayout(
    
        sidebarPanel(

            fluidRow(
                column(nameWidth),
                column(inputWidth, h2("Класс 1", style = "color: red; text-align: center; margin-top: 0"))
            ),

            makeCountRow("n1", 10, 200, 50),
            makeCovMatrixRow("cov1", 1, 20, c(5, 5)),
            makeExpectedRow("mu1", -10, 10, c(-5, 0)),

            fluidRow(
                column(nameWidth),
                column(inputWidth, h2("Класс 2", style = "color: blue; text-align: center; margin-top: 0"))
            ),

            makeCountRow("n2", 10, 200, 50),
            makeCovMatrixRow("cov2", 1, 20, c(5, 5)),
            makeExpectedRow("mu2", -10, 10, c(10, 0)),

            fluidRow(
                column(nameWidth, h5("Количество итераций")),
                column(inputWidth,
                    fluidRow(
                        column(4, makeColoredText("Adaline", "darkgreen")),
                        column(4, makeColoredText("Персептрон", "purple")),
                        column(4, makeColoredText("Регрессия", "orange")),
                        column(4, makeColoredOutput("adaline", "darkgreen")),
                        column(4, makeColoredOutput("perseptron", "purple")),
                        column(4, makeColoredOutput("regression", "orange"))
                    )
                )
            )

        ),

        mainPanel(

            plotOutput(outputId = "plot", height = "600px")

        )
    
    )
 
)