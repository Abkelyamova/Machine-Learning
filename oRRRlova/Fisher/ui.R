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

makeCovMatrixRow = function(idIn, idOut, min, max, values) {
    fluidRow(
        column(nameWidth, h5("Ковар. матрица (разброс по X и Y)")),
        column(inputWidth,
            fluidRow(
                column(9, makeSlider(paste0(idIn, "X"), min, max, values[1])),
                column(3, textOutput(paste0(idOut, "X"))),
                column(9, makeSlider(paste0(idIn, "Y"), min, max, values[2])),
                column(3, textOutput(paste0(idOut, "Y")))
            )
        )
    )
}

makeExpectedRow = function(idIn, idOut, min, max, values) {
    fluidRow(
        column(nameWidth, h5("Мат. ожидание (отклонение)")),
        column(inputWidth,
            fluidRow(
                column(9, makeSlider(paste0(idIn, "X"), min, max, values[1])),
                column(3, textOutput(paste0(idOut, "X"))),
                column(9, makeSlider(paste0(idIn, "Y"), min, max, values[2])),
                column(3, textOutput(paste0(idOut, "Y")))
            )
        )
    )
}

ui <- fluidPage(
    titlePanel("Линейный дискриминант Фишера (ЛДФ)"),

    sidebarLayout(
    
        sidebarPanel(

            fluidRow(
                column(nameWidth),
                column(inputWidth, h2("Класс 1", style = "color: red; text-align: center"))
            ),

            makeCountRow("n1", 100, 5000, 500),
            makeExpectedRow("mu1In", "mu1Out", -10, 10, c(0, 0)),

            fluidRow(
                column(nameWidth),
                column(inputWidth, h2("Класс 2", style = "color: blue; text-align: center"))
            ),

            makeCountRow("n2", 100, 5000, 500),
            makeExpectedRow("mu2In", "mu2Out", -10, 10, c(10, 0)),

            fluidRow(
                column(nameWidth),
                column(inputWidth, h2("Общее", style = "color: darkgreen; text-align: center"))
            ),

            makeCovMatrixRow("covIn", "covOut", 1, 20, c(5, 5))
            
        ),

        mainPanel(

            plotOutput(outputId = "plot", height = "600px")

        )
    
    )
 
)