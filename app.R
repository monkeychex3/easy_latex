#
# a Shiny App by Adam Coger on 10/10/2022
#

library(shiny)
library(shinydashboard)

header <-  htmltools::tagQuery(dashboardHeader(title = "This should make LaTeX writing faster", titleWidth = "75%"))

header <- header$addAttrs(style = "position: relative")$
  find(".navbar.navbar-static-top")$
  append(
    HTML("<div style=\"text-align: center; margin-top: 5px; color: white;\">",
      "by Adam Coger<br>acoger@bgsu.edu</div>")
  )$allTags()


ui <- dashboardPage(
  header = header,
  
  sidebar = dashboardSidebar(disable = TRUE),
  
  body = dashboardBody(
    fluidRow(
      column(
        width = 6,
        selectInput(inputId = "typeOfSymbol",
          label = "Main Symbol",
          choices = c("Sum", "Product", "Integral", "Matrix", "Augmented Matrix")
        )
      ),
      column(
        width = 6,
        uiOutput("simpleInput"),
        uiOutput("matrixInput")
      )
    ),
    fluidRow(
      uiOutput("simpleLatex"),
      uiOutput("matrixLatex")
    ),
    fluidRow(
      column(width = 3),
      column(
        width = 6,
        verbatimTextOutput("simpleString"),
        verbatimTextOutput("matrixString")
      ),
      column(width = 3,
        actionButton("innerClipboard", label = "Copy Inner Text"),
        br(),
        actionButton("clipboardWithDollar", label = "Copy With Dollars")
      )
    )
  )
)

server <- function(input, output, session){
  
  
  simpleSymbols <- c("Sum", "Product", "Integral")
  matrixSymbols <- c("Matrix", "Augmented Matrix")
  
  
  output$simpleInput <- renderUI({
    req(input$typeOfSymbol %in% simpleSymbols)
    
    list(
      textInput(
        inputId = "upper", label = "above symbol",
        value = "up top"
      ),
      textInput(
        inputId = "lower", label = "below symbol",
        value = "under"
      ),
      textInput(
        inputId = "inner", label = "RHS",
        value = "righthandside"
      )
    )
  })
  
  
  symbol <- reactive(
    switch(input$typeOfSymbol,
      "Sum" = r"(\sum\limits)",
      "Product" = r"(\prod\limits)",
      "Integral" = r"(\int\limits)"
    )
  )
  
  
  formula <- reactive({
    req(input$typeOfSymbol %in% simpleSymbols)
    
    paste0(
      "$$", symbol(),
      "^{", input$upper, "}",
      "_{", input$lower, "}",
      input$inner,
      "$$"
    )
  })
  
  
  output$simpleString <- renderText({
    req(input$typeOfSymbol %in% simpleSymbols)
    
    formula()
  })
  
  
  output$simpleLatex <- renderUI({
    req(input$typeOfSymbol %in% simpleSymbols)
    
    h2(withMathJax(
      formula()
    ))
  })
  
  
  doubleBackslash <- r"(\\\\)"
  
  latexifyMatrix <- function(input, elePerRow){
    paste0(
      r"($$\begin{bmatrix})", "\n",
      gsub(
        pattern = ",",
        replacement = " & ",
        x = gsub(
          pattern = paste0(
            "(" ,
            paste0(rep(",[^,]+", elePerRow - 1),
              collapse = ""), "),"
          ),
          replacement = paste0(
            "\\1 ", doubleBackslash, " ", "\n"
          ),
          x = input
        )
      ),
      "\n", r"(\end{bmatrix}$$)",
      sep = ""
    )
  }
  
  
  latexifyAugMatrix <- function(input, elePerRow = 4){
    augmentLine <- paste0(
      "{", 
      paste0(rep("c", elePerRow - 1), collapse = ""),
      "|c}"
    )
    paste0(
      r"($${\left[)", "\n", r"(\begin{array})", augmentLine, "\n",
      gsub(
        pattern = ",",
        replacement = " & ",
        x = gsub(
          pattern = paste0(
            "(" ,
            paste0(rep(",[^,]+", times = elePerRow - 1),
              collapse = ""), "),"
          ),
          replacement = paste0(
            "\\1 ", doubleBackslash, " ", "\n"
          ),
          x = input
        )
      ),
      "\n", r"(\end{array})", "\n", r"(\right]}$$)",
      collapse = ""
    )
  }
  
  
  output$matrixInput <- renderUI({
    req(input$typeOfSymbol %in% matrixSymbols)
    
    list(
      numericInput(inputId = "elementsPerRow",
        label = "Elements Per Row",
        min = 1, max = 50, value = 2, step = 1),
      textInput(inputId = "matrixElements",
        label = "Elements of matrix (separate by commas)", value = "a,b,c,d")
    )
  })
  
  
  matrixFormula <- reactive({
    req(input$elementsPerRow)
    
    switch(
      EXPR = input$typeOfSymbol,
      "Matrix" = latexifyMatrix(
        input$matrixElements,
        input$elementsPerRow
      ),
      "Augmented Matrix" = latexifyAugMatrix(
        input$matrixElements,
        input$elementsPerRow
      )
    )
  })
  
  output$matrixString <- renderText({
    req(input$typeOfSymbol %in% matrixSymbols)
    
    matrixFormula()
  })
  
  
  output$matrixLatex <- renderUI({
    req(input$typeOfSymbol %in% matrixSymbols)
    
    h2(withMathJax(
      matrixFormula()
    ))
  })
  
  
  observeEvent(eventExpr = input$clipboardWithDollar,
    handlerExpr = {
      switch(
        EXPR = input$typeOfSymbol,
        "Sum" = writeClipboard(formula()),
        "Product" = writeClipboard(formula()),
        "Integral" = writeClipboard(formula()),
        "Matrix" = writeClipboard(matrixFormula()),
        "Augmented Matrix" = writeClipboard(matrixFormula())
      )
    }
  )
  
  
  observeEvent(eventExpr = input$innerClipboard,
    handlerExpr = {
      switch(
        EXPR = input$typeOfSymbol,
        "Sum" = writeClipboard(
          trimws(formula(), whitespace = "[$]")
          ),
        "Product" = writeClipboard(
          trimws(formula(), whitespace = "[$]")
          ),
        "Integral" = writeClipboard(
          trimws(formula(), whitespace = "[$]")
        ),
        "Matrix" = writeClipboard(
          trimws(matrixFormula(), whitespace = "[$]")
          ),
        "Augmented Matrix" = writeClipboard(
          trimws(matrixFormula(), whitespace = "[$]")
        )
      )
    }
  )
  
}

shinyApp(ui = ui, server = server)
