library("methods", exclude = c("removeClass", "show"))
suppressPackageStartupMessages(library("shiny", quietly = TRUE))
if (!requireNamespace("shinyjs", quietly = TRUE)) {
  install.packages("shinyjs")
  install.packages("colourpicker") # Necessarily absent, as imports shinyjs
}
library("shinyjs", exclude = c("colourInput", "colourPicker",
                               "updateColourInput", "runExample"))

if (!requireNamespace("colourpicker", quietly = TRUE)) {
  install.packages("colourpicker")
}
library("colourpicker", exclude = c("runExample"))

if (!requireNamespace("Ternary", quietly = TRUE)) {
  install.packages("Ternary")
}
library("Ternary")

palettes <- list("#91aaa7",
                 c("#969660", "#c3dfca"),
                 c("#be83ae", "#2ea7af", "#fbcdcf"),
                 c("#72c5a9", "#b7c5ff", "#dbdabb", "#a28bac"),
                 c("#59c4c0", "#ea9a9a", "#7998a6", "#e9d7a9", "#9c9379"),
                 c("#e8b889", "#67c6eb", "#e5d5c2", "#938fba", "#64b69a", "#779c7b"),
                 c("#c4808f", "#5ba08f", "#f0a693", "#ccd7fe", "#cdb87e", "#c6aae2", "#d2dad8"),
                 c("#d0847f", "#63a5d7", "#d7b981", "#5a9bbb", "#9bb67e", "#dea6d5", "#91967e", "#ca7f96"),
                 c("#8b93a8", "#ccb97e", "#8e9dd7", "#57a384", "#dbb1e7", "#2da7af", "#d68986", "#75d2f9", "#e4d1f0"),
                 c("#dfcf92", "#40b3cb", "#b88a61", "#ecb2e0", "#d6dbbc", "#a28bae", "#edcfeb", "#7498ab", "#b187a0", "#8f939c"),
                 c("#a98f70", "#7be5e2", "#d295c0", "#9ae2bd", "#d3b7f1", "#eca88d", "#8993cd", "#ffc7bb", "#8695a8", "#b3e1df", "#b6878a"),
                 c("#eaa9d3", "#7ac09b", "#fdaca8", "#8ce7e4", "#eed69b", "#70a4d9", "#e8d6ba", "#589bbb", "#959672", "#d0dbd1", "#9b9282", "#d9d9c6"),
                 c("#7498ab", "#e5bd8a", "#7ed8ff", "#de8f8e", "#46bac6", "#ffc0d3", "#57ae96", "#f7cddd", "#50a098", "#b58a6d", "#add49d", "#a18da1", "#cedad9"),
                 c("#8097a4", "#d0dea9", "#a78cc3", "#aee4bf", "#bb82a8", "#5dc9c6", "#b88690", "#26a3b9", "#ad8e6f", "#a4e2ef", "#869a65", "#efcfdd", "#60a089", "#9e927b"),
                 c("#b9aae5", "#bbd69c", "#e2adde", "#77a777", "#f8abc8", "#8ee7ce", "#f2a1a5", "#81bdf1", "#f2bb91", "#b8dcfe", "#aeb276", "#f2cdef", "#e8d6b2", "#8d92b0", "#b7878d"),
                 c("#c3d79b", "#b28cc0", "#64a985", "#e3a7d4", "#2ea2aa", "#e69892", "#85c6f9", "#fbd1a0", "#7696be", "#89996c", "#ddcdff", "#719d89", "#f5cde6", "#b6e0da", "#e8d4cd", "#b5ddfa"),
                 c("#a98d83", "#84e1ff", "#bb8964", "#46b1d1", "#ffbfa5", "#6199c0", "#bbcb8f", "#bf82ab", "#85ddc4", "#eea0ba", "#c1d8ff", "#c3818b", "#c5c6ff", "#999388", "#e8cbff", "#ffb5b6", "#d2dad7"),
                 c("#faccde", "#60a987", "#c6abe4", "#6f9e77", "#c48093", "#a5e5d3", "#cc8f6f", "#499fae", "#d9dca6", "#7796b8", "#bee1ba", "#b4daff", "#919583", "#e2d3e9", "#47a19b", "#ebd4bc", "#7c9993", "#a9e3e0"),
                 c("#739e6e", "#ffbfd9", "#43b6bb", "#e8ad88", "#5e9bce", "#c2af75", "#a8e0fe", "#fad0a8", "#679e8d", "#ffc7b1", "#abe5c0", "#ac8d78", "#c5dddc", "#a48f84", "#cadfb0", "#899694", "#fdcdc1", "#d1dad5", "#dfd8c4"),
                 c("#6e9c93", "#ffb4b3", "#7ec6a2", "#eeccfe", "#cddb9d", "#8a90c5", "#dcb983", "#77bff0", "#f0ab92", "#90ddff", "#f1d3a9", "#b5c2fe", "#c1e1b7", "#7596ba", "#bce1c4", "#a88c96", "#5a9daf", "#b18b80", "#d4d6f3", "#949577"),
                 c("#e7d6bb", "#749ed5", "#f9d29d", "#67b3e2", "#d09773", "#65ccec", "#d38585", "#7fe8ef", "#cf8190", "#94e8cd", "#ae8cc1", "#b3cf95", "#cbc0fc", "#94a66c", "#eeccff", "#ada368", "#e9a6ce", "#48a297", "#ffc1df", "#799c7a", "#facbe0", "#5d9e9a", "#ffc6c1", "#619bb0", "#fccdcb", "#7197bb", "#b1e4c3", "#9390b1", "#c3e0c0", "#a98c90", "#ade3ce", "#9c927d", "#c2dafe", "#869881", "#e6d3dc", "#6e9ba4", "#bde0d0", "#8196a4", "#b2e1df", "#b9deea")
)

ltyInput <- function (id, name, val, none = TRUE) {
  selectInput(id, paste(name, "line type"),
              c(if (none) list("None" = "blank"),
              list("Solid" = "solid", "Dotted" = "dotted",
                   "Dashed" = "dashed", "Dot-Dash" = "dotdash",
                   "Long-dash" = "longdash", "Two-dash" = "twodash")),
              val)
}
pchInput <- function (id, name, val) {
  selectInput(id, name, 
              list(
                "Data column 4" = 904,
                "Data column 5" = 905,
                "Data column 6" = 906,
                "Square" = 0,
                "Circle" = 1,
                "Triangle-up" = 2,
                "Plus" = 3,
                "Cross" = 4,
                "Diamond" = 5,
                "Triangle-down" = 6,
                "Crossed-square" = 7,
                "Star" = 8,
                "Plussed-diamond" = 9,
                "Plussed-circle" = 10,
                "Snowflake" = 11,
                "Plussed-square" = 12,
                "Crossed-circle" = 13,
                "Triangle-in-square" = 14,
                "Filled square" = 15,
                "Filled circle" = 16,
                "Filled triangle" = 17,
                "Filled diamond" = 18
                ),
              val)
}
cexInput <- function (id, name, val) {
  sliderInput(id, name, 0, 4, val, step = 0.01)
}
lwdInput <- function (id, name, val) {
  sliderInput(id, paste(name, "line width"), 0, 6, val, step = 0.01)
}
fontInput <- function (id, name, val) {
  selectInput(id, paste0(name, " font style"),
              list("Plain" = 1, "Bold" = 2, "Italic" = 3, "Bold-italic" = 4),
              val)
}


# Define UI for app that draws a histogram ----
ui <- fluidPage(title = "Ternary plotter", theme = "Ternary.css",
  useShinyjs(),
                
  sidebarLayout(
    sidebarPanel(
      tabsetPanel(
        tabPanel("Load data",
           tags$div("Upload a csv or spreadsheet, where the first three columns",
                    "list the co-ordinates of each point.  Columns 4 to 6 may",
                    "be used to specify the points' style."),
           fileInput("datafile", "Data", placeholder = "No data file selected",
                     accept = c(".csv", ".txt", ".xls", ".xlsx")),
           textOutput(outputId = "dataStatus"),
           textInput("dim1", "Column one", ""),
           textInput("dim2", "Column two", ""),
           textInput("dim3", "Column three", ""),
           tags$p("Use the tabs above to edit display settings.")
           ),
        tabPanel("Plot display",
           
           selectInput("point", "Plot direction", 
                       list("Up" = "up", "Right" = "right", "Down" = "down",
                            "Left" = "left"), "up"),
           colourInput("col", "Background colour", "#ffffff"),
           checkboxGroupInput("display", "Display options", 
                              list("Clockwise" = "clockwise",
                                   "Isometric" = "isometric",
                                   "Tip labels" = "show.tip.labels",
                                   "Axis labels" = "show.axis.labels",
                                   "Axis tick labels" = "axis.labels",
                                   "Axis tick marks" = "axis.tick",
                                   "Rotate tick labels" = "axis.rotate"), 
                              c("clockwise", "isometric", "axis.labels",
                                "show.axis.labels", "axis.tick", "axis.rotate")),
           lwdInput("axis.lwd", "Axis", 1),
           ltyInput("axis.lty", "Axis", "solid"),
           colourInput("axis.col", "Axis colour", "black"),
           lwdInput("ticks.lwd", "Axis ticks", 1),
           sliderInput("ticks.length", "Axis tick length", 0, 0.1, 0.025),
           colourInput("ticks.col", "Axis tick colour", "darkgrey"),
           
           #axis.pos = NULL,
           
          #xlim = NULL,
          #ylim = NULL,

          #atip.rotate = NULL,
          #btip.rotate = NULL,
          #ctip.rotate = NULL,
          #atip.pos = NULL,
          #btip.pos = NULL,
          #ctip.pos = NULL,
          #padding = 0.08,
          #col = NA,
          ),
        tabPanel("Grids",
           sliderInput("grid.lines" , "Main grid lines", 1, 42, 10),
           sliderInput("grid.minor.lines", "Minor grid lines:", 0, 10, 4),
           colourInput("grid.col", "Grid colour", "darkgrey"),
           colourInput("grid.minor.col", "Grid secondary colour", "lightgrey"),
           ltyInput("grid.lty", "Grid", "solid"),
           ltyInput("grid.minor.lty", "Secondary grid", "solid"),
           lwdInput("grid.lwd", "Grid", par("lwd")),
           lwdInput("grid.minor.lwd", "Secondary grid", par("lwd")),
           ),
        tabPanel("Labels",
           cexInput("lab.cex", "Axis label size", 1),
           fontInput("lab.font", "Axis label", 1),
           sliderInput("lab.offset", "Axis label offset", -0.3, 0.5, 0.16, step = 0.005),
           colourInput("lab.col", "Axis label colour", "black"),
           
           cexInput("axis.cex", "Tick label size", 0.8),
           fontInput("axis.font", "Tick", par("font")),
           
           
           cexInput("tip.cex", "Tip label size", 1),
           fontInput("tip.font", "Tip" , 1),
           colourInput("tip.col", "Tip colour", "black"),
         ),
        tabPanel("Points",
          selectInput("points.type", "Plot type", 
                      list("Points only" = "p",
                           "Lines" = "l",
                           "Connected points" = "b",
                           "Text" = "text"),
                      "p"),
          selectInput("text.source", "Text to display",
                      list("Row names" = 0,
                           "Data column 4" = 4,
                           "Data column 5" = 5,
                           "Data column 6" = 6),
                      0),
          pchInput("points.pch", "Point shape", 16),
          selectInput("points.col.by", "Point colour",
                      list("Data column 4" = 4,
                           "Data column 5" = 5,
                           "Data column 6" = 6,
                           "User-specified" = 0),
                      0),
          colourInput("points.col", "Colour", "#222222"),
          cexInput("points.cex", "Point size", 1.8),
          lwdInput("points.lwd", "Connecting", 1),
          ltyInput("points.lty", "Connecting", "solid", FALSE),
          )
      ),
    ),

  # Sidebar layout with input and output definitions ----
  
    mainPanel(
      tabsetPanel(
        tabPanel("Plot",
                 fluidRow(plotOutput(outputId = "plot")),
                 fluidRow(id = "saveButtons",
                   tags$span("Save as: "),
                   downloadButton("saveR", "R script"),
                   downloadButton("savePdf", "PDF"),
                   downloadButton("savePng", "PNG"),
                   tags$span("PNG size: ", id = "pngSizeLabel"),
                   numericInput("pngSize", NULL, 800, 100,
                               width = "70px", step = 10),
                   tags$span("pixels"),
                 ),
        ),
        tabPanel("R code",
                 fluidRow(verbatimTextOutput("code")),
        )
      ),
      withTags(
        div(id = "caption",
          p("For additional features, see ",
            a(href = "https://ms609.github.io/Ternary/articles/Ternary.html", "the package manual,"),
            "or",
            a(href = "https://github.com/ms609/TernaryApp/issues/new?title=Missing+feature:+",
             "request"),
            " their addition to this app."),
          p("If using figures in a publication, please cite Smith (2017). ",
            "\"Ternary: An R Package for Creating Ternary Plots.\" ",
            "Comprehensive R Archive Network, doi:",
            a(href = "https://dx.doi.org/10.5281/zenodo.1068996",
              "10.5281/zenodo.1068996")
          ),
        )
      ),
    )
  )

  
  # References and notes
)

server <- function(input, output, session) {
  
  r <- reactiveValues()
  displaySetting <- function(id) id %in% input$display
  
  filePath <- reactive({
    fileInput <- input$datafile
    exampleFile <- system.file("TernaryApp", "example.csv", package = "Ternary")
    if (is.null(fileInput)) {
      if (exampleFile == "") {
        ghFile <- "https://raw.githubusercontent.com/ms609/TernaryApp/master/example.csv"
        candidate <- tryCatch({
          read.csv(ghFile)
          output$dataStatus <- renderText(
            "Data / example files not found; loaded from GitHub.")
          ghFile
          }, warning = function (e) {
            output$dataStatus <- renderText(
              "Data / example files not found; could not load from GitHub.")
            ""
          })
      } else {
        output$dataStatus <- renderText(paste(
          "Data file not found; using example from", exampleFile))
        candidate <- exampleFile
      }
    } else {
      candidate <- fileInput$datapath
      if (is.null(candidate)) {
        output$dataStatus <- renderText({"Data file not found; using example."})
        candidate <- exampleFile
      } else {
        r$fileName <- fileInput$name
        output$dataStatus <- renderText({paste0("Loaded data from ", fileInput$name)})
      }
    }
    
    # Return:
    candidate
  })
  
  fileExt <- reactive({
    fp <- filePath()
    if (nchar(fp) < 2) "<none>" else substr(fp, nchar(fp) - 3, nchar(fp))
  })
  
  myData <- reactive({
    fp <- filePath()
    ret <- switch(fileExt(),
                  ".csv" = read.csv(fp),
                  ".txt" = read.table(fp),
                  ".xls" = readxl::read_excel(fp),
                  "xlsx" = readxl::read_excel(fp),
                  {
                    output$dataStatus <- renderText({
                      paste0("Unsupported file extension: ", fileExt())})
                    matrix(0, 0, 3)
                  }
    )
    cn <- colnames(ret)
    updateTextInput(session, "dim1", value = cn[1])
    updateTextInput(session, "dim2", value = cn[2])
    updateTextInput(session, "dim3", value = cn[3])
    
    ret
  })
  
  dataLabels <- reactive({
    candidates <- colnames(ret)
    if (is.null(candidates)) rep("", 3L) else candidates
  })
  
  axisLabels <- reactive({
    if (displaySetting("show.axis.labels")) {
      c(input$dim1, input$dim2, input$dim3)
    } else rep(NULL, 3)
  })
  
  
  tipLabels <- reactive({
    if (displaySetting("show.tip.labels")) {
      c(input$dim1, input$dim2, input$dim3)
    } else rep(NULL, 3)
  })
  
  PchValue <- function (pch) {
    nPch <- as.numeric(pch)
    if (nPch < 900L) nPch else as.numeric(myData()[, nPch - 900L])
  }
  PchText <- function (pch) {
    nPch <- as.numeric(pch)
    if (nPch < 900L) pch else paste0("myData[, ", nPch - 900L, "]")
  }
  DataCol <- function (i) {
    if(ncol(myData()) >= i) myData()[, i] else {
      message("Data only has ", ncol(myData()), " columns.")
      0
    }
  }
  PtCol <- function () {
    switch (input$points.col.by,
            "0" = input$points.col,
            "4" = DataCol(4),
            "5" = DataCol(5),
            "6" = DataCol(6))
  }
  PtColTxt <- function () {
    switch (input$points.col.by,
            "0" = paste0("\"", input$points.col, "\""),
            "4" = "myData[, 4]",
            "5" = "myData[, 5]",
            "6" = "myData[, 6]")
  }
  TextLabels <- function () {
    switch(input$text.source, 
           "0" = if (is.null(rownames(myData()))) seq_len(nrow(myData())) else
             rownames(myData()),
           "4" = DataCol(4),
           "5" = DataCol(5),
           "6" = DataCol(6))
  }
  TextLabelsText <- function () {
    switch (input$text.source, 
            "0" = if (is.null(rownames(myData()))) {
              "seq_len(nrow(myData))"
            } else {
              "rownames(myData)"
            },
            "4" = "myData[, 4]",
            "5" = "myData[, 5]",
            "6" = "myData[, 6]")
  }
  observeEvent(input$points.type, {
    if (input$points.type == "text") {
      showElement("text.source")
    } else {
      hideElement("text.source")
    }
    if (input$points.type %in% c("p", "b")) {
      showElement("points.pch")
    } else {
      hideElement("points.pch")
    }
    if (input$points.type %in% c("l", "b")) {
      showElement("points.lwd")
      showElement("points.lty")
    } else {
      hideElement("points.lwd")
      hideElement("points.lty")
    }
  })
  observeEvent(input$points.col.by, {
    if (input$points.col.by == "0") {
      showElement("points.col") 
    } else {
      hideElement("points.col")
    }
  })
  observeEvent(input$display, ignoreNULL = FALSE, {
    tipLabelSetting <- if ("show.tip.labels" %in% input$display) {
      showElement
    } else {
      hideElement
    }
    tipLabelSetting("tip.cex")
    tipLabelSetting("tip.font")
    tipLabelSetting("tip.col")
    axisLabelSetting <- if ("show.axis.labels" %in% input$display) {
      showElement
    } else {
      hideElement
    }
    axisLabelSetting("lab.cex")
    axisLabelSetting("lab.font")
    axisLabelSetting("lab.offset")
    
    tickLabelSetting <- if ("axis.labels" %in% input$display) {
      showElement
    } else {
      hideElement
    }
    tickLabelSetting("axis.cex")
    tickLabelSetting("axis.font")
  })
  
  makePlot <- function () {
    
    par(mar = rep(0, 4))
    TernaryPlot(
      atip = tipLabels()[1],
      btip = tipLabels()[2],
      ctip = tipLabels()[3],
      alab = axisLabels()[1],
      blab =  axisLabels()[2],
      clab = axisLabels()[3],
      lab.offset = input$lab.offset,
      lab.col = input$lab.col,
      point = input$point,
      clockwise = displaySetting("clockwise"),
      xlim = NULL,
      ylim = NULL,
      lab.cex = input$lab.cex,
      lab.font = as.numeric(input$lab.font),
      tip.cex = input$tip.cex,
      tip.font = as.numeric(input$tip.font),
      tip.col = input$tip.col,
      isometric = displaySetting("isometric"),
      atip.rotate = NULL,
      btip.rotate = NULL,
      ctip.rotate = NULL,
      atip.pos = NULL,
      btip.pos = NULL,
      ctip.pos = NULL,
      padding = 0.08,
      col = input$col,
      grid.lines = input$grid.lines,
      grid.col = input$grid.col,
      grid.lty = input$grid.lty,
      grid.lwd = input$grid.lwd,
      grid.minor.lines = input$grid.minor.lines,
      grid.minor.col = input$grid.minor.col,
      grid.minor.lty = input$grid.minor.lty,
      grid.minor.lwd = input$grid.minor.lwd,
      axis.lty = input$axis.lty,
      axis.labels = displaySetting("axis.labels"),
      axis.cex = input$axis.cex,
      axis.font = as.numeric(input$axis.font),
      axis.rotate = displaySetting("axis.rotate"),
      #axis.pos = input$axis.pos,
      axis.tick = displaySetting("axis.tick"),
      axis.lwd = input$axis.lwd,
      ticks.lwd = input$ticks.lwd,
      ticks.length = input$ticks.length,
      axis.col = input$axis.col,
      ticks.col = input$ticks.col
    )
    if (input$points.type == "text") {
      TernaryText(myData()[, 1:3],
                  labels = TextLabels(),
                  cex = input$points.cex,
                  pch = PchValue(input$points.pch),
                  col = PtCol()
      )
    } else {
      if (dim(myData())[1] > 0) {
          
        TernaryPoints(myData()[, 1:3],
                      cex = input$points.cex,
                      pch = PchValue(input$points.pch),
                      lwd = input$points.lwd,
                      lty = input$points.lty,
                      col = PtCol(),
                      type = input$points.type)
      }
    }
    
  }
  
  rScript <- function() {
    paste0(
      "# Include the full path to your data file here if necessary:\n",
      "myData <- ", switch(fileExt(), ".csv" = "read.csv",
                           ".txt" = "read.table",
                           ".xls" = "readxl::read_excel",
                           "xlsx" = "readxl::read_excel", "read.csv"), 
      "(\"", r$fileName, "\")\n\n",
      
      "TernaryPlot(
  atip = \"", tipLabels()[1], "\",
  btip = \"", tipLabels()[2], "\",
  ctip = \"", tipLabels()[3], "\",
  alab = \"", axisLabels()[1], "\",
  blab = \"", axisLabels()[2], "\",
  clab = \"", axisLabels()[3], "\",
  lab.offset = ", input$lab.offset, ",
  lab.col = \"", input$lab.col, "\",
  point = \"", input$point, "\",
  clockwise = ", displaySetting("clockwise"), ",
  xlim = NULL,
  ylim = NULL,
  lab.cex = ", input$lab.cex, ",
  lab.font = ", input$lab.font, ",
  tip.cex = ", input$tip.cex, ",
  tip.font = ", input$tip.font, ",
  tip.col = \"", input$tip.col, "\",
  isometric = ", displaySetting("isometric"), ",
  atip.rotate = NULL,
  btip.rotate = NULL,
  ctip.rotate = NULL,
  atip.pos = NULL,
  btip.pos = NULL,
  ctip.pos = NULL,
  padding = 0.08,
  col = \"", input$col, "\",
  grid.lines = ", input$grid.lines, ",
  grid.col = \"", input$grid.col, "\",
  grid.lty = \"", input$grid.lty, "\",
  grid.lwd = ", input$grid.lwd, ",
  grid.minor.lines = ", input$grid.minor.lines, ",
  grid.minor.col = \"", input$grid.minor.col, "\",
  grid.minor.lty = \"", input$grid.minor.lty, "\",
  grid.minor.lwd = ", input$grid.minor.lwd, ",
  axis.lty = \"", input$axis.lty, "\",
  axis.labels = ", displaySetting("axis.labels"), ",
  axis.cex = ", input$axis.cex, ",
  axis.font = ", input$axis.font, ",
  axis.rotate = ", displaySetting("axis.rotate"), ",
  axis.tick = ", displaySetting("axis.tick"), ",
  axis.lwd = ", input$axis.lwd, ",
  ticks.lwd = ", input$ticks.lwd, ",
  ticks.length = ", input$ticks.length, ",
  axis.col = \"", input$axis.col, "\",
  ticks.col = \"", input$ticks.col, "\"
)\n\n",
      
      if (input$points.type == "text") {
        paste0(
          "TernaryText(myData[, 1:3],
  labels = ", TextLabelsText(), ",
  cex = ", input$points.cex, ",
  pch = ", PchText(input$points.pch), ",
  col = \"", input$points.col, "\"\n)")
      } else {
        paste0("TernaryPoints(myData[, 1:3],
  type = \"", input$points.type, "\",
  cex = ", input$points.cex, ",
  pch = ", PchText(input$points.pch), ",
  lwd = ", input$points.lwd, ",
  lty = \"", input$points.lty, "\",
  col = ", PtColTxt(), "\n)")
      }
    )
  }
  
  output$plot <- renderPlot(makePlot())
  output$code <- renderText(rScript())
  output$savePng <- downloadHandler(
    filename = "TernaryPlot.png",
    content = function (file) {
      png(file, width = input$pngSize, height = input$pngSize)
      makePlot()
      dev.off()
    })
  output$savePdf <- downloadHandler(
    filename = "TernaryPlot.pdf",
    content = function (file) {
      pdf(file, 
          title = paste0("Ternary plot", 
                         if(filePath() != "") paste0("  from ", filePath())))
      makePlot()
      dev.off()
    })
  output$saveR <- downloadHandler(
    filename = "TernaryPlot.R",
    content = function (file) {
      writeLines(rScript(), file)
    })
        
}

shinyApp(ui = ui, server = server)
