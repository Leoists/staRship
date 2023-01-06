# Use the mouse to select points
# Original version written by Yohann Demont

# THIS IS HOW TO USE 3D RGL PLOTS IN SHINY *WITH* POINT SELECTION

if (!require("shiny"))
  stop("This demo requires shiny.")

if (!requireNamespace("crosstalk"))
  stop("This demo requires crosstalk.")

library(rgl)
library(DT)

ui <- fluidPage(
  sidebarLayout(
    mainPanel(tabsetPanel(id = "navbar",
                          selected = "3D",
                          tabPanel(title = "2D",
                                   plotOutput("plot_2D", brush = brushOpts(id = "plot_2D_brush",
                                                                           resetOnNew = TRUE,
                                                                           direction = "xy")),
                                   verbatimTextOutput("brush_info_2D")),
                          tabPanel(title = "3D",
                                   uiOutput("plot_3D_mousemode"),
                                   rglwidgetOutput("plot_3D"),
                                   verbatimTextOutput("brush_info_3D"),
                                   verbatimTextOutput("selected"),
                                   dataTableOutput('dt3d'),
                                   'DT using sharedData',
                                   dataTableOutput('dt3da'),
                                   'DT using selectionFunction3d',
                                   dataTableOutput('dt3db')
                                   )
    )),
    sidebarPanel(
      actionButton('debug','Debug'),
      selectInput("plot_x", label = "x feature", choices = colnames(iris)[-5], selected = colnames(iris)[1]),
                 selectInput("plot_y", label = "y feature", choices = colnames(iris)[-5], selected = colnames(iris)[2]),
                 selectInput("plot_z", label = "z feature", choices = colnames(iris)[-5], selected = colnames(iris)[3]),
                 actionButton(inputId = "reset_brush", label = "reset brush"))
  ))

server <- function(input, output, session) {
  # 2D
  output$plot_2D <- renderPlot({
    plot(x = iris[, input$plot_x],
         y = iris[, input$plot_y],
         col = as.integer(iris[, "Species"]))
  })
  output$brush_info_2D <- renderPrint(str(input$plot_2D_brush))
  
  observeEvent(input$debug,browser());
  
  rv <- reactiveValues(iris=mutate(iris,selected=0));
  
  
  
  # DT
  # shareddata
  output$dt3d <- renderDataTable({
    datatable(arrange(rv$iris,desc(selected)),options = list(paging = FALSE)) %>% 
      formatStyle('selected',target='row',backgroundColor = styleEqual(0:1,c('white','yellow')));
  })
  output$dt3da <- renderDataTable({
    if(length(input$rgl_3D_brush) == 0 || input$rgl_3D_brush$state == "inactive") return(NULL);
    out <- iris[as.logical(sharedData$selection()),];
    #if(NROW(out) == 0) browser();
    out;
    });
  # selfunction
  output$dt3db <- renderDataTable({
    if(length(brush<-input$rgl_3D_brush) == 0 || input$rgl_3D_brush$state == "inactive") return(NULL);
      iris[selectionFunction3d(brush)(iris[,c(input$plot_x,input$plot_y,input$plot_z)]),];
    });
  
  # 3D
  sharedData <- NULL
  output$brush_info_3D <- renderPrint({print(input$rgl_3D_brush, verbose = TRUE)})
  
  # How to use selectionFunction3d ?
  output$selected <- renderPrint({
    if(length(input$rgl_3D_brush) == 0 || input$rgl_3D_brush$state == "inactive") return(NULL)
    cat("Selections from crosstalk:\n")
    # Need as.logical because selection() might return NULL
    print(which(as.logical(sharedData$selection())))
    cat("Selections using function:\n")
    f <- selectionFunction3d(input$rgl_3D_brush)
    which(f(iris[, c(input$plot_x, input$plot_y, input$plot_z)]))
  })

  observe({
    if(length(input$rgl_3D_brush) == 0) return()
    if(input$rgl_3D_brush$state == "inactive" || length(as.logical(sharedData$selection()))==0) {
      rv$iris$selected <- 0;
      return()};
    rv$iris$selected <- as.logical(sharedData$selection())+0;
  });
  
    
  output$plot_3D_mousemode <-
    renderUI({
      rglMouse( default = "selecting",
                stayActive = FALSE,
                choices = c("selecting","trackball"),
                sceneId = "plot_3D")
    })
  open3d(useNULL = TRUE)
  output$plot_3D <- renderRglwidget({
    clear3d()
    dat <- iris[, c(input$plot_x, input$plot_y, input$plot_z, "Species")]
    dat$id <-as.character(seq_len(nrow(iris)))
    plot3d(x = dat[, 1:3], type = "s", size = 1, col = as.integer(iris[, "Species"]), aspect = TRUE)
    sharedData <<- rglShared(id = text3d(dat[, 1:3], text = dat[, "id"], adj = -0.5),
                             group = "SharedData_plot_3D_ids",
                             deselectedFade = 0,
                             selectedIgnoreNone = FALSE)
    shinyResetBrush(session, "rgl_3D_brush")
    rglwidget(shared = sharedData,
              shinyBrush = "rgl_3D_brush")
  })
  observeEvent(input$reset_brush, {
    session$resetBrush("plot_2D_brush")
    shinyResetBrush(session, "rgl_3D_brush")
  })
}

shinyApp(ui, server)
