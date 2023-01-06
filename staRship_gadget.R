library(shiny)
library(miniUI)

# We'll wrap our Shiny Gadget in an addin.
# Let's call it 'clockAddin()'.
showSystems <- function(ship,cols=c('distance','x','y','z','visited'
                                    ,"atmosphere", "gravity", "temperature"
                                    ,"water", "resources")
                        ,coldef=list(x=colDef(format=colFormat(digits=5))
                                     ,y=colDef(format=colFormat(digits=5))
                                     ,z=colDef(format=colFormat(digits=5))
                                     ,distance=colDef(format=colFormat(digits=5)))){
  knownsystems <- ship$planetLocalDB;
  knownsystems$distance <- p3Distance(knownsystems[,1:3],ship$coords);
  knownsystems <- arrange(knownsystems,distance);
  knownsystems <- cbind(knownsystems,extractPlanetInfo(knownsystems$info));
  coldef <- select(knownsystems,where(is.numeric) & any_of(cols) & 
                     !where(is.integer)) %>% names %>% 
    sapply(colDef,format=colFormat(digits=5),simplify=F)
  # Our ui will be a simple gadget page, which
  # simply displays the time in a 'UI' output.
  ui <- miniPage(
    gadgetTitleBar("Star Chart"),
    miniContentPanel(
      reactableOutput("known_systems")
    )
  );
  
  server <- function(input, output, session) {
    results <- reactiveValues(selected=subset(knownsystems,1==0));
    output$known_systems <- renderReactable(reactable(knownsystems[,cols]
                                                      ,columns=coldef
                                                      ,selection = 'single'
                                                      ,onClick='select'
                                                      ,pagination = F));
    
    observeEvent(input$known_systems__reactable__selected,{
      results$selected <- knownsystems[getReactableState('known_systems','selected'),]});
    # Listen for 'done' events. When we're finished, we'll
    # insert the current time, and then stop the gadget.
    observeEvent(input$done, {
      stopApp(results$selected);
    })
    
  }
  
  # We'll use a pane viwer, and set the minimum height at
  # 300px to ensure we get enough screen space to display the clock.
  viewer <- paneViewer(300)
  runGadget(ui, server, viewer = viewer)
  
}

