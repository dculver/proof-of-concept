library(shiny)
library(jsonlite)
library(dplyr)
library(ggplot2)
library(S7) # The crucial fix for the ggplot2 WebAssembly bug

ui <- fluidPage(
  tags$head(tags$script(HTML("
    async function getDataFromIDB() {
      return new Promise((resolve, reject) => {
        let request = indexedDB.open('OT_Simulation_DB', 1);
        request.onsuccess = (e) => {
          let db = e.target.result;
          let tx = db.transaction('sessions', 'readonly');
          let store = tx.objectStore('sessions');
          let getReq = store.get('active_session');
          getReq.onsuccess = () => {
            if(getReq.result) {
              resolve(getReq.result.data);
            } else {
              resolve('NO_DATA');
            }
          };
        };
        request.onerror = () => resolve('ERROR');
      });
    }
    $(document).on('shiny:connected', async function(event) {
      let data = await getDataFromIDB();
      Shiny.setInputValue('jspsych_data', data);
    });
  "))),
  
  titlePanel("OT Clinical Reasoning Feedback"),
  
  # Loading indicator
  conditionalPanel(
    condition = "input.jspsych_data == null",
    h4(style = "color: #2980b9;", "Analyzing simulation data & rendering visuals...")
  ),
  
  # Layout: Text on top, Plot on bottom
  uiOutput("feedback_ui"),
  br(),
  plotOutput("gaze_heatmap", width = "100%", height = "500px")
)

server <- function(input, output, session) {
  
  # 1. The Text Feedback (Clinical Reasoning)
  output$feedback_ui <- renderUI({
    req(input$jspsych_data)
    if(input$jspsych_data == "NO_DATA") return(h3("No session data found in your browser."))
    
    tryCatch({
      data <- fromJSON(input$jspsych_data, flatten = TRUE)
      events_list <- data$case_events
      events <- events_list[!sapply(events_list, is.null)][[1]]
      
      viewed_video <- "video" %in% events$state
      
      tagList(
        h3("Clinical Reasoning Summary"),
        if(viewed_video) {
          p(style = "color: green; font-size: 16px;", "Excellent work! You observed the patient's ADL performance before submitting your evaluation.") 
        } else {
          p(style = "color: red; font-size: 16px;", "Feedback: You made an intervention recommendation without observing the ADL video. Direct observation is critical for determining appropriate adaptive equipment.")
        }
      )
    }, error = function(e) { p("Text analysis error: ", e$message) })
  })
  
  # 2. The Visual Feedback (Eye-Tracking Heatmap)
  output$gaze_heatmap <- renderPlot({
    req(input$jspsych_data)
    if(input$jspsych_data == "NO_DATA") return(NULL)
    
    tryCatch({
      data <- fromJSON(input$jspsych_data, flatten = TRUE)
      
      # Extract the WebGazer array (filtering out empty trials like the mic test)
      gaze_list <- data$webgazer_data
      gaze_raw <- gaze_list[!sapply(gaze_list, is.null)][[1]]
      
      # Fallback if calibration failed or no gaze data exists
      if(length(gaze_raw) == 0 || is.null(gaze_raw$x)) {
        plot(1, type="n", axes=FALSE, xlab="", ylab="", main="No valid eye-tracking data found for this session.")
        return()
      }
      
      # Convert the JSON list to a clean R Dataframe
      df <- as.data.frame(gaze_raw)
      
      # Draw the Heatmap
      # Note: We use -y because web browsers draw coordinates top-to-bottom, but R draws bottom-to-top!
      ggplot(df, aes(x = x, y = -y)) +  
        stat_density_2d(aes(fill = after_stat(level)), geom = "polygon", alpha = 0.6) +
        scale_fill_gradientn(colors = c("blue", "green", "yellow", "red")) +
        theme_minimal() +
        theme(
            axis.text = element_blank(),
            axis.ticks = element_blank(),
            panel.grid = element_blank()
        ) +
        labs(
            title = "Visual Attention Density",
            subtitle = "Where you focused during the case review",
            x = "", y = "", fill = "Gaze Intensity"
        )
        
    }, error = function(e) { 
      plot(1, type="n", axes=FALSE, xlab="", ylab="", main=paste("Error rendering plot:", e$message)) 
    })
  })
}

shinyApp(ui, server)