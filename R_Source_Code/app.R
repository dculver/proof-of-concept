library(shiny)
library(jsonlite)
library(dplyr)
library(ggplot2)
library(S7)

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
  
  # A loading message that will disappear once R finishes thinking
  conditionalPanel(
    condition = "input.jspsych_data == null",
    h4(style = "color: #2980b9;", "Analyzing simulation data...")
  ),
  
  uiOutput("feedback_ui")
)

server <- function(input, output, session) {
  
  output$feedback_ui <- renderUI({
    # Wait until JS sends the data
    req(input$jspsych_data)
    
    if(input$jspsych_data == "NO_DATA") {
      return(h3("No session data found in your browser. Please run the simulation first."))
    }
    
    # tryCatch prevents the White Screen of Death and prints the error
    tryCatch({
      
      # 1. Parse the JSON and flatten it into a dataframe
      data <- fromJSON(input$jspsych_data, flatten = TRUE)
      
      # 2. Extract the specific row that actually contains the case events
      # We filter out all the NULL rows (like mic, camera, calibration)
      events_list <- data$case_events
      events <- events_list[!sapply(events_list, is.null)][[1]]
      
      # 3. Logic: Did they click the video tab?
      viewed_video <- "video" %in% events$state
      
      # 4. Build the UI response
      tagList(
        h3("Clinical Reasoning Summary"),
        if(viewed_video) {
          p(style = "color: green;", "Excellent work! You observed the patient's ADL performance before submitting your evaluation.") 
        } else {
          p(style = "color: red;", "Feedback: You made an intervention recommendation without observing the ADL video. Direct observation is critical for determining appropriate adaptive equipment.")
        }
      )
      
    }, error = function(e) {
      # If R crashes, print the error to the screen
      tagList(
        h3(style = "color: red;", "Analysis Error"),
        p("R encountered an error while processing the JSON data:"),
        code(e$message)
      )
    })
  })
}

shinyApp(ui, server)