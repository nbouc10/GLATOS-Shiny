library(shiny)
library(glatos)

falsedetfilter <- function(...){
#Set max file size
options(shiny.maxRequestSize = 300*1024^2)

#UI
ui <- fluidPage(
  #Set title
  titlePanel("Run GLATOS False Detections Filter"),
  #use fluidrow to align elements
  fluidRow(
    column(4,
            #Button for file upload
            fileInput("upload", NULL, buttonLabel = "Upload...", multiple = TRUE, accept = ".csv"),
            #Set tf
            numericInput("lag", "Threshold time interval (seconds)", value = 0, min = 0, max = 180000),
            #button for running filter
            actionButton("runfilter", "Run false detections filter", class = "btn-lg btn-success"),
            #checkbox to show plot
            checkboxInput("Showplot", "Show proportion exceeding threshold?"),
            #display head of upload
            #tableOutput("Preview"),
            #display head of filtered detections
            #tableOutput("Preview_dets"),
            #button to download filtered detections
            downloadButton("download1", "Download filtered detections")
    ),
    column(8,
           #plot showing proportion of detections exceeding filter
           plotOutput("plot_dets", height = "500px")
           )
  
  )
)

#SERVER
server <- function(input, output, session) {
  # Read in uploaded CSV
  data <- reactive({
    req(input$upload)  # Ensure a file is uploaded
    
    # Check if the uploaded file is a .csv
    ext <- tools::file_ext(input$upload$name)
    if (ext != "csv") {
      showModal(modalDialog(
        title = "Invalid File Type",
        "Please upload a valid .csv file.",
        easyClose = TRUE,
        footer = NULL
      ))
      return(NULL)  # Return NULL if file is not a .csv
    }
    
    # Proceed with reading the .csv file
    read.csv(input$upload$datapath)
  })
  
  # Reactive for filtered detections
  filtered_results <- eventReactive(input$runfilter, {
    req(data())  # Ensure data is available
    
    # Run false_detections() without plotting (Shiny will handle the plotting separately)
    filtered_data <- false_detections(det = data(), tf = input$lag, show_plot = FALSE)
    
    return(filtered_data)
  })
  
  # Render uploaded data
  #output$Preview <- renderTable(head(data()))
  
  # Render filtered data
  # output$Preview_dets <- renderTable({
  #   req(filtered_results())  # Ensure filtered results are available
  #   head(filtered_results())
  # })
  
  # Render plot inside Shiny
  output$plot_dets <- renderPlot({
    req(input$Showplot, filtered_results())  # Only plot if checkbox is checked
    
    # Run false_detections() again, but only for the plot
    false_detections(det = filtered_results(), tf = input$lag, show_plot = TRUE)
  })
  
  # Download filtered detections
  output$download1 <- downloadHandler(
    filename = function() {
      paste0(tools::file_path_sans_ext(input$upload$name), "_filtered.csv")
    },
    content = function(file) {
      req(filtered_results())  # Ensure filtered results are available
      write.csv(filtered_results(), file, row.names = FALSE)
    }
  )
}

# Run the application 
shinyApp(ui = ui, server = server)
}