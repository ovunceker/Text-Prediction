library(shiny)

fluidPage(
  titlePanel("Text Predictor"),

  sidebarLayout(
    sidebarPanel(
      width = 4,
      h4("Input Settings"),

      textAreaInput("input_text",
                    "Type your text here:",
                    value = "",
                    placeholder = "Example: 'I want to' or 'How are you'",
                    rows = 3),

      sliderInput("num_predictions",
                  "Number of predictions:",
                  min = 1, max = 10, value = 3),

      sliderInput("max_candidates",
                  "Search depth:",
                  min = 5, max = 20, value = 10),

      actionButton("predict_btn", "Get Predictions",
                   class = "btn-primary"),

      actionButton("clear_btn", "Clear",
                   class = "btn-default"),

      hr(),

      h4("Try these examples:"),
      actionButton("ex1", "I want to"),
      actionButton("ex2", "How are you"),
      actionButton("ex3", "Thank you for")
    ),

    mainPanel(
      width = 8,
      h3("Next Word Predictions"),
      h5("(Wait for model information to be seen before making predictions)"),

      wellPanel(
        verbatimTextOutput("predictions_output")
      ),

      hr(),

      h4("Model Information"),
      fluidRow(
        column(6,
               tableOutput("model_stats")
        ),
        column(6,
               h5("Recent Prediction Times"),
               tableOutput("prediction_times")
        )
      )
    )
  )
)
