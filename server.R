library(shiny)
library(quanteda)
library(dplyr)
library(stringr)

calculate_kneser_ney_prob <-
  function(kn_model, ngram_order, context, word){
    discount <- attr(kn_model, "discount")

    if (ngram_order == 1) {
      # Searching for the row
      uni_row <- kn_model$ngram_1[kn_model$ngram_1$prediction == word, ]
      if (nrow(uni_row) == 0) {
        return(0)
      }

      continuation_count <- uni_row$continuation_count[1]
      total_types <- uni_row$total_continuation_types[1]

      return(continuation_count / total_types)
    }

    ngram_df <- kn_model[[paste0("ngram_", ngram_order)]]

    if (!is.null(context) && context != "") {
      # Again searching for the row
      matching_ngram <- ngram_df[ngram_df$context == context &
                                   ngram_df$prediction == word, ]
    } else {
      return(0)
    }

    if (nrow(matching_ngram) == 0) {
      return(0)
    }

    count <- matching_ngram$frequency[1]
    context_total <- matching_ngram$context_total[1]

    # This is the probability evaluation part
    discounted_prob <- max(count - discount, 0) / context_total

    num_following <- matching_ngram$num_following_words[1]
    lambda <- (discount / context_total) * num_following

    if (ngram_order > 2) {
      # Backoff part
      context_parts <- str_split(context, "_")[[1]]
      backoff_context <- paste(context_parts[-1], collapse = "_")
    } else if (ngram_order == 2) {
      backoff_context <- NULL
    }

    lower_prob <- calculate_kneser_ney_prob(kn_model,
                                            ngram_order - 1,
                                            backoff_context, word)

    return(discounted_prob + lambda * lower_prob)
  }

predict_kneser_ney_fast <- function(kn_model, input_text, top_k = 5,
                                    max_candidates = 20) {
  # Handle empty input
  #if (input_text == "" || is.null(input_text) || nchar(trimws(input_text)) == 0) {
  #  message("Using Kneser-Ney unigram probabilities for empty input")
  #  unigrams <- kn_model$ngram_1
  #  unigrams$probability <- unigrams$continuation_count / unigrams$total_continuation_types
  #  results <- unigrams[order(-unigrams$probability), c("prediction", "probability")]
  #  return(head(results, top_k))
  #}

  # Tokenize the input
  input_toks <- tokens(input_text,
                       remove_punct = TRUE,
                       remove_numbers = TRUE,
                       remove_symbols = TRUE,
                       remove_url = TRUE) %>%
    tokens_tolower()

  input_words <- as.character(input_toks[[1]])
  num_words <- length(input_words)

  ngram_max <- attr(kn_model, "ngram_max")
  # Choosing which n-gram level to use
  target_order <- min(num_words + 1, ngram_max)

  if (target_order > 1) {
    # Pick the last words from input
    context_words <- tail(input_words, target_order - 1)
    context <- paste(context_words, collapse = "_")

    # Looking for the correct rows again
    ngram_df <- kn_model[[paste0("ngram_", target_order)]]
    candidates_df <- ngram_df[ngram_df$context == context, ]

    # Backoff part
    if (nrow(candidates_df) == 0) {
      message("No candidates found at order ", target_order,
              ", backing off...")
      new_input <- paste(input_words[-1], collapse = " ")
      return(predict_kneser_ney_fast(kn_model, new_input,
                                     top_k, max_candidates))
    }

    # If there are too many candidates, limit the number
    candidates_df <- candidates_df[order(-candidates_df$frequency), ]
    candidates_df <- head(candidates_df, max_candidates)
    candidates <- candidates_df$prediction

  } else {
    # For single word input, use Kneser-Ney unigrams
    message("Using Kneser-Ney unigram probabilities")
    unigrams <- kn_model$ngram_1
    unigrams$probability <-
      unigrams$continuation_count / unigrams$total_continuation_types
    results <-
      unigrams[order(-unigrams$probability), c("prediction", "probability")]
    return(head(results, top_k))
  }

  # Calculate Kneser-Ney probabilities for limited candidates
  message("Calculating Kneser-Ney probabilities for ",
          length(candidates), " candidates...")
  prob_df <- data.frame(
    word = character(),
    probability = numeric(),
    stringsAsFactors = FALSE
  )

  for (candidate in candidates) {
    prob <-
      calculate_kneser_ney_prob(kn_model, target_order, context, candidate)
    prob_df <-
      rbind(prob_df, data.frame(word = candidate, probability = prob))
  }

  # Return top K predictions
  results <- prob_df %>%
    arrange(desc(probability)) %>%
    head(top_k)

  return(results)
}

function(input, output, session){
  # Load the pre-trained model
  kn_model <- reactive({
    readRDS("kneser_ney_model.rds")
  })

  # Store prediction times
  prediction_times <- reactiveVal(data.frame(Input = character(),
                                             Time_Seconds = numeric()))

  # Get predictions when button is clicked
  predictions <- eventReactive(input$predict_btn, {
    req(input$input_text)
    req(kn_model())

    # Measure prediction time
    start_time <- Sys.time()


    # Get predictions with progress indicator
    withProgress(message = 'Calculating predictions...', value = 0.5, {
      result <- predict_kneser_ney_fast(
        kn_model(),
        input_text = input$input_text,
        top_k = input$num_predictions,
        max_candidates = input$max_candidates
      )
    })

    end_time <- Sys.time()
    prediction_time <- round(as.numeric(end_time - start_time), 2)

    # Store input and prediction time
    new_entry <- data.frame(
      Input = input$input_text,
      Time_Seconds = prediction_time
    )
    prediction_times(rbind(prediction_times(), new_entry))

    # Add time to the result
    attr(result, "prediction_time") <- prediction_time

    return(result)
  })

  # Display predictions
  output$predictions_output <- renderPrint({
    preds <- predictions()

    if (is.null(preds) || nrow(preds) == 0) {
      cat("No predictions found. Try a different phrase.\n")
    } else {
      prediction_time <- attr(preds, "prediction_time")
      cat("Top predictions for: '", input$input_text, "'\n\n", sep = "")
      cat("Prediction time: ", prediction_time, " seconds\n\n")
      for (i in 1:nrow(preds)) {
        cat(sprintf("%d. %-12s (%.2f%%)\n",
                    i, preds$word[i], preds$probability[i] * 100))
      }
    }
  })

  # Display model statistics
  output$model_stats <- renderTable({
    model <- kn_model()
    stats <- data.frame(
      Metric = c("Unigrams", "Bigrams", "Trigrams", "4-grams"),
      Count = c(
        format(nrow(model$ngram_1), big.mark = ","),
        format(nrow(model$ngram_2), big.mark = ","),
        format(nrow(model$ngram_3), big.mark = ","),
        format(nrow(model$ngram_4), big.mark = ",")
      )
    )
    stats
  }, bordered = TRUE)

  # Update the history table to show prediction times
  output$prediction_times <- renderTable({
    times_data <- prediction_times()
    if (nrow(times_data) > 5) {
      times_data <- tail(times_data, 5)
    }
    times_data
  }, bordered = TRUE)

  # Clear button
  observeEvent(input$clear_btn, {
    updateTextAreaInput(session, "input_text", value = "")
  })

  # Example buttons
  observeEvent(input$ex1, {
    updateTextAreaInput(session, "input_text", value = "I want to")
  })

  observeEvent(input$ex2, {
    updateTextAreaInput(session, "input_text", value = "How are you")
  })

  observeEvent(input$ex3, {
    updateTextAreaInput(session, "input_text", value = "Thank you for")
  })
}
