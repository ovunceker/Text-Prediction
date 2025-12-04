library(quanteda)
library(dplyr)
library(data.table)
library(stringr)

twitter_lines <- readLines("/Users/ovunc/Desktop/Code_Related/R/NLP Project/final/en_US/en_US.twitter.txt", encoding = "UTF-8", skipNul = TRUE, warn = FALSE)
blogs_lines <- readLines("/Users/ovunc/Desktop/Code_Related/R/NLP Project/final/en_US/en_US.blogs.txt", encoding = "UTF-8", skipNul = TRUE, warn = FALSE)
news_lines <- readLines("/Users/ovunc/Desktop/Code_Related/R/NLP Project/final/en_US/en_US.news.txt", encoding = "UTF-8", skipNul = TRUE, warn = FALSE)

all_text <- c(news_lines, blogs_lines, twitter_lines)
sample_text <- sample(all_text, 100000)

create_basic_ngrams <- function(text_data, ngram_max = 4){
  # Tokenization
  toks <- tokens(text_data,
                 remove_punct = TRUE,
                 remove_numbers = TRUE,
                 remove_symbols = TRUE,
                 remove_url = TRUE) %>%
    tokens_tolower()

  ngram_list <- list()

  for (n in 1:ngram_max){

    ngrams <- tokens_ngrams(toks, n = n)
    dfm_obj <- dfm(ngrams)
    feature_freqs <- colSums(dfm_obj)

    ngram_df <- data.frame(
      feature = names(feature_freqs),
      frequency = as.numeric(feature_freqs),
      stringsAsFactors = FALSE
    )

    # Sort by frequency (highest first)
    ngram_df <- ngram_df[order(-ngram_df$frequency), ]

    if (n > 1){
      # Splitting last word as prediction
      parts <- str_split_fixed(ngram_df$feature, "_", n)
      context <- apply(parts[, 1:(n-1), drop = FALSE], 1,
                       function(x) paste(x, collapse = "_"))
      prediction <- parts[, n]
      ngram_df$context <- context
      ngram_df$prediction <- prediction
      ngram_df$feature <- NULL
    } else {
      ngram_df$prediction <- ngram_df$feature
      ngram_df$feature <- NULL
    }
    ngram_list[[n]] <- ngram_df
  }

  names(ngram_list) <- paste0("ngram_", 1:ngram_max)

  return(ngram_list)
}

basic_ngrams <- create_basic_ngrams(sample_text, ngram_max = 4)

calculate_kneser_ney_stats <- function(basic_ngrams){
  kn_ngrams <- basic_ngrams
  bigram_df <- kn_ngrams$ngram_2

  # How many unique words come before prediction
  continuation_counts <- bigram_df %>%
    group_by(prediction) %>%
    summarise(continuation_count = n_distinct(context)) %>%
    ungroup()

  # Merging unigrams with continuation_counts column
  kn_ngrams$ngram_1 <- kn_ngrams$ngram_1 %>%
    left_join(continuation_counts, by = "prediction") %>%
    mutate(continuation_count = ifelse(is.na(continuation_count),
                                       0, continuation_count))

  # Adding total bigram numbers as a column to the unigrams
  total_continuation_types <- nrow(bigram_df)
  kn_ngrams$ngram_1$total_continuation_types <- total_continuation_types

  for (n in 2:length(kn_ngrams)){
    ngram_df <- kn_ngrams[[n]]

    # Sums the frequencies where the context appears
    context_totals <- ngram_df %>%
      group_by(context) %>%
      summarise(context_total = sum(frequency)) %>%
      ungroup()

    # How many different words followed by the same context
    context_diversity <- ngram_df %>%
      group_by(context) %>%
      summarise(num_following_words = n()) %>%
      ungroup()

    # Adding those two to the n-gram table
    kn_ngrams[[n]] <- ngram_df %>%
      left_join(context_totals, by = "context") %>%
      left_join(context_diversity, by = "context")
  }

  attr(kn_ngrams, "discount") <- 0.75
  attr(kn_ngrams, "ngram_max") <- length(kn_ngrams)

  return(kn_ngrams)
}

kn_ngrams <- calculate_kneser_ney_stats(basic_ngrams)

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

saveRDS(kn_ngrams, "kneser_ney_model.rds")

fast_accuracy_check <- function(kn_model, test_cases, sample_size = 30) {
  set.seed(123)
  sampled_cases <- test_cases[sample(length(test_cases), sample_size)]

  correct <- 0
  results <- data.frame()

  for (i in 1:length(sampled_cases)) {
    test_case <- sampled_cases[[i]]

    cat(sprintf("%d/%d: '%s' -> ", i, sample_size, test_case$context))

    tryCatch({
      preds <- predict_kneser_ney_fast(kn_model, test_case$context, top_k = 1, max_candidates = 10)

      if (nrow(preds) > 0) {
        predicted_word <- preds$word[1]
        is_correct <- (predicted_word == test_case$true_word)

        if (is_correct) {
          correct <- correct + 1
          cat("✓ '", predicted_word, "'\n", sep = "")
        } else {
          cat("✗ predicted:'", predicted_word, "' actual:'", test_case$true_word, "'\n", sep = "")
        }

        results <- rbind(results, data.frame(
          context = test_case$context,
          predicted = predicted_word,
          actual = test_case$true_word,
          correct = is_correct
        ))
      }
    }, error = function(e) {
      cat("ERROR - skipping\n")
    })
  }

  accuracy <- correct / length(sampled_cases)

  cat("\n=== QUICK ACCURACY RESULTS ===\n")
  cat("Accuracy:", round(accuracy * 100, 1), "%\n")
  cat("Correct:", correct, "/", length(sampled_cases), "\n")
  cat("Sample size:", sample_size, "\n")

  return(list(accuracy = accuracy, results = results))
}

accuracy_results <- fast_accuracy_check(kn_ngrams, test_cases, 300)


