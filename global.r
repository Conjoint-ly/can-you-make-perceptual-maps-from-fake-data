library(readr)
library(data.table)
library(psych)
library(qgraph)
library(httr)
library(jsonlite)
library(ade4)
openAIkey <- readLines("openai.key")

#### Define functions ####
requestChatCompletion <- function(messages, model) {
  body <- list(
    model = model,
    messages = messages,
    temperature = 1,
    max_tokens = 6,
    top_p = 1,
    frequency_penalty = 0,
    presence_penalty = 0
  )
  r <- POST('https://api.openai.com/v1/chat/completions',
            body = toJSON(body, auto_unbox = TRUE),
            httr::add_headers(Authorization = paste("Bearer", openAIkey)),
            httr::content_type('application/json'))
  response <- content(r)
  return(response)
}

getAnswers <- function(preprompt, questions, model = "gpt-4") {
  completions <- c()
  messages <- list(
    list(
      role = "system",
      content = preprompt
    )
  )
  for (i in seq_along(questions)) {
    messages <- append(messages, list(
      list(
        role = "user",
        content = questions[i]
      )
    ))
    completions <- append(completions, requestChatCompletion(messages = messages, model)$choices[[1]]$
      message$
      content)
    messages <- append(messages, list(
      list(
        role = "assistant",
        content = completions[i]
      )
    ))
  }
  return(completions)
}

plot_mds <- function(fit, brands, file, title, colours) {
  x <- fit$points[, 1]
  y <- fit$points[, 2]
  plot_map(brands, x, y, file, title, colours)
}


plot_map <- function(brands, x, y, file, title, colours) {

  brands <- unlist(brands) |> unname()
  x <- unlist(x) |> unname()
  y <- unlist(y) |> unname()

  png(file, width = 600, height = 600)

  x <- x - mean(x)
  y <- y - mean(y)

  # Rotate the plot so that the first point is at North
  rotation_angle = -1 * atan2(y, x)[1]
  x_rotated = x * cos(rotation_angle) - y * sin(rotation_angle)
  y_rotated = x * sin(rotation_angle) + y * cos(rotation_angle)
  y <- x_rotated
  x <- y_rotated

  # Flop the plot so that the second point is to the right
  if (x[2] < x[1]) {
    x <- -x
  }

  plot(x, y,
       xlab = "Dimension 1", ylab = "Dimension 2", main = title,
       pch = 19, col = colours,
       xlim = c(min(x) - 0.1, max(x) + 0.1),
       ylim = c(min(y) - 0.1, max(y) + 0.1))
  text(x, y, brands, cex = 01, pos = 3, col = colours)
  dev.off()
}

compare_similarity_matrices <- function(...) {
  inputs <- list(...)
  comparisons <- matrix(NA, nrow = length(inputs), ncol = length(inputs))
  for (i in seq_along(inputs)) {
    for (j in seq_along(inputs)) {
      if (i > j) {
        Di <- as.dist(1 - inputs[[i]])
        Dj <- as.dist(1 - inputs[[j]])
        comparisons[i, j] <- mantel.randtest(Di, Dj)$obs
      }
    }
  }
  comparisons
}

compare_similarity_matrices <- function(...) {
  inputs <- list(...)
  comparisons <- matrix(NA, nrow = length(inputs), ncol = length(inputs))
  for (i in seq_along(inputs)) {
    for (j in seq_along(inputs)) {
      if (i > j) {
        Di <- as.dist(1 - inputs[[i]])
        Dj <- as.dist(1 - inputs[[j]])
        comparisons[i, j] <- mantel.randtest(Di, Dj)$obs
      }
    }
  }
  comparisons
}

compare_similarity_matrices_by_most_similar_brand <- function(...) {
  inputs <- list(...)
  for (i in seq_along(inputs)) {
    diag( inputs[[i]]) <- -Inf
  }
  comparisons <- matrix(NA, nrow = length(inputs), ncol = length(inputs))
  for (i in seq_along(inputs)) {
    for (j in seq_along(inputs)) {
      if (i > j) {
        Bi <- colnames( inputs[[i]])[apply( inputs[[i]], 1, which.max)]
        Bj <- colnames( inputs[[j]])[apply( inputs[[j]], 1, which.max)]
        comparisons[i, j] <- mean(Bi == Bj)
      }
    }
  }
  comparisons
}

compare_similarity_matrices_by_most_dissimilar_brand <- function(...) {
  inputs <- list(...)
  for (i in seq_along(inputs)) {
    diag( inputs[[i]]) <- Inf
  }
  comparisons <- matrix(NA, nrow = length(inputs), ncol = length(inputs))
  for (i in seq_along(inputs)) {
    for (j in seq_along(inputs)) {
      if (i > j) {
        Bi <- colnames( inputs[[i]])[apply( inputs[[i]], 1, which.min)]
        Bj <- colnames( inputs[[j]])[apply( inputs[[j]], 1, which.min)]
        comparisons[i, j] <- mean(Bi == Bj)
      }
    }
  }
  comparisons
}

nameDims<-function(M, names) {
  colnames(M) <- names
    rownames(M) <- names
  M[-1,-ncol(M)]
}