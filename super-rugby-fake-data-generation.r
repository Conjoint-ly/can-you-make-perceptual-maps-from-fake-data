source("global.r")

#### Data generation ####
brands <- unname(unlist(read_csv("simple-facts-about-teams.csv")[, 1]))
preprompt <- "You are a New Zealander."
question <- "Please continue the following sentence: The Super Rugby team %s is similar to the Super Rugby team "
N <- 100

for (model in c("gpt-4", "gpt-3.5-turbo","gpt-4-1106-preview")) {
  attempted_completions <- matrix(NA, nrow = N, ncol = length(brands), dimnames = list(NULL, brands))
  pb <- txtProgressBar(min = 0, max = N, style = 3)
  for (attempt in seq_len(N)) {
    setTxtProgressBar(pb, attempt)
    questions <- c()
    shiffled_brand_order <- sample(seq_along(brands))
    shiffled_brands <- brands[shiffled_brand_order]
    for (brand in shiffled_brands) {
      questions <- c(questions, sprintf(question, brand))
    }
    completions <- getAnswers(preprompt, questions, model)
    attempted_completions[attempt,] <- completions[order(shiffled_brand_order)]
  }
  close(pb)
  write.csv(attempted_completions, sprintf("super-rugby-fake-data/completions-%s.csv", model), row.names = FALSE)

  fake_similarities <- matrix(NA, nrow = length(brands), ncol = length(brands), dimnames = list(brands, brands))
  for (object_row in seq_along(brands)) {
    for (subject_column in seq_along(brands)) {
      fake_similarities[object_row, subject_column] <- mean(grepl(
        brands[object_row],
        attempted_completions[, subject_column],
        ignore.case = TRUE))
    }
  }
  write.csv(fake_similarities, sprintf("super-rugby-fake-data/fake-similarities-%s.csv", model))
}

