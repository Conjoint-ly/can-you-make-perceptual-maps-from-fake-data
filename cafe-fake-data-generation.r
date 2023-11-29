source("global.r")

#### Data generation ####
brands <- c("Gloria Jean's Coffees", "Hudsons Coffee", "Jamaica Blue",
            "Kurtosh", "Max Brenner", "McCafe",
            "Starbucks", "The Coffee Club", "Zarraffa's Coffee")
brands_short <- c("Gloria Jean", "Hudsons", "Jamaica Blue",
                  "Kurtosh", "Max Brenner", "McCafe",
                  "Starbucks", "Coffee Club", "Zarraffa")
preprompt <- "You are an Australian. You will now be asked several questions about Australian cafe chains."
question <- "Please continue the following sentence: The cafe chain %s is similar to the cafe chain "
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
  write.csv(attempted_completions, sprintf("cafe-fake-data/completions-%s.csv", model), row.names = FALSE)

  fake_similarities <- matrix(NA, nrow = length(brands), ncol = length(brands), dimnames = list(brands, brands))
  for (object_row in seq_along(brands)) {
    for (subject_column in seq_along(brands)) {
      fake_similarities[object_row, subject_column] <- mean(grepl(
        brands_short[object_row],
        attempted_completions[, subject_column],
        ignore.case = TRUE))
    }
  }
  write.csv(fake_similarities, sprintf("cafe-fake-data/fake-similarities-%s.csv", model))
}

