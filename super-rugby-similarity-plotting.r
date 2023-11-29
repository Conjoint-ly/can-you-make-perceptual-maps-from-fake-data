source("global.r")

#### Simple facts about teams ####
facts <- read_csv("super-rugby-facts-about-teams.csv") |> as.data.table()
colours <- rgb(
  as.numeric(paste("0x", facts$R, sep = '')) / 255,
  as.numeric(paste("0x", facts$G, sep = '')) / 255,
  as.numeric(paste("0x", facts$B, sep = '')) / 255)
brands <- unname(unlist(facts$Team))
NZbrands <- facts[`New Zealand` == 1, Team]
AUbrands <- facts[`Australia` == 1, Team]

# Create a frame based on country and add some noise
noisy_frame <- facts[, c("Australia", "New Zealand")] |> as.data.frame()
noisy_frame$noise1 <- rnorm(nrow(noisy_frame), 0, 0.2)
noisy_frame$noise2 <- rnorm(nrow(noisy_frame), 0, 0.2)
row.names(noisy_frame) <- brands

# Plot MSD
facts_fit <- cmdscale(dist(noisy_frame), eig = TRUE, k = 2)
plot_mds(facts_fit, brands, "super-rugby-outputs/all-a-mds.png", "A", colours)

# Plot MSD for NZ only
facts_fit_nz <- cmdscale(dist(noisy_frame[noisy_frame$`New Zealand` == 1,]), eig = TRUE, k = 2)
plot_mds(facts_fit_nz, NZbrands, "super-rugby-outputs/nz-a-mds.png", "A - NZ only", colours[facts$`New Zealand`])

# Plot MSD for Australia only
facts_fit_au <- cmdscale(dist(noisy_frame[noisy_frame$`Australia` == 1,]), eig = TRUE, k = 2)
plot_mds(facts_fit_au, AUbrands, "super-rugby-outputs/au-a-mds.png", "A - AU only", colours[facts$Australia])


#### Read actual response data ####
real_similarities <- read_csv("super-rugby-real-similarities.csv")
combined_similarities <- as.matrix(real_similarities[, -1]) + t(as.matrix(real_similarities[, -1]))
rownames(combined_similarities) <- brands
combined_similarities[is.na(combined_similarities)] <- Inf

# Plot MSD
combined_fit <- cmdscale(as.dist(1 - combined_similarities), eig = TRUE, k = 2)
plot_mds(combined_fit, brands, "super-rugby-outputs/all-b-mds.png", "B", colours)

# Plot MSD for NZ only
combined_fit_nz <- cmdscale(as.dist(1 - combined_similarities[NZbrands, NZbrands]), eig = TRUE, k = 2)
plot_mds(combined_fit_nz, NZbrands, "super-rugby-outputs/nz-b-mds.png", "B - NZ only", colours[facts$`New Zealand`])

# Plot MSD for Australia only
combined_fit_au <- cmdscale(as.dist(1 - combined_similarities[AUbrands, AUbrands]), eig = TRUE, k = 2)
plot_mds(combined_fit_au, AUbrands, "super-rugby-outputs/au-b-mds.png", "B - AU only", colours[facts$Australia])


#### Read fake response data from GPT-4 ####
fake_similarities <- read_csv("super-rugby-fake-data/fake-similarities-gpt-4.csv")
combined_fake_similarities <- as.matrix(fake_similarities[, -1]) + t(as.matrix(fake_similarities[, -1]))
rownames(combined_fake_similarities) <- brands
diag(combined_fake_similarities) <- Inf

# Plot MSD
combined_fake_fit <- cmdscale(as.dist(1 - combined_fake_similarities), eig = TRUE, k = 2)
plot_mds(combined_fake_fit, brands, "super-rugby-outputs/all-c-mds.png", "C", colours)

# Plot MSD for NZ only
combined_fake_fit_nz <- cmdscale(as.dist(1 - combined_fake_similarities[NZbrands, NZbrands]), eig = TRUE, k = 2)
plot_mds(combined_fake_fit_nz, NZbrands, "super-rugby-outputs/nz-c-mds.png", "C - NZ only", colours[facts$`New Zealand`])

# Plot MSD for Australia only
combined_fake_fit_au <- cmdscale(as.dist(1 - combined_fake_similarities[AUbrands, AUbrands]), eig = TRUE, k = 2)
plot_mds(combined_fake_fit_au, AUbrands, "super-rugby-outputs/au-c-mds.png", "C - AU only", colours[facts$Australia])


#### Read fake response data from GPT-3.5-Turbo ####
fake_similarities3 <- read_csv("super-rugby-fake-data/fake-similarities-gpt-3.5-turbo.csv")
combined_fake_similarities3 <- as.matrix(fake_similarities3[, -1]) + t(as.matrix(fake_similarities3[, -1]))
rownames(combined_fake_similarities3) <- brands
diag(combined_fake_similarities3) <- Inf

# Plot MSD
combined_fake_fit3 <- cmdscale(as.dist(1 - combined_fake_similarities3), eig = TRUE, k = 2)
plot_mds(combined_fake_fit3, brands, "super-rugby-outputs/all-d-mds.png", "D", colours)

# Plot MSD for NZ only
combined_fake_fit_nz3 <- cmdscale(as.dist(1 - combined_fake_similarities3[NZbrands, NZbrands]), eig = TRUE, k = 2)
plot_mds(combined_fake_fit_nz3, NZbrands, "super-rugby-outputs/nz-d-mds.png", "D - NZ only", colours[facts$`New Zealand`])

# Plot MSD for Australia only
combined_fake_fit_au3 <- cmdscale(as.dist(1 - combined_fake_similarities3[AUbrands, AUbrands]), eig = TRUE, k = 2)
plot_mds(combined_fake_fit_au3, AUbrands, "super-rugby-outputs/au-d-mds.png", "D - AU only", colours[facts$Australia])


#### Read fake response data from GPT-4-Turbo ####

fake_similarities4p <- read_csv("super-rugby-fake-data/fake-similarities-gpt-4-1106-preview.csv")
combined_fake_similarities4p <- as.matrix(fake_similarities4p[, -1]) + t(as.matrix(fake_similarities4p[, -1]))
rownames(combined_fake_similarities4p) <- brands
diag(combined_fake_similarities4p) <- Inf

# Plot MSD
combined_fake_fit4p <- cmdscale(as.dist(1 - combined_fake_similarities4p), eig = TRUE, k = 2)
plot_mds(combined_fake_fit4p, brands, "super-rugby-outputs/all-e-mds.png", "E", colours)

# Plot MSD for NZ only
combined_fake_fit_nz4p <- cmdscale(as.dist(1 - combined_fake_similarities4p[NZbrands, NZbrands]), eig = TRUE, k = 2)
plot_mds(combined_fake_fit_nz4p, NZbrands, "super-rugby-outputs/nz-e-mds.png", "E - NZ only", colours[facts$`New Zealand`])

# Plot MSD for Australia only
combined_fake_fit_au4p <- cmdscale(as.dist(1 - combined_fake_similarities4p[AUbrands, AUbrands]), eig = TRUE, k = 2)
plot_mds(combined_fake_fit_au4p, AUbrands, "super-rugby-outputs/au-e-mds.png", "E - AU only", colours[facts$Australia])


#### Compare distance matrices in fake and real ####
compare_similarity_matrices(
  combined_fake_similarities3,
  combined_fake_similarities,
  combined_fake_similarities4p,
  combined_similarities) |>
  nameDims(c("GPT-3.5-Turbo (D)", "GPT-4 (C)", "GPT-4-Turbo (E)", "Real (B)"))

compare_similarity_matrices_by_most_similar_brand(
  combined_fake_similarities3,
  combined_fake_similarities,
  combined_fake_similarities4p,
  combined_similarities) |>
  nameDims(c("GPT-3.5-Turbo (D)", "GPT-4 (C)", "GPT-4-Turbo (E)", "Real (B)"))

# Most dissimilar
compare_similarity_matrices_by_most_similar_brand(
  -combined_fake_similarities3,
  -combined_fake_similarities,
  -combined_fake_similarities4p,
  -combined_similarities) |>
  nameDims(c("GPT-3.5-Turbo (D)", "GPT-4 (C)", "GPT-4-Turbo (E)", "Real (B)"))