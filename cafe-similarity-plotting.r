source("global.r")

#### Simple facts about teams ####
brands <- c("Gloria Jean's Coffees", "Hudsons Coffee", "Jamaica Blue",
            "Kurtosh", "Max Brenner", "McCafe",
            "Starbucks", "The Coffee Club", "Zarraffa's Coffee")
colours = c("#FF0000", "#FF8000", "#FFFF00", "#00FF00", "#00FFFF", "#0000FF", "#FF00FF", "#8000FF", "#FF0080")

#### Read fake response data from GPT-3.5-Turbo ####
fake_similarities3 <- read_csv("cafe-fake-data/fake-similarities-gpt-3.5-turbo.csv")
combined_fake_similarities3 <- as.matrix(fake_similarities3[, -1]) + t(as.matrix(fake_similarities3[, -1]))
rownames(combined_fake_similarities3) <- brands
diag(combined_fake_similarities3) <- Inf

# Plot MSD
combined_fake_fit3 <- cmdscale(as.dist(1 - combined_fake_similarities3), eig = TRUE, k = 2)
plot_mds(combined_fake_fit3, brands, "cafe-outputs/f-mds.png", "F", colours)


#### Read fake response data from GPT-4 ####
fake_similarities <- read_csv("cafe-fake-data/fake-similarities-gpt-4.csv")
combined_fake_similarities <- as.matrix(fake_similarities[, -1]) + t(as.matrix(fake_similarities[, -1]))
rownames(combined_fake_similarities) <- brands
diag(combined_fake_similarities) <- Inf

# Plot MSD
combined_fake_fit <- cmdscale(as.dist(1 - combined_fake_similarities), eig = TRUE, k = 2)
plot_mds(combined_fake_fit, brands, "cafe-outputs/g-mds.png", "G", colours)


#### Read fake response data from GPT-4-Turbo ####
fake_similarities4p <- read_csv("cafe-fake-data/fake-similarities-gpt-4-1106-preview.csv")
combined_fake_similarities4p <- as.matrix(fake_similarities4p[, -1]) + t(as.matrix(fake_similarities4p[, -1]))
rownames(combined_fake_similarities4p) <- brands
diag(combined_fake_similarities4p) <- Inf

# Plot MSD
combined_fake_fit4p <- cmdscale(as.dist(1 - combined_fake_similarities4p), eig = TRUE, k = 2)
plot_mds(combined_fake_fit4p, brands, "cafe-outputs/i-mds.png", "I", colours)


#### Read actual response data ####
real_similarities <- read_csv("cafe-real-similarities.csv")
combined_similarities <- as.matrix(real_similarities[, -1]) + t(as.matrix(real_similarities[, -1]))
rownames(combined_similarities) <- brands
colnames(combined_similarities) <- brands
combined_similarities[is.na(combined_similarities)] <- Inf

# Plot MSD
combined_fit <- cmdscale(as.dist(1 - combined_similarities), eig = TRUE, k = 2)
plot_mds(combined_fit, brands, "cafe-outputs/k-mds.png", "K", colours)


#### Expert-based maps ####
expertMaps <- read_csv("cafe-expert-based-maps.csv")
plot_map(expertMaps[, 1], expertMaps[, 2], expertMaps[, 3], "cafe-outputs/l-expert-map.png", "L", colours)
plot_map(expertMaps[, 1], expertMaps[, 4], expertMaps[, 5], "cafe-outputs/m-expert-map.png", "M", colours)


#### Compare distance matrices in fake and real ####
compare_similarity_matrices_by_most_similar_brand(
  combined_fake_similarities3,
  combined_fake_similarities,
  combined_fake_similarities4p,
  -as.matrix(dist(expertMaps[,2:3])),
  -as.matrix(dist(expertMaps[,4:5])),
  combined_similarities) |>
  nameDims(c("GPT-3.5-Turbo (F)", "GPT-4 (G)", "GPT-4-Turbo (I)", "First expert (L)","Second expert (M)", "Real (K)"))

# Most dissimilar
compare_similarity_matrices_by_most_similar_brand(
  -combined_fake_similarities3,
  -combined_fake_similarities,
  -combined_fake_similarities4p,
  as.matrix(dist(expertMaps[,2:3])),
  as.matrix(dist(expertMaps[,4:5])),
  -combined_similarities) |>
  nameDims(c("GPT-3.5-Turbo (F)", "GPT-4 (G)", "GPT-4-Turbo (I)", "First expert (L)","Second expert (M)", "Real (K)"))
