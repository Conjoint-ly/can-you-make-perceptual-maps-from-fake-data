# Can you make perceptual maps from fake data? (A replication study)

See ________

## Common files

* [openai.key](openai.key) - The OpenAI key used to access their API (not included in the repo).
* [global.r](global.r) - Loads libraries and functions used in the other scripts.

## Study on Super Rugby teams

* **Datasets**:
  * [super-rugby-real-data.xlsx](super-rugby-real-data.xlsx) — The real data from the original study.
  * [super-rugby-real-similarities.csv](super-rugby-real-similarities.csv) — The real similarity matrix from the original study. To be read of % of responses who said that [column] was more similar to [row].
  * [super-rugby-facts-about-teams.csv](super-rugby-facts-about-teams.csv) — A simple dataset about a team each team containing the RGB colour of the team's kit, the team's name, and the team's country (coded as Australia or New Zealand).
  * The generated fake data is in the [super-rugby-fake-data](super-rugby-fake-data/) folder.
* **Code**:
  * [super-rugby-fake-data-generation.r](super-rugby-fake-data-generation.r) — The R script used to generate the fake data.
  * [super-rugby-similarity-plotting.r](super-rugby-similarity-plotting.r) — The R script used to plot the similarity charts (graphs and MDS).
* **Output charts** are in the [super-rugby-outputs](super-rugby-outputs/) folder.

## Study about Australian cafe chains

* **Datasets**:
  * [cafe-real-data.xlsx](cafe-real-data.xlsx) — The real data from the original study.
  * [cafe-real-similarities.csv](cafe-real-similarities.csv) — The real similarity matrix from the original study. To be read of % of responses who said that [column] was more similar to [row].
  * The generated fake data is in the [cafe-fake-data](cafe-fake-data/) folder.
  * [cafe-expert-based-maps.csv](cafe-expert-based-maps.csv) — Two expert-based perceptual maps.
* **Code**:
  * [cafe-fake-data-generation.r](cafe-fake-data-generation.r) — The R script used to generate the fake data.
  * [cafe-similarity-plotting.r](cafe-similarity-plotting.r) — The R script used to plot the similarity charts (graphs and MDS).
* **Output charts** are in the [cafe-outputs](cafe-outputs/) folder.