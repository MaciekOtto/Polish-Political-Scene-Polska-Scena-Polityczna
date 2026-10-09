# =============================================================================
#  Polish Political Scene - Multivariate Analysis
#  Perception of nine Polish politicians described by 17 personality traits
#
#  Author : Maciej Otto
#  Run    : set the working directory to the repository root, then source this file.
#           Input Excel files are expected in the folder defined by DATA_DIR.
# =============================================================================
#
#  Contents
#    0. Setup
#    1. Respondent profile and trait ratings (descriptive statistics)
#    2. Association between categorical variables (chi-square, contingency coefficients)
#    3. Questionnaire reliability (Cronbach's alpha, split-half reliability)
#    4. Similarity of politicians - binary (0/1) data: Jaccard, simple matching
#    5. Hierarchical clustering - binary data
#    6. Mixed-type variables - Gower distance and hierarchical clustering
#    7. Correlation structure and suitability for factor analysis
#    8. Number of factors / components
#    9. Principal component analysis (PCA)
#   10. Exploratory factor analysis (EFA)
#   11. Correspondence analysis (traits x politicians)
# =============================================================================


# 0. SETUP --------------------------------------------------------------------

DATA_DIR <- "data"

# NOTE: psych is loaded before tidyverse on purpose, so that ggplot2::alpha()
# does not mask psych::alpha(); psych::alpha() is also always called explicitly.
required_packages <- c(
  "readxl", "psych", "tidyverse", "DescTools", "corrplot", "RColorBrewer",
  "vegan", "proxy", "cluster", "factoextra", "reshape2", "plot.matrix",
  "ca", "ggrepel", "gplots"
)

missing_packages <- setdiff(required_packages, rownames(installed.packages()))
if (length(missing_packages) > 0) install.packages(missing_packages)
invisible(lapply(required_packages, library, character.only = TRUE))

read_data <- function(file) read_excel(file.path(DATA_DIR, file))

# Corrected contingency coefficient: C / C_max
corrected_contingency <- function(C, n_rows, n_cols) {
  if (n_rows == n_cols) {
    c_max <- sqrt((n_cols - 1) / n_cols)
  } else {
    c_max <- (sqrt((n_cols - 1) / n_cols) + sqrt((n_rows - 1) / n_rows)) / 2
  }
  C / c_max
}


# 1. RESPONDENT PROFILE AND TRAIT RATINGS -------------------------------------

## 1.1 Trait ratings (cechy.xlsx): first column = respondent ID,
##     columns 1-17 = first politician (Tusk), columns 18-34 = second (Trz.)
traits_raw  <- read_data("cechy.xlsx")
traits      <- traits_raw[, -1]
traits_tusk <- traits[, 1:17]
traits_trz  <- traits[, 18:34]

if ("trza_innowacyjny" %in% names(traits_trz)) {
  traits_trz$trza_innowacyjny <- as.numeric(traits_trz$trza_innowacyjny)
}
summary(traits_tusk)
summary(traits_trz)

## 1.2 Respondent characteristics (ankietowani.xlsx)
respondents <- read_data("ankietowani.xlsx") %>%
  rename(any_of(c(gender = "p\u0142e\u0107",              # płeć
                  education = "wykszta\u0142cenie",       # wykształcenie
                  age = "wiek")))
summary(respondents)

respondents$gender <- factor(respondents$gender, levels = c(1, 2),
                             labels = c("Women", "Men"))
respondents$education <- factor(respondents$education, levels = 1:4,
                                labels = c("Primary", "Vocational",
                                           "Secondary", "Higher"))

# Gender
gender_table <- table(respondents$gender)
pie(gender_table,
    main   = "Gender distribution",
    col    = c("lightcoral", "lightblue"),
    labels = paste(names(gender_table), "\n", gender_table),
    border = "white")

# Education
ggplot(respondents, aes(x = education, fill = education)) +
  geom_bar() +
  scale_fill_manual(values = c("Primary"    = "#4F81BD",
                               "Vocational" = "#C0504D",
                               "Secondary"  = "#9BBB59",
                               "Higher"     = "#8064A2")) +
  labs(title = "Education level distribution",
       x = "Education level", y = "Number of respondents") +
  theme_minimal() +
  theme(legend.position = "none",
        axis.text.x = element_text(angle = 45, hjust = 1))

# Age
ggplot(respondents, aes(x = age)) +
  geom_histogram(binwidth = 1, fill = "#C0504D", color = "white") +
  scale_x_continuous(breaks = seq(18, 93, by = 5)) +
  labs(title = "Age distribution", x = "Age", y = "Frequency") +
  theme_minimal()


# 2. ASSOCIATION BETWEEN CATEGORICAL VARIABLES --------------------------------
#    Age vs. answer to question x22 (tablice.xlsx)

cross_data <- read_data("tablice.xlsx") %>%
  rename(any_of(c(age = "wiek")))

cross_tab <- table(cross_data$age, cross_data$x22)
cross_tab

# Row and column percentages
row_percent <- round(100 * prop.table(cross_tab, margin = 1), 2)
col_percent <- round(100 * prop.table(cross_tab, margin = 2), 2)
row_percent
col_percent

mosaicplot(cross_tab, color = TRUE, main = "Age vs. answer to x22")

# Chi-square test of independence
chi_test <- chisq.test(cross_tab)
chi_test
chi_test$expected                 # expected counts
cross_tab - chi_test$expected     # observed minus expected
chi_test$stdres                   # standardised residuals

# Table with margins (counts and percentages)
addmargins(cross_tab)
addmargins(round(cross_tab / sum(cross_tab) * 100, 2))

# Association measures
DescTools::Phi(cross_tab)
DescTools::TschuprowT(cross_tab)
DescTools::CramerV(cross_tab)
DescTools::ContCoef(cross_tab)
DescTools::YuleY(cross_tab)   # defined for 2x2 tables only
DescTools::YuleQ(cross_tab)   # defined for 2x2 tables only

# The contingency coefficient has to be corrected manually
C_coef <- DescTools::ContCoef(cross_tab)
corrected_contingency(C_coef, n_rows = nrow(cross_tab), n_cols = ncol(cross_tab))


# 3. QUESTIONNAIRE RELIABILITY ------------------------------------------------
#    24 items (x1-x24) in pytania.xlsx

items <- as.data.frame(read_data("pytania.xlsx"))
if ("trza_innowacyjny" %in% names(items)) {
  items$trza_innowacyjny <- as.numeric(items$trza_innowacyjny)
}
summary(items)

total_score <- rowSums(items)
var_total   <- var(total_score)

## Split-half reliability for three alternative splits of the 24 items
item_names <- paste0("x", 1:24)
alt_split  <- c("x2", "x4", "x5", "x8", "x10", "x11",
                "x14", "x16", "x17", "x20", "x22", "x23")

splits <- list(
  first_vs_second_half = list(first  = item_names[1:12],
                              second = item_names[13:24]),
  odd_vs_even          = list(first  = item_names[seq(1, 24, by = 2)],
                              second = item_names[seq(2, 24, by = 2)]),
  alternative          = list(first  = alt_split,
                              second = setdiff(item_names, alt_split))
)

split_half_reliability <- function(data, first, second, total_variance) {
  score_1 <- rowSums(data[, first])
  score_2 <- rowSums(data[, second])

  alpha_of <- function(cols) {
    suppressWarnings(suppressMessages(
      psych::alpha(data[, cols], check.keys = TRUE)$total$raw_alpha
    ))
  }

  r_halves <- cor(score_1, score_2)

  list(
    halves = data.frame(
      statistic = c("mean", "sum", "std. deviation", "variance", "Cronbach's alpha"),
      half_1 = c(mean(score_1), sum(score_1), sd(score_1), var(score_1), alpha_of(first)),
      half_2 = c(mean(score_2), sum(score_2), sd(score_2), var(score_2), alpha_of(second))
    ),
    correlation_between_halves = r_halves,
    # Spearman-Brown: a value close to 1 means the items consistently
    # measure the same construct
    spearman_brown = (2 * r_halves) / (1 + r_halves),
    guttman        = 2 * (total_variance - var(score_1) - var(score_2)) / total_variance,
    scores = data.frame(half_1 = score_1, half_2 = score_2)
  )
}

reliability <- lapply(splits, function(s) {
  split_half_reliability(items, s$first, s$second, var_total)
})

for (name in names(reliability)) {
  cat("\n---", name, "---\n")
  print(reliability[[name]]$halves)
  cat("Correlation between halves:", reliability[[name]]$correlation_between_halves, "\n")
  cat("Spearman-Brown            :", reliability[[name]]$spearman_brown, "\n")
  cat("Guttman                   :", reliability[[name]]$guttman, "\n")
}

# Density of the two half-scores (first vs. second half of the questionnaire)
ggplot(reliability$first_vs_second_half$scores) +
  geom_density(aes(x = half_1), fill = "blue",  alpha = 0.6) +
  geom_density(aes(x = half_2), fill = "green", alpha = 0.6) +
  labs(title = "Density of scores in the two halves of the questionnaire",
       x = "Sum of item scores", y = "Density") +
  theme_minimal()

psych::cor.plot(items)


# 4. SIMILARITY OF POLITICIANS - BINARY DATA ----------------------------------
#    zerojeden.xlsx: first column = politician, other columns = 0/1 features

binary_raw <- as.data.frame(read_data("zerojeden.xlsx"))
str(binary_raw)

binary_df <- binary_raw[, -1]
rownames(binary_df) <- binary_raw[[1]]

## 4.1 Jaccard distance and similarity
jaccard_dist <- vegan::vegdist(binary_df, method = "jaccard", binary = TRUE)
jaccard_dist_matrix <- round(as.matrix(jaccard_dist), 3)
jaccard_sim_matrix  <- round(1 - jaccard_dist_matrix, 3)
jaccard_dist_matrix
jaccard_sim_matrix

## 4.2 Distance heatmaps (factoextra)
binary_dist <- factoextra::get_dist(binary_df, method = "binary", stand = FALSE)
factoextra::fviz_dist(binary_dist, order = TRUE,
                      gradient = list(low = "ivory", mid = "lightblue", high = "midnightblue"))

## 4.3 Jaccard similarity heatmap (ggplot2)
jaccard_sim_long <- reshape2::melt(jaccard_sim_matrix,
                                   varnames = c("Politician_1", "Politician_2"))

ggplot(jaccard_sim_long, aes(Politician_1, Politician_2, fill = value)) +
  geom_tile() +
  scale_fill_gradient2(low = "white", mid = "pink", high = "blue", midpoint = 0.5) +
  coord_fixed() +
  labs(title = "Jaccard similarity matrix", fill = "Similarity") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

## 4.4 Colour-coded Jaccard distance matrix (plot.matrix)
old_par <- par(mar = c(9, 10, 3, 7))
plot(jaccard_dist_matrix, digits = 2, text.cell = list(cex = 0.8),
     col = c("green", "yellow", "orange", "red"),
     breaks = c(0, 0.25, 0.5, 0.75, 1),
     las = 2, xlab = "", ylab = "", main = "", cex.axis = 0.8)
par(old_par)

## 4.5 Simple matching (Sokal-Michener) similarity
simple_matching_sim <- as.matrix(proxy::simil(binary_df, method = "simple matching"))
diag(simple_matching_sim) <- 1
round(simple_matching_sim, 3)


# 5. HIERARCHICAL CLUSTERING - BINARY DATA ------------------------------------

hc_jaccard <- hclust(jaccard_dist, method = "average")
hc_jaccard$height

plot(hc_jaccard, main = "", xlab = "Politicians", ylab = "Linkage height", cex = 0.8)
clusters_jaccard <- cutree(hc_jaccard, k = 3)
clusters_jaccard
rect.hclust(hc_jaccard, k = 3, border = c("violetred2", "navy", "tomato"))


# 6. MIXED-TYPE VARIABLES - GOWER DISTANCE ------------------------------------
#    mieszane.xlsx: quantitative and qualitative features; only the
#    qualitative ones are converted to factors.

mixed_raw <- as.data.frame(read_data("mieszane.xlsx"))
str(mixed_raw)

mixed_df <- mixed_raw[, -1]
rownames(mixed_df) <- mixed_raw[[1]]

# "TAK" / "NIE" are the YES / NO values as stored in the data file
yes_no_cols <- c("x2", "x4", "x8")
mixed_df[yes_no_cols] <- lapply(mixed_df[yes_no_cols], factor, levels = c("TAK", "NIE"))
str(mixed_df)

gower_dist <- cluster::daisy(mixed_df, metric = "gower")
round(as.matrix(gower_dist), 3)

hc_gower <- hclust(gower_dist, method = "complete")
hc_gower$height

plot(hc_gower, main = "", xlab = "Politicians", ylab = "Linkage height",
     cex = 0.8, ylim = c(0, max(hc_gower$height)))
clusters_gower <- cutree(hc_gower, k = 2)
clusters_gower
rect.hclust(hc_gower, k = 2, border = c("navy", "tomato"))


# 7. CORRELATION STRUCTURE AND SUITABILITY FOR FACTOR ANALYSIS ----------------

## 7.1 Item means (in the report: add a table explaining what each x_i stands for)
item_means <- data.frame(item = colnames(items), mean = colMeans(items))
item_means

## 7.2 Internal consistency of the whole questionnaire
DescTools::CronbachAlpha(items)

## 7.3 Spearman correlation matrix
cor_spearman <- cor(items, method = "spearman")
cor_rounded  <- round(cor_spearman, 2)
cor_rounded

corrplot(cor_rounded, method = "square", diag = FALSE, order = "hclust",
         addrect = 3, rect.col = "darkblue", rect.lwd = 3, tl.pos = "d")
corrplot(cor_rounded, method = "ellipse", order = "AOE", type = "upper")
pairs(items)

## 7.4 Bartlett's test and Kaiser-Meyer-Olkin measure
# H0 of Bartlett's test: the correlation matrix is an identity matrix.
# Rejecting H0 means the variables are correlated, so factor analysis makes sense.
# Items with low MSA values (e.g. x2) may be removed - then re-check KMO.
psych::cortest.bartlett(items)
psych::KMO(items)


# 8. NUMBER OF FACTORS / COMPONENTS -------------------------------------------

n_obs <- nrow(items)

psych::vss(items)
psych::fa.parallel(items)
psych::fa.parallel(items, fa = "fa", fm = "pa",
                   main = "Scree plot (Pearson, principal axis)")
abline(h = 1, col = "green", lwd = 2, lty = 2)
psych::fa.parallel(items, fa = "fa", fm = "ml",
                   main = "Scree plot (Pearson, maximum likelihood)")
abline(h = 1, col = "green", lwd = 2, lty = 2)
psych::fa.parallel(cor_spearman, fa = "fa", fm = "pa", n.obs = n_obs,
                   main = "Scree plot (Spearman, principal axis)")
abline(h = 1, col = "green", lwd = 2, lty = 2)
psych::fa.parallel(cor_spearman, fa = "fa", fm = "minres", n.obs = n_obs,
                   main = "Scree plot (Spearman, minres)")
abline(h = 1, col = "green", lwd = 2, lty = 2)


# 9. PRINCIPAL COMPONENT ANALYSIS ---------------------------------------------

pca_10_none     <- psych::principal(items, nfactors = 10, rotate = "none", cor = "cor")
pca_2_none      <- psych::principal(items, nfactors = 2,  rotate = "none", cor = "cor")
pca_2_varimax   <- psych::principal(items, nfactors = 2,  rotate = "varimax", cor = "cor")
pca_2_oblimin   <- psych::principal(items, nfactors = 2,  rotate = "oblimin", cor = "cor")
pca_sp_none     <- psych::principal(cor_spearman, nfactors = 2, rotate = "none")
pca_sp_varimax  <- psych::principal(cor_spearman, nfactors = 2, rotate = "varimax")

pca_10_none
pca_2_none
pca_2_varimax;  psych::fa.diagram(pca_2_varimax)
pca_2_oblimin;  psych::fa.diagram(pca_2_oblimin)    # oblique rotation
pca_sp_none;    psych::fa.diagram(pca_sp_none)      # Spearman matrix
pca_sp_varimax; psych::fa.diagram(pca_sp_varimax)   # Spearman matrix + rotation

# Compare the diagrams / methods above and pick one for interpretation.
pca_fit <- prcomp(items, scale. = TRUE)
factoextra::fviz_pca_biplot(pca_fit)


# 10. EXPLORATORY FACTOR ANALYSIS ---------------------------------------------

## Maximum likelihood
ml_sp_none    <- psych::fa(cor_spearman, nfactors = 2, rotate = "none",    fm = "ml", residuals = TRUE)
ml_sp_varimax <- psych::fa(cor_spearman, nfactors = 2, rotate = "varimax", fm = "ml", residuals = TRUE)
ml_raw_varimax <- psych::fa(items,       nfactors = 2, rotate = "varimax", fm = "ml", residuals = TRUE)
ml_sp_none; ml_sp_varimax; ml_raw_varimax
psych::fa.diagram(ml_sp_varimax)
psych::fa.diagram(ml_raw_varimax)
biplot(ml_raw_varimax)

## Principal axis
pa_sp_none    <- psych::fa(cor_spearman, nfactors = 2, rotate = "none",    fm = "pa", residuals = TRUE)
pa_sp_varimax <- psych::fa(cor_spearman, nfactors = 2, rotate = "varimax", fm = "pa", residuals = TRUE)
pa_raw_varimax <- psych::fa(items,       nfactors = 2, rotate = "varimax", fm = "pa", residuals = TRUE)
pa_sp_none; pa_sp_varimax; pa_raw_varimax
psych::fa.diagram(pa_sp_varimax)
psych::fa.diagram(pa_raw_varimax)
biplot(pa_sp_varimax)

## Minimum residual (minres) with different rotations
for (rotation in c("none", "varimax", "quartimax", "equamax")) {
  fit <- psych::fa(cor_spearman, nfactors = 2, rotate = rotation, fm = "minres")
  cat("\n--- minres,", rotation, "rotation ---\n")
  print(fit)
  psych::fa.diagram(fit)
}


# 11. CORRESPONDENCE ANALYSIS (TRAITS x POLITICIANS) --------------------------
#     srednie.xlsx: mean ratings, 17 traits (rows) x 9 politicians (columns);
#     first column = trait ID (dropped).

trait_labels <- c("HONEST", "TRUSTWORTHY", "COMPETENT", "CONSISTENT",
                  "CHARISMATIC", "COMMUNICATIVE", "CULTURED", "LOYAL",
                  "RESPONSIBLE", "MEDIA-SAVVY", "INNOVATIVE", "TOLERANT",
                  "DIPLOMATIC", "RESPECTED", "ELOQUENT", "INFLUENTIAL",
                  "ATTRACTIVE")

means_raw   <- read_data("srednie.xlsx")
score_table <- as.matrix(means_raw[, -1])
stopifnot(nrow(score_table) == length(trait_labels))   # check the row order!
rownames(score_table) <- trait_labels
score_table

## 11.1 Relative frequencies and profiles
relative_freq  <- prop.table(score_table)               # matrix P
row_profiles   <- prop.table(score_table, margin = 1)
col_profiles   <- prop.table(score_table, margin = 2)
average_profile <- rowSums(relative_freq)               # average column profile (row masses)

addmargins(relative_freq)
addmargins(row_profiles, margin = 2)
addmargins(col_profiles, margin = 1)

## 11.2 Politician profiles: deviation of each politician's trait profile from
##      the average profile. Positive values = trait more important for that politician.
# NOTE: the original script subtracted the row profile from the column profile
#       (col_profiles - row_profiles). If your report is based on that version,
#       use this line instead:  politician_profiles <- col_profiles - row_profiles
politician_profiles <- col_profiles - average_profile

profiles_long <- as.data.frame(politician_profiles) %>%
  mutate(Trait = rownames(politician_profiles)) %>%
  pivot_longer(-Trait, names_to = "Politician", values_to = "Value") %>%
  mutate(Trait = factor(Trait, levels = trait_labels))

ggplot(profiles_long, aes(x = Trait, y = Value, color = Politician, group = Politician)) +
  geom_line() +
  geom_point() +
  labs(title = "Politician profiles", x = "Trait", y = "Deviation from average profile") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

## 11.3 Chi-square test and association coefficients
# NOTE: the table contains mean scores, not counts - interpret with caution.
chi_ca <- chisq.test(score_table)
chi_ca
chi_ca$expected

DescTools::TschuprowT(score_table)
DescTools::CramerV(score_table)
C_ca <- DescTools::ContCoef(score_table)
corrected_contingency(C_ca, n_rows = nrow(score_table), n_cols = ncol(score_table))

## 11.4 Correspondence analysis
ca_fit <- ca::ca(score_table, graph = FALSE)
ca_fit
ca_fit$sv                                   # singular values
plot(ca_fit)

eigenvalues <- factoextra::get_eigenvalue(ca_fit)
eigenvalues
total_inertia <- sum(eigenvalues[, 1])      # sum of eigenvalues
total_inertia
# The first two dimensions should explain most of the inertia (~90% in the report).
factoextra::fviz_screeplot(ca_fit, addlabels = TRUE, ylim = c(0, 50))

## 11.5 Row results (traits)
ca_rows <- factoextra::get_ca_row(ca_fit)
ca_fit$rowmass                              # row masses
ca_rows$coord                               # coordinates
ca_rows$cos2                                # squared cosines
rowSums(ca_rows$cos2[, 1:2])                # quality of representation in 2 dimensions
ca_rows$contrib                             # contribution to dimensions
ca_rows$inertia
(ca_rows$inertia / sum(ca_rows$inertia)) * 100   # relative inertia (%)

## 11.6 Column results (politicians)
ca_cols <- factoextra::get_ca_col(ca_fit)
ca_fit$colmass
ca_cols$coord
ca_cols$cos2
rowSums(ca_cols$cos2[, 1:2])                # quality of representation in 2 dimensions
ca_cols$contrib
ca_cols$inertia
(ca_cols$inertia / sum(ca_cols$inertia)) * 100

## 11.7 Visualisation
factoextra::fviz_ca_biplot(ca_fit, repel = TRUE)

factoextra::fviz_ca_row(ca_fit, col.row = "cos2",
                        gradient.cols = c("#00AFBB", "#E7B800", "#FC4E77"),
                        repel = TRUE)

gplots::balloonplot(as.table(score_table), main = "", xlab = "", ylab = "",
                    label = TRUE, show.margins = FALSE)
