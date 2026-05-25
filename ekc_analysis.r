## 1. CHARGEMENT DES LIBRAIRIES
library(readxl)
library(dplyr)
library(ggplot2)
library(tidyr)
library(plm)      
library(stargazer) 
library(ggplot2)
library(gridExtra)

## 2. IMPORTATION ET NETTOYAGE DES DONNEES
df_first <- read_excel("data first part.xlsx", sheet = "Data")
df_first <- na.omit(df_first) # Suppression des valeurs manquantes

# Importation des données format panel (pour le modèle EKC)
df_ekc <- read_excel("ekc_data.xlsx", sheet = "data")

# Création des variables logarithmiques pour le modèle
df_ekc <- df_ekc %>%
  mutate(
    ln_gdp = log(gdp),
    ln_ggem = log(ggem),
    ln_gdp_sq = log(gdp)^2
  )

## 3. STATISTIQUES DESCRIPTIVES
cat("\n--- STATISTIQUES DESCRIPTIVES (Moyenne & Ecart-Type) ---\n")

# Calcul des moyennes et Ecarts-types pour chaque colonne (sauf Time)
stats_desc <- df_first %>%
  select(-Time) %>%
  summarise(across(everything(), list(mean = mean, sd = sd), .names = "{.col}_{.fn}")) %>%
  pivot_longer(cols = everything(), names_to = "Variable", values_to = "Valeur")

print(stats_desc)

## 4. ANALYSE GRAPHIQUE (Time Series & Scatter Plots)
# --- Séries Temporelles (Double Axe) ---
plot_double_axis <- function(time, left, right, left_lab, right_lab, main) {
  par(mar = c(5, 4, 4, 5) + 0.1) # Augmenter la marge droite
  plot(time, left, type = "l", col = "blue", lwd = 2,
       xlab = "Année", ylab = left_lab, main = main)
  par(new = TRUE)
  plot(time, right, type = "l", col = "red", lwd = 2, lty = 2,
       axes = FALSE, xlab = "", ylab = "")
  axis(4)
  mtext(right_lab, side = 4, line = 3)
  legend("topleft", legend = c(left_lab, right_lab),
         col = c("blue", "red"), lty = c(1, 2), lwd = 2, bty = "n", cex = 0.8)
}

# Affichage des graphiques pour les 4 pays
par(mfrow = c(2, 2)) # Grille 2x2
plot_double_axis(df_first$Time, df_first$gdp_bra, df_first$ggem_bra, "PIB", "GES", "Brésil")
plot_double_axis(df_first$Time, df_first$gdp_rus, df_first$ggem_rus, "PIB", "GES", "Russie")
plot_double_axis(df_first$Time, df_first$gdp_ind, df_first$ggem_ind, "PIB", "GES", "Inde")
plot_double_axis(df_first$Time, df_first$gdp_chi, df_first$ggem_chi, "PIB", "GES", "Chine")
par(mfrow = c(1, 1)) # Reset grille

# --- Scatter Plots (Relation brute PIB vs GES) ---
par(mfrow = c(2, 2))
plot(df_first$gdp_bra, df_first$ggem_bra, main="Brésil: PIB vs GES", xlab="PIB", ylab="GES", pch=19, col="darkgreen")
plot(df_first$gdp_chi, df_first$ggem_chi, main="Chine: PIB vs GES", xlab="PIB", ylab="GES", pch=19, col="red")
plot(df_first$gdp_rus, df_first$ggem_rus, main="Russie: PIB vs GES", xlab="PIB", ylab="GES", pch=19, col="blue")
plot(df_first$gdp_ind, df_first$ggem_ind, main="Inde: PIB vs GES", xlab="PIB", ylab="GES", pch=19, col="orange")
par(mfrow = c(1, 1))


# Création du Graphique 2 : Relation ln(GGEM) vs ln(GDP)Â²
ggplot(df_ekc, aes(x = ln_gdp, y = ln_ggem)) +
  geom_point(aes(color = country), alpha = 0.7, size = 2) +
  geom_smooth(method = "loess", color = "black", size = 0.8, se = FALSE) +
  facet_wrap(~ country, scales = "free") + # Division par pays
  theme_bw() +
  labs(title = "Trajectoire des Emissions par Pays",
       subtitle = "Relation Log-Log avec tendance linéaire ajustée",
       x = "Log(PIB par habitant)",
       y = "Log(Emissions GES)",
       color = "Pays") +
  theme(strip.background = element_rect(fill = "#f0f0f0"),
        strip.text = element_text(face = "bold", size = 11),
        legend.position = "none")


## 5. MODELISATION ECONOMETRIQUE (EKC)
# Estimation du modèle Within (Effets fixes Pays + Année)
ekc_twoways <- plm(ln_ggem ~ ln_gdp + I(ln_gdp^2), 
                   data = df_ekc, 
                   index = c("country", "time"), 
                   model = "within", 
                   effect = "twoways")

summary(ekc_twoways)


## 6. CALCUL DU POINT DE RETOURNEMENT (TURNING POINT)
# Récupération des coefficients
beta1 <- coef(ekc_twoways)["ln_gdp"]       # Coefficient terme linéaire
beta2 <- coef(ekc_twoways)["I(ln_gdp^2)"]  # Coefficient terme carré

# Vérification de la forme en U inversé (beta2 doit etre négatif)
is_inverted_U <- beta2 < 0

# Calcul du point de retournement (en log puis en niveau)
# Formule sommet parabole : x = -b / 2a
turning_point_log <- -beta1 / (2 * beta2)
turning_point_usd <- exp(turning_point_log)

cat("\n--- ANALYSE DU POINT DE RETOURNEMENT ---\n")
cat("Coefficient Beta 2 (Quadratique) :", beta2, "\n")
cat("Forme en U inversé validée ? :", is_inverted_U, "\n")
cat("Point de retournement (Log PIB) :", turning_point_log, "\n")
cat("Point de retournement (PIB $)   :", round(turning_point_usd, 2), "$\n")


## 7. VISUALISATION FINALE : COURBE EKC THEORIQUE vs REALITE
# Création d'une séquence de PIB fictive pour tracer la courbe lisse
gdp_seq <- seq(from = min(df_ekc$gdp), to = max(df_ekc$gdp), length.out = 300)
ln_gdp_seq <- log(gdp_seq)

# Prédiction théorique (centrÃ©e sur la moyenne des données pour l'ajustement visuel)
# Note : C'est une visualisation des effets marginaux
y_pred_theoretical <- beta1 * ln_gdp_seq + beta2 * ln_gdp_seq^2

# Ajustement constant pour aligner la courbe théorique sur le nuage de points
mean_y_actual <- mean(df_ekc$ln_ggem)
mean_y_pred   <- mean(beta1 * df_ekc$ln_gdp + beta2 * df_ekc$ln_gdp^2)
constant_adj  <- mean_y_actual - mean_y_pred

plot_data <- data.frame(
  gdp = gdp_seq,
  ln_ggem_pred = y_pred_theoretical + constant_adj
)

# Graphique final
p <- ggplot() +
  # Points réels
  geom_point(data = df_ekc, aes(x = gdp, y = ln_ggem, color = country), alpha = 0.6, size = 2) +
  
  # Courbe de régression EKC
  geom_line(data = plot_data, aes(x = gdp, y = ln_ggem_pred), color = "black", size = 1.2) +
  
  # Ligne verticale du point de retournement
  geom_vline(xintercept = turning_point_usd, linetype = "dashed", color = "red") +
  
  # Annotation
  annotate("text", x = turning_point_usd, y = max(df_ekc$ln_ggem), 
           label = paste("Point de retournement :\n", round(turning_point_usd, 0), "$"), 
           hjust = 1.1, vjust = 1, color = "red", fontface = "bold") +
  
  # Mise en forme
  scale_x_log10(labels = scales::dollar_format()) +
  labs(title = "Courbe Environnementale de Kuznets (EKC) - Pays BRIC",
       subtitle = paste("Estimation Fixed Effects. Turning Point:", round(turning_point_usd,0), "$"),
       x = "PIB par habitant (Log Scale)",
       y = "Log(Emissions de CO2)",
       color = "Pays") +
  theme_minimal()

print(p)


########


install.packages("neuralnet")

library(readxl)
library(dplyr)
library(neuralnet)
library(ggplot2)


## 8. PREPARATION DES DONNEES
df_ekc <- read_excel("ekc_data.xlsx", sheet = "data")

# Les réseaux de neurones sont très sensibles A l'échelle des données.
# Il FAUT normaliser les données (Min-Max Scaling) pour qu'elles soient entre 0 et 1.

normalize <- function(x) {
  return ((x - min(x)) / (max(x) - min(x)))
}

# Création d'un dataframe normalisé pour l'entrainement
data_nn <- df_ekc %>%
  select(gdp, ggem) %>%
  mutate(
    gdp_norm = normalize(gdp),
    ggem_norm = normalize(ggem)
  )

# On garde les valeurs Min/Max pour "dénormaliser" (remettre en $) plus tard
min_gdp <- min(df_ekc$gdp)
max_gdp <- max(df_ekc$gdp)
min_ggem <- min(df_ekc$ggem)
max_ggem <- max(df_ekc$ggem)


## 9. ENTRAINEMENT DU RESEAU DE NEURONES
set.seed(123) # Pour avoir des résultats reproductibles

# Configuration du réseau :
# - Entrée : PIB (normalisé)
# - Sortie : GES (normalisé)
# - hidden = c(3, 2) : Deux couches cachées (une de 3 neurones, une de 2)
# - linear.output = TRUE : Car on fait de la rÃ©gression (prédire un chiffre), pas de la classification.

nn_model <- neuralnet(ggem_norm ~ gdp_norm, 
                      data = data_nn, 
                      hidden = c(4, 2),  # Vous pouvez changer la structure ici
                      linear.output = TRUE,
                      stepmax = 1e6)

# Affichage du graphique du réseau (Architecture)
plot(nn_model, rep = "best")


## 10. PREDICTION ET DENORMALISATION
# On génère une séquence de PIB (de min à max) pour voir la courbe apprise par le réseau
test_gdp_seq <- seq(min(df_ekc$gdp), max(df_ekc$gdp), length.out = 100)
test_gdp_norm <- (test_gdp_seq - min_gdp) / (max_gdp - min_gdp) # Normalisation

# Prédiction par le réseau de neurones
pred_nn_norm <- compute(nn_model, data.frame(gdp_norm = test_gdp_norm))$net.result

# Dénormalisation des prédictions (revenir à l'échelle réelle)
pred_nn_real <- pred_nn_norm * (max_ggem - min_ggem) + min_ggem

# Création dataframe pour le graphique
df_pred <- data.frame(gdp = test_gdp_seq, ggem_pred = pred_nn_real)


## 11. VISUALISATION : RESULTATS DU RESEAU DE NEURONES
ggplot() +
  # Points réels (Données brutes)
  geom_point(data = df_ekc, aes(x = gdp, y = ggem), color = "grey50", alpha = 0.6) +
  
  # Courbe apprise par le Réseau de Neurones (Rouge)
  geom_line(data = df_pred, aes(x = gdp, y = ggem_pred), color = "red", size = 1.5) +
  
  labs(title = "Approximation EKC par Réseau de Neurones",
       subtitle = "Le réseau apprend la relation non-linéaire sans formule pré-définie",
       x = "PIB par habitant ($)",
       y = "Emissions de GES (Tonnes/hab)") +
  theme_minimal()


########


install.packages("neuralnet")

library(readxl)
library(dplyr)
library(neuralnet)
library(ggplot2)


## 12. PREPARATION DES DONNEES (NORMALISATION)
# Les réseaux de neurones ne fonctionnent pas bien avec des chiffres énormes (PIB > 10000).
# Il est IMPERATIF de normaliser les données entre 0 et 1 (Min-Max Scaling).

df_ekc <- read_excel("ekc_data.xlsx", sheet = "data")

# Fonction de normalisation
normalize <- function(x) {
  return ((x - min(x)) / (max(x) - min(x)))
}

# On garde les valeurs Min/Max pour "dénormaliser" (remettre en $) à la fin
min_gdp <- min(df_ekc$gdp)
max_gdp <- max(df_ekc$gdp)
min_ggem <- min(df_ekc$ggem)
max_ggem <- max(df_ekc$ggem)

# Création du dataset d'entrainement normalisé
df_nn <- df_ekc %>%
  select(gdp, ggem) %>%
  mutate(
    gdp_norm = normalize(gdp),
    ggem_norm = normalize(ggem)
  )


## 13. ENTRAINEMENT DU RESEAU DE NEURONES
set.seed(123) # Fixer l'aléatoire pour avoir toujours le meme résultat

# Création du modèle
# hidden = c(3, 2) : 2 couches cachées (une de 3 neurones, une de 2)
# linear.output = TRUE : Car on veut prédire un chiffre (régression), pas une classe.
nn_model <- neuralnet(ggem_norm ~ gdp_norm, 
                      data = df_nn, 
                      hidden = c(3, 2), 
                      linear.output = TRUE,
                      stepmax = 1e6) # Augmente le nb d'étapes max pour assurer la convergence

# Affichage de l'architecture du réseau (Input -> Hidden Layers -> Output)
plot(nn_model, rep = "best", main = "Architecture du RÃ©seau de Neurones")


## 14. GENERATION DE LA COURBE PREDITE
# On crée une séquence de PIB fictive (de 0 à 1) pour tracer la ligne rouge lisse
gdp_seq_norm <- seq(0, 1, length.out = 200)
test_data <- data.frame(gdp_norm = gdp_seq_norm)

# Prédiction par le réseau
nn_pred <- compute(nn_model, test_data)

# Dénormalisation des résultats (Retour aux vraies valeurs $ et Tonnes)
# Formule : Valeur_Norm * (Max - Min) + Min
gdp_seq_real <- gdp_seq_norm * (max_gdp - min_gdp) + min_gdp
ggem_pred_real <- nn_pred$net.result * (max_ggem - min_ggem) + min_ggem

# Création du dataframe final pour le graphique
df_prediction <- data.frame(
  gdp = gdp_seq_real,
  ggem_pred = ggem_pred_real
)


## 15. GRAPHIQUE FINAL : NEURAL NETWORK vs REALITE
ggplot() +
  # A. Les points réels (Nuage de points gris)
  geom_point(data = df_ekc, aes(x = gdp, y = ggem), color = "grey50", alpha = 0.5, size = 2) +
  
  # B. La courbe apprise par le Réseau de Neurones (Ligne Rouge)
  geom_line(data = df_prediction, aes(x = gdp, y = ggem_pred), color = "red", size = 1.5) +
  
  # C. Mise en forme
  labs(title = "Approximation EKC par Réseau de Neurones (Neural Net)",
       subtitle = "Le modèle apprend la non-linéarité sans formule mathématique imposée",
       x = "PIB par Habitant ($)",
       y = "Emissions de GES (Tonnes/hab)") +
  theme_minimal() +
  theme(plot.title = element_text(face="bold"))


# Récupération des données et du modèle
df <- read_excel("ekc_data.xlsx", sheet = "data")
df$ln_ggem <- log(df$ggem)
df$ln_gdp  <- log(df$gdp)

# Estimation du modèle (si pas déjà fait)
ekc_model <- plm(ln_ggem ~ ln_gdp + I(ln_gdp^2), 
                 data = df, 
                 index = c("country", "time"), 
                 model = "within", 
                 effect = "twoways")

# Extraction des coefficients
b1 <- coef(ekc_model)["ln_gdp"]       # Devrait etre ~ 1.11
b2 <- coef(ekc_model)["I(ln_gdp^2)"]  # Devrait etre ~ -0.049

# Calcul de l'élasticité pour chaque observation
# Formule : E = b1 + 2 * b2 * ln(GDP)
df$elasticity <- b1 + 2 * b2 * df$ln_gdp


# Graphique : Evolution de l'élasticité dans le temps
ggplot(df, aes(x = time, y = elasticity, color = country)) +
  geom_line(size = 1.2) +
  # Zone de Découplage Relatif (entre 0 et 1)
  geom_rect(aes(xmin = -Inf, xmax = Inf, ymin = 0, ymax = 1), fill = "white", 
            alpha = 0.005, color = NA) +
  
  geom_hline(yintercept = 1, linetype = "dashed", color = "black") + # Seuil critique
  geom_hline(yintercept = 0, linetype = "dashed", color = "darkgreen") + # Objectif
  
  labs(title = "Dynamique du Découplage PIB/GES",
       subtitle = "Evolution de l'élasticité-revenu des Emissions",
       y = "Elasticité (Var% Emissions / Var% PIB)",
       x = "Année") +
  theme_minimal()
