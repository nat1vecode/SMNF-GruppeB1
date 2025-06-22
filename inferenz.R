library(dplyr)
library(psych)

data_combined <- read.csv("data_combined.csv")
web_app_data <- read.csv("web_app_data.csv")



filtered_data_combined <- data_combined %>%
  filter(age > 18 & age < 120) 




datengesamt <- filtered_data_combined %>% full_join(web_app_data, by = "participantId")


daten_clean <- datengesamt %>%
  filter(!is.na(confidence), !is.na(system), !is.na(time_on_task), !is.na(aiknowledge))

deskriptiv <- daten_clean %>%
  summarise(
    n_confidence = sum(!is.na(confidence)),
    mean_conf = mean(confidence),
    sd_conf = sd(confidence),
    median_conf = median(confidence),
    min_conf = min(confidence),
    max_conf = max(confidence),
    
    n_time = sum(!is.na(time_on_task)),
    mean_time = mean(time_on_task),
    sd_time = sd(time_on_task),
    median_time = median(time_on_task),
    min_time = min(time_on_task),
    max_time = max(time_on_task),
    
    n_ai = sum(!is.na(aiknowledge)),
    mean_ai = mean(aiknowledge),
    sd_ai = sd(aiknowledge),
    median_ai = median(aiknowledge),
    min_ai = min(aiknowledge),
    max_ai = max(aiknowledge)
  )

# Schritt 1: Annahme, dein summarise-Ergebnis heißt 'deskriptiv'
# Beispiel: deskriptiv <- daten_clean %>% summarise(...)

# Schritt 2: Definiere die Variablennamen als Vektor
variablen <- c("confidence", "time_on_task", "aiknowledge")

# Schritt 3: Werte aus 'deskriptiv' auslesen
n <- c(deskriptiv$n_confidence, deskriptiv$n_time, deskriptiv$n_ai)
mean <- c(deskriptiv$mean_conf, deskriptiv$mean_time, deskriptiv$mean_ai)
sd <- c(deskriptiv$sd_conf, deskriptiv$sd_time, deskriptiv$sd_ai)
median <- c(deskriptiv$median_conf, deskriptiv$median_time, deskriptiv$median_ai)
min <- c(deskriptiv$min_conf, deskriptiv$min_time, deskriptiv$min_ai)
max <- c(deskriptiv$max_conf, deskriptiv$max_time, deskriptiv$max_ai)

# Schritt 4: Dataframe bauen
deskriptiv_tabelle <- data.frame(
  Variable = variablen,
  n = n,
  mean = round(mean, 2),
  sd = round(sd, 2),
  median = round(median, 2),
  min = min,
  max = max
)

# Schritt 5: Tabelle ausgeben
print(deskriptiv_tabelle)







abs_haeufigkeit <- table(daten_clean$system)
rel_haeufigkeit <- prop.table(table(daten_clean$system)) * 100

metrische_variablen <- daten_clean %>%
  select(confidence, time_on_task, aiknowledge)


metrische_statistik <- describe(metrische_variablen)[, c("n", "mean", "median", "min", "max", "sd")]

metrische_statistik <- round(metrische_statistik, 2)

kategoriale_tabelle <- data.frame(
  System = names(abs_haeufigkeit),
  Anzahl = as.vector(abs_haeufigkeit),
  Prozent = round(as.vector(rel_haeufigkeit), 1)
)

kategoriale_tabelle

# Mittelwert der Confidence-Werte pro User für die Baseline-Bedingung berechnen
baseline_means <- datengesamt %>%
  filter(system == "B") %>%                # Nur Baseline-Durchläufe
  group_by(participantId) %>%                     # Gruppieren nach Nutzer
  summarise(mean_confidence_baseline = mean(confidence, na.rm = TRUE))  # Mittelwert berechnen

ki_means <- datengesamt %>%
  filter(system %in% c("E", "R")) %>%  # Nur KI-Systeme
  group_by(participantId) %>%
  summarise(mean_confidence_ki = mean(confidence, na.rm = TRUE))

vergleich <- baseline_means %>%
  full_join(ki_means, by = "participantId")

vergleich 

summary(vergleich)


t.test(vergleich$mean_confidence_baseline, vergleich$mean_confidence_ki, paired = TRUE)


  