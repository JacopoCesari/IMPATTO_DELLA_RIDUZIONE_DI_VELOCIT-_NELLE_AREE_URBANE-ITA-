setwd("C:/Users/jacop/OneDrive - Alma Mater Studiorum Università di Bologna/Studio/Scienze Statistiche LT/Tesi/Incidenti/Toronto")

library(readr)
library(dplyr)
library(car)
library(ResourceSelection)
library(pscl)
library(nnet)
library(car)
library(pROC)
library(ggplot2)
library(gridExtra)
library(margins)
library(boot)
library(tidyverse)
library(skimr)
library(lubridate)
library(reshape2)
library(forcats)
library(ggmosaic)

Serious_Collisions <- read_csv("Serious Collisions.csv")
table(Serious_Collisions$RDSFCOND)

###PULIZIA MODELLO###

short_data <- Serious_Collisions[,c(3,8,11:15,17,19,22,24,25,40:44)]
sum(is.na(short_data))
str(short_data)
data <- short_data %>%
  filter(
    ROAD_CLASS %in% c("Local","Collector","Laneway","Minor Arterial") &
    !VISIBILITY %in% c("None") &
    !LIGHT %in% c("None") &
    !ACCLASS %in% c("None") &
    !INJURY %in% c("Unknown") &
    !VEHTYPE %in% c("None","Unknown","Other") &
    !DRIVCOND %in% c("Unknown") 
  ) %>%
  mutate(
    TRAFFCTL = case_when(
      TRAFFCTL %in% c("No Control","None") ~ 0, # None
      TRUE ~ 1 # Some
    ),
    BADWEATHER = case_when(
      VISIBILITY %in% c("Clear") ~ 0, 
      TRUE ~ 1 # Unclear
    ),
    DARKNESS = case_when(
      LIGHT %in% c("Daylight") ~ 0, 
      TRUE ~ 1 # Night
    ),
    RDSFDANGER = case_when(
      RDSFCOND %in% c("Dry","None") ~ 0, #Safe
      TRUE ~ 1 #Dangerous
    ),
    INVTYPE = case_when(
      INVTYPE %in% c("Cyclist", "Cyclist Passenger") ~ "Bicycle",
      INVTYPE %in% c("Driver", "Driver - Not Hit","Passenger","Vehicle Owner")  ~ "Car",
      INVTYPE %in% c("Truck Driver", "Trailer Owner") ~ "Truck",
      INVTYPE %in% c("Moped Driver", "Motorcycle Driver", "Moped Passenger", "Motorcycle Passenger") ~ "Motorcycle",
      INVTYPE %in% c("Pedestrian", "Pedestrian - Not Hit","Wheelchair") ~ "Pedestrian",
      INVTYPE %in% c("None","Witness","In-Line Skater", "Other Property Owner","Other" ) ~ "Other",
      TRUE ~ NA_character_
    ),
    VEHTYPE = case_when(
      VEHTYPE %in% c("Automobile, Station Wagon", "Passenger Van", "Taxi", "Police Vehicle") ~ "Car",
      VEHTYPE %in% c("Bicycle", "Rickshaw") ~ "Bicycle",
      VEHTYPE %in% c("Moped", "Motorcycle", "Off Road - 2 Wheels", "Off Road - 4 Wheels", "Off Road - Other") ~ "Motorcycle",
      VEHTYPE %in% c("Bus (Other) (Go Bus, Gray Coa", "Intercity Bus", "Municipal Transit Bus (TTC)", "School Bus", "Street Car") ~ "Bus",
      VEHTYPE %in% c("Fire Vehicle", "Other Emergency Vehicle", "Ambulance","Construction Equipment", "Delivery Van", "Tow Truck", "Truck-Tractor", "Truck - Car Carrier", "Truck - Closed (Blazer, etc)", "Truck - Dump", "Truck - Open", "Truck - Tank", "Truck (other)") ~ "Truck/Lorry",
      TRUE ~ NA_character_
    ),
    SPEEDING = case_when(
      SPEEDING %in% c("None") ~ 0, 
      TRUE ~ 1 # Yes
    ),
    WRONGDRIV = case_when(
      AG_DRIV %in% c("None") ~ 0, 
      TRUE ~ 1 # Yes
    ),
    REDLIGHT = case_when(
      REDLIGHT %in% c("None") ~ 0, 
      TRUE ~ 1 # Yes
    ),
    ALCOHOL = case_when(
      ALCOHOL %in% c("None") ~ 0, 
      TRUE ~ 1 # Yes
    ),
    DISABILITY = case_when(
      DISABILITY %in% c("None") ~ 0, 
      TRUE ~ 1 # Yes
    ),
    DRIVCOND = case_when(
      DRIVCOND %in% c("None","Normal") ~ 0, 
      TRUE ~ 1 # Some
    ),
    DRIVACT = case_when(
      DRIVACT %in% c("None","Driving Properly") ~ 0, 
      TRUE ~ 1 #Wrong
    )
  ) %>%
  select(-VISIBILITY, -LIGHT, -RDSFCOND, -AG_DRIV
          )

data$VEHTYPE <- factor(data$VEHTYPE, levels = c("Car", "Bicycle", "Bus", "Truck/Lorry", "Motorcycle"))
data$ACCLASS <- factor(data$ACCLASS, levels = c("Non-Fatal Injury", "Fatal"))
data$ROAD_CLASS <- factor(data$ROAD_CLASS, levels = c("Local", "Collector", "Laneway", "Minor Arterial"))
data$INVTYPE <- factor(data$INVTYPE, levels = c("Car", "Truck", "Other","Motorcycle", "Bicycle", "Pedestrian"))
data$INJURY <- factor(data$INJURY, levels = c("None", "Minor", "Minimal", "Major", "Fatal"), ordered = TRUE)

sum(is.na(data))
data <- na.omit(data)










###ANALISI DESCRITTIVA###

# Definizione del tema personalizzato
theme_custom <- theme_minimal() +
  theme(
    text = element_text(family = "Arial", color = "#333333"),
    plot.title = element_text(size = 14, face = "bold"),
    axis.title = element_text(size = 12),
    axis.text = element_text(size = 10),
    legend.title = element_text(size = 12),
    legend.text = element_text(size = 10)
  )


excel_palette <- c("#008080", "#70DBDB", "#FFC000", "#FFE699", "#974706", "#9CAF88")



gradation_palette<- c(
  "#70DBDB", # Teal chiaro (estremo chiaro)
  "#4dd0e1", # Teal chiaro e vivace
  "#26c6da", # Teal molto chiaro e brillante
  "#00bcd4", # Teal brillante
  "#009688", # Teal scuro ma più chiaro e brillante
  "#008080"
)
gradation_palette2 <- c(
  "#B2EBF2",  # Teal chiarissimo (aggiunto)
  "#70DBDB",  # Teal chiaro
  "#4dd0e1",  # Teal chiaro e vivace
  "#00bcd4",  # Teal brillante
  "#009688",  # Teal scuro ma più chiaro e brillante
  "#008080"   # Teal scuro
)
str(data)
summary(data)
skim(data)
table(data$INVTYPE)

#Analisi temporale
yearly_counts <- data %>%
  mutate(Year = lubridate::year(DATE)) %>%
  filter(Year != 2023) %>%
  group_by(Year) %>%
  summarise(Total = n(),
            Serious = sum(INJURY %in% c("Major","Fatal")))

ggplot(yearly_counts, aes(x = Year)) +
  geom_line(aes(y = Total, color = "Totale incidenti")) +
  geom_point(aes(y = Total, color = "Totale incidenti")) +
  geom_line(aes(y = Serious, color = "Incidenti gravi/fatali")) +
  geom_point(aes(y = Serious, color = "Incidenti gravi/fatali")) +
  geom_smooth(aes(y = Total), method = "lm", se = FALSE, linetype = "dashed", color = excel_palette[2], show.legend = FALSE) +
  geom_smooth(aes(y = Serious), method = "lm", se = FALSE, linetype = "dashed", color = excel_palette[6], show.legend = FALSE) +
  geom_vline(xintercept = 2016, color = "red", linetype = "dashed") +
  geom_vline(xintercept = 2019, color = "red", linetype = "dashed") +
  theme_custom +
  labs(x = "Anno",
       y = "Numero di incidenti",
       color = "Legenda") +
  ggtitle("Incidenti stradali a Toronto") + 
  scale_x_continuous(breaks = seq(min(yearly_counts$Year), max(yearly_counts$Year), by = 2)) +
  scale_color_manual(values = c("Totale incidenti" = excel_palette[1],
                                "Incidenti gravi/fatali" = excel_palette[5])) +
  theme(legend.position = "bottom",
        legend.text = element_text(size = 12),
        plot.title = element_text(hjust = 0.5, size = 14))

#Dati Europei
years <- c(2013:2022)
values <- c(24213, 24128, 24358, 23888, 23392, 23328, 22756, 18833, 19917, 20653)

dataEU <- data.frame(Year = years, Value = values)

ggplot(dataEU, aes(x = Year, y = Value)) +
  geom_line(color = excel_palette[5]) +
  geom_point(color = excel_palette[5]) +
  geom_smooth(method = "lm", se = FALSE, linetype = "dashed", color = excel_palette[6], show.legend = FALSE) +
  labs(x = "Anno",
       y = "Numero di incidenti",
       color = "Legenda") +
  scale_x_continuous(breaks = seq(min(dataEU$Year), max(dataEU$Year), by = 2)) +
  theme_minimal(base_size = 12) 
  theme(legend.position = "bottom",
        legend.text = element_text(size = 12))

#Analisi INJURY
table(data$INJURY)
injury_frequencies <- prop.table(table(data$INJURY)) * 100
round(injury_frequencies,2)

ggplot(data, aes(x = INJURY)) +
  geom_bar(fill = gradation_palette2[1:5]) +
  labs(title = "Frequenza delle gravità", x = "Gravità incidente", y = "Conteggio") +
  theme_classic() +
  theme(plot.title = element_text(hjust = 0.5, size = 14, face= "bold"))

#Analisi INVTYPE
invtype_frequency <- table(data$INVTYPE)
print(invtype_frequency)

injury_by_invtype <- data %>%
  group_by(INVTYPE, INJURY) %>%
  summarise(count = n()) %>%
  mutate(percentage = count / sum(count) * 100)
print(injury_by_invtype)

ggplot(data, aes(x = fct_infreq(INVTYPE))) +
  geom_bar(aes(fill = INVTYPE)) +
  scale_fill_manual(values = gradation_palette) +
  labs(x = "INVTYPE", y = "Conteggio") +
  theme_custom +
  theme(legend.position = "none", 
        axis.text.x = element_text(angle = 45, hjust = 1))

ggplot(injury_by_invtype %>% filter(INVTYPE != "Other"), aes(x = factor(INVTYPE), y = percentage, fill = INJURY)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(x = NULL, y = "Percentuale") +  # Rimuove la label dell'asse x
  ggtitle("Gravità dell'incidente per utente coinvolto") +
  theme_custom +
  scale_fill_manual(values = gradation_palette[2:6], name = "INJURY") + 
  theme(legend.position = "right",
        legend.text = element_text(size = 12),
        plot.title = element_text(hjust = 0.5, size = 14)) +
  coord_flip()

#Analisi INVTYPE temporale
plot_data <- data %>%
  mutate(period = ifelse(lubridate::year(DATE) < 2016, "2006-2015", "2016-2023")) %>%
  filter(INJURY %in% c("Major", "Fatal")) %>%
  group_by(period, INVTYPE) %>%
  summarise(
    count = n(),
    .groups = 'drop'
  ) %>%
  # Calcolo del totale degli incidenti per bicicletta e pedone
  group_by(period) %>%
  mutate(
    total_bike_ped = sum(count[INVTYPE %in% c("Bicycle", "Pedestrian")]),
    total_all = sum(count),
    relative_freq = ifelse(INVTYPE %in% c("Bicycle", "Pedestrian"), 
                           total_bike_ped / total_all,
                           (total_all - total_bike_ped) / total_all),
    category = ifelse(INVTYPE %in% c("Bicycle", "Pedestrian"), "Bikes & Pedestrians", "Others")
  ) %>%
  select(period, category, relative_freq)

ggplot(plot_data, aes(x = period, y = relative_freq, fill = category)) +
  geom_bar(stat = "identity", position = "dodge") +
  geom_text(aes(label = scales::percent(relative_freq, accuracy = 0.1)), 
            position = position_dodge(width = 0.9), vjust = -0.5, size = 4) +
  scale_y_continuous(labels = scales::percent, limits = c(0, 1)) +
  scale_fill_manual(values = c(excel_palette[1], excel_palette[2])) +  
  labs(x = "Periodo",
       y = "Frequenza relativa",
       fill = "INVTYPE"
  ) +
  theme_minimal() +
  theme(legend.position = "bottom",
        legend.text = element_text(size = 12))

#Analisi Fattori interni
contributing_factors <- data %>%
  select(SPEEDING, WRONGDRIV, REDLIGHT, ALCOHOL, DRIVACT, DRIVCOND) %>%
  colSums()
total_factors <- sum(contributing_factors)
percentages <- (contributing_factors / total_factors) * 100

ordered_factors <- sort(percentages, decreasing = TRUE)

barplot(ordered_factors,
        ylab = "Percentuale",
        las = 2,
        cex.names = 0.7,
        cex.lab = 0.8,
        col = gradation_palette2,
        ylim = c(0, max(percentages) * 1.1)
        )
title(main = "Fattori relativi al conducente", 
      font.main = 2,          
      cex.main = 1.4,         
      line = 1                
)

risky_behavior_rate <- mean(data$SPEEDING | data$DRIVACT | data$DRIVCOND | data$WRONGDRIV | data$ALCOHOL | data$REDLIGHT) * 100
print(risky_behavior_rate)

long_data <- data %>%
  pivot_longer(cols = c(SPEEDING, DRIVACT, DRIVCOND, WRONGDRIV, ALCOHOL, REDLIGHT),
               names_to = "variable",
               values_to = "value")
injury_by_factor <- long_data %>%
  group_by(variable, value, INJURY) %>%
  summarise(count = n()) %>%
  mutate(percentage = count / sum(count) * 100)

ggplot(injury_by_factor, aes(x = INJURY, y = percentage, fill = factor(value))) +
  geom_bar(stat = "identity", position = "stack") +
  labs(x = "Gravità lesione", y = "Percentuale", fill = "Valore") +
  facet_wrap(~ variable, scales = "free_x", labeller = as_labeller(c(
    SPEEDING = "Eccesso di velocità",
    DRIVACT = "Comportamento scorretto",
    DRIVCOND = "Condizione alterata",
    WRONGDRIV = "Guida imprudente e distratta",
    ALCOHOL = "Stato di ebrezza",
    REDLIGHT = "Passaggio al semaforo rosso"
  ))) +
  scale_y_continuous(limits = c(0, 110), expand = c(0, 0)) +
  theme_custom +
  scale_fill_manual(values = excel_palette[c(2, 1)],
                    name = "Presenza fattore",
                    labels = c("No", "Sì"))

injury_by_factor_normalized <- injury_by_factor %>%
  group_by(variable, INJURY) %>%
  mutate(percentage = percentage / sum(percentage) * 100) %>%
  mutate(percentage = round(percentage, 1)) %>%
  ungroup()

#100%
ggplot(injury_by_factor_normalized, aes(x = INJURY, y = percentage, fill = factor(value))) +
  geom_bar(stat = "identity", position = "stack") +
  labs( title="Variabili segnalate per gravità dell'incidente", y = "Percentuale", fill = "Valore") +
  facet_wrap(~ variable, scales = "free_x") + 
  scale_y_continuous(limits = c(0, 110), expand = c(0, 0)) +
  theme_custom +
  scale_fill_manual(values = excel_palette[c(2, 1)],
                    name = "Presenza fattore",
                    labels = c("No", "Sì")) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1, size = 8),
    strip.text.x = element_text(size = 9),
    legend.position = "bottom",
    legend.text = element_text(size = 10),
    plot.title = element_text(hjust = 0.5, size = 14),
    axis.title.x = element_text(size = 10),
    axis.title.y = element_text(size = 10)
  )

#Analisi Fattori Multipli
data <- data %>%
  mutate(risk_factors = SPEEDING + ALCOHOL + WRONGDRIV + DRIVACT + DRIVCOND + REDLIGHT + DISABILITY)

risk_factors_impact <- data %>%
  group_by(risk_factors) %>%
  summarise(
    accident_count = n(),
    severe_injury_rate = mean(INJURY %in% c("Major", "Fatal"))
  )
print(risk_factors_impact)

ggplot(risk_factors_impact, aes(x = risk_factors, y = severe_injury_rate)) +
  geom_line(color = excel_palette[1]) +
  geom_point(color = excel_palette[1]) +
  labs(x = "Fattori di rischio", y = "Tasso di lesioni gravi/fatali") +
  theme_custom

#Analisi SPEEDING
speeding_percentage <- mean(data$SPEEDING) * 100
print(speeding_percentage)

serious_data <- data %>%
  filter(
    INJURY %in% c("Major", "Fatal")
  )
serious_speeding_percentage <- mean(serious_data$SPEEDING) * 100
print(serious_speeding_percentage)

injury_by_speeding <- data %>%
  group_by(SPEEDING, INJURY) %>%
  summarise(count = n()) %>%
  mutate(percentage = count / sum(count) * 100)
print(injury_by_speeding)

injury_normalized <- injury_by_speeding %>%
  group_by(INJURY) %>%
  mutate(percentage = percentage / sum(percentage) * 100) %>%
  mutate(percentage = round(percentage, 1)) %>%
  ungroup()

ggplot(injury_normalized, aes(x = INJURY, y = percentage, fill = factor(SPEEDING))) +
  geom_bar(stat = "identity", position = "stack") +
  labs(title = "Presenza di eccesso di velocità per gravità dell'incidente",
       y = "Percentuale") +
  scale_y_continuous(limits = c(0, 110), expand = c(0, 0)) +
  theme_custom +
  scale_fill_manual(values = excel_palette[c(2, 1)],
                    name = "Presenza fattore",
                    labels = c("No", "Sì")) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1, size = 8),
    strip.text.x = element_text(size = 9),
    legend.position = "bottom",
    legend.text = element_text(size = 10),
    plot.title = element_text(hjust = 0.5, size = 14),
    axis.title.x = element_text(size = 10),
    axis.title.y = element_text(size = 10)
  )



#Analisi Fattori Esterni (irrilevanti)
external_factors <- data %>%
  select(DARKNESS,BADWEATHER,RDSFDANGER,TRAFFCTL) %>%
  colSums()

total_ex_factors <- sum(external_factors)
ex_percentages <- (external_factors / total_ex_factors) * 100

ordered_ex_factors <- sort(ex_percentages, decreasing = TRUE)

barplot(ordered_ex_factors,
        ylab = "Percentuale",
        las = 2,
        cex.names = 0.7,
        cex.lab = 0.8,
        col = gradation_palette2,
        ylim = c(0, max(percentages) * 1.1)
)
title(main = "Fattori esterni", 
      font.main = 2,          
      cex.main = 1.4,         
      line = 1                
)

external_factors_rate <- mean(data$DARKNESS  | data$BADWEATHER | data$ TRAFFCTL | data$RDSFDANGER) * 100
print(external_factors_rate)

ef_long_data <- data %>%
  pivot_longer(cols = c(DARKNESS,BADWEATHER,RDSFDANGER,TRAFFCTL),
               names_to = "variable",
               values_to = "value")
injury_by_ex_factor <- ef_long_data %>%
  group_by(variable, value, INJURY) %>%
  summarise(count = n()) %>%
  mutate(percentage = count / sum(count) * 100)

ggplot(injury_by_ex_factor, aes(x = INJURY, y = percentage, fill = factor(value))) +
  geom_bar(stat = "identity", position = "stack") +
  labs(x = "Gravità lesione", y = "Percentuale", fill = "Valore") +
  facet_wrap(~ variable, scales = "free_x", labeller = as_labeller(c(
    DARKNESS = "Notte",
    BADWEATHER = "Maltempo",
    RDSFDANGER = "Strada pericolosa",
    TRAFFCTL = "Controllo stradale"
  ))) +
  scale_y_continuous(limits = c(0, 110), expand = c(0, 0)) +
  theme_custom +
  scale_fill_manual(values = excel_palette[c(2, 1)],
                    name = "Presenza fattore",
                    labels = c("No", "Sì"))

injury_by_ex_factor_normalized <- injury_by_ex_factor %>%
  group_by(variable, INJURY) %>%
  mutate(percentage = percentage / sum(percentage) * 100) %>%
  mutate(percentage = round(percentage, 1)) %>%
  ungroup()

#100%
ggplot(injury_by_ex_factor_normalized, aes(x = INJURY, y = percentage, fill = factor(value))) +
  geom_bar(stat = "identity", position = "stack") +
  labs( title="Variabili segnalate per gravità dell'incidente", y = "Percentuale", fill = "Valore") +
  facet_wrap(~ variable, scales = "free_x") + 
  scale_y_continuous(limits = c(0, 110), expand = c(0, 0)) +
  theme_custom +
  scale_fill_manual(values = excel_palette[c(2, 1)],
                    name = "Presenza fattore",
                    labels = c("No", "Sì")) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1, size = 8),
    strip.text.x = element_text(size = 9),
    legend.position = "bottom",
    legend.text = element_text(size = 10),
    plot.title = element_text(hjust = 0.5, size = 14),
    axis.title.x = element_text(size = 10),
    axis.title.y = element_text(size = 10)
  )



#Analisi Correlazione
cor_matrix <- cor(data %>% select_if(is.numeric), use = "complete.obs")
heatmap(cor_matrix, main = "Correlazione tra le variabili", col = colorRampPalette(excel_palette)(100))





###MODELLO BINOMIALE###

data <- data %>%
  mutate(
    INJURY = case_when(
      INJURY %in% c("None","Minimal","Minor") ~ 0, 
      TRUE ~ 1 #Major,Fatal
      )
  )
str(data)

# Creazione del modello di regressione logistica binaria
binLogit <- glm(INJURY ~  REDLIGHT + BADWEATHER + DRIVCOND + ROAD_CLASS + TRAFFCTL +  RDSFDANGER + INVTYPE + DRIVACT +  SPEEDING +   ALCOHOL + DISABILITY + DARKNESS, 
                  data = data, 
                  family = binomial(link = "logit"))
#rimossi:  WRONGDRIV +

# Riepilogo del modello
summary(binLogit)

# Calcolo dei VIF per verificare la multicollinearità
vif(binLogit)

# Test di Hosmer-Lemeshow per la bontà di adattamento
hoslem.test(binLogit$y, fitted(binLogit), g = 10)

# Calcolo dello pseudo R-quadro di McFadden
pR2(binLogit)["McFadden"]

# Istogramma delle probabilità previste
hist(fitted(binLogit), breaks = 10, 
     main = "Distribuzione delle Probabilità Previste", 
     xlab = "Probabilità Previste")

# Calcolo dell'AUC e curva ROC
roc_obj <- roc(data$INJURY, fitted(binLogit))
auc_value <- auc(roc_obj)
print(paste("AUC:", auc_value))
# AUC: 0.861, indicando una capacità discriminativa moderatamente buona.

# Visualizzazione della curva ROC
plot(roc_obj)
title(main="Curva ROC",
      font.main = 2,          
      cex.main = 1.4,         
      line = 2.5  
      )

# Analisi dei residui
par(mfrow = c(2,2))
plot(binLogit)

# Calcolo degli effetti marginali medi
marg_effects <- margins(binLogit)
summary(marg_effects)

#calcolare odds ratios
exp(coef(binLogit))

# Validazione incrociata (esempio con 5-fold)
set.seed(123)
cv_error <- cv.glm(data, binLogit, K = 5)
print(paste("Errore di validazione incrociata:", cv_error$delta[1]))
#L'errore di validazione incrociata è 0.125, suggerendo una performance ragionevole su dati non visti.

# Matrice di confusione
predictions <- ifelse(fitted(binLogit) > 0.5, 1, 0)
conf_matrix <- table(Predicted = predictions, Actual = data$INJURY)
print(conf_matrix)

# Calcolo di accuratezza, precisione, richiamo e F1-score
accuracy <- sum(diag(conf_matrix)) / sum(conf_matrix)
precision <- conf_matrix[2,2] / sum(conf_matrix[2,])
recall <- conf_matrix[2,2] / sum(conf_matrix[,2])
f1_score <- 2 * (precision * recall) / (precision + recall)

print(paste("Accuratezza:", accuracy))
print(paste("Precisione:", precision))
print(paste("Richiamo:", recall))
print(paste("F1-score:", f1_score))

#Performance del modello:
#Accuratezza: 84%
#Precisione: 89%
#Richiamo: 63%
#F1-score: 0.74, un buon bilanciamento tra precisione e richiamo.