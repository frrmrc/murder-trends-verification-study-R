library(forcats)  
library(rio)
library (dplyr)
library(sjmisc)
library(tidyr)
library(stringr)
library(rcompanion)
library(ggplot2)
library(scales)
library(nortest)
library(DescTools)

df_import=import("C:/Users/MarcoFerrarini/OneDrive - ITS Angelo Rizzoli/Desktop/esame_modelli_statistici/database.csv")
#ESPLORAZIONE con df vecchio#####################
any(duplicated(df_import$`Record ID`))
str(df_import)
table(df_import$`Agency Code`)
table(df_import$`Agency Type`)
table(df_import$State)
table(df_import$`Record Source`)
table(df_import$`Crime Solved`)
table(df_import$`Victim Count`)
table(df_import$Weapon)
table(df_import$`Perpetrator Race`)
table(df_import$`Perpetrator Ethnicity`)
table(df_import$Incident)
table(df_import$`Victim Sex`)
table(df_import$`Perpetrator Count`)

#riesco a trovare jeffrey dahmer?####
df_clean %>% filter(state == "Wisconsin" & year==1991 & perpetrator_sex=='Male' & victim_sex=='Male'& perpetrator_race=='White'&city =='Milwaukee'& perpetrator_age==31)

#esplorazione df_clean####
str(df_clean)
df_clean %>% filter(victim_sex == "Unknown", crime_solved=='Yes')
df_clean %>% filter(agency_code == "Unknown" | agency_name == "Unknown" |agency_type == "Unknown"  )
df_clean %>% filter(victim_sex == "Unknown", perpetrator_age>0)
df_clean %>%
  group_by(agency_name, city) %>%
  summarise(unique_codes = n_distinct(agency_code)) %>%
  filter(unique_codes > 1)
df_clean %>%
  group_by(agency_code) %>%
  summarise(unique_names = n_distinct(agency_name),
            unique_cities = n_distinct(city)) %>%
  filter(unique_names > 1 | unique_cities > 1)
df_clean %>%
  summarise(across(everything(), ~ sum(is.na(.))))
df_clean %>%
  filter(is.na(perpetrator_age))
table(df_clean$victim_age)
table(df_clean$rel_victim_2_perp)
df_clean %>% filter(crime_solved == TRUE & perpetrator_sex=='Unknown')
df_clean %>% filter(victim_age == 1 &rel_victim_2_perp=='Boyfriend/Girlfriend'  )
df_clean %>% filter(victim_age < 10 & perpetrator_age<10)
df_clean %>%
  filter(perpetrator_age>victim_age & rel_victim_2_perp %in%  c("Father", "Mother"))
table(df_crazy$record_source)
table(df_clean$city)
table(df_clean$record_source)
table(df_clean$victim_race)
table(df_clean$perpetrator_age)


#PULIZIA####################
df_clean <- df_import %>% rename(
  record_id             = `Record ID`,
  agency_code           = `Agency Code`,
  agency_name           = `Agency Name`,
  agency_type           = `Agency Type`,
  city                  = `City`,
  state                 = `State`,
  year                  = `Year`,
  month                 = `Month`,
  incident              = `Incident`,
  crime_type            = `Crime Type`,
  crime_solved          = `Crime Solved`,
  victim_sex            = `Victim Sex`,
  victim_age            = `Victim Age`,
  victim_race           = `Victim Race`,
  victim_ethnicity      = `Victim Ethnicity`,
  perpetrator_sex       = `Perpetrator Sex`,
  perpetrator_age       = `Perpetrator Age`,
  perpetrator_race      = `Perpetrator Race`,
  perpetrator_ethnicity = `Perpetrator Ethnicity`,
  rel_victim_2_perp     = `Relationship`, # relationship indica chi è la vittime per l'assassino, es. se si indica wife, la vittime era la moglie
  weapon                = `Weapon`,
  victim_count          = `Victim Count`,
  perpetrator_count     = `Perpetrator Count`,
  record_source         = `Record Source`
  
)
df_clean$crime_solved = df_clean$crime_solved == "Yes"

df_clean <- df_clean %>% # creo una colonna con valore bool per indicare problemi con l'età 
  mutate(problematic_age = 
           perpetrator_age %in% c(0, 1, 99, 998) |
           victim_age %in% c(99, 998) |
           (victim_age < 10 & rel_victim_2_perp %in% c(
             "Wife", "Husband", "Common-Law Wife", "Common-Law Husband",  'Stepmother', 'Stepmother', 'Stepfather', 'Mother', 'Father', 
             "Ex-Wife", "Ex-Husband", "Boyfriend", "Girlfriend", 
             "Boyfriend/Girlfriend", "Employee")))

df_clean <- df_clean %>% # setto a na l'età per valori illogici, questo in vista di analisi future
  mutate(across(c(agency_name, agency_code, agency_type), ~ na_if(., "Unknown")))%>%
  mutate(across(c(perpetrator_age), ~ ifelse(. %in% c(0,1,99, 998), NA, .)))%>%
  mutate(across(c(victim_age), ~ ifelse(. %in% c(99, 998), NA, .))) %>%
  mutate(victim_age = ifelse(victim_age < 10 & rel_victim_2_perp %in% c(
    "Wife", "Husband", "Common-Law Wife", "Common-Law Husband", 'Stepmother', 'Stepmother', 'Stepfather', 'Mother', 'Father', 
    "Ex-Wife", "Ex-Husband", "Boyfriend", "Girlfriend", 
    "Boyfriend/Girlfriend", "Employee"), NA, victim_age))

df_clean <- df_clean %>%
  mutate(crime_solved = if_else(
    crime_solved == TRUE & perpetrator_sex == "Unknown" & is.na(perpetrator_age),
    FALSE,
    crime_solved
  ))

df_clean <- df_clean %>% # setto a na la relazione quando la stessa non ha senso ccon il sesso della vittima
  mutate(rel_victim_2_perp = if_else(
    (victim_sex == "Male" & rel_victim_2_perp %in%
       c("Wife",'Stepdaughter', "Common-Law Wife", "Ex-Wife", "Stepmother", "Mother", "Girlfriend", "Sister", "Daughter")) |
      (victim_sex == "Female" & rel_victim_2_perp %in%
         c("Husband", 'Stepson',"Common-Law Husband", "Ex-Husband", "Stepfather", "Father", "Boyfriend", "Brother", "Son")),
    NA_character_,
    rel_victim_2_perp
  ))

df_clean <- df_clean %>%
  filter(!(perpetrator_age > victim_age & 
             rel_victim_2_perp %in% c("Father", "Mother")))
df_clean= select(df_clean, -c(incident, perpetrator_count, victim_count) )

#test di normalità### ####
numerical_vars <- c("year", "victim_age", "perpetrator_age")
for (var in numerical_vars) {
  values <- na.omit(df_clean[[var]])
  sample_values <- sample(values, min(length(values), 5000))  # campione 
  p <- ggqqplot(sample_values,
                title = paste("Q-Q plot ", var),
                xlab = "Quantili teorici",
                ylab = var,
                color = "blue",
                fill = "lightblue",
                ggtheme = theme_minimal(),
                shape = 16,
                size = 0.3)}





#ORA ANALIZZIAMO COME VARIA L'ETà DELLA VITIMA E ASSASSINO PER RAZZA
# check omicidi aumentati del 66% tra 1984 e 88 articolo violenza tra neri new york times####
df_black_m_15_24 = df_clean %>% 
  filter(victim_sex=='Male'&
           victim_age > 15 & victim_age < 24 & 
           victim_race=='Black' & 
           year<=1988
  )
homicide_by_year_black_m <- df_black_m_15_24 %>%
  group_by(year) %>%
  summarise(homicide_count = n())

# Aggiungiamo una colonna per calcolare la variazione percentuale annuale
homicide_by_year_black_m <- homicide_by_year %>%
  mutate(percentage_change = (homicide_count - lag(homicide_count)) / lag(homicide_count) * 100)

ggplot(homicide_by_year_black_m, aes(x = year, y = homicide_count)) +
  geom_line(size = 1,color = "blue") +
  geom_point() +
  labs(title = "Numero di Omicidi tra Maschi Neri (15-24 anni)", 
       x = "Anno", y = "Numero di Omicidi") +
  my_theme

popolazione_black_males_15_24 <- tibble(
  year = 1980:1989,
  popolazione = c(2806000, 2828000, 2816000, 2802000,2784000, 2768000, 2769000, 2750000, 2723000, 2687000)
)

tasso_omicidio <- homicide_by_year_black_m %>%
  left_join(popolazione_black_males_15_24, by = "year") %>%
  mutate(
    tasso_per_100000 = (homicide_count / popolazione) * 100000,
    proporzione = homicide_count / popolazione,
    errore_standard = sqrt((proporzione * (1 - proporzione)) / popolazione),
    intervallo_basso = proporzione - 1.96 * errore_standard,
    intervallo_alto = proporzione + 1.96 * errore_standard,
    intervallo_basso_100000 = intervallo_basso * 100000,
    intervallo_alto_100000 = intervallo_alto * 100000
  )

tasso_1984 <- tasso_omicidio$tasso_per_100000[tasso_omicidio$year == 1984]
tasso_1988 <- tasso_omicidio$tasso_per_100000[tasso_omicidio$year == 1988]
errore_standard_1984 <- tasso_omicidio$errore_standard[tasso_omicidio$year == 1984]
errore_standard_1988 <- tasso_omicidio$errore_standard[tasso_omicidio$year == 1988]

# Calcola gli estremi degli intervalli di confidenza dei tassi
IC_1984_basso <- tasso_1984 - 1.96 * errore_standard_1984
IC_1984_alto  <- tasso_1984 + 1.96 * errore_standard_1984

IC_1988_basso <- tasso_1988 - 1.96 * errore_standard_1988
IC_1988_alto  <- tasso_1988 + 1.96 * errore_standard_1988

# Calcola aumento percentuale centrale
aumento_percentuale <- ((tasso_1988 - tasso_1984) / tasso_1984) * 100

# Calcola aumento percentuale minimo e massimo
aumento_percentuale_min <- ((IC_1988_basso - IC_1984_alto) / IC_1984_alto) * 100
aumento_percentuale_max <- ((IC_1988_alto - IC_1984_basso) / IC_1984_basso) * 100

# Risultati
cat("Aumento percentuale centrale:", aumento_percentuale, "%\n")
cat("Intervallo plausibile aumento percentuale: [", aumento_percentuale_min, "% ,", aumento_percentuale_max, "%]\n")

# grafico
ggplot(tasso_omicidio, aes(x = year, y = tasso_per_100000)) +
  geom_line(size=1, color='blue') +
  geom_point() +
  labs(title = "Tasso di omicidio tra maschi neri (15-24 anni)",
       x = "Anno", 
       y = "Tasso di omicidio per 100.000")+ my_theme 


# ORA VOGLIO ANALIZZARE MEGLIO IL FENOMENO####
# confrontiamo l'età media delle vittime per razza con anova ####
anova_result <- aov(victim_age ~ victim_race, data = df_clean)
summary(anova_result)
ttt=TukeyHSD(anova_result)

tukey_df <- as.data.frame(ttt$victim_race)
tukey_df$coppie <- rownames(tukey_df)

# Filtra le coppie che contengono "Unknown"
tukey_df <- tukey_df[!grepl("Unknown", tukey_df$coppie), ]

# Ordina i dati
tukey_df <- tukey_df[order(tukey_df$diff), ]
tukey_df$coppie <- factor(tukey_df$coppie, levels = tukey_df$coppie)

# Grafico 
ggplot(tukey_df, aes(x = coppie, y = diff)) +
  geom_bar(stat = "identity", fill = "#ADFF2F") +
  # Rimuoviamo geom_errorbar per togliere le linee nere
  coord_flip() +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
  labs(title = "Differenza di mean(victim_age) per victim_race", 
       x = "", 
       y = "Differenza victim_age") +
  my_theme +
  theme(panel.grid.major.y = element_blank())

# vediamo come varia nel tempo l'eta delle vittime####

cor.test(df_clean$year, df_clean$victim_age)
cor.test(df_clean$year, df_clean$perpetrator_age)


df_age <- df_clean %>%
  filter(!is.na(victim_age)& !victim_race=='Unknown') %>%  # Filtra i record senza età
  group_by(year, victim_race) %>%  # Raggruppa per anno e razza
  summarise(mean_age = mean(victim_age, na.rm = TRUE)) %>%  # Calcola l'età media
  ungroup()

# Crea il grafico a linee
ggplot(df_age, aes(x = year, y = mean_age, color = victim_race, group = victim_race)) +
  geom_line(size=1) + e
  geom_point() + 
  labs(
    title = "Età Media delle Vittime nel Tempo per victim_race",
    x = "Anno",
    y = "Età Media",
    color = "victim_race"
  ) +
  my_theme +  
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1)
  )
# come cambia l'età dell'assassino per razza####
df_age <- df_clean %>%
  filter(!is.na(perpetrator_age)& !perpetrator_race=='Unknown') %>%  # Filtra i record senza età
  group_by(year, perpetrator_race) %>%  # Raggruppa per anno e razza
  summarise(mean_age = mean(perpetrator_age, na.rm = TRUE)) %>%  # Calcola l'età media
  ungroup()

# Crea il grafico a linee
ggplot(df_age, aes(x = year, y = mean_age, color = perpetrator_race, group = perpetrator_race)) +
  geom_line(size=1) +  # Grafico a linee
  geom_point() +  # Aggiunge i punti sui dati
  labs(
    title = "Età Media degli Assassini nel Tempo per perpetator_race",
    x = "Anno",
    y = "Età Media",
    color = "perpetrator_race"
  ) +
  my_theme +  # Usa uno stile pulito per il grafico
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1)  # Ruota i valori sull'asse x per una miglior leggibilità
  )

# estiste una relazione tra razza e arma?####
# Creiamo una tabella di contingenza tra 'weapon' e 'victim_race'
weapon_race_table <- table(df_clean$weapon, df_clean$perpetrator_race)
chi_square_test <- chisq.test(weapon_race_table)
chi_square_test
weapon_race_table
cramers_v <- CramerV(weapon_race_table)
print(cramers_v)
df_weapon_race=as.data.frame(weapon_race_table)
names(df_weapon_race) <- c("Weapon", "Race", "Count")
# Calcolo totale dei crimini (globale)
total_crimes <- sum(df_weapon_race$Count)
# Percentuale di ogni arma
weapon_totals <- df_weapon_race %>%
  group_by(Weapon) %>%
  summarise(Weapon_Total = sum(Count)) %>%
  mutate(Weapon_Percent = Weapon_Total / total_crimes * 100)
# Definiamo quali armi mantenere (sopra il 2%)
weapons_to_keep <- weapon_totals %>%
  filter(Weapon_Percent >= 2) %>%
  pull(Weapon)
# Riassegniamo 'Altro' alle armi rare
df_weapon_race_mod <- df_weapon_race %>%
  mutate(Weapon = as.character(Weapon)) %>% 
  mutate(Weapon_Grouped = ifelse(Weapon %in% weapons_to_keep, Weapon, "Altro"))
# Ora ricalcoliamo il totale per ogni razza + gruppo arma
df_weapon_race_summary <- df_weapon_race_mod %>%
  group_by(Race, Weapon_Grouped) %>%
  summarise(Count = sum(Count), .groups = 'drop')
# Per ogni razza calcoliamo il totale dei crimini (per normalizzare)
df_weapon_race_summary <- df_weapon_race_summary %>%
  group_by(Race) %>%
  mutate(Total_Race = sum(Count),
         Percent = Count / Total_Race * 100)
# Ora creiamo il grafico
ggplot(df_weapon_race_summary, aes(x = Race, y = Percent, fill = Weapon_Grouped)) +
  geom_bar(stat = "identity", position = "fill", color = "black") +
  labs(title = "Distribuzione Percentuale delle Armi per perpetrator_race",
       x = "perpetrator_race",
       y = "Percentuale di Omicidi",
       fill = "Arma") +
  my_theme +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_y_continuous(labels = scales::percent_format(scale = 100))

#ORA ANALIZZIAMO LE AFFERMAZIONI DI INTIMATE PARTNER HOMICIDE STATISTICS#####
#FUNZIONE####

partner_victim_ci <- function(df, year, sex) {
  rel_m <- c('Boyfriend', 'Boyfriend/Girlfriend', 'Common-Law Husband', 'Ex-Husband', 'Husband')
  rel_f <- c('Girlfriend', 'Boyfriend/Girlfriend', 'Common-Law Wife', 'Ex-Wife', 'Wife')
  
  d <-df[df$year == year & df$victim_sex == sex, ]
  relazioni <- if (sex == "Male") rel_m else if (sex == "Female") rel_f else character(0)
  
  not_unknown <- !is.na(d$rel_victim_2_perp) & d$rel_victim_2_perp != "Unknown"
  n <- sum(not_unknown)
  k <- sum(d$rel_victim_2_perp %in% relazioni, na.rm = TRUE)
  
  if (n == 0) return(NA)
  
  p <- k / n
  se <- 1.96 * sqrt(p * (1 - p) / n)
  list(proportion = p, ci = c(p - se, p + se), n = n, partner = k)
}



partner_victim_ci(df_clean, 2008, "Female")
#From 1980 to 2008, 16.3% victims were killed by an intimate partner#####
df_80_08=df_clean %>% 
  filter(year<2009)
partener_victim=sum(df_80_08$rel_victim_2_perp %in% 
                      c('Boyfriend', 'Boyfriend/Girlfriend', 
                        'Common-Law Husband','Common-Law Wife','Girlfriend',
                        'Ex-Husband','Ex-Wife','Girlfriend','Husband','Wife'))


other_victims <- sum(df_80_08$rel_victim_2_perp != 'Unknown', na.rm = TRUE)
partener_victim
other_victims
partner_victims_ratio=partener_victim/other_victims
partner_victims_ratio
se_partner_victims=1.96*sqrt(partner_victims_ratio*(1-partner_victims_ratio)/other_victims)
partner_victims_ratio+se_partner_victims
partner_victims_ratio-se_partner_victims
# rifiutiamo ipotesi nulla, 


#VECCHIO: #ora vediamo The percentage of males killed by an intimate fell from 10.4% in 1980 to 4.9% in 2008#####
df_males_by_p80=df_clean %>% 
  filter( year==1980 & victim_sex=='Male')
df_males_by_p08=df_clean %>% 
  filter(year==2008 & victim_sex=='Male')
partener_victim_m_80=sum(df_males_by_p80$rel_victim_2_perp %in% 
                           c('Boyfriend', 'Boyfriend/Girlfriend', 
                             'Common-Law Husband','Ex-Husband','Husband'))

other_victims <- sum(df_males_by_p80$rel_victim_2_perp != 'Unknown', na.rm = TRUE)
partener_victim_m_80
other_victims
partner_victims_ratio=partener_victim_m_80/other_victims
partner_victims_ratio
se_partner_victims=1.96*sqrt(partner_victims_ratio*(1-partner_victims_ratio)/other_victims)
partner_victims_ratio+se_partner_victims
partner_victims_ratio-se_partner_victims

#bah

partener_victim_m_08=sum(df_males_by_p08$rel_victim_2_perp %in% 
                           c('Boyfriend', 'Boyfriend/Girlfriend', 
                             'Common-Law Husband','Ex-Husband','Husband'))

other_victims <- sum(df_males_by_p08$rel_victim_2_perp != 'Unknown', na.rm = TRUE)
partener_victim_m_08
other_victims
partner_victims_ratio=partener_victim_m_08/other_victims
partner_victims_ratio
se_partner_victims=1.96*sqrt(partner_victims_ratio*(1-partner_victims_ratio)/other_victims)
partner_victims_ratio+se_partner_victims
partner_victims_ratio-se_partner_victims


#CON FUNZIONE ora vediamo The percentage of males killed by an intimate fell from 10.4% in 1980 to 4.9% in 2008#####
partner_victim_ci(df_clean, 1980, "Male")
partner_victim_ci(df_clean, 2008, "Male")

#The percentage of females killed by an intimate declined from 43% in 1980 to#####
#38% in 1995. Aft er 1995, that percentage gradually increased, reaching 45% in 2008####

#43% in 1980
partner_victim_ci(df_clean, 1980, "Female")
#38% in 1995
partner_victim_ci(df_clean, 1995, "Female")
#45% in 2008
partner_victim_ci(df_clean, 2008, "Female")


#chi quadro sesso vittima e relazione con assassino####
df_chi_2_victim_sex_relation <- df_clean %>% filter(!is.na(rel_victim_2_perp))
contingency_relation <- table(df_chi_2_victim_sex_relation$rel_victim_2_perp, df_chi_2_victim_sex_relation$victim_sex)
chisq_relation <- chisq.test(contingency_relation)
chisq_relation
contingency_relation
cramers_v <- CramerV(contingency_relation)
print(cramers_v)
# relazione con vittima donna - chisquare e grafico####
df_female <- df_clean %>% filter(victim_sex == "Female")
df_female_relation <- df_female %>% filter(!is.na(rel_victim_2_perp))

ggplot(df_female_relation %>%
         filter(!is.na(rel_victim_2_perp)) %>%
         count(rel_victim_2_perp, victim_sex) %>%
         top_n(10, n), aes(x = rel_victim_2_perp, y = n, fill = victim_sex)) +
  geom_bar(stat = "identity", position = "dodge", fill='red') +
  labs(title = "Female victims relation with perpetrator",
       x = "Relazione tra la vittima e l'assassino", 
       y = "Numero di Crimini",
       fill = "Sesso della Vittima") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


# ora vediamo con gli uomini  - chiquare e grafico####
df_male <- df_clean %>% filter(victim_sex == "Male")
df_male_relation <- df_male %>% filter(!is.na(rel_victim_2_perp))
ggplot(df_male_relation %>%
         filter(!is.na(rel_victim_2_perp)) %>%
         count(rel_victim_2_perp, victim_sex) %>%
         top_n(10, n), aes(x = rel_victim_2_perp, y = n, fill = victim_sex)) +
  geom_bar(stat = "identity", position = "dodge", fill='green') +
  labs(title = "Male victims relation with perpetrator",
       x = "Relazione tra la vittima e l'assassino", 
       y = "Numero di Crimini",
       fill = "Sesso della Vittima") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))



# età, sesso, razza, città influenzano la probabilità di risoluzione dell'omicidio ####
#tema grafici####
my_theme <- theme_minimal(base_family = "sans") +
  theme(
    plot.title = element_text(size = 18, face = "bold", hjust = 0.5, color = "#333333"),
    plot.subtitle = element_text(size = 14, hjust = 0.5, color = "#666666"),
    plot.caption = element_text(size = 10, color = "#999999"),
    axis.title = element_text(size = 14, color = "#333333"),
    axis.text = element_text(size = 12, color = "#555555"),
    panel.grid.major = element_line(size = 0.3, color = "#DDDDDD"),
    panel.grid.minor = element_blank(),
    axis.line = element_line(size = 0.4, color = "#999999"),
    plot.background = element_rect(fill = "white", color = NA),
    panel.background = element_rect(fill = "white", color = NA),
    legend.background = element_rect(fill = "white", color = NA),
    legend.key = element_rect(fill = "white", color = NA),
    legend.text = element_text(size = 12, color = "#444444"),
    legend.title = element_text(size = 14, color = "#333333", face = "bold")
  )






library(dplyr)
library(ggplot2)
#TODO: grafico a linee con crimini risolti e totali per anno
# Calcola percentuale di crimini risolti per anno####
# Raggruppa per anno e calcola totali
df_resolution <- df_clean %>%
  filter(!is.na(crime_solved), !is.na(year)) %>%
  group_by(year) %>%
  summarise(
    totali = n(),
    risolti = sum(crime_solved)
  ) %>%
  pivot_longer(cols = c(totali, risolti), names_to = "Tipo", values_to = "Conteggio")

# Grafico a linee
ggplot(df_resolution, aes(x = year, y = Conteggio, color = Tipo)) +
  geom_line(size = 1.) +
  geom_point(size = 1.5) +
  labs(
    title = "Omicidi Totali e Risolti nel Tempo",
    x = "Anno",
    y = "Numero di Casi",
    color = "Tipo di Omicidio"
  ) + my_theme+
  theme_minimal()




