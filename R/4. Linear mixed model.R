# ---- Forslag til R-script for modellering ----

# ---- (1) Forberedelser ----

## 1.0) Pakker ----
library(dplyr)
library(tidyr)
library(lme4)
library(lmerTest)
library(broom.mixed)
library(modelsummary)
library(here)
library(irr)
library(influence.ME)
library(patchwork)

## 1.1) Velge hvor det skal lagres----

out_sav <- here("R", "master_output")
#---

## 1.2) Innlesing av datasett ----
data <- readRDS(here("R","master_output", "bearbeidet datasett", "fullt_datasett_komplett_2026-04-21_22-53.rds")) 

## 1.3) Enkel rydding ----
# Fordi det er blitt gjort en matematisk utregning tidligere, ref kodeblokk 4.1, der alle radene for den testen hadde NA også ble det gjort en utregning 
# der man brukte na.rm =TRUE, så vil man gjøre utregningen på ingenting og få NaN. Eg velger derfor å endre alle NaN til NA for å ikkje miste data i analysene. 

data <- data %>% 
  mutate(
    across(
      where(is.numeric),
      ~ na_if(., NaN)
    ) 
  ) %>% 
  mutate(
    phase =factor(phase, levels = c("t1", "t2", "t3"))
  )

## 1.4) Lage endringstabell ----

### 1.4.1) Finner alle dei numeriske variablene (de eg ønsker å kjøre endring på)
variabler_delta <- data %>% 
  
  # Tar vekk dei kolonnene som eg ikkje tenker å kjøre endringsanalyse på
  select(-eilo_id, -phase, -dato, -starttid) %>% 
  select(where(is.numeric)) %>% 
  # Lagrer navnene på kolonnene i en vektor for senere bruk
  names()

### 1.4.2) Lager delta 
delta_long <- data %>% 
  select(eilo_id, phase, all_of(variabler_delta)) %>%
  
  # Gjør om til long format for å behandle alle variablene likt
  pivot_longer(
    cols = -c(eilo_id, phase),
    names_to = "variabel",
    values_to = "verdi"
  ) %>%
  
  # Gjør om til wide format for å gjøre det enkelt å regne ut delta. Har en rad per id + variabel
  pivot_wider(
    id_cols = c(eilo_id,variabel),
    names_from = phase,
    values_from = verdi
  ) %>% 
  
  # Lager en kolonne delta som tar for seg alle dei ønskede variablene og regner ut endring
  # delta_1 = t2 - t1
  # delta_2 = t3 - t2
  mutate(
    delta_1 = t2 - t1,
    delta_2 = t3 - t2
  )

### 1.4.4) For å gjøre tabellen mer oversiktlig
delta_tabell <- delta_long %>% 
  
  # Legger pre, post og delta i en kolonne
  pivot_longer(
    cols = c(t1,t2,t3,delta_1,delta_2),
    names_to = "phase2",
    values_to = "verdi"
  ) %>% 
  
  # Ønsker at for kvar id og fase, får eg tre rader og alle variablene ligg bortover som kolonner
  pivot_wider(
    id_cols = c(eilo_id, phase2),
    names_from = variabel,
    values_from = verdi
  ) %>% 
  
  # Sørger for at rekkefølgen blir pre -> post -> delta
  mutate(
    phase2 = factor(phase2, levels = c("t1", "t2", "t3", "delta_1", "delta_2"))
  ) %>% 
  rename(
    phase = "phase2"
  ) %>% 
  arrange(eilo_id, phase) # Gir ei ryddig sortering per id


# ---- (2) Lineær blandede modeller ----

## 2.1) Modellforutsetninger ----

# Planen er å benytte seg av en LMM med within og between endringer.
# For at den skal være valid og reliabel så må nokon forutsetninger være på plass
## 1) Lineær sammenheng mellom CLE og tid
## 2) Residualene burde være normalfordelte, homoskedastiske og random intercept er normalfordelt
## 3) Uavhengighet mellom individer
## 4) CLE må varierer innen personer
## 5) Målefeil i CLE bør ikkje dominere endringen
## 6) Tidspunktene må være meningsfulle

# Derfor gjøres det noen avsjekker

### 2.1.1) Kontrollere for within/between forskjeller ----

# Skal det gjøres en person-mean centering (Mundlak) bør følgende stemme:
## 1) cle_between = personens gjennomsnitt (konstant innen person)
## 2) cle_within = observasjon - personens gjennomsnitt
## 3) cle_within har mean = 0 innen hver person

# Lager snitt for cle-tot (CLE E) og avstanden fra sitt personen eget snitt (within)
data2 <- data %>% 
  group_by(eilo_id) %>% 
  mutate(
    cle_between = mean(cle_tot, na.rm = TRUE),
    cle_within = cle_tot - cle_between
  ) %>% 
  ungroup()

# Sjekker om cle_within har mean = 0 innen hver person
data2 %>% 
  group_by(eilo_id) %>% 
  summarise(mean_within = mean(cle_within, na.rm = TRUE)) %>% 
  summarise(max_abs = max(abs(mean_within), na.rm = TRUE))

# Finner at det maksimale absolutte avviket innen hver deltaker var på 2.92e-16
# Som vil si at innen-person komponenten hadde forventet gjennomsnitt lik null, 
# og bekrefter korrekt implementering av dekomponeringen.

# For å få en oversikt over kor mange deltakere som bidrar til between og within effektene
# lager eg en oversikt over kor mange deltakere som har kor mange tester

data2 %>% 
  count(eilo_id) %>% 
  count(n)

# Resultatet er:
## 35 deltakere har kun 1 test
## 24 deltakere har kun 2 tester
## 244 deltakere har 3 tester

# Derfor er det 268 deltakere som har 2 eller flere målinger, og 244 deltakere som har full
# longitudinell informasjon. 

# Sikkerhetssjekk: kor mykje within-varians har datasettet faktisk
# Om mange har sd_within = 0, så blir det vanskelig å estimere cle_within

data2 %>% 
  group_by(eilo_id) %>% 
  summarise(
    sd_within = sd(cle_tot, na.rm = TRUE),
    n_obs = sum(!is.na(cle_tot))
    ) %>% 
  filter(n_obs >= 2) %>% # For å isolere reell null within-varians, vil eg kun ta dei som har 2 eller 3 tester
  summarise(
    prop_no_within = mean(sd_within == 0, na.rm = TRUE),
    median_sd_within = median(sd_within, na.rm = TRUE)
    )

# Resultatet viser at cirka 20% av deltakerne har null i within-varians, som vil si at 
# 1 av 5 ikkje endrer CLE-score E mellom testene.Within-effekten blir derfor drevet av dei 80% som faktisk endrer seg
# Median innan-person SD er 0.58, som tyder på reell, ikkje triviell longitudinell variasjon, og at det er tilstrekkelig informasjon til å estimere cle_within.


## 2.2) Hovedanalyse: Kjører LMM (m1) med person-mean centering (Mundlak) ----

m1 <- lmer(tid ~ cle_within + cle_between + phase + (1 | eilo_id), data = data2)

summary(m1)

VarCorr(m1)

# Utfall: tid
# Faste effekter: 
# cle_within: endring i CLE innan individet
# cle_between: forskjell mellom individ
# phase: tids/intervensjonseffekt
# Random effekt:
# (1 | eilo_id): ulik baseline mellom peroner

# Oppsummering av tabellen

## Tar med KI
tidy_m1 <- tidy(
  m1,
  effects = "fixed",
  conf.int = TRUE,
  conf.level = 0.95,
  conf.method = "Wald"
)

modelsummary(
  m1,
  estimate  = "{estimate}",
  statistic = "[{conf.low}, {conf.high}]",
  fmt = 2,
  stars = TRUE
)

### Tabell av resultatene ----

# Fixed effects
fixed_tab <- broom.mixed::tidy(m1, effects = "fixed", conf.int = TRUE, conf.method = "Wald") %>% 
  mutate(
    term = recode(term,
                  "cle_within" = "CLE-E (within-person",
                  "cle_between" = "CLE-E (between-person)",
                  "phaset2" = "Timepoint T2",
                  "phaset3" = "Timepoint T3"
    ),
    value = sprintf("%.2f [%.2f,%.2f]", estimate, conf.low, conf.high)
  ) %>% 
  select(term, value) %>% 
  mutate(section = "Fixed effects")

# Random effects
vc <- as.data.frame(VarCorr(m1))

random_tab <- tibble(
  section = "Random effects",
  term = c("SD (participant intercept", "Residual SD"),
  value = c(
    sprintf("%.2f", sqrt(vc$vcov[vc$grp == "eilo_id"])),
    sprintf("%.2f", sigma(m1))
  )
)

# Model fit
r2_vals <- r2(m1)
icc_val <- icc(m1)$ICC_adjusted
rmse_val <- rmse(m1)

fit_tab <- tibble(
  section = "Model fit",
  term = c("Observations", "ICC", "R2 (marginal)","R2 (betinget)", "RMSE"),
  value = c(
    nobs(m1),
    sprintf("%.2f", icc_val),
    sprintf("%.2f", r2_vals$R2_marginal),
    sprintf("%.2f", r2_vals$R2_conditional),
    sprintf("%.2f", rmse_val)
  )
)

# Slå sammen
final_tab <- bind_rows(fixed_tab, random_tab, fit_tab)

# Lag gt-tabell
results_main <- final_tab %>% 
  gt(groupname_col = "section") %>% 
  cols_label(
    term = "",
    value = "Estimate (95% CI)"
  ) %>% 
  tab_header(
    title = md("**Linear mixed model with Mundlak-decomposition**")
  ) %>% 
  cols_align(
    align = "left",
    columns = term
  ) %>% 
  cols_align(
    align = "center",
    columns = value
  ) %>% 
  tab_options(
    table.font.size = px(13),
    data_row.padding = px(6),
    row_group.font.weight = "bold"
  ) %>%
  tab_source_note(source_note =  "Fixed effects is presented as coefficients with 95% confidence intervals.")

results_main


### 2.2.1) Sjekke modellen for singular fit + konvergens ----

# En singular fit tyder på at modellen estimerer random-effekt variansen (svært lite variasn i random intercept)
# som i praksis er 0 eller veldig nært 0. Dvs at dataene ikkje gir støtte for at denne random-effekten faktisk varierer

# Om modellen IKKJE er singular fit bety at modellen estimerer reell mellom individ-variasjon.
# Dvs at deltakarane skil seg systematisk i baseline-nivå av tid, utover dei faste effektene. 
# Om modellen derimot ER SINGULAR FIT, indikerer det at dataene ikkje gir tilstrekkelig informasjon til å 
# estimere variansen i random intercept slik modellen er spesifisert. 

# I en Mundlak (person-mean centering) modeller vil cle_between forklare delar av variasjonen som elles ville blitt fanga opp
# random intercept

isSingular(m1, tol = 1e-4)

# Gir FALSE, og modellen estimerer reell mellom-individ variasjon. 

### 2.2.2) Residualdiagnostikk ----

# Skal sjekke følgende:
  # 1) Homoskedastisitet/mønster: residual versus fitted
  # 2) Normalitet: QQ-plot
  # 3) Outliers/influens
  # 4) Sjekke residualer per fase

# Klargjøring

# Data som faktisk blei brukt i modellen (utan rader som blei droppa)
data_m1 <- model.frame(m1)

res <- resid(m1)    # Marignale residualer
fit <- fitted(m1)   # Fitted
data_m1$.resid <- res
data_m1$.fitted <- fit

#### Homoskedastisitet / mønster ----

plot(data_m1$.fitted, data_m1$.resid,
     xlab = "Fitted", ylab = "Residual",
     main = "Residual vs Fitted")
abline(h = 0, lty = 2)
lines(lowess(data_m1$.fitted, data_m1$.resid), lxd = 2)

# Tolkning og beskrivelse av resultat av plottet er notert i OneNote arbeidsdokumentet
# "Loggføring" dato 24.01.26

#### Normalitet: QQ-plot ----

## QQ for residualer
qqnorm(data_m1$.resid); qqline(data_m1$.resid)

## QQ for random intercept
ri <- ranef(m1)$eilo_id[[1]]
qqnorm(ri); qqline(ri)

# Grunnet litt tyngre haler ønsket eg å se kva for ID-ar det gjaldt - kan sammenligne om det er 
# dei samme outliers som blei funnet ved Cooks'distance.

# random intercept (BLUPs) per ID
ri <- ranef(m1)$eilo_id[[1]]              # ein numerisk vektor
names(ri) <- rownames(ranef(m1)$eilo_id)  # set ID-namn

# Top 5 høgast og lågast
ri_top5  <- sort(ri, decreasing = TRUE)[1:5]
ri_bot5  <- sort(ri, decreasing = FALSE)[1:5]

ri_top5
ri_bot5

# Tabell oversikt over top 5 og bunn 5
data.frame(
  eilo_id = c(names(ri_top5), names(ri_bot5)),
  ranef_intercept = c(unname(ri_top5), unname(ri_bot5)),
  which = c(rep("top5", 5), rep("bottom5", 5))
)

# Sjekke residualer per fase

# Siden det var endring av variansen over tid, fra t1 til t3, er det nyttig å se på variansen

## Boksplott per fase
boxplot(.resid ~phase, data = data_m1,
        main = "Residualer per fase", xlab = "Fase", ylab = "Residual")
abline(h = 0, lty = 2)

## Residual versus fitted, farga per fase 
plot(
  data_m1$.fitted, data_m1$.resid,
  col = as.numeric(data_m1$phase),
  xlab = "Fitted", ylab = "Residual",
  main = "Residual vs Fitted (farga per fase)"
)
abline(h = 0, lty = 2)

legend(
  "topright",
  legend = levels(data_m1$phase),
  col = seq_along(levels(data_m1$phase)),
  pch = 1
)

#### Figur:Samla figur for Residualdiagnostikken -----

# A: QQ-plot random intercept
ri <- ranef(m1)$eilo_id[[1]]
ri_df <- data.frame(ri = ri)

p_res_ri <- ggplot(ri_df, aes(sample = ri)) +
  stat_qq() +
  stat_qq_line() +
  labs(title = "A  Q-Q-plot for random intercept",
       x = "Theoretical quantiles",
       y = "Sample quantiles") +
  theme_classic()

p_res_ri

# B: QQ-plot residualene
qqnorm(data_m1$.resid); qqline(data_m1$.resid)

resid_df <- data.frame(resid = data_m1$.resid)

p_resid <- ggplot(resid_df, aes(sample = resid)) +
  stat_qq() +
  stat_qq_line() +
  labs(
    title = "B  Q-Q-plot for residuals",
    x = "Theoretical quantiles",
    y = "Sample quantiles"
  ) +
  theme_classic()

p_resid

# C: Residualer vs fitted
p_rvsf <- ggplot(data_m1, aes(x = .fitted, y = .resid)) +
  geom_point() +
  geom_hline(yintercept = 0, linetype = "dashed") +
  labs(
    title = "C  Residuals vs predicted values",
    x = "Predicted values",
    y = "Residuals"
  ) +
  theme_classic()

p_rvsf

# D: Residualer vs fitted (per fase)
p_rvsf_phase <- ggplot(data_m1, aes(x = .fitted, y = .resid, colour = phase)) +
  geom_point() +
  geom_hline(yintercept = 0, linetype = "dashed") +
  labs(
    title = "D  Residuals vs predicted values by phase",
    x = "Predicted values",
    y = "Residuals",
    colour = "Phase"
  ) +
  theme_classic()

p_rvsf_phase

# Kombiner figurene
diagnostic_plot <- (p_res_ri | p_resid) /
  (p_rvsf | p_rvsf_phase)

diagnostic_plot

# Lagre figuren i høg oppløsning
ggsave("diagnostic_plots_model.png",
       diagnostic_plot,
       width = 10,
       height = 8,
       dpi = 300)


#### Outliers / inluens: enkeltpersoner (per ID) -----

## Rask sjekk om en ID skill seg ut - residualer oppsumert per ID

id_res <- data_m1 %>% 
  group_by(eilo_id) %>% 
  summarise(
    n = n(),
    mean_resid = mean(.resid, na.rm = TRUE),
    sd_resid = sd(.resid, na.rm = TRUE),
    max_abs = max(abs(.resid), na.rm = TRUE)
  ) %>% 
  arrange(desc(max_abs))

head(id_res, 10)

boxplot(.resid ~ eilo_id, data = data_m1, outline = TRUE,
        main = "Residulaer per ID", las =2)

## Influensanalyse - leave-one-cluster-out - se på påverknad per gruppe = ID

# Klargjør for å kunne bruke cooks-distance på mixed model
infl <- influence(m1, group = "eilo_id")

### Cook's distance per ID
cd <- cooks.distance(infl)

plot(cd, type = "h", ylab = "Cook's distance", main = "Influence per ID")
abline(h=4/length(cd), lty = 2)


#Identifiserer dei over terskel 4/N
threshold <- 4 / length(cd)
influential <- which(cd > threshold)
influential_ids <- names(cd > threshold)


# Kjører modellen uten dei innflytelsesrike
data_uten <- data2[!data2$eilo_id %in% influential_ids, ]
modell_uten <- lmer(tid ~ cle_within + cle_between + phase + (1 | eilo_id), data = data_uten)

# Sammenligner modellen før og etter eg har tatt ut dei som er over terskel
summary(m1)
summary(modell_uten)

# Konklusjon: Ingen endring i estimatene


#### Missingness ----

# Forberedelser

# Finner ut kven som har t3
has_t3 <- data2 %>% 
  filter(phase == "t3") %>% 
  distinct(eilo_id) %>% 
  mutate(has_t3 = 1)

# Slår sammen med baseline (t1)
baseline <- data2 %>% 
  filter(phase == "t1") %>% 
  left_join(has_t3, by = "eilo_id") %>% 
  mutate(has_t3 = ifelse(is.na(has_t3), 0, has_t3))

# Sammenligne baseline CLE E (cle_tot)
t.test(cle_tot~ has_t3, data = baseline)

# Sammenligne baseline tid
t.test(tid ~ has_t3, data = baseline)

baseline %>%
  group_by(has_t3) %>%
  summarise(
    n = n(),
    cle_mean = mean(cle_tot, na.rm = TRUE),
    tid_mean = mean(tid, na.rm = TRUE),
    cle_sd = sd(cle_tot, na.rm = TRUE),
    tid_sd = sd(tid, na.rm = TRUE)
  )

boxplot(cle_tot ~ has_t3, data = baseline,
        names = c("Mangler t3", "Har t3"),
        ylab = "Baseline CLE")

boxplot(tid ~ has_t3, data = baseline,
        names = c("Mangler t3", "Har t3"),
        ylab = "Baseline tid")
