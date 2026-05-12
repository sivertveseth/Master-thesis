# ---- Monte Carlo simuleringer ----

# 1.0) Laste inn pakker ----
library(dplyr)
library(tidyr)
library(lme4)
library(purrr)
library(here)
library(irr)
library(ggplot2)
library(gt)

## 1.1) Innlesing av datasett ----

data <- readRDS(here("R","master_output", "bearbeidet datasett", "fullt_datasett_komplett_2026-04-21_22-53.rds")) 

## 1.2) Enkel rydding ----
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

# 2) Estimering av variansstruktur fra datasettet ----

## VIKTIG!

# Design: N = 303 deltakere, T (tidspunkter) = 3 tidspunkter
# Sann modell: within- og between-effekt av CLE
# Kanditater:
  ## Modell A: y ~ cle_tot + phase + (1 | eilo_id) (enkel regresjon, miks within og between)
  ## Modell B: y ~ cle_tot_within + cle_tot_between + phase + (1 | id) (Mundlak)
# Evalueringsmål: bias, RMSE, 95% KI-dekning, feil fortegn
# Scenario:
  ## 1) within = between, rho = 0
  ## 2) within er ikkje lik between, rho = 0
  ## 3) within er ikkje lik between, rho er ikkje lik 0

# Formål: Undersøke om en vanlig lineær regresjon er vel som bra som Mundlak til å fange opp endringen.

set.seed(1)

data2 <- data %>% 
  group_by(eilo_id) %>% 
  mutate(cle_between = mean(cle_tot, na.rm = TRUE),
         cle_within = cle_tot - cle_between) %>% 
  ungroup()

sd_cle_between <- sd(distinct(data2, eilo_id, cle_between)$cle_between, na.rm = TRUE)
sd_cle_within <- sd(data2$cle_within, na.rm = TRUE)

## ICC og SD for y

# Velger å bruke variansstrukturen fra datasettet for å gjøre simuleringen mest mulig realistisk

# Nullmodell

# Deler totalvariansen for tid i to deler
  ## Mellom person
  ## Innen person (er residualet = støy, endring over tid,                                     målefeil osv..)
m0 <- lmer(tid ~ 1 +        # Estimerer et gjennomsnitt (intercept) for tid
             (1 | eilo_id), # Kvar person får sitt eige avvik frå gjennomsnittet (random intercept)
           data = data2, 
           REML = TRUE)     # REML: Gir typisk mindre bias i variansestimat

vc <- as.data.frame(VarCorr(m0))   # Legger varianskomponentene fra modellen i en dataframe
sd_u <- sqrt(vc$vcov[1])           # vc$vcov[1] = variansen, sqrt() = SD til random intercept
sd_e <- sigma(m0)                  # residual SD
icc <- sd_u^2 / (sd_u^2 + sd_e^2)  # Beregner ICC

# ICC (Intraclass Correlation Coefficient)
# Er andelen av totalvariansen i tid som ligg mellom deltagerne. Resten, 1 - ICC, er innen person (over tid + målefeil + dag til dag variasjon + alt som ikkje er konstant mellom målingene)
# ICC kan hjelpe å se hvor stabil outcome (tid). 
  ##  Høg ICC -> deltagerne er stabile over tid, mykje mellom-person dominans
    ### En enkel modell har større risiko for å mistolke mellom-person dominans som              endringseffekt
  ## Lav ICC -> det er mer innen-person variasjon
    ### Det er meir informasjon til å identifisere within-effekt

# 3) Definerer den "sanne modellen"

# Scenario 1:
# Estimatet til within er lik estimatet til between, og korrelasjonen mellom random intercept og CLE er lik 0
  # Tolkning: Begge modellene skal fungere fint

# Scenario 2: 
# Estimatet til within og between er ikkje lik, og korrelasjonen mellom random intercept og CLE er lik 0
  # Tolkning: Den enkle modellen estimerer et komprimiss, der det er en vektet blanding av estimatetene til between og within. 

# Scenario 3: 
# Estimatet til within og between er ikkje lik, men korrelasjonen mellom random intercept og CLE er lik.
  # Modell A er bias

# Lagt inn som en tabell

scenarios <- tibble::tribble(
  ~scenario, ~beta_w, ~beta_b, ~rho_u_cle,
  "S1_equal_noCorr", -5, -5, 0.0,
  "S2_unequal_noCorr", -2.3, -10, 0.0,
  "S3_unequal_Corr", -2.3, -10, 0.5
)

# Viktig: Den enkle random-effects modellen forutsetter at individspesifikke, uobserverte faktorer som påvirker prestasjon ikkje er systematisk relatert til eksponeringen (CLE). Dersom denne antagelsen brytes, kan estimatet for CLE bli skjevt som følge av uobserverte konfunderende faktorer. 

# Den enkle modellen forutsetter både at innen- og mellom-person effektene er like, og at individspesifikke, uobserverte faktorer ikke er korrelert med eksponeringen. Dersom disse antakelsene brytes, vil modellen enten estimere en vektet kombinasjon av innen- og mellom-person effekter eller gi systematisk skjeve estimater. Mundlak-dekomopnering gjør det mulig å skille disse effektene og tillate korrelasjon mellom eksponering og individspesifikke effekter. 

# 4) Funksjon som simulerer data etter en sann modell ----

## Har en fast CLE_between for person
## Har en fast CLE-within per måling
## Målefeil i CLE
## Random intercept u_i som kan korrelere med CLE_between

# Lager et kunstig datasett for den sanne modellen:
# y_it = b0 + beta_w (withinCLE) + beta_b (between CLE) + fase + u_i + e_it

# Den naive modellen antar at beta_w = beta_b

sim_once <- function(N = 303,                       # Antall personer
                     T = 3,                         # Antall tidspunkter
                     beta_w, beta_b,                # Sann within- og between effekt
                     rho_u_cle,                     # Korrelasjon mellom random intercept og                                                       CLE
                     sd_cle_between, sd_cle_within, # Hvor mykje variasjon som finnes
                     sd_u, sd_e, sd_me = 0.6,       # sd_me = moderat målefeil
                     gamma_phase = c(0, -10, -20),  # Faseeffekt
                     b0 = 650){                     # Gjennomsnittlig tid
  
  # LAger datastruktur - Person 1 har T målinger, Person 2 har T målinger osv..
  id <- rep(1:N, each = T) 
  phase <- factor(rep(paste0("t", 1:T), times = N), levels = paste0("t", 1:T))
  
  # fast between CLE
  cle_bet <- rnorm(N, 0, sd_cle_between)  # Lager en verdi per person, personens typiske nivå av CLE
  
  # u_i korrelert med cle_b (kontrollert via rho)
  a <- rho_u_cle * sd_u / sd_cle_between
  sd_u_indep <- sqrt(pmax(sd_u^2 - (a^2 * sd_cle_between^2), 0))
  u <- a * cle_bet + rnorm(N, 0, sd_u_indep)  # Random intercept: f.eks treningsbakgrunn, genetikk, kapasitet
  
  # fast within CLE per observasjon
  cle_w <- rnorm(N*T, 0, sd_cle_within)  # Variasjonen innen person over tid
  
  cle_true <- rep(cle_bet, each = T) + cle_w  # Sann CLE: cle_it = between + within
  cle_obs <- cle_true + rnorm(N*T, 0, sd_me)  # Legger til målefeil
  
  # Mundlak-komponenter på observert CLE
  cle_between_obs <- ave(cle_obs, id, FUN = mean)
  cle_within_obs <- cle_obs - cle_between_obs
  
  # utfall: Bruker dei sanne effektene og ikkje cle_obs
  ph <- gamma_phase[as.integer(phase)]
  y <- b0 +                           # Gjennomsnittlig prestasjon
    beta_w * cle_w +                  # Innen-person effekt
    beta_b * rep(cle_bet, each = T) + # Mellom-person effekt
    ph +                              # Faseeffekt
    rep(u, each = T) +                # Uobserverte stabile forskjeller
    rnorm(N*T, 0, sd_e)               # Tilfeldig støy
  
  data.frame(
    id = factor(id), phase = phase, y = y,
    cle_obs = cle_obs,
    cle_between_obs = cle_between_obs,
    cle_within_obs = cle_within_obs
  )
}

# 5) Funksjon som esitmerer modellene og henter ut mål

## Bruker samme estimat/KI-utrekk hver gang.

get_wald_ci <- function(fit, term){
  fe <- lme4::fixef(fit)
  if (!(term %in% names(fe))) {
    stop("Term '", term, "' finst ikkje i fixef. Har: ", paste(names(fe), collapse = ", "))
  }
  V <- as.matrix(vcov(fit))
  if (!(term %in% rownames(V))) {
    stop("Term '", term, "' finst ikkje i vcov(). Har: ", paste(rownames(V), collapse = ", "))
  }
  b <- fe[[term]]
  se <- sqrt(V[term, term])
  z <- qnorm(0.975)
  c(est = b, low = b - z*se, high = b + z*se)
}

fit_and_score <- function(dat, beta_w, beta_b){
  
  out <- tryCatch({
    mA <- lme4::lmer(y ~ cle_obs + phase + (1|id), data = dat, REML = FALSE)
    mB <- lme4::lmer(y ~ cle_within_obs + cle_between_obs + phase + (1|id), data = dat, REML = FALSE)
    
    A  <- get_wald_ci(mA, "cle_obs")
    Bw <- get_wald_ci(mB, "cle_within_obs")
    Bb <- get_wald_ci(mB, "cle_between_obs")
    
    tibble::tibble(
      estA  = as.numeric(A["est"]),  lowA  = as.numeric(A["low"]),  highA = as.numeric(A["high"]),
      estBw = as.numeric(Bw["est"]), lowBw = as.numeric(Bw["low"]), highBw = as.numeric(Bw["high"]),
      estBb = as.numeric(Bb["est"]), lowBb = as.numeric(Bb["low"]), highBb = as.numeric(Bb["high"]),
      beta_w = beta_w, beta_b = beta_b,
      ok = TRUE
    )
  }, error = function(e){
    tibble::tibble(
      estA=NA_real_, lowA=NA_real_, highA=NA_real_,
      estBw=NA_real_, lowBw=NA_real_, highBw=NA_real_,
      estBb=NA_real_, lowBb=NA_real_, highBb=NA_real_,
      beta_w = beta_w, beta_b = beta_b,
      ok = FALSE,
      err = conditionMessage(e)
    )
  })
  
  out
}

# 6) Kjøre simuleringer (R = 1000) per scenario

run_scenario <- function(R = 1000, N = 303, T = 3,
                         beta_w, beta_b, rho_u_cle,
                         sd_cle_between, sd_cle_within,
                         sd_u, sd_e, sd_me = 0.6,
                         gamma_phase = c(0, -10, -20),
                         b0 = 650){
  
  purrr::map_dfr(1:R, \(r){
    
    dat_r <- sim_once(
      N = N, T = T,
      beta_w = beta_w, beta_b = beta_b, rho_u_cle = rho_u_cle,
      sd_cle_between = sd_cle_between,
      sd_cle_within  = sd_cle_within,
      sd_u = sd_u, sd_e = sd_e, sd_me = sd_me,
      gamma_phase = gamma_phase,
      b0 = b0
    )
    
    fit_and_score(dat_r, beta_w = beta_w, beta_b = beta_b)
  })
}

# 7) Oppsummer (bias, RMSE, dekning, feil fortegn)

summarise_perf <- function(est, low, high, truth){
  bias <- mean(est - truth, na.rm = TRUE)
  rmse <- sqrt(mean((est - truth)^2, na.rm = TRUE))
  cover <- mean(low <= truth & truth <= high, na.rm = TRUE)
  wrong_sign <- mean(sign(est) != sign(truth), na.rm = TRUE)
  c(bias = bias, rmse = rmse, cover95 = cover, wrong_sign = wrong_sign,
    mean_est = mean(est, na.rm = TRUE), sd_est = sd(est, na.rm = TRUE))
}

summarise_all <- function(res){
  bw <- unique(res$beta_w)[1]
  bb <- unique(res$beta_b)[1]
  
  A  <- summarise_perf(res$estA,  res$lowA,  res$highA,  truth = bw)
  Bw <- summarise_perf(res$estBw, res$lowBw, res$highBw, truth = bw)
  Bb <- summarise_perf(res$estBb, res$lowBb, res$highBb, truth = bb)
  
  out <- rbind(A_mixed_vs_within=A, B_within=Bw, B_between=Bb)
  tibble::as_tibble(out, rownames="model") %>%
    dplyr::mutate(n_fail = sum(res$ok == FALSE, na.rm = TRUE))
}

# 8) Resultat 

results <- scenarios %>% 
  mutate(sim = purrr::pmap(list(beta_w, beta_b, rho_u_cle),
                           \(bw, bb, rho){
                             run_scenario(R = 1000, N = 303, T = 3,
                                          beta_w = bw, beta_b = bb, rho_u_cle = rho,
                                          sd_cle_between = sd_cle_between,
                                          sd_cle_within  = sd_cle_within,
                                          sd_u = sd_u, sd_e = sd_e,
                                          sd_me = 0)
                           })) %>% 
  mutate(summary = purrr::map(sim, summarise_all))


## Tabell
results_long <- results %>%
  select(scenario, summary) %>%
  unnest(summary)

results_long %>%
  arrange(scenario, model) %>%
  gt() %>%
  fmt_number(columns = where(is.numeric), decimals = 3)


## Figur 

raw_all <- results %>%
  select(scenario, sim) %>%
  unnest(sim)


# 1) Long-format for within-sammenligning (Mixed vs Mundlak-within)
plot_dat <- raw_all %>%
  filter(ok) %>%                       # sikkerhet, siden du har ok-kolonne
  select(scenario, beta_w, estA, estBw) %>%
  pivot_longer(
    cols = c(estA, estBw),
    names_to = "model",
    values_to = "est"
  ) %>%
  mutate(
    model = recode(model,
                   estA  = "Mixed (cle_obs)",
                   estBw = "Mundlak within (cle_within)")
  )

# 2) Oppsummer: mean + 2.5/97.5 persentil
sum_dat <- plot_dat %>%
  group_by(scenario, model) %>%
  summarise(
    mean_est = mean(est, na.rm = TRUE),
    lo = quantile(est, 0.025, na.rm = TRUE),
    hi = quantile(est, 0.975, na.rm = TRUE),
    .groups = "drop"
  )

# 3) Sann within-verdi per scenario (stipla linje)
truth <- plot_dat %>%
  group_by(scenario) %>%
  summarise(beta_w = first(beta_w), .groups = "drop")

# 4) Plot (facet per scenario)
ggplot(sum_dat, aes(x = model, y = mean_est, color = model)) +
  geom_hline(
    data = truth,
    aes(yintercept = beta_w),
    linetype = "dashed",
    color = "black",
    inherit.aes = FALSE
  ) +
  geom_errorbar(aes(ymin = lo, ymax = hi), width = 0.15) +
  geom_point(size = 3) +
  facet_wrap(~ scenario, nrow = 1) +
  labs(
    title = "Mean estimate og 95% Monte Carlo-intervall for within-effekten",
    x = NULL,
    y = "Estimert within-effekt",
    color = "Modell"
  ) +
  geom_text(
    data = truth,
    aes(x = 1.5, y = beta_w, label = paste("True =", beta_w)),
    vjust = -1,
    inherit.aes = FALSE
  ) +
  theme_minimal() +
  theme(
    legend.position = "bottom",
    strip.text = element_text(face = "bold")
  )


