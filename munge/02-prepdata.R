cleandata <- cleandata %>%
  mutate(
    Marital.status_cat = case_when(
      Marital.status %in% c("1-Single", "4-Separated or Divorced") ~ "Single",
      Marital.status %in% c("2-Married", "3-Defacto (stable relationship, not married)", "5-Same sex partner") ~ "Married"
    ),
    Tertiary.Education_cat = case_when(
      Tertiary.Education %in% c("1-No", "4-Dropped out") ~ "No university",
      Tertiary.Education %in% c("2-Graduated", "3-Still attending") ~ "University"
    ),
    Current.job.situation_cat = case_when(
      Current.job.situation %in% c("1-Full time work", "2-Part time work") ~ "Employed",
      TRUE ~ "Unemployed"
    ),
    Current.living.situation_cat = case_when(
      Current.living.situation == "5-Alone" ~ "Alone",
      TRUE ~ "Not alone"
    ),
    Income_cat = case_when(
      `Participant.+.partner's.income` %in% c(
        "< 189.000 kr", "189.000 kr - 378.000 kr",
        "< € 21K (< € 405 gross / wk)", "€ 21K - € 42K (€ 405 - € 809 gross /wk 322"
      ) ~ "Low",
      `Participant.+.partner's.income` %in% c(
        "387.000 kr - 567.000 kr", "576.000 kr - 756.000 kr", "765.000 kr - 945.000 kr",
        "€ 43K - € 63K (€ 810 - € 1209 gross /wk)",
        "€ 64K - € 84K (€ 1210 - €1614 gross /wk)",
        "€ 85K - € 105K (€ 1615 - € 2019 gross /wk)"
      ) ~ "Middle",
      `Participant.+.partner's.income` %in% c(
        "> 1260.000 kr", "954.000 kr - 1260.000 kr",
        "> € 140K (> € 2690 gross/wk)", "€ 106K - € 140K (€ 2020 - € 2690 gross /wk)"
      ) ~ "High"
    ),
    tmp_Cigarettes = Cigarettes.Pre.Preg + Cigarettes.1.trim + Cigarettes.1.visit,
    Cigarettes_cat = if_else(tmp_Cigarettes == 0, "No", "Yes"),
    tmp_Alcohol = Alcohol.Pre.Preg + Alcohol.1.trim + Alcohol.1.visit,
    Alcohol_cat = if_else(tmp_Alcohol == 0, "No", "Yes"),
    Other.Drugs_cat = case_when(
      Other.Drugs.Pre.Preg == "No" &
        Other.Drugs.1.Trim == "No" &
        Other.drugs.1.visit == "No" ~ "No",
      Other.Drugs.Pre.Preg == "Yes" |
        Other.Drugs.1.Trim == "Yes" |
        Other.drugs.1.visit == "Yes" ~ "Yes"
    ),
    Gest_age_cat = case_when(
      `Gest.age.Delivery.(w)` <= 31 ~ 2,
      `Gest.age.Delivery.(w)` < 37 ~ 1,
      `Gest.age.Delivery.(w)` >= 37 ~ 0
    ),
    Gest_age_postterm = case_when(
      `Gest.age.Delivery.(w)` < 42 ~ 0,
      `Gest.age.Delivery.(w)` >= 42 ~ 1
    ),
    Customised.Birthweight.Centile_num = as.numeric(Customised.Birthweight.Centile),
    Customised.Birthweight.Centile_sga = if_else(Customised.Birthweight.Centile < 2.5, 1, 0),
    Customised.Birthweight.Centile_lga = if_else(Customised.Birthweight.Centile > 97.5, 1, 0),
    Cord.arterial.pH_cat = case_when(
      Cord.arterial.pH < 7 ~ 2,
      Cord.arterial.pH < 7.17 ~ 1,
      Cord.arterial.pH >= 7.17 ~ 0
    ),
    Cord.artery.BE_cat = case_when(
      Cord.artery.BE <= -12 ~ 2,
      Cord.artery.BE < -6 ~ 1,
      Cord.artery.BE >= -6 ~ 0
    ),
    Apgar.score.1.min_cat = case_when(
      Apgar.score.1.min < 4 ~ 2,
      Apgar.score.1.min < 7 ~ 1,
      Apgar.score.1.min >= 7 ~ 0
    ),
    Apgar.score.5.min_cat = case_when(
      Apgar.score.5.min < 4 ~ 2,
      Apgar.score.5.min < 7 ~ 1,
      Apgar.score.5.min >= 7 ~ 0
    ),
    Intubation.at.birth = case_when(
      Intubation.at.birth == "2-Yes" ~ 1,
      Intubation.at.birth == "1-No" ~ 0,
      TRUE ~ NA_real_
    )
  ) %>%
  mutate(across(!c(
    "Age",
    "BMI"
  ), as.factor)) %>%
  rename(
    Gestational.diabetes_GDM = `Gestational.diabetes.(GDM)`,
    Gestational.hypertension_GH = `Destational.hypertension.(GH)`
  )


# koll <- cleandata %>% select(Cord.arterial.pH_cat, Cord.arterial.pH, Cord.artery.BE, Cord.artery.BE_cat)
# koll <- cleandata %>% select(Apgar.score.1.min, Apgar.score.1.min_cat, Apgar.score.5.min, Apgar.score.5.min_cat)
