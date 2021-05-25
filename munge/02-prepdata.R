cleandata <- cleandata %>%
  mutate(
    Age_cat = case_when(
      Age <= 35 ~ "<=35",
      Age > 35 ~ ">35"
    ),
    BMI_cat = factor(case_when(
      BMI < 25 ~ 1,
      BMI < 30 ~ 2,
      BMI >= 30 ~ 3
    ),
    levels = 1:3, labels = c("<25", "25-<30", ">=30")
    ),
    Marital.status_cat = factor(case_when(
      Marital.status %in% c("1-Single", "4-Separated or Divorced") ~ 2,
      Marital.status %in% c("2-Married", "3-Defacto (stable relationship, not married)", "5-Same sex partner") ~ 1
    ), levels = 1:2, labels = c("Married", "Single")),
    Tertiary.Education_cat = factor(case_when(
      Tertiary.Education %in% c("1-No", "4-Dropped out") ~ 2,
      Tertiary.Education %in% c("2-Graduated", "3-Still attending") ~ 1
    ), levels = 1:2, labels = c("University", "No university")),
    Current.job.situation_cat = factor(case_when(
      is.na(Current.job.situation) ~ NA_real_,
      Current.job.situation %in% c("1-Full time work", "2-Part time work") ~ 1,
      TRUE ~ 2
    ), levels = 1:2, labels = c("Employed", "Unemployed")),
    Current.living.situation_cat = factor(case_when(
      is.na(Current.living.situation) ~ NA_real_,
      Current.living.situation == "5-Alone" ~ 2,
      TRUE ~ 1
    ), levels = 1:2, labels = c("Not alone", "Alone")),
    Income_cat = factor(case_when(
      `Participant.+.partner's.income` %in% c(
        "< 189.000 kr", "189.000 kr - 378.000 kr",
        "< E 21K (< E 405 gross / wk)", "E 21K - E 42K (E 405 - E 809 gross /wk"
      ) ~ 3,
      `Participant.+.partner's.income` %in% c(
        "387.000 kr - 567.000 kr", "576.000 kr - 756.000 kr", "765.000 kr - 945.000 kr",
        "E 43K - E 63K (E 810 - E 1209 gross /wk)",
        "E 64K - E 84K (E 1210 - E1614 gross /wk)",
        "E 85K - E 105K (E 1615 - E 2019 gross /wk)"
      ) ~ 2,
      `Participant.+.partner's.income` %in% c(
        "> 1260.000 kr", "954.000 kr - 1260.000 kr",
        "> E 140K (> E 2690 gross/wk)", "E 106K - E 140K (E 2020 - E 2690 gross /wk)"
      ) ~ 1
    ), levels = 1:3, labels = c("High", "Middle", "Low")),
    Income_cat_imp = replace_na(Income_cat, "Middle"),
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
    Cord.arterial.pH_cat = case_when(
      Cord.arterial.pH < 7 ~ 2,
      Cord.arterial.pH < 7.17 ~ 1,
      Cord.arterial.pH >= 7.17 ~ 0
    ),
    Cord.arterial.pH_cat_imp = if_else(is.na(Cord.arterial.pH_cat) & Apgar.score.5.min_cat == 0, 0, Cord.arterial.pH_cat),
    Cord.artery.BE_cat = case_when(
      Cord.artery.BE < -12 ~ 2,
      Cord.artery.BE < -6 ~ 1,
      Cord.artery.BE >= -6 ~ 0
    ),
    Cord.artery.BE_cat_imp = if_else(is.na(Cord.artery.BE_cat) & Apgar.score.5.min_cat == 0, 0, Cord.artery.BE_cat),
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
