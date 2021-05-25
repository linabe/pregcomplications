
ProjectTemplate::reload.project()

checkass <- function(outvar, data = cleandata, adjvars = modvars, pom = FALSE) {
  if (!pom) {
    mod <- glm(formula(paste0(outvar, "== '1' ~ ", paste(adjvars, collapse = " + "))),
      data = data,
      family = binomial(link = "logit")
    )

    # Outliers ---------------------------------------------------------------
    plot(mod, which = 4, id.n = 3)

    # Multicollinearity -------------------------------------------------------
    car::vif(mod)
  }

  if (pom) {
    mod <- polr(formula(paste0(outvar, " ~ ", paste(adjvars, collapse = " + "))),
      data = data,
      Hess = TRUE
    )

    # Test prop odds assumption -----------------------------------------------

    brant(mod)
  }
}

checkass("Preeclampsia...226")
checkass("Gestational.hypertension_GH")
checkass("Gestational.diabetes_GDM")
checkass("Emergency.SC")
checkass("Placental.Abruption...238", adjvars = modvars2)
checkass("Gest_age_cat", cleandata %>% filter(`Intrauterine.fetal.death.REMOVE=1` != 1), pom = TRUE)
checkass("Gest_age_postterm", cleandata %>% filter(`Intrauterine.fetal.death.REMOVE=1` != 1))
checkass(
  "Customised.Birthweight.Centile_sga",
  cleandata %>% filter(`Intrauterine.fetal.death.REMOVE=1` != 1)
)
checkass(
  "Customised.Birthweight.Centile_lga",
  cleandata %>% filter(`Intrauterine.fetal.death.REMOVE=1` != 1)
)
checkass("Cord.arterial.pH_cat_imp",
  cleandata %>% filter(`Intrauterine.fetal.death.REMOVE=1` != 1), 
  pom = TRUE
)
checkass("Cord.artery.BE_cat_imp",
  cleandata %>% filter(`Intrauterine.fetal.death.REMOVE=1` != 1), 
  pom = TRUE
)
checkass("Apgar.score.1.min_cat", cleandata %>% filter(`Intrauterine.fetal.death.REMOVE=1` != 1), pom = TRUE)
checkass("Apgar.score.5.min_cat", cleandata %>% filter(`Intrauterine.fetal.death.REMOVE=1` != 1), pom = TRUE)
checkass("Intubation.at.birth", cleandata %>% filter(`Intrauterine.fetal.death.REMOVE=1` != 1))
