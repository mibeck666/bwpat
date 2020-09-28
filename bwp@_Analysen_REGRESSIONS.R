##Regression Models
###
##Bayesian Multilevel Regressions (Random Slopes)

list_vector_value <- full_data %>% select(starts_with("wert")) %>% names()

list_vector_job <- full_data %>% select(starts_with("ber")) %>% names()

formulas_values <- list_vector_value %>% map_chr(paste0, " ~ Studie + Geschlecht + relevel(HISCED, ref = \"Sek II (Lehre oder Matur)\") + mighin + scale(HISEI, center = TRUE) + (1 | code_KL)")

names(formulas_values) <- list_vector_value

formulas_job <- list_vector_job %>% map_chr(paste0, " ~ Studie + Geschlecht + relevel(HISCED, ref = \"Sek II (Lehre oder Matur)\") + mighin + scale(HISEI, center = TRUE) + (1 | code_KL)")

names(formulas_job) <- list_vector_job

##Life Goals / Values
########################
########################

#Compile Model - Deactivate, because 
#mod_values_stan <- lapply(formulas_values, function(form) stan_lmer(form, data = full_data, REML = FALSE))

#save(mod_values_stan, file = file.path(pfad, Sys.Date(), "values_stan.R"))

#sum_values_mod <- map(mod_values_stan, summary)

#Coeff-Table
load(file = file.path(pfad, "values_stan.R"))

table_coeff_values <- map(mod_values_stan, coeff_table)

##Job Goals 
########################
########################

#Compile Model
#mod_job_stan <- lapply(formulas_job, function(form) stan_lmer(form, data = full_data, REML = FALSE))

#save(mod_job_stan, file = file.path(pfad, Sys.Date(), "job_stan.R"))

#sum_job_mod <- map(mod_job_stan, summary)


#Coeff-Table
load(file = file.path(pfad, "job_stan.R"))

table_coeff_job <- map(mod_job_stan, coeff_table)
