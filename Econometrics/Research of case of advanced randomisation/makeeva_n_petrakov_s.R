# авторы кода - Макеева Наталья, Петраков Сергей, э-401, 2020

library(dplyr)
library(ggplot2)
library(reshape2)
library(stargazer)
library(readstata13)
library(testthat)
library(tidyverse)
library(plm)
library(AER)
set.seed(42)

vars_for_balance <- c("level_randomization",
                      "nb_registered_pr12t1_an",
                      "prop_leftabstention_an",
                      "prop_turnout_pr07t1_an",
                      "prop_turnout_pr07t2_an",
                      "prop_royal_pr07t1_an",
                      "prop_royal_pr07t2_an",
                      "population",
                      "region_id2",
                      "region_id3",
                      "region_id4",
                      "region_id5",
                      "region_id6",
                      "region_id7",
                      "region_id8",
                      "region_id9",
                      "region_id10",
                      "region_id11",
                      "region_id12",
                      "region_id13",
                      "region_id14",
                      "region_id15",
                      "region_id16",
                      "region_id17",
                      "region_id18",
                      "region_id19",
                      "region_id20",
                      "region_id21",
                      "region_id22",
                      "region_id23",
                      "share_men",
                      "share_0014",
                      "share_1529",
                      "share_3044",
                      "share_4559",
                      "share_6074",
                      "share_75p",
                      "share_working_pop",
                      "share_unemployed",
                      "median_income")


controls <- c("nb_registered_pr12t1_an", #????
              "population",
              "share_men",
              "share_0014",
              "share_1529",
              "share_3044",
              "share_4559",
              "share_6074",
              "share_unemployed",
              "share_working_pop",
              "population_delta",
              "share_men_delta",
              "share_0014_delta",
              "share_1529_delta",
              "share_3044_delta",
              "share_4559_delta",
              "share_6074_delta",
              "share_unemployed_delta",
              "share_working_pop_delta")



main_data <- read.dta13("Analysis/analysis.dta")

new_names <- attr(main_data, "var.labels")
new_names[12] <- 'treatment'
new_names <- new_names[match(c(vars_for_balance, 'treatment'), names(main_data))]

base_randomization <- read.dta13("Intermediate/base_randomization_v2.dta")

### RANDOMIZATION ###
generate_stratum <- function(base_randomization) {
  
  percint_level <- base_randomization %>%
    filter(level_randomization == 1) %>%
    group_by(departement_code, territory)%>%
    arrange(desc(prop_leftabstention)) %>%
    mutate(stratum = (row_number()-1)%/%5+1)%>%
    group_by(departement_code, territory, stratum)%>%
    mutate(stratum_size = n())%>%
    ungroup()%>%
    group_by(departement_code, territory)%>%
    mutate(nb_strata = max(stratum))
  
  municipality_level <- base_randomization %>%
    filter(level_randomization == 0) %>%
    group_by(territory, departement_code) %>%
    arrange(desc(prop_leftabstention_mun)) %>%
    mutate(stratum = (row_number()-1)%/%5+1) %>% 
    group_by(departement_code, territory, stratum)%>%
    mutate(stratum_size = n())%>%
    ungroup()%>%
    group_by(departement_code, territory)%>%
    mutate(nb_strata = max(stratum))
  
  final <- rbind(percint_level, municipality_level)
  
  return(final)
}

#stratified <- generate_stratum(base_randomization)



generate_treatment <- function(base_randomization){
  
  percint_level <- generate_stratum(base_randomization) %>%
    filter(level_randomization == 1)%>%
    arrange(municipality_code, precinct_code)%>%
    mutate(rand = runif(n()))%>%
    arrange(departement_code, territory, stratum, rand)%>%
    group_by(departement_code, territory, stratum)%>%
    mutate(rand2 = row_number())%>%
    mutate(nb_strata_included = 1)%>%
    mutate(treatment = ifelse(stratum == 1 & stratum_size == 5 & rand2 == 5,0,100))%>%
    mutate(treatment = ifelse(stratum == 1 & stratum_size == 5 & rand2 != 5,1,treatment))%>%
    mutate(treatment = ifelse(stratum == 1 & stratum_size != 5 & rand <= 0.2,0,treatment))%>%
    mutate(treatment = ifelse(stratum == 1 & stratum_size != 5 & rand > 0.2,1,treatment))
  for (i in 2:15){
    percint_level <- percint_level%>%
      mutate(nb_registered_treatment = ifelse(treatment == 1, nb_registered_prim,0))%>%
      group_by(departement_code, territory)%>%
      mutate(temporary_count = sum(nb_registered_treatment))%>%
      mutate(nb_strata_included = ifelse(temporary_count< target_ter & nb_strata >= i, i, nb_strata_included))%>%
      mutate(treatment = ifelse(nb_strata_included == i & stratum == i & stratum_size == 5 & rand2 == 5,0,treatment))%>%
      mutate(treatment = ifelse(nb_strata_included == i & stratum == i & stratum_size == 5 & rand2 != 5,1,treatment))%>%
      mutate(treatment = ifelse(nb_strata_included == i & stratum == i & stratum_size != 5 & rand <= 0.2,0,treatment))%>%
      mutate(treatment = ifelse(nb_strata_included == i & stratum == i & stratum_size != 5 & rand > 0.2,1,treatment))%>%
      select(-c(nb_registered_treatment, temporary_count))
  }
  
  municipality_level <- generate_stratum(base_randomization) %>%
    filter(level_randomization == 0)%>%
    arrange(municipality_code)%>%
    mutate(rand = runif(n()))%>%
    arrange(departement_code, territory, stratum, rand)%>%
    group_by(departement_code, territory, stratum)%>%
    mutate(rand2 = row_number())%>%
    mutate(nb_strata_included = ifelse(territory_excluded!=1,1,0))%>%
    mutate(treatment = ifelse(territory_excluded != 1 & stratum == 1 & stratum_size == 5 & rand2 == 5,0,100))%>%
    mutate(treatment = ifelse(territory_excluded != 1 & stratum == 1 & stratum_size == 5 & rand2 != 5,1,treatment))%>%
    mutate(treatment = ifelse(territory_excluded != 1 & stratum == 1 & stratum_size != 5 & rand <= 0.2,0,treatment))%>%
    mutate(treatment = ifelse(territory_excluded != 1 & stratum == 1 & stratum_size != 5 & rand > 0.2,1,treatment))
  
  for (i in 2:15){
    municipality_level <- municipality_level%>%
      mutate(nb_registered_treatment = ifelse(treatment == 1, nb_registered_mun,0))%>%
      group_by(departement_code, territory)%>%
      mutate(temporary_count = sum(nb_registered_treatment))%>%
      mutate(nb_strata_included = ifelse(territory_excluded != 1 & temporary_count< target_ter & nb_strata >= i, i, nb_strata_included))%>%
      mutate(treatment = ifelse(territory_excluded != 1 & nb_strata_included == i & stratum == i & stratum_size == 5 & rand2 == 5,0,treatment))%>%
      mutate(treatment = ifelse(territory_excluded != 1 & nb_strata_included == i & stratum == i & stratum_size == 5 & rand2 != 5,1,treatment))%>%
      mutate(treatment = ifelse(territory_excluded != 1 & nb_strata_included == i & stratum == i & stratum_size != 5 & rand <= 0.2,0,treatment))%>%
      mutate(treatment = ifelse(territory_excluded != 1 & nb_strata_included == i & stratum == i & stratum_size != 5 & rand > 0.2,1,treatment))%>%
      select(-c(nb_registered_treatment, temporary_count))
  }
  
  final <- rbind(percint_level, municipality_level)%>%
    select(-c(rand,rand2))%>%
    mutate(treatment = ifelse(treatment == 100, NA, treatment))%>%
    mutate(nb_strata_included = ifelse(nb_strata_included == 0, NA, nb_strata_included))
  return(final)
}

treatment <- generate_treatment(base_randomization)


test_treatment <- function(base_randomization) {
  max_treated <- generate_treatment(base_randomization) %>%  group_by(territory, departement_code, stratum) %>%
    summarize(treated=sum(treatment), control=sum(1 - treatment)) %>% group_by() %>%
    summarize(max(ifelse(treated + control == 5, treated, 0)))
  expect_true(max_treated == 4)
}


test_generate_stratum <- function(base_randomization) {
  result <- generate_stratum(base_randomization) %>%
    group_by(territory, departement_code, stratum) %>%
    summarize(max_PO=max(prop_leftabstention), count=length(prop_leftabstention)) %>%
    group_by(territory, departement_code) %>% summarize( max_count=max(count)) %>%
    group_by() %>% summarise(max_count=max(max_count))
  expect_true(result$max_count == 5)
}


test_that("test_generate_stratum", {test_generate_stratum(base_randomization)})


### BALANCE ON COVARIATES ###

summary_table <- function(data){
  summary_data_mean <- data%>%
    group_by(treatment) %>% summarise_at(vars_for_balance, funs(round(mean(.,na.rm = TRUE), 3)))
  summary_data_sd <- data%>%
    group_by(treatment) %>% summarise_at(vars_for_balance, funs(round(sd(.,na.rm = TRUE), 3)))
  summary_data_n <- data%>%
    group_by(treatment) %>% summarise_at(vars_for_balance, funs(length(which(!is.na(.)))))
  summary_transpose_mean <- as.data.frame(t(as.matrix(summary_data_mean)))
  summary_transpose_sd <- as.data.frame(t(as.matrix(summary_data_sd)))
  summary_transpose_n <- as.data.frame(t(as.matrix(summary_data_n)))
  summary_transpose_n$n <- summary_transpose_n$V1+summary_transpose_n$V2
  combined<-cbind(summary_transpose_mean, summary_transpose_sd)[, c(1, 3, 2, 4)]
  combined<-cbind(combined, summary_transpose_n$n)
  combined<-combined[-1,]
  colnames(combined)<- c("Mean (Control)", "Sd (Control)", "Mean (Treatment)", "Sd (Treatment)", "Number of observations")
  row.names(combined) <- c("Randomization at precinct level", "Number of registered citizens", "Potential to win votes, PO", 
                           "Voter turnout, 2007 pres. election, first round", "Voter turnout, 2007 pres. election, second round",
                           "PS vote share, 2007 pres. election, first round", "PS vote share, 2007 pres. election, second round",
                           "Population of the municipality", "Ile-de-France", "Champagne-Ardenne", 
                           "Picardie", "Haute-Normandie", "Centre-Val de Loire", "Basse-Normandie",
                           "Bourgogne", "Nord-Pas-de-Calais", "Lorraine", "Alsace", "Franche-Comté", " Pays-de-la-Loire",
                           "Bretagne", "Poitou-Charentes", "Aquitaine", "Midi-Pyrénées", "Limousin", "Rhône-Alpes",
                           "Auvergne", "Languedoc-Roussillon", "Provence-Alpes-Côte-d’Azur", "Corse",
                           "Share of men", "Share of the population with age 0-14", "Share of the population with age 15-29",
                           "Share of the population with age 30-44", "Share of the population with age 45-59",
                           "Share of the population with age 60-74", "Share of the population with age 75 and older",
                           "Share of working population within 15-64", "Share of unemployed (among working population)",
                           "Median income")
  return(combined)
}

balance_on_covariates <- function (data) {
  t_test <- c()
  for (i in vars_for_balance){
    control <- filter(data,treatment == 0) %>% select(i) 
    treatment <- filter(data,treatment == 1) %>% select(i)
    t_test <- append(t_test, round(t.test(treatment,control)$p.value, digits = 3))}
  combined <- summary_table(data)
  combined<-cbind(combined, t_test)[, c(1, 2, 3, 4, 6, 5)]
  colnames(combined)[5] <- "p-value treatment = control"
  row.names(combined)[1] <- "Randomization at precinct level"
  return(combined)
}

filtered <- main_data[(main_data$territory_in==1) & (main_data$merge_results12 == 1),c(vars_for_balance, "treatment")]
final_results <- balance_on_covariates(filtered)


stargazer(final_results, type='text', summary=FALSE) # KEEP THIS LINE SO THAT WE CAN SEE YOUR OUTPUT HERE.
# ADD A LATEX/HTML output for your purposes
stargazer(final_results, type='text', summary=FALSE, out = 'table1.html')

###########################
### Результаты ###
###########################

#1
main_data <- read.dta13("Analysis/analysis.dta")

#2
filtered_main_data <- main_data %>% 
  filter(territory_in == 1)


#4
make_models <- function(data1, data2, data3, y, previous_results = NULL, controls = NULL){
  #data <- filtered_main_data %>% filter(merge_results07 == 1)
  #data1 <- filtered_main_data %>% filter((merge_results07 == 1)&(merge_results02 == 1))
  model1 <- plm(reformulate(termlabels = c("treatment"), response = y),
                data=data1,
                index=c("stratum_identifier"),
                model="within")
  model2 <- plm(reformulate(termlabels = c("treatment", previous_results), response = y),
                data=data2,
                index=c("stratum_identifier"),
                model="within")
  model3 <- plm(reformulate(termlabels = c(c("treatment"), previous_results, controls), response = y),
                data=data3,
                index=c("stratum_identifier"),
                model="within")
  return(list(model1, model2, model3))
}


data11 <- filtered_main_data %>% filter(merge_results12 == 1)
data22 <- filtered_main_data %>% filter(merge_results12 == 1 & merge_results07 == 1)
data33 <- filtered_main_data %>% filter(merge_results12 == 1 & merge_results07 == 1)

first5 <- make_models(data11, data22, data33, "prop_hollande_pr12t1_an", c("prop_royal_pr07t1_an", "prop_leftabstention_an"), controls)
second5 <- make_models(data11, data22, data33, "prop_hollande_pr12t2_an", c("prop_royal_pr07t2_an", "prop_leftabstention_an"), controls) 
third5 <- make_models(data11, data22, data33, "prop_hollande_pr12t12_an", c("prop_royal_pr07t12_an", "prop_leftabstention_an"), controls)

mean_21 <- mean(sapply(filtered_main_data %>% 
                         filter((treatment == 0) & (merge_results12 == 1)) %>% 
                         select(prop_hollande_pr12t1_an), as.numeric))

mean_22 <- mean(sapply(filtered_main_data %>% 
                         filter((treatment == 0) & (merge_results12 == 1) & (merge_results07 == 1)) %>% 
                         select(prop_hollande_pr12t1_an), as.numeric))

mean_24 <- mean(sapply(filtered_main_data %>% 
                         filter((treatment == 0) & (merge_results12 == 1)) %>% 
                         select(prop_hollande_pr12t2_an), as.numeric))

mean_25 <- mean(sapply(filtered_main_data %>% 
                         filter((treatment == 0) & (merge_results12 == 1)& (merge_results07 == 1)) %>% 
                         select(prop_hollande_pr12t2_an), as.numeric))

mean_27 <- mean(sapply(filtered_main_data %>% 
                         filter((treatment == 0) & (merge_results12 == 1)) %>% 
                         select(prop_hollande_pr12t12_an), as.numeric))


mean_28 <- mean(sapply(filtered_main_data %>% 
                         filter((treatment == 0) & (merge_results12 == 1) & (merge_results07 == 1)) %>% 
                         select(prop_hollande_pr12t12_an), as.numeric))



data1 <- filtered_main_data %>% filter(merge_results07 == 1)
data2 <- filtered_main_data %>% filter((merge_results07 == 1)&(merge_results02 == 1))
data3 <- filtered_main_data %>% filter((merge_results07 == 1)&(merge_results02 == 1))

first <- make_models(data1, data2, data3, "prop_royal_pr07t1_an", "prop_jospin_pr02t1_an", controls)
second <- make_models(data1, data2, data3, "prop_royal_pr07t2_an", "prop_jospin_pr02t2_an", controls)
third <- make_models(data1, data2, data3, "prop_royal_pr07t12_an", "prop_jospin_pr02t12_an", controls)


mean_11 <- mean(sapply(filtered_main_data %>% 
                         filter((treatment == 0) & (merge_results07 == 1)) %>% 
                         select(prop_royal_pr07t1_an), as.numeric))


mean_23 <- mean(sapply(filtered_main_data %>% 
                         filter((treatment == 0) & (merge_results07 == 1)  & (merge_results02 == 1)) %>% 
                         select(prop_royal_pr07t1_an), as.numeric))

mean_44 <- mean(sapply(filtered_main_data %>% 
                         filter((treatment == 0) & (merge_results07 == 1)) %>% 
                         select(prop_royal_pr07t2_an), as.numeric))



mean_55 <- mean(sapply(filtered_main_data %>% 
                         filter((treatment == 0) & (merge_results07 == 1) & (merge_results02 == 1)) %>% 
                         select(prop_royal_pr07t2_an), as.numeric))



mean_77 <- mean(sapply(filtered_main_data %>% 
                         filter((treatment == 0) & (merge_results07 == 1)) %>% 
                         select(prop_royal_pr07t12_an), as.numeric))


mean_88 <- mean(sapply(filtered_main_data %>% 
                         filter((treatment == 0) & (merge_results07 == 1) & (merge_results02 == 1)) %>% 
                         select(prop_royal_pr07t12_an), as.numeric))



rob_se5 <- list(sqrt(diag(vcovHC(first5[[1]], type = "HC1"))),
                sqrt(diag(vcovHC(first5[[2]], type = "HC1"))),
                sqrt(diag(vcovHC(first5[[3]], type = "HC1"))),
                sqrt(diag(vcovHC(second5[[1]], type = "HC1"))),
                sqrt(diag(vcovHC(second5[[2]], type = "HC1"))),
                sqrt(diag(vcovHC(second5[[3]], type = "HC1"))),
                sqrt(diag(vcovHC(third5[[1]], type = "HC1"))),
                sqrt(diag(vcovHC(third5[[2]], type = "HC1"))),
                sqrt(diag(vcovHC(third5[[3]], type = "HC1"))))

rob_se <- list(sqrt(diag(vcovHC(first[[1]], type = "HC1"))),
               sqrt(diag(vcovHC(first[[2]], type = "HC1"))),
               sqrt(diag(vcovHC(first[[3]], type = "HC1"))),
               sqrt(diag(vcovHC(second[[1]], type = "HC1"))),
               sqrt(diag(vcovHC(second[[2]], type = "HC1"))),
               sqrt(diag(vcovHC(second[[3]], type = "HC1"))),
               sqrt(diag(vcovHC(third[[1]], type = "HC1"))),
               sqrt(diag(vcovHC(third[[2]], type = "HC1"))),
               sqrt(diag(vcovHC(third[[3]], type = "HC1"))))


stargazer(first5, second5, third5,
          type='text', title = 'Table 5—Impact on Hollande\'s vote share', 
          add.lines=list(c("Strata fixed effects", "X", "X", "X","X", "X", "X","X", "X", "X"),
                         c("Control for past outcome and PO", "", "X", "X", "", "X", "X", "", "X", "X"),
                         c("Additional controls", "","","X", "","","X", "","","X"),
                         c("Mean in control group", round(mean_21, 4), round(mean_22, 4),round(mean_22, 4),
                           round(mean_24, 4), round(mean_25, 4), round(mean_25, 4), round(mean_27, 4), round(mean_28, 4), round(mean_28, 4))),
          se = rob_se5,
          keep.stat=c("n", "rsq"),
          keep = c("\\btreatment\\b"),
          star.char = c("", "", ""),
          summary=TRUE, digits = 4, 
          column.labels   = c("First round", "Second round", "Average of first and second rounds"),
          column.separate = c(3, 3, 3),
          dep.var.labels.include = FALSE, table.layout = "m==c-#-!tas-", out = 'Table5.html', omit.table.layout = "n")

stargazer(first, second, third,
          type='text', title = 'Table 10—Placebo-Impact on Royal’s vote share in 2007', 
          add.lines=list(c("Strata fixed effects", "X", "X", "X","X", "X", "X","X", "X", "X"),
                         c("Control for past outcome", "", "X", "X", "", "X", "X", "", "X", "X"),
                         c("Additional controls", "","","X", "","","X", "","","X"),
                         c("Mean in control group", round(mean_11, 4), round(mean_23, 4),round(mean_23, 4),
                            round(mean_44, 4), round(mean_55, 4), round(mean_55, 4), 
                            round(mean_77, 4), round(mean_88, 4), round(mean_88, 4))),
          se = rob_se,
          keep.stat=c("n", "rsq"),
          keep = c("\\btreatment\\b"),
          star.char = c("", "", ""),
          summary=TRUE, digits = 4, 
          column.labels   = c("First round", "Second round", "Average of first and second rounds"),
          column.separate = c(3, 3, 3),
          dep.var.labels.include = FALSE, table.layout = "m==c-#-!tas-", out = 'Table10.html', omit.table.layout = "n")
  