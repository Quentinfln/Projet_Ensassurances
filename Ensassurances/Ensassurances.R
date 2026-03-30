# ============================================================
# app.R - Projet ENSAssuRances : Analyse des sinistres auto
# ============================================================

shiny::runApp()

# -----------------------------
# 1) Packages
# -----------------------------
required_packages <- c(
  "shiny", "shinydashboard", "tidyverse", "readxl", "lubridate",
  "janitor", "DT", "plotly", "stringr", "forcats", "tidyr",
  "purrr", "scales", "leaflet", "sf", "viridisLite"
)

to_install <- required_packages[!required_packages %in% installed.packages()[, "Package"]]
if (length(to_install) > 0) install.packages(to_install)

library(shiny)
library(shinydashboard)
library(tidyverse)
library(readxl)
library(lubridate)
library(janitor)
library(DT)
library(plotly)
library(stringr)
library(forcats)
library(tidyr)
library(purrr)
library(scales)
library(leaflet)
library(sf)
library(viridisLite)

options(scipen = 999)
Sys.setlocale("LC_TIME", "French")

# -----------------------------
# 2) Fonctions utilitaires
# -----------------------------

safe_read_xlsx <- function(path, type = c("contrat", "sinistre")) {
  type <- match.arg(type)
  df <- readxl::read_xlsx(path)
  
  if (type == "contrat") {
    names(df) <- c(
      "idx_ct", "idx_year", "vh_immat", "sit_start_date", "sit_end_date",
      "sit_expo", "drv1_age", "drv1_sex", "drv1_drive_licence_type",
      "drv1_drive_licence_age", "vh_age", "ct_frm", "ct_ass_base",
      "ct_ass0km", "ct_ass_vhr", "vh_segment", "vh_marque", "vh_energy",
      "vh_weight", "vh_din", "vh_value", "vh_group", "vh_class",
      "ct_usage", "ct_km", "ct_deduc", "claims_ant", "ct_insee",
      "id1_ass_base", "id1_ass0km", "id1_ass_vhr",
      "id2_ass_base", "id2_ass0km", "id2_ass_vhr",
      "id3_ass_base", "id3_ass0km", "id3_ass_vhr",
      "cot_ass_base", "cot_ass0km", "cot_ass_vhr"
    )
  }
  
  if (type == "sinistre") {
    names(df) <- c(
      "idx_sin", "gar_sin", "surv_sin", "decl_sin",
      "clo_sin", "gest_sin", "mt_eval", "mt_regl"
    )
  }
  
  df
}

mode_impute <- function(x) {
  x <- x[!is.na(x) & x != ""]
  if (length(x) == 0) return(NA)
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}

normalize_yes_no <- function(x) {
  x <- as.character(x)
  case_when(
    x %in% c("O", "Oui", "oui", "1", 1, TRUE) ~ "Oui",
    x %in% c("N", "Non", "non", "0", 0, FALSE) ~ "Non",
    TRUE ~ "Inconnu"
  )
}

normalize_sex <- function(x) {
  x <- as.character(x)
  case_when(
    x %in% c("H", "M", "Masculin", "Homme") ~ "Homme",
    x %in% c("F", "Feminin", "Féminin", "Femme") ~ "Femme",
    TRUE ~ "Inconnu"
  )
}

normalize_energy <- function(x) {
  x <- str_to_title(as.character(x))
  case_when(
    str_detect(x, "Essence") ~ "Essence",
    str_detect(x, "Diesel") ~ "Diesel",
    str_detect(x, "Elect") | str_detect(x, "Élect") ~ "Électrique",
    str_detect(x, "Hybr") ~ "Hybride",
    TRUE ~ "Autre / Inconnu"
  )
}

normalize_usage <- function(x) {
  x <- as.character(x)
  case_when(
    x %in% c("Pri", "Privé", "Prive") ~ "Privé",
    x %in% c("Pro", "Professionnel") ~ "Professionnel",
    TRUE ~ "Autre / Inconnu"
  )
}

normalize_licence <- function(x) {
  x <- str_to_title(as.character(x))
  case_when(
    str_detect(x, "Traditionnel") ~ "Traditionnel",
    str_detect(x, "Accompagn") ~ "Conduite accompagnée",
    TRUE ~ "Autre / Inconnu"
  )
}

age_band <- function(x) {
  cut(
    x,
    breaks = c(18, 25, 35, 45, 55, 65, Inf),
    labels = c("18-24", "25-34", "35-44", "45-54", "55-64", "65+"),
    right = FALSE
  )
}

vehicle_age_band <- function(x) {
  cut(
    x,
    breaks = c(-Inf, 2, 5, 10, 15, Inf),
    labels = c("0-2 ans", "3-5 ans", "6-10 ans", "11-15 ans", "15+ ans"),
    right = TRUE
  )
}

value_band <- function(x) {
  cut(
    x,
    breaks = c(-Inf, 5000, 10000, 20000, 30000, Inf),
    labels = c("<5k", "5k-10k", "10k-20k", "20k-30k", "30k+"),
    right = TRUE
  )
}

impute_missing <- function(df) {
  df %>%
    mutate(
      drv1_age = ifelse(is.na(drv1_age), median(drv1_age, na.rm = TRUE), drv1_age),
      drv1_drive_licence_age = ifelse(
        is.na(drv1_drive_licence_age),
        median(drv1_drive_licence_age, na.rm = TRUE),
        drv1_drive_licence_age
      ),
      vh_age = ifelse(is.na(vh_age), median(vh_age, na.rm = TRUE), vh_age),
      vh_value = ifelse(is.na(vh_value), median(vh_value, na.rm = TRUE), vh_value),
      vh_weight = ifelse(is.na(vh_weight), median(vh_weight, na.rm = TRUE), vh_weight),
      vh_din = ifelse(is.na(vh_din), median(vh_din, na.rm = TRUE), vh_din),
      claims_ant = ifelse(is.na(claims_ant), 0, claims_ant),
      drv1_sex = replace_na(drv1_sex, "Inconnu"),
      drv1_drive_licence_type = replace_na(drv1_drive_licence_type, "Inconnu"),
      vh_segment = replace_na(vh_segment, "Inconnu"),
      vh_marque = replace_na(vh_marque, "Inconnu"),
      vh_energy = replace_na(vh_energy, "Inconnu"),
      vh_group = replace_na(as.character(vh_group), "Inconnu"),
      vh_class = replace_na(as.character(vh_class), "Inconnu"),
      ct_usage = replace_na(ct_usage, "Inconnu"),
      ct_km = replace_na(ct_km, "Inconnu"),
      ct_insee = replace_na(ct_insee, "Inconnu")
    )
}

prepare_data <- function(contrat_path = "Contrat.xlsx", sinistre_path = "Sinistre.xlsx") {
  
  # ----- lecture
  contrats_raw <- safe_read_xlsx(contrat_path, type = "contrat")
  sinistres_raw <- safe_read_xlsx(sinistre_path, type = "sinistre")
  
  # ----- nettoyage contrats
  contrats <- contrats_raw %>%
    distinct() %>%
    mutate(
      idx_ct = as.character(idx_ct),
      idx_year = as.integer(idx_year),
      vh_immat = as.character(vh_immat),
      
      sit_start_date = suppressWarnings(lubridate::ymd(sit_start_date)),
      sit_end_date   = suppressWarnings(lubridate::ymd(sit_end_date)),
      
      sit_expo = as.numeric(sit_expo),
      drv1_age = as.numeric(drv1_age),
      drv1_drive_licence_age = as.numeric(drv1_drive_licence_age),
      vh_age = as.numeric(vh_age),
      vh_weight = as.numeric(vh_weight),
      vh_din = as.numeric(vh_din),
      vh_value = as.numeric(vh_value),
      claims_ant = as.integer(claims_ant),
      ct_deduc = as.integer(ct_deduc),
      
      drv1_sex = normalize_sex(drv1_sex),
      drv1_drive_licence_type = normalize_licence(drv1_drive_licence_type),
      vh_energy = normalize_energy(vh_energy),
      ct_usage = normalize_usage(ct_usage),
      ct_km = normalize_yes_no(ct_km),
      
      ct_ass_base = ifelse(is.na(ct_ass_base), 0, as.integer(ct_ass_base)),
      ct_ass0km = ifelse(is.na(ct_ass0km), 0, as.integer(ct_ass0km)),
      ct_ass_vhr = ifelse(is.na(ct_ass_vhr), 0, as.integer(ct_ass_vhr)),
      
      cot_ass_base = ifelse(is.na(cot_ass_base), 0, as.numeric(cot_ass_base)),
      cot_ass0km = ifelse(is.na(cot_ass0km), 0, as.numeric(cot_ass0km)),
      cot_ass_vhr = ifelse(is.na(cot_ass_vhr), 0, as.numeric(cot_ass_vhr)),
      
      ct_insee = str_pad(as.character(ct_insee), width = 5, side = "left", pad = "0"),
      
      departement = case_when(
        str_sub(ct_insee, 1, 2) %in% c("97", "98") ~ str_sub(ct_insee, 1, 3),
        TRUE ~ str_sub(ct_insee, 1, 2)
      )
    ) %>%
    impute_missing() %>%
    mutate(
      age_conducteur_classe = age_band(drv1_age),
      age_vehicule_classe = vehicle_age_band(vh_age),
      valeur_vehicule_classe = value_band(vh_value),
      jeune_conducteur = if_else(drv1_age < 25, "Oui", "Non"),
      vehicule_electrique = if_else(vh_energy == "Électrique", "Oui", "Non"),
      ancien_sinistre = if_else(claims_ant > 0, "Oui", "Non"),
      exposition = ifelse(is.na(sit_expo) | sit_expo <= 0, 1, sit_expo)
    )
  
  # ----- nettoyage sinistres
  sinistres <- sinistres_raw %>%
    distinct() %>%
    mutate(
      idx_sin = as.character(idx_sin),
      gar_sin = as.character(gar_sin),
      surv_sin = suppressWarnings(lubridate::ymd(surv_sin)),
      decl_sin = suppressWarnings(lubridate::ymd(decl_sin)),
      clo_sin  = suppressWarnings(lubridate::ymd(clo_sin)),
      gest_sin = suppressWarnings(lubridate::ymd(gest_sin)),
      mt_eval = as.numeric(mt_eval),
      mt_regl = as.numeric(mt_regl),
      annee_surv = year(surv_sin),
      annee_decl = year(decl_sin),
      delai_declaration = as.numeric(decl_sin - surv_sin),
      delai_cloture = as.numeric(clo_sin - surv_sin),
      montant_restant = mt_eval - mt_regl
    )
  
  # ----- table de correspondance contrat-sinistre
  claim_cols <- c(
    "id1_ass_base", "id1_ass0km", "id1_ass_vhr",
    "id2_ass_base", "id2_ass0km", "id2_ass_vhr",
    "id3_ass_base", "id3_ass0km", "id3_ass_vhr"
  )
  
  links <- contrats %>%
    select(idx_ct, idx_year, all_of(claim_cols)) %>%
    pivot_longer(
      cols = all_of(claim_cols),
      names_to = "slot_garantie",
      values_to = "idx_sin"
    ) %>%
    filter(!is.na(idx_sin), idx_sin != "") %>%
    mutate(
      garantie_source = case_when(
        str_detect(slot_garantie, "ass_base") ~ "AssBase",
        str_detect(slot_garantie, "ass0km") ~ "Ass0km",
        str_detect(slot_garantie, "ass_vhr") ~ "AssVHR",
        TRUE ~ "Autre"
      )
    ) %>%
    distinct(idx_ct, idx_year, idx_sin, .keep_all = TRUE)
  
  # ----- jointure
  claims_joined <- links %>%
    left_join(sinistres, by = "idx_sin") %>%
    left_join(
      contrats %>%
        select(
          idx_ct, idx_year, drv1_age, drv1_sex, drv1_drive_licence_type,
          drv1_drive_licence_age, vh_age, vh_segment, vh_marque, vh_energy,
          vh_group, vh_class, vh_value, vh_weight, vh_din, ct_frm, ct_usage,
          ct_km, claims_ant, ct_insee, departement, age_conducteur_classe,
          age_vehicule_classe, valeur_vehicule_classe, jeune_conducteur,
          vehicule_electrique, ancien_sinistre, exposition
        ),
      by = c("idx_ct", "idx_year")
    )
  
  # ----- indicateur de sinistralité par contrat
  claims_by_contract <- claims_joined %>%
    group_by(idx_ct, idx_year) %>%
    summarise(
      nb_sinistres = n_distinct(idx_sin, na.rm = TRUE),
      total_mt_eval = sum(mt_eval, na.rm = TRUE),
      total_mt_regl = sum(mt_regl, na.rm = TRUE),
      .groups = "drop"
    )
  
  contrats_enrichis <- contrats %>%
    left_join(claims_by_contract, by = c("idx_ct", "idx_year")) %>%
    mutate(
      nb_sinistres = replace_na(nb_sinistres, 0),
      total_mt_eval = replace_na(total_mt_eval, 0),
      total_mt_regl = replace_na(total_mt_regl, 0),
      frequence_sinistre = nb_sinistres / exposition
    )
  
  # ----- résumé qualité
  missing_summary <- bind_rows(
    contrats_enrichis %>%
      summarise(across(everything(), ~ sum(is.na(.)))) %>%
      pivot_longer(everything(), names_to = "variable", values_to = "nb_na") %>%
      mutate(table = "contrats"),
    sinistres %>%
      summarise(across(everything(), ~ sum(is.na(.)))) %>%
      pivot_longer(everything(), names_to = "variable", values_to = "nb_na") %>%
      mutate(table = "sinistres")
  ) %>%
    arrange(desc(nb_na))
  
  duplicates_summary <- tibble(
    table = c("contrats", "sinistres"),
    nb_doublons = c(
      nrow(contrats_raw) - nrow(distinct(contrats_raw)),
      nrow(sinistres_raw) - nrow(distinct(sinistres_raw))
    )
  )
  
  # ----- cas spécifiques demandés
  subsets <- list(
    vehicules_electriques = contrats_enrichis %>% filter(vehicule_electrique == "Oui"),
    jeunes_conducteurs = contrats_enrichis %>% filter(jeune_conducteur == "Oui"),
    petit_rouleur = contrats_enrichis %>% filter(ct_km == "Oui")
  )
  
  # ----- statistiques "apply / lapply / sapply"
  numeric_vars <- contrats_enrichis %>%
    select(where(is.numeric)) %>%
    names()
  
  stats_apply <- sapply(
    contrats_enrichis %>% select(all_of(numeric_vars)),
    function(x) c(
      moyenne = mean(x, na.rm = TRUE),
      mediane = median(x, na.rm = TRUE),
      ecart_type = sd(x, na.rm = TRUE)
    )
  ) %>%
    t() %>%
    as.data.frame() %>%
    rownames_to_column("variable")
  
  list(
    contrats = contrats,
    sinistres = sinistres,
    links = links,
    claims_joined = claims_joined,
    contrats_enrichis = contrats_enrichis,
    missing_summary = missing_summary,
    duplicates_summary = duplicates_summary,
    subsets = subsets,
    stats_apply = stats_apply
  )
}
# -----------------------------
# 3) Chargement initial
# -----------------------------
data_list <- prepare_data("Contrat.xlsx", "Sinistre.xlsx")

contrats <- data_list$contrats
sinistres <- data_list$sinistres
links <- data_list$links
claims_joined <- data_list$claims_joined
contrats_enrichis <- data_list$contrats_enrichis
missing_summary <- data_list$missing_summary
duplicates_summary <- data_list$duplicates_summary
subsets <- data_list$subsets
stats_apply <- data_list$stats_apply

# -----------------------------
# 4) Préparation données carte
# -----------------------------
risk_dep <- contrats_enrichis %>%
  group_by(departement) %>%
  summarise(
    nb_contrats = n(),
    nb_sinistres = sum(nb_sinistres, na.rm = TRUE),
    exposition = sum(exposition, na.rm = TRUE),
    frequence = nb_sinistres / exposition,
    .groups = "drop"
  ) %>%
  filter(!is.na(departement), departement != "Inconnu")

# Carte France départements via geojson public
# Le code est prévu pour fonctionner si l'URL est accessible.
geojson_url <- "https://france-geojson.gregoiredavid.fr/repo/departements.geojson"

france_dep <- NULL
try({
  france_dep <- sf::st_read(geojson_url, quiet = TRUE)
}, silent = TRUE)

if (!is.null(france_dep)) {
  # Harmonisation du code département selon jeux de données
  dep_col <- names(france_dep)[str_detect(names(france_dep), "code|nom")]
  if ("code" %in% names(france_dep)) {
    france_dep <- france_dep %>% mutate(departement = as.character(code))
  } else if ("properties.code" %in% names(france_dep)) {
    france_dep <- france_dep %>% mutate(departement = as.character(`properties.code`))
  } else {
    france_dep <- france_dep %>% mutate(departement = as.character(.[[dep_col[1]]]))
  }
  
  map_data_dep <- france_dep %>%
    left_join(risk_dep, by = "departement")
} else {
  map_data_dep <- NULL
}


# -----------------------------
# 5) UI
# -----------------------------
ui <- dashboardPage(
  dashboardHeader(title = "ENSAssuRances - Analyse Sinistres"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Accueil", tabName = "accueil", icon = icon("home")),
      menuItem("Qualité des données", tabName = "qualite", icon = icon("broom")),
      menuItem("Jointures & sous-ensembles", tabName = "jointures", icon = icon("link")),
      menuItem("Visualisations", tabName = "visuels", icon = icon("chart-column")),
      menuItem("Risque élevé", tabName = "risque", icon = icon("triangle-exclamation")),
      menuItem("Carte géographique", tabName = "carte", icon = icon("map")),
      menuItem("Synthèse décisionnelle", tabName = "synthese", icon = icon("lightbulb"))
    )
  ),
  dashboardBody(
    tags$head(
      tags$style(HTML("
        .small-box h3 {font-size: 24px;}
        .content-wrapper, .right-side {background-color: #f7f7f7;}
      "))
    ),
    tabItems(
      
      # ---------------- ACCUEIL ----------------
      tabItem(
        tabName = "accueil",
        fluidRow(
          valueBoxOutput("vb_contrats", width = 3),
          valueBoxOutput("vb_sinistres", width = 3),
          valueBoxOutput("vb_freq", width = 3)
        ),
        fluidRow(
          box(
            width = 9, title = "Résumé du projet", status = "primary", solidHeader = TRUE,
            p("Cette application permet d’analyser les sinistres automobiles à partir des données de contrats et de sinistres."),
            
            tags$br(),
            
            p("Concrètement, elle fait les étapes suivantes :"),
            
            tags$ul(
              tags$li("nettoyer les données (gérer les valeurs manquantes, enlever les doublons, corriger les formats) ;"),
              tags$li("préparer les données pour l’analyse (créer des variables utiles pour des indicateurs) ;"),
              tags$li("relier les contrats et les sinistres entre eux grâce aux identifiants ;"),
              tags$li("visualiser les données avec des graphiques ;"),
              tags$li("identifier les profils les plus à risque (conducteurs, véhicules) ;"),
              tags$li("visualiser les zones géographiques où il y a le plus de sinistres.")
            ),
            
            tags$br(),
            
            p("Le but est de mieux comprendre la sinistralité et de repérer les situations les plus risquées.")
          )
        )
      ),
      
      # ---------------- QUALITE ----------------
      tabItem(
        tabName = "qualite",
        fluidRow(
          box(width = 12, title = "Doublons détectés", status = "warning", solidHeader = TRUE, DTOutput("duplicates_tbl"))
        ),
        fluidRow(
          box(width = 12, title = "Valeurs manquantes", status = "warning", solidHeader = TRUE, DTOutput("missing_tbl"))
        ),
        fluidRow(
          box(width = 6, title = "Distribution de l'âge du conducteur", status = "primary", solidHeader = TRUE, plotlyOutput("hist_age")),
          box(width = 6, title = "Distribution des sinistres antérieurs", status = "primary", solidHeader = TRUE, plotlyOutput("hist_claims_ant"))
        ),
        fluidRow(
          box(width = 12, title = "Répartition des modalités catégorielles", status = "primary", solidHeader = TRUE,
              selectInput("cat_var", "Variable catégorielle", choices = c("vh_segment", "vh_energy", "drv1_sex", "ct_km", "ct_usage", "vh_group")),
              plotlyOutput("cat_plot"))
        )
      ),
      
      # ---------------- JOINTURES ----------------
      tabItem(
        tabName = "jointures",
        fluidRow(
          box(width = 12, title = "Impact des jointures", status = "primary", solidHeader = TRUE,
              verbatimTextOutput("jointure_info"))
        ),
        fluidRow(
          box(width = 12, title = "Cas spécifiques extraits", status = "info", solidHeader = TRUE,
              selectInput("subset_choice", "Sous-ensemble", choices = c("vehicules_electriques", "jeunes_conducteurs", "petit_rouleur")),
              DTOutput("subset_tbl"))
        ),
        fluidRow(
          box(width = 12, title = "Table contrat-sinistre jointe", status = "success", solidHeader = TRUE,
              DTOutput("joined_tbl"))
        )
      ),
      
      # ---------------- VISUELS ----------------
      tabItem(
        tabName = "visuels",
        fluidRow(
          box(width = 6, title = "Histogramme du total de sinistres par année", status = "primary", solidHeader = TRUE, plotlyOutput("plot_sin_year")),
          box(width = 6, title = "Distribution des contrats par année d'exercice", status = "primary", solidHeader = TRUE, plotlyOutput("plot_contract_year"))
        ),
        fluidRow(
          box(width = 6, title = "Bar plot de répartition des types/segments de véhicules", status = "primary", solidHeader = TRUE, plotlyOutput("plot_segment")),
          box(width = 6, title = "Répartition des véhicules selon l'alimentation", status = "primary", solidHeader = TRUE, plotlyOutput("plot_energy"))
        ),
        fluidRow(
          box(width = 6, title = "Distribution des véhicules selon leur groupe", status = "primary", solidHeader = TRUE, plotlyOutput("plot_group")),
          box(width = 6, title = "Histogramme des contrats avec/sans petit rouleur", status = "primary", solidHeader = TRUE, plotlyOutput("plot_ctkm"))
        ),
        fluidRow(
          box(width = 6, title = "Nombre de sinistres en fonction de l'âge du sociétaire", status = "primary", solidHeader = TRUE, plotlyOutput("plot_age_claims")),
          box(width = 6, title = "Nombre de sinistres selon le nombre de sinistres antécédents", status = "primary", solidHeader = TRUE, plotlyOutput("plot_ant_claims"))
        ),
        fluidRow(
          box(width = 6, title = "Nombre de sinistres par segment commercial du véhicule", status = "primary", solidHeader = TRUE, plotlyOutput("plot_seg_claims")),
          box(width = 6, title = "Nombre et pourcentage de sinistres selon le sexe", status = "primary", solidHeader = TRUE, plotlyOutput("plot_sex_claims"))
        )
      ),
      
      # ---------------- RISQUE ----------------
      tabItem(
        tabName = "risque",
        fluidRow(
          box(
            width = 12, title = "Top profils à risque élevé", status = "danger", solidHeader = TRUE,
            p("Le score affiché ci-dessous est un score simple construit à partir de la fréquence de sinistre, du coût moyen des sinistres et du montant total réglé."),
            p("Un conducteur est considéré comme plus à risque lorsqu’il a beaucoup de sinistres, des sinistres coûteux, ou plusieurs antécédents."),
            p("À l’inverse, un conducteur est moins risqué s’il a peu de sinistres et des coûts faibles."),
            p(tags$b("Interprétation du score : "), "le score est compris entre 0 et 1. Plus il est proche de 1, plus le profil est risqué. Plus il est proche de 0, moins le profil est risqué."),
            DTOutput("risk_tbl")
          )
        ),
        fluidRow(
          box(width = 6, title = "Top marques les plus risquées", status = "danger", solidHeader = TRUE, plotlyOutput("plot_risk_marque")),
          box(width = 6, title = "Top segments les plus risqués", status = "danger", solidHeader = TRUE, plotlyOutput("plot_risk_segment"))
        )
      ),
      
      # ---------------- CARTE ----------------
      tabItem(
        tabName = "carte",
        fluidRow(
          box(
            width = 12, title = "Carte des zones géographiques à risque", status = "primary", solidHeader = TRUE,
            p("La carte est construite à partir du code INSEE du contrat, agrégé au département. L'indicateur affiché est la fréquence de sinistre = nombre de sinistres / exposition."),
            conditionalPanel(
              condition = "true",
              leafletOutput("risk_map", height = 650)
            )
          )
        )
      ),
      tabItem(
        tabName = "synthese",
        fluidRow(
          box(
            width = 12, title = "Synthèse orientée décision", status = "warning", solidHeader = TRUE,
            p("Cette synthèse met en avant :"),
            tags$ul(
              tags$li("repérer les profils conducteurs les plus exposés aux sinistres ;"),
              tags$li("identifier les segments ou marques de véhicules qui présentent une fréquence de sinistre plus élevée ;"),
              tags$li("mettre en évidence les zones géographiques où la sinistralité est plus forte ;"),
              tags$li("aider à adapter la prévention, la tarification et le suivi des contrats les plus sensibles.")
            )
          )
        ),
        fluidRow(
          valueBoxOutput("vb_top_segment", width = 4),
          valueBoxOutput("vb_top_marque", width = 4),
          valueBoxOutput("vb_top_departement", width = 4)
        ),
        fluidRow(
          box(
            width = 6, title = "Profils à surveiller", status = "danger", solidHeader = TRUE,
            htmlOutput("txt_profils")
          ),
          box(
            width = 6, title = "Actions possibles", status = "success", solidHeader = TRUE,
            htmlOutput("txt_actions")
          )
        ),
        fluidRow(
          box(
            width = 12, title = "Lecture décisionnelle", status = "primary", solidHeader = TRUE,
            htmlOutput("txt_decision")
          )
        )
      )
    )
  )
)


# -----------------------------
# 6) SERVER
# -----------------------------
server <- function(input, output, session) {
  
  # KPI
  output$vb_contrats <- renderValueBox({
    valueBox(
      format(nrow(contrats_enrichis), big.mark = " "),
      "Nombre de contrats",
      icon = icon("file-contract"),
      color = "blue"
    )
  })
  
  output$vb_sinistres <- renderValueBox({
    valueBox(
      format(nrow(sinistres), big.mark = " "),
      "Nombre de sinistres",
      icon = icon("car-burst"),
      color = "red"
    )
  })
  
  output$vb_jointure <- renderValueBox({
    valueBox(
      format(nrow(claims_joined), big.mark = " "),
      "Lignes après jointure",
      icon = icon("link"),
      color = "aqua"
    )
  })
  
  output$vb_freq <- renderValueBox({
    valueBox(
      percent(sum(contrats_enrichis$nb_sinistres, na.rm = TRUE) / sum(contrats_enrichis$exposition, na.rm = TRUE), accuracy = 0.01),
      "Fréquence moyenne de sinistre",
      icon = icon("chart-line"),
      color = "green"
    )
  })
  
  output$stats_apply_tbl <- renderDT({
    datatable(stats_apply, options = list(pageLength = 8, scrollX = TRUE))
  })
  
  # Qualité
  output$duplicates_tbl <- renderDT({
    datatable(duplicates_summary, options = list(dom = "tip", pageLength = 5))
  })
  
  output$missing_tbl <- renderDT({
    datatable(missing_summary, options = list(pageLength = 10, scrollX = TRUE))
  })
  
  output$hist_age <- renderPlotly({
    df <- contrats_enrichis %>%
      count(age_conducteur_classe)
    
    p <- ggplot(df, aes(x = age_conducteur_classe, y = n)) +
      geom_col(fill = "#2C7FB8") +
      labs(x = "Classe d'âge du conducteur", y = "Nombre de contrats") +
      theme_minimal()
    
    ggplotly(p)
  })
  
  output$hist_claims_ant <- renderPlotly({
    p <- ggplot(contrats_enrichis, aes(x = claims_ant)) +
      geom_bar(fill = "#F28E2B") +
      labs(x = "Nombre de sinistres antérieurs", y = "Nombre de contrats") +
      theme_minimal()
    ggplotly(p)
  })
  
  output$hist_vh_age <- renderPlotly({
    p <- ggplot(contrats_enrichis, aes(x = vh_age)) +
      geom_histogram(bins = 30, fill = "#59A14F", color = "white") +
      labs(x = "Âge du véhicule", y = "Nombre de contrats") +
      theme_minimal()
    ggplotly(p)
  })
  
  output$cat_plot <- renderPlotly({
    req(input$cat_var)
    df <- contrats_enrichis %>%
      count(.data[[input$cat_var]], sort = TRUE) %>%
      rename(modalite = 1)
    
    p <- ggplot(df, aes(x = fct_reorder(modalite, n), y = n)) +
      geom_col(fill = "#4E79A7") +
      coord_flip() +
      labs(x = NULL, y = "Nombre") +
      theme_minimal()
    
    ggplotly(p)
  })
  
  # Jointures
  output$jointure_info <- renderPrint({
    cat("Nombre de contrats :", nrow(contrats), "\n")
    cat("Nombre de sinistres :", nrow(sinistres), "\n")
    cat("Nombre d'identifiants de sinistres retrouvés dans Contrat :", nrow(links), "\n")
    cat("Nombre de lignes après left_join :", nrow(claims_joined), "\n")
    cat("Nombre de sinistres distincts joints :", n_distinct(claims_joined$idx_sin, na.rm = TRUE), "\n")
    cat("Nombre de contrats ayant au moins un sinistre :", sum(contrats_enrichis$nb_sinistres > 0), "\n")
  })
  
  output$subset_tbl <- renderDT({
    datatable(subsets[[input$subset_choice]] %>% head(1000), options = list(pageLength = 10, scrollX = TRUE))
  })
  
  output$joined_tbl <- renderDT({
    datatable(claims_joined %>% select(idx_ct, idx_year, idx_sin, gar_sin, surv_sin, mt_eval, mt_regl, drv1_age, drv1_sex, vh_segment, vh_energy, ct_km) %>% head(2000),
              options = list(pageLength = 10, scrollX = TRUE))
  })
  
  # Visualisations demandées
  output$plot_sin_year <- renderPlotly({
    df <- sinistres %>%
      count(annee_surv, name = "nb_sinistres") %>%
      filter(!is.na(annee_surv))
    p <- ggplot(df, aes(x = factor(annee_surv), y = nb_sinistres)) +
      geom_col(fill = "#D62728") +
      labs(x = "Année", y = "Total de sinistres") +
      theme_minimal()
    ggplotly(p)
  })
  
  output$plot_contract_year <- renderPlotly({
    df <- contrats_enrichis %>% count(idx_year)
    p <- ggplot(df, aes(x = factor(idx_year), y = n)) +
      geom_col(fill = "#1F77B4") +
      labs(x = "Année d'exercice", y = "Nombre de contrats") +
      theme_minimal()
    ggplotly(p)
  })
  
  output$plot_segment <- renderPlotly({
    df <- contrats_enrichis %>% count(vh_segment, sort = TRUE)
    p <- ggplot(df, aes(x = fct_reorder(vh_segment, n), y = n)) +
      geom_col(fill = "#9467BD") +
      coord_flip() +
      labs(x = "Segment commercial", y = "Nombre de contrats") +
      theme_minimal()
    ggplotly(p)
  })
  
  output$plot_energy <- renderPlotly({
    df <- contrats_enrichis %>% count(vh_energy, sort = TRUE)
    p <- ggplot(df, aes(x = fct_reorder(vh_energy, n), y = n, fill = vh_energy)) +
      geom_col(show.legend = FALSE) +
      coord_flip() +
      labs(x = "Alimentation", y = "Nombre de véhicules") +
      theme_minimal()
    ggplotly(p)
  })
  
  output$plot_group <- renderPlotly({
    df <- contrats_enrichis %>%
      count(vh_group, sort = TRUE) %>%
      slice_max(order_by = n, n = 15)
    p <- ggplot(df, aes(x = fct_reorder(vh_group, n), y = n)) +
      geom_col(fill = "#FF7F0E") +
      coord_flip() +
      labs(x = "Groupe véhicule", y = "Nombre") +
      theme_minimal()
    ggplotly(p)
  })
  
  output$plot_ctkm <- renderPlotly({
    df <- contrats_enrichis %>% count(ct_km)
    p <- ggplot(df, aes(x = ct_km, y = n, fill = ct_km)) +
      geom_col(show.legend = FALSE) +
      labs(x = "Option petit rouleur", y = "Nombre de contrats") +
      theme_minimal()
    ggplotly(p)
  })
  
  output$plot_age_claims <- renderPlotly({
    df <- contrats_enrichis %>%
      group_by(age_conducteur_classe) %>%
      summarise(nb_sinistres = sum(nb_sinistres, na.rm = TRUE), .groups = "drop")
    p <- ggplot(df, aes(x = age_conducteur_classe, y = nb_sinistres)) +
      geom_col(fill = "#2CA02C") +
      labs(x = "Classe d'âge du sociétaire", y = "Nombre de sinistres") +
      theme_minimal()
    ggplotly(p)
  })
  
  output$plot_ant_claims <- renderPlotly({
    df <- contrats_enrichis %>%
      group_by(claims_ant) %>%
      summarise(nb_sinistres = sum(nb_sinistres, na.rm = TRUE), .groups = "drop") %>%
      arrange(claims_ant)
    p <- ggplot(df, aes(x = factor(claims_ant), y = nb_sinistres)) +
      geom_col(fill = "#8C564B") +
      labs(x = "Nombre de sinistres antérieurs", y = "Nombre de sinistres observés") +
      theme_minimal()
    ggplotly(p)
  })
  
  output$plot_seg_claims <- renderPlotly({
    df <- contrats_enrichis %>%
      group_by(vh_segment) %>%
      summarise(nb_sinistres = sum(nb_sinistres, na.rm = TRUE), .groups = "drop") %>%
      arrange(desc(nb_sinistres))
    p <- ggplot(df, aes(x = fct_reorder(vh_segment, nb_sinistres), y = nb_sinistres)) +
      geom_col(fill = "#17BECF") +
      coord_flip() +
      labs(x = "Segment commercial", y = "Nombre de sinistres") +
      theme_minimal()
    ggplotly(p)
  })
  
  output$plot_sex_claims <- renderPlotly({
    df <- contrats_enrichis %>%
      group_by(drv1_sex) %>%
      summarise(nb_sinistres = sum(nb_sinistres, na.rm = TRUE), .groups = "drop") %>%
      mutate(pct = nb_sinistres / sum(nb_sinistres))
    p <- ggplot(df, aes(x = drv1_sex, y = nb_sinistres, text = paste0("Pourcentage : ", percent(pct, accuracy = 0.1)))) +
      geom_col(fill = "#E377C2") +
      labs(x = "Sexe", y = "Nombre de sinistres") +
      theme_minimal()
    ggplotly(p, tooltip = c("x", "y", "text"))
  })
  
  # Risque élevé
  risk_profiles <- reactive({
    contrats_enrichis %>%
      mutate(
        cout_moyen = ifelse(nb_sinistres > 0, total_mt_eval / nb_sinistres, 0),
        score_risque = scales::rescale(frequence_sinistre, to = c(0, 1), from = range(frequence_sinistre, na.rm = TRUE)) * 0.5 +
          scales::rescale(cout_moyen, to = c(0, 1), from = range(cout_moyen, na.rm = TRUE)) * 0.3 +
          scales::rescale(total_mt_regl, to = c(0, 1), from = range(total_mt_regl, na.rm = TRUE)) * 0.2
      ) %>%
      arrange(desc(score_risque)) %>%
      select(
        idx_ct, idx_year, drv1_age, drv1_sex, vh_marque, vh_segment, vh_energy,
        claims_ant, ct_km, nb_sinistres, frequence_sinistre, total_mt_eval,
        total_mt_regl, score_risque
      )
  })
  
  output$risk_tbl <- renderDT({
    datatable(risk_profiles() %>% head(1000), options = list(pageLength = 10, scrollX = TRUE))
  })
  
  output$plot_risk_marque <- renderPlotly({
    df <- contrats_enrichis %>%
      group_by(vh_marque) %>%
      summarise(
        exposition = sum(exposition, na.rm = TRUE),
        nb_sinistres = sum(nb_sinistres, na.rm = TRUE),
        frequence = nb_sinistres / exposition,
        .groups = "drop"
      ) %>%
      arrange(desc(frequence)) %>%
      slice_head(n = 10)
    
    p <- ggplot(df, aes(x = fct_reorder(vh_marque, frequence), y = frequence)) +
      geom_col(fill = "#D62728") +
      coord_flip() +
      scale_y_continuous(labels = percent_format(accuracy = 0.1)) +
      labs(x = "Marque", y = "Fréquence de sinistre") +
      theme_minimal()
    ggplotly(p)
  })
  
  output$plot_risk_segment <- renderPlotly({
    df <- contrats_enrichis %>%
      group_by(vh_segment) %>%
      summarise(
        exposition = sum(exposition, na.rm = TRUE),
        nb_sinistres = sum(nb_sinistres, na.rm = TRUE),
        frequence = nb_sinistres / exposition,
        .groups = "drop"
      ) %>%
      arrange(desc(frequence))
    
    p <- ggplot(df, aes(x = fct_reorder(vh_segment, frequence), y = frequence)) +
      geom_col(fill = "#B22222") +
      coord_flip() +
      scale_y_continuous(labels = percent_format(accuracy = 0.1)) +
      labs(x = "Segment", y = "Fréquence de sinistre") +
      theme_minimal()
    ggplotly(p)
  })
  
  # Carte
  output$risk_map <- renderLeaflet({
    if (is.null(map_data_dep)) {
      leaflet() %>%
        addTiles() %>%
        addPopups(
          lng = 2.35, lat = 46.85,
          popup = "Impossible de charger le fond départemental. Vérifie l'accès internet pour lire le geojson."
        )
    } else {
      pal <- colorNumeric(
        palette = "YlOrRd",
        domain = map_data_dep$frequence,
        na.color = "#dddddd"
      )
      
      leaflet(map_data_dep) %>%
        addProviderTiles("CartoDB.Positron") %>%
        addPolygons(
          fillColor = ~pal(frequence),
          fillOpacity = 0.8,
          color = "#555555",
          weight = 1,
          smoothFactor = 0.2,
          popup = ~paste0(
            "<b>Département : </b>", departement, "<br>",
            "<b>Contrats : </b>", scales::comma(nb_contrats, accuracy = 1), "<br>",
            "<b>Sinistres : </b>", scales::comma(nb_sinistres, accuracy = 1), "<br>",
            "<b>Exposition : </b>", round(exposition, 2), "<br>",
            "<b>Fréquence : </b>", percent(frequence, accuracy = 0.01)
          )
        ) %>%
        addLegend(
          "bottomright",
          pal = pal,
          values = ~frequence,
          title = "Fréquence de sinistre",
          labFormat = labelFormat(suffix = "")
        )
    }
  })
  
  synthese_segment <- reactive({
    contrats_enrichis %>%
      group_by(vh_segment) %>%
      summarise(
        exposition = sum(exposition, na.rm = TRUE),
        nb_sinistres = sum(nb_sinistres, na.rm = TRUE),
        frequence = nb_sinistres / exposition,
        .groups = "drop"
      ) %>%
      filter(!is.na(vh_segment), vh_segment != "Inconnu", exposition > 0) %>%
      arrange(desc(frequence)) %>%
      slice(1)
  })
  
  synthese_marque <- reactive({
    contrats_enrichis %>%
      group_by(vh_marque) %>%
      summarise(
        exposition = sum(exposition, na.rm = TRUE),
        nb_sinistres = sum(nb_sinistres, na.rm = TRUE),
        frequence = nb_sinistres / exposition,
        .groups = "drop"
      ) %>%
      filter(!is.na(vh_marque), vh_marque != "Inconnu", exposition > 0) %>%
      arrange(desc(frequence)) %>%
      slice(1)
  })
  
  synthese_dep <- reactive({
    contrats_enrichis %>%
      group_by(departement) %>%
      summarise(
        exposition = sum(exposition, na.rm = TRUE),
        nb_sinistres = sum(nb_sinistres, na.rm = TRUE),
        frequence = nb_sinistres / exposition,
        .groups = "drop"
      ) %>%
      filter(!is.na(departement), departement != "Inconnu", exposition > 0) %>%
      arrange(desc(frequence)) %>%
      slice(1)
  })
  
  synthese_age <- reactive({
    contrats_enrichis %>%
      group_by(age_conducteur_classe) %>%
      summarise(
        exposition = sum(exposition, na.rm = TRUE),
        nb_sinistres = sum(nb_sinistres, na.rm = TRUE),
        frequence = nb_sinistres / exposition,
        .groups = "drop"
      ) %>%
      filter(!is.na(age_conducteur_classe), exposition > 0) %>%
      arrange(desc(frequence)) %>%
      slice(1)
  })
  
  output$vb_top_segment <- renderValueBox({
    x <- synthese_segment()
    valueBox(
      paste0(x$vh_segment),
      "Segment le plus risqué",
      icon = icon("car"),
      color = "red"
    )
  })
  
  output$vb_top_marque <- renderValueBox({
    x <- synthese_marque()
    valueBox(
      paste0(x$vh_marque),
      "Marque la plus risquée",
      icon = icon("industry"),
      color = "orange"
    )
  })
  
  output$vb_top_departement <- renderValueBox({
    x <- synthese_dep()
    valueBox(
      paste0("Dépt. ", x$departement),
      "Zone la plus risquée",
      icon = icon("location-dot"),
      color = "yellow"
    )
  })
  
  output$txt_profils <- renderUI({
    age <- synthese_age()
    seg <- synthese_segment()
    dep <- synthese_dep()
    
    HTML(paste0(
      "<p>Les résultats montrent que certains profils ressortent davantage que d’autres.</p>",
      "<ul>",
      "<li>La classe d’âge la plus exposée est <b>", age$age_conducteur_classe, "</b> avec une fréquence de sinistre d’environ <b>", percent(age$frequence, accuracy = 0.1), "</b>.</li>",
      "<li>Le segment de véhicule le plus exposé est <b>", seg$vh_segment, "</b> avec une fréquence d’environ <b>", percent(seg$frequence, accuracy = 0.1), "</b>.</li>",
      "<li>La zone géographique la plus sensible est le <b>département ", dep$departement, "</b> avec une fréquence proche de <b>", percent(dep$frequence, accuracy = 0.1), "</b>.</li>",
      "</ul>"
    ))
  })
  
  output$txt_actions <- renderUI({
    HTML(
      paste0(
        "<p>À partir de ces résultats, plusieurs actions peuvent être envisagées :</p>",
        "<ul>",
        "<li>renforcer la prévention sur les profils les plus exposés ;</li>",
        "<li>surveiller davantage certains segments ou certaines marques ;</li>",
        "<li>adapter la politique tarifaire sur les profils ou zones les plus risqués ;</li>",
        "<li>mettre en place un suivi plus précis des contrats ayant déjà plusieurs antécédents de sinistre.</li>",
        "</ul>"
      )
    )
  })
  
  output$txt_decision <- renderUI({
    age <- synthese_age()
    marque <- synthese_marque()
    seg <- synthese_segment()
    dep <- synthese_dep()
    
    HTML(paste0(
      "<p>Globalement, cette étude permet de mieux cibler les contrats à surveiller. ",
      "L’analyse montre que le risque ne dépend pas d’un seul facteur, mais de la combinaison entre le profil conducteur, le type de véhicule, l’historique de sinistres et la zone géographique.</p>",
      "<p>Dans une logique de décision, on peut retenir que :</p>",
      "<ul>",
      "<li>les conducteurs de la classe <b>", age$age_conducteur_classe, "</b> semblent être les plus exposés ;</li>",
      "<li>la marque <b>", marque$vh_marque, "</b> et le segment <b>", seg$vh_segment, "</b> ressortent comme plus sensibles ;</li>",
      "<li>le département <b>", dep$departement, "</b> mérite une attention particulière.</li>",
      "</ul>",
      "<p>Cette synthèse peut donc servir de base pour orienter la prévention, ajuster la tarification et prioriser le suivi des contrats les plus risqués.</p>"
    ))
  })
  
  
}

# -----------------------------
# 7) Lancement
# -----------------------------
shinyApp(ui, server)

