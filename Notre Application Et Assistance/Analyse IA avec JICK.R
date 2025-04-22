# Installation des packages
packages <- c("dplyr","shiny","shinythemes", "tools","gtsummary", "gt", "shinydashboard", "DT", "readxl", "ggplot2","httr", "jsonlite", "plotly", "stringr", "shinyjs", "rmarkdown", "haven", "bslib", "forcats", "lubridate", "caret", "shinycssloaders","shinyBS","shinyWidgets","shinyFiles","shinyFiles")

for (pack in packages){
  if(!requireNamespace(pack, quietly = TRUE)){
    install.packages(pack)
  }
  library(pack, character.only = TRUE)
}


# La fonctionnalité de statistiques descriptives sans connexion

# dashboard_module.R

dashboard_ui <- function(id) {
  ns <- NS(id)
  # UI du module
  tagList(
    h3("📊 Statistiques descriptives"),
    
    
    tabsetPanel(
      # Univariées - Quantitatives
      tabPanel("Univariées - Quantitatives",
               tabsetPanel(
                 tabPanel("📊 Histogramme",
                          fluidRow(
                            column(4,
                                   selectInput(ns("vars_quant_hist"), "Variable :", choices = NULL),
                                   sliderInput(ns("bins"), "Nombre de barres :", min = 5, max = 50, value = 10)),
                            column(8, plotOutput(ns("hist")))
                          )
                 ),
                 tabPanel("📈 Boxplot",
                          fluidRow(
                            column(4, selectInput(ns("vars_quant_box"), "Variable :", choices = NULL)),
                            column(8, plotOutput(ns("box")))
                          )
                 ),
                 tabPanel("📋 Tableau de fréquences",
                          fluidRow(
                            column(4, selectInput(ns("vars_quant_tab"), "Variable :", choices = NULL)),
                            column(8, dataTableOutput(ns("tab_quant")))
                          )
                 ),
                 tabPanel("🧮 Résumé",
                          fluidRow(
                            column(4, selectInput(ns("vars_quant_sum"), "Variable :", choices = NULL)),
                            column(8, verbatimTextOutput(ns("summary_quant")))
                          )
                 )
               )
      ),
      
      # Univariées - Qualitatives
      tabPanel("Univariées - Qualitatives",
               tabsetPanel(
                 tabPanel("📋 Tableau de fréquences",
                          fluidRow(
                            column(4, selectInput(ns("vars_qual_tab"), "Variable :", choices = NULL)),
                            column(8, dataTableOutput(ns("tab_qual")))
                          )
                 ),
                 tabPanel("📊 Diagramme en barres",
                          fluidRow(
                            column(4, selectInput(ns("vars_qual_bar"), "Variable :", choices = NULL)),
                            column(8, plotOutput(ns("bar")))
                          )
                 )
               )
      ),
      
      # Bivariées
      tabPanel("Bivariées",
               tabsetPanel(
                 tabPanel("📋 Tableau croisé",
                          fluidRow(
                            column(4,
                                   selectInput(ns("vars_tab_1"), "Variable 1 :", choices = NULL),
                                   selectInput(ns("vars_tab_2"), "Variable 2 :", choices = NULL)),
                            column(8, gt_output(ns("tab_croix")))
                          )
                 ),
                 tabPanel("📌 Nuage de points",
                          fluidRow(
                            column(4,
                                   selectInput(ns("vars_x"), "X :", choices = NULL),
                                   selectInput(ns("vars_y"), "Y :", choices = NULL),
                                   selectInput(ns("vars_color"), "Couleur :", choices = NULL)),
                            column(8, plotOutput(ns("points")))
                          )
                 )
               )
      )
    )
  )
}
# Module serveur
dashboard_server <- function(id, data) {
  moduleServer(id, function(input, output, session) {
    # Chargement des données
    observeEvent(data(), {
      df <- data()
      num_vars <- names(df)[sapply(df, is.numeric)]
      cat_vars <- names(df)[!sapply(df, is.numeric)]
      # Mettre à jour les choix des variables dans les sélecteurs
      updateSelectInput(session, "vars_quant_hist", choices = num_vars)
      updateSelectInput(session, "vars_quant_box", choices = num_vars)
      updateSelectInput(session, "vars_quant_tab", choices = num_vars)
      updateSelectInput(session, "vars_quant_sum", choices = num_vars)
      updateSelectInput(session, "vars_qual_tab", choices = cat_vars)
      updateSelectInput(session, "vars_qual_bar", choices = cat_vars)
      updateSelectInput(session, "vars_tab_1", choices = cat_vars)
      updateSelectInput(session, "vars_tab_2", choices = names(df))
      updateSelectInput(session, "vars_x", choices = num_vars)
      updateSelectInput(session, "vars_y", choices = num_vars)
      updateSelectInput(session, "vars_color", choices = cat_vars)
    })
    
    # Histogramme
    output$hist <- renderPlot({
      req(data(), input$vars_quant_hist)
      ggplot(data(), aes(x = .data[[input$vars_quant_hist]])) +
        geom_histogram(bins = input$bins, fill = "steelblue", color = "white") +
        labs(title = paste("Histogramme de", input$vars_quant_hist))
    })
    
    # Boxplot
    output$box <- renderPlot({
      req(data(), input$vars_quant_box)
      ggplot(data(), aes(y = .data[[input$vars_quant_box]])) +
        geom_boxplot(fill = "orange") +
        labs(title = paste("Boxplot de", input$vars_quant_box))
    })
    
    # Tableau quanti
    output$tab_quant <- renderDataTable({
      req(data(), input$vars_quant_tab)
      df <- data()
      tableau <- table(df[[input$vars_quant_tab]])
      datatable(as.data.frame(tableau))
    })
    
    # Résumé quanti
    output$summary_quant <- renderPrint({
      req(data(), input$vars_quant_sum)
      summary(data()[[input$vars_quant_sum]])
    })
    
    # Tableau quali
    output$tab_qual <- renderDataTable({
      req(data(), input$vars_qual_tab)
      df <- data()
      tableau <- table(df[[input$vars_qual_tab]])
      datatable(as.data.frame(tableau))
    })
    
    # Barplot
    output$bar <- renderPlot({
      req(data(), input$vars_qual_bar)
      ggplot(data(), aes(x = .data[[input$vars_qual_bar]])) +
        geom_bar(fill = "forestgreen") +
        labs(title = paste("Diagramme en barres de", input$vars_qual_bar))
    })
    
    # Tableau croisé
    output$tab_croix <- render_gt({
      req(data(), input$vars_tab_1, input$vars_tab_2)
      base <- data()[, c(input$vars_tab_1, input$vars_tab_2)]
      tbl_summary(base, by = all_of(input$vars_tab_1), missing = "no") %>%
        bold_labels() %>%
        italicize_levels() %>%
        as_gt()
    })
    
    # Nuage de points
    output$points <- renderPlot({
      req(data(), input$vars_x, input$vars_y, input$vars_color)
      ggplot(data(), aes(x = .data[[input$vars_x]],
                         y = .data[[input$vars_y]],
                         color = .data[[input$vars_color]])) +
        geom_point() +
        labs(title = paste("Nuage de points :", input$vars_x, "vs", input$vars_y))
    })
  })
}



# Base des codes essentiels sans connexion


# Vecteurs de questions et réponses
questions <- c(
  "Combien y a-t-il de valeurs manquantes dans mes données ?",
  "Quels sont les doublons dans mon jeu de données ?",
  "Supprimer les lignes avec des valeurs manquantes",
  "Remplacer les valeurs manquantes par la moyenne",
  "Afficher les noms des colonnes",
  "Afficher les types de colonnes",
  "Afficher les 6 premières lignes",
  "Afficher les 6 dernières lignes",
  "Nombre de lignes",
  "Nombre de colonnes",
  "Résumé statistique des données",
  "Créer un histogramme",
  "Créer un boxplot",
  "Créer un nuage de points",
  "Créer un graphique ggplot",
  "Créer un graphique à barres",
  "Filtrer les lignes selon une condition",
  "Créer une nouvelle colonne",
  "Supprimer une colonne",
  "Renommer une colonne",
  "Trier par une variable",
  "Compter les valeurs uniques",
  "Convertir une variable en facteur",
  "Convertir en numérique",
  "Fusionner deux data frames",
  "Combiner deux data frames verticalement",
  "Combiner deux data frames horizontalement",
  "Extraire des colonnes spécifiques",
  "Extraire des lignes spécifiques",
  "Calculer la moyenne d'une colonne",
  "Calculer la médiane",
  "Calculer l'écart-type",
  "Calculer la variance",
  "Calculer la somme",
  "Calculer les quantiles",
  "Grouper les données par une variable",
  "Créer une table de contingence",
  "Voir la distribution d'une variable",
  "Créer une matrice de corrélation",
  "Afficher les valeurs uniques",
  "Enlever les doublons",
  "Créer un dataframe",
  "Lire un fichier CSV",
  "Lire un fichier Excel",
  "Exporter en CSV",
  "Exporter en Excel",
  "Afficher une table interactive",
  "Remplacer des valeurs",
  "Sélectionner les lignes où une variable est NA",
  "Créer une table de fréquence",
  "Vérifier si une colonne est factor",
  "Vérifier si une colonne est numérique",
  "Changer le type d'une colonne",
  "Créer des classes à partir d'une variable continue",
  "Afficher les 10 lignes les plus grandes",
  "Afficher les corrélations avec une variable cible",
  "Afficher les NA par colonne",
  "Afficher les NA par ligne",
  "Créer un tableau résumé avec dplyr",
  "Filtrer avec dplyr",
  "Trier avec dplyr",
  "Sélectionner des colonnes avec dplyr",
  "Créer une variable avec mutate",
  "Afficher structure dplyr",
  "Joindre deux tableaux avec left_join",
  "Créer un graphique avec plotly",
  "Afficher le nombre d'observations par groupe",
  "Afficher un graphique en camembert",
  "Afficher un graphique en ligne",
  "Afficher une heatmap",
  "Afficher un violin plot",
  "Afficher les lignes contenant un mot-clé",
  "Vérifier les valeurs aberrantes",
  "Supprimer les lignes avec outliers",
  "Afficher la table croisée avec proportions",
  "Arrondir une variable numérique",
  "Faire un test de corrélation",
  "Faire un test t de comparaison",
  "Faire une régression linéaire simple",
  "Voir les coefficients d'un modèle",
  "Créer un modèle avec caret",
  "Séparer les données en train/test",
  "Standardiser les variables",
  "Remplacer les zéros par NA",
  "Afficher la moyenne par groupe",
  "Créer un graphique en barres empilées",
  "Afficher un graphique de densité",
  "Fusionner plusieurs fichiers CSV",
  "Créer une colonne conditionnelle avec ifelse",
  "Réordonner les niveaux d’un facteur",
  "Convertir un facteur en date",
  "Créer une date à partir de plusieurs colonnes",
  "Extraire l’année d’une date",
  "Extraire le mois d’une date",
  "Créer des colonnes à partir de texte avec regex",
  "Ajouter un titre et un sous-titre à un graphique",
  "Changer les couleurs dans ggplot",
  "Sauvegarder un graphique en PNG",
  "Faire un facet_wrap dans ggplot",
  "Voir les valeurs les plus fréquentes d'une colonne",
  "Calculer le mode d’une variable",
  "Comparer plusieurs colonnes entre elles",
  "Faire un test ANOVA",
  "Faire une régression logistique",
  "Calculer un intervalle de confiance",
  "Faire un test du chi²",
  "Évaluer un modèle avec RMSE",
  "Remplacer les NA par la médiane",
  "Supprimer les colonnes avec trop de NA",
  "Vérifier la normalité d'une variable",
  "Détecter les colonnes constantes",
  "Lire un fichier JSON",
  "Lire un fichier XML",
  "Lire un fichier RDS",
  "Sauvegarder un objet avec saveRDS",
  "Lister les fichiers dans un dossier",
  "Afficher la mémoire utilisée par un objet",
  "Afficher la taille des colonnes",
  "Mesurer le temps d'exécution",
  "Lister les fonctions d’un package",
  "Joindre deux data frames avec right_join",
  "Joindre deux data frames avec full_join",
  "Joindre deux data frames avec inner_join",
  "Comment importer un fichier CSV ?",
  "Comment importer un fichier Excel ?",
  "Comment exporter un data frame en CSV ?",
  "Comment exporter un data frame en Excel ?",
  "Comment importer un fichier JSON ?",
  "Comment importer un fichier RDS ?",
  "Comment sauvegarder un data frame au format RDS ?",
  "Comment importer un fichier XML ?",
  "Comment importer plusieurs fichiers CSV dans un dossier ?",
  "Comment sauvegarder un graphique sous forme d'image (PNG, JPEG) ?",
  "Comment exporter un graphique interactif avec plotly ?",
  "Comment lire un fichier depuis une URL ?",
  "Comment sauvegarder un modèle de machine learning ?",
  "Comment compresser des fichiers CSV ?",
  "Comment exporter un fichier sous un autre format (par ex. TSV, TXT) ?",
  "Comment importer un fichier SPSS ?",
  "Comment importer un fichier Stata ?",
  "Comment importer un fichier SAS ?",
  "Comment exporter un data frame en format JSON ?",
  "Comment exporter un data frame en format Feather ?",
  "Comment importer des données à partir d'une base de données SQL ?",
  "Comment exporter un data frame dans une base de données SQL ?"
)

reponses <- c(
  "sum(is.na(data))",
  "data[duplicated(data), ]",
  "data <- na.omit(data)",
  "data$col[is.na(data$col)] <- mean(data$col, na.rm = TRUE)",
  "colnames(data)",
  "str(data)",
  "head(data)",
  "tail(data)",
  "nrow(data)",
  "ncol(data)",
  "summary(data)",
  "hist(data$col)",
  "boxplot(data$col)",
  "plot(data$x, data$y)",
  "ggplot(data, aes(x = x, y = y)) + geom_point()",
  "ggplot(data, aes(x = factor)) + geom_bar()",
  "subset(data, col > 10)",
  "data$new_col <- data$x + data$y",
  "data$col <- NULL",
  "colnames(data)[colnames(data) == 'old'] <- 'new'",
  "data <- data[order(data$col), ]",
  "table(data$col)",
  "data$col <- as.factor(data$col)",
  "data$col <- as.numeric(data$col)",
  "merge(df1, df2, by = 'id')",
  "rbind(df1, df2)",
  "cbind(df1, df2)",
  "data[, c('col1', 'col2')]",
  "data[1:10, ]",
  "mean(data$col, na.rm = TRUE)",
  "median(data$col, na.rm = TRUE)",
  "sd(data$col, na.rm = TRUE)",
  "var(data$col, na.rm = TRUE)",
  "sum(data$col, na.rm = TRUE)",
  "quantile(data$col, probs = c(0.25, 0.5, 0.75))",
  "aggregate(data$val ~ data$group, FUN = mean)",
  "table(data$col1, data$col2)",
  "table(cut(data$col, breaks = 5))",
  "cor(data[sapply(data, is.numeric)], use = 'complete.obs')",
  "unique(data$col)",
  "data <- data[!duplicated(data), ]",
  "data <- data.frame(x = 1:10, y = rnorm(10))",
  "data <- read.csv('fichier.csv')",
  "data <- readxl::read_excel('fichier.xlsx')",
  "write.csv(data, 'sortie.csv', row.names = FALSE)",
  "writexl::write_xlsx(data, 'sortie.xlsx')",
  "DT::datatable(data)",
  "data$col[data$col == 'ancien'] <- 'nouveau'",
  "data[is.na(data$col), ]",
  "prop.table(table(data$col))",
  "is.factor(data$col)",
  "is.numeric(data$col)",
  "data$col <- as.character(data$col)",
  "cut(data$col, breaks = 3)",
  "head(data[order(-data$col), ], 10)",
  "cor(data$col, data$cible, use = 'complete.obs')",
  "colSums(is.na(data))",
  "rowSums(is.na(data))",
  "data %>% group_by(group) %>% summarise(moyenne = mean(val, na.rm = TRUE))",
  "data %>% filter(col > 10)",
  "data %>% arrange(desc(col))",
  "data %>% select(col1, col2)",
  "data %>% mutate(nouvelle = col1 * 2)",
  "glimpse(data)",
  "left_join(df1, df2, by = 'id')",
  "plotly::plot_ly(data, x = ~x, y = ~y, type = 'scatter', mode = 'markers')",
  "data %>% count(group)",
  "ggplot(data, aes(x = '', fill = factor)) + geom_bar(width = 1) + coord_polar('y')",
  "ggplot(data, aes(x = temps, y = valeur)) + geom_line()",
  "heatmap(cor(data[sapply(data, is.numeric)]), symm = TRUE)",
  "ggplot(data, aes(x = group, y = val)) + geom_violin()",
  "data[grepl('mot', data$col), ]",
  "boxplot.stats(data$col)$out",
  "data <- data[!data$col %in% boxplot.stats(data$col)$out, ]",
  "prop.table(table(data$col1, data$col2), margin = 1)",
  "data$col <- round(data$col, 2)",
  "cor.test(data$x, data$y)",
  "t.test(data$valeur ~ data$groupe)",
  "lm(valeur ~ predicteur, data = data)",
  "summary(lm(valeur ~ predicteur, data = data))",
  "train(valeur ~ ., data = data, method = 'lm')",
  "sample <- sample.split(data$col, SplitRatio = 0.7)",
  "scale(data)",
  "data[data == 0] <- NA",
  "aggregate(data$valeur, list(data$groupe), mean)",
  "ggplot(data, aes(x = groupe, fill = catégorie)) + geom_bar(position = 'stack')",
  "ggplot(data, aes(x = val)) + geom_density()",
  "file_list <- list.files(path='.', pattern='*.csv', full.names=TRUE); data <- do.call(rbind, lapply(file_list, read.csv))",
  "data$new_col <- ifelse(data$col > 10, 'haut', 'bas')",
  "data$facteur <- factor(data$facteur, levels = c('Bas', 'Moyen', 'Haut'))",
  "data$date <- as.Date(as.character(data$facteur_date), format='%Y-%m-%d')",
  "data$date <- as.Date(paste(data$annee, data$mois, data$jour, sep = '-'))",
  "format(data$date, '%Y')",
  "format(data$date, '%m')",
  "stringr::str_extract(data$texte, '\\\\d+')",
  "ggplot(data, aes(x, y)) + geom_point() + ggtitle('Titre principal', subtitle = 'Sous-titre')",
  "ggplot(data, aes(x, y, color = group)) + geom_point() + scale_color_brewer(palette = 'Set2')",
  "ggsave('mon_graph.png', width = 8, height = 5)",
  "ggplot(data, aes(x, y)) + geom_point() + facet_wrap(~ groupe)",
  "sort(table(data$col), decreasing = TRUE)[1:5]",
  "Mode <- function(x) names(sort(table(x), decreasing=TRUE))[1]; Mode(data$col)",
  "cor(data[, c('var1', 'var2', 'var3')], use = 'complete.obs')",
  "aov(valeur ~ groupe, data = data)",
  "glm(cible ~ x1 + x2, data = data, family = 'binomial')",
  "t.test(data$col)$conf.int",
  "chisq.test(table(data$col1, data$col2))",
  "caret::RMSE(predict(model, test), test$y)",
  "data$col[is.na(data$col)] <- median(data$col, na.rm = TRUE)",
  "data <- data[, colMeans(is.na(data)) < 0.5]",
  "shapiro.test(data$col)",
  "constant_cols <- sapply(data, function(x) length(unique(x)) == 1)",
  "jsonlite::fromJSON('fichier.json')",
  "xml2::read_xml('fichier.xml')",
  "readRDS('objet.rds')",
  "saveRDS(objet, 'objet.rds')",
  "list.files(path = '.', pattern = '*.csv')",
  "object.size(data)",
  "sapply(data, object.size)",
  "system.time(expr)",
  "lsf.str('package:ggplot2')",
  "right_join(df1, df2, by = 'id')",
  "full_join(df1, df2, by = 'id')",
  "inner_join(df1, df2, by = 'id')",
  "data <- read.csv('fichier.csv')",
  "library(readxl); data <- read_excel('fichier.xlsx')",
  "write.csv(data, 'fichier_sortie.csv', row.names = FALSE)",
  "writexl::write_xlsx(data, 'fichier_sortie.xlsx')",
  "data <- jsonlite::fromJSON('fichier.json')",
  "data <- readRDS('fichier.rds')",
  "saveRDS(data, 'fichier.rds')",
  "library(xml2); data <- read_xml('fichier.xml')",
  "file_list <- list.files(path='.', pattern='*.csv', full.names=TRUE); data <- do.call(rbind, lapply(file_list, read.csv))",
  "ggsave('graphique.png', plot = last_plot(), width = 8, height = 5)",
  "plotly::saveWidget(ggplotly(last_plot()), 'graphique_interactif.html')",
  "data <- read.csv(url('https://example.com/fichier.csv'))",
  "saveRDS(model, 'modele.rds')",
  "zip('fichiers.zip', list.files(path = '.', pattern = '*.csv'))",
  "write.table(data, 'fichier_sortie.tsv', sep = '\t', row.names = FALSE)",
  "data <- haven::read_sav('fichier.sav')",
  "data <- haven::read_dta('fichier.dta')",
  "data <- haven::read_sas('fichier.sas7bdat')",
  "jsonlite::write_json(data, 'fichier_sortie.json')",
  "feather::write_feather(data, 'fichier.feather')",
  "library(DBI); con <- dbConnect(RMySQL::MySQL(), dbname = 'base_de_donnees'); data <- dbGetQuery(con, 'SELECT * FROM table')",
  "library(DBI); con <- dbConnect(RMySQL::MySQL(), dbname = 'base_de_donnees'); dbWriteTable(con, 'table', data)"
  
)

# interface utilisateur
ui <- fluidPage(
  tags$head(
    tags$style(HTML("
      body {
        background-color: white;
        font-family: 'Segoe UI', sans-serif;
        color: #333;
      }

      .reponse-code {
        background-color: #f4f4f4;
        color: #1e3a5f;
        border-left: 5px solid #007acc;
        padding: 10px;
        font-family: 'Fira Code', 'Courier New', monospace;
        font-size: 1.1em;
        font-weight: bold;
        white-space: pre-wrap;
        border-radius: 5px;
        margin-top: 15px;
      }

      #copy-btn {
        background-color: #007acc;
        color: white;
        border: none;
        padding: 6px 12px;
        margin-top: 10px;
        border-radius: 4px;
        cursor: pointer;
      }

      #copy-btn:hover {
        background-color: #005c99;
      }

      #code-result {
        border: none;
        width: 100%;
        background-color: #f4f4f4;
        color: #1e3a5f;
        font-family: 'Fira Code', 'Courier New', monospace;
        font-size: 1.1em;
        font-weight: bold;
        resize: none;
        overflow: hidden;
        white-space: pre-wrap;
      }
    ")),
    # Script pour copier le code
    tags$script(HTML("
      function copyCode() {
        var codeText = document.getElementById('code-result');
        codeText.select();
        codeText.setSelectionRange(0, 99999); // mobile support
        document.execCommand('copy');
        alert('Code copié dans le presse-papiers !');
      }
    "))
  ),
  # Titre de l'application
  titlePanel("💬 Chatbot R : Copiez vos commandes !"),
  # Barre de navigation
  sidebarLayout(
    sidebarPanel(
      selectInput("question", "Choisissez une question :", 
                  choices = questions, selectize = TRUE),
      actionButton("submit", "Obtenir le code")
    ),
    # Espace pour le code
    mainPanel(
      h4("💡 Code correspondant :"),
      uiOutput("reponse"),
      actionButton("copy", "📋 Copier le code", onclick = "copyCode()", id = "copy-btn")
    )
  )
)
# Serveur
server <- function(input, output) {
  observeEvent(input$submit, {
    idx <- match(input$question, questions)
    code <- reponses[idx]
    output$reponse <- renderUI({
      tags$textarea(id = "code-result", class = "reponse-code", rows = 3, code)
    })
  })
  
}





# L'interface utilisateur


# Autoriser les uploads jusqu'à 500 Mo
options(shiny.maxRequestSize = 500*1024^2)

api_key_groq <- "Clef-API-GROQ"
openai_key <- "clef-API-OPENAI"
# Remplacez par votre clé API OpenAI

# === UI ===
ui <- fluidPage(
  useShinyjs(),
  theme = bs_theme(bootswatch = "flatly"),
  
  titlePanel("🧠 Analyse IA avec JICK"),
  
  tags$style(HTML("
    .chat-card {
      margin-bottom: 10px;
      padding: 10px;
      border-radius: 10px;
      box-shadow: 0 2px 4px rgba(0,0,0,0.1);
    }
    .user-msg { background-color: #e3f2fd; text-align: right; }
    .bot-msg { background-color: #f1f8e9; }
    .chatbox {
      height: 400px;
      overflow-y: scroll;
      border: 1px solid #ccc;
      padding: 10px;
      background: #ffffff;
    }
  ")),
  # les boutons
  sidebarLayout(
    sidebarPanel(
      selectInput("lang", "Langue", choices = c("Français", "English")),
      fileInput("dataset", "📁 Charger un fichier", accept = c(".csv", ".xlsx", ".xls", ".dta")),
      textInput("user_message", "💬 Question", placeholder = "Posez votre question ici..."),
      actionButton("send_question", "Envoyer", class = "btn btn-primary"),
      br(), br(),
      downloadButton("download_code", "📜 Code R"),
      downloadButton("download_csv", "📁 Résultats"),
      downloadButton("download_chat", "💬 Chat"),
      downloadButton("download_report", "📄 Rapport"),
      actionButton("reset_chat", "🧹 Réinitialiser", class = "btn-danger"),
      
    ),
    mainPanel(
      tabsetPanel(
        tabPanel("📂 Données", tableOutput("data_preview")),
        tabPanel("📊 Résumé visuel", tableOutput("summary_table")),
        tabPanel("📈 Graphique", plotlyOutput("auto_plot")),
        tabPanel("📜 Code R", verbatimTextOutput("code_r_output")),
        tabPanel("💬 Chat IA", div(class = "chatbox", uiOutput("chat_ui"))),
        tabPanel("📊 Statistiques",
                 hr(),
                 actionButton("toggle_code_box", "🧩 Code Essentiel et statistique descriptive", class = "btn btn-info"),
                 br(), br(),
                 conditionalPanel(
                   condition = "input.toggle_code_box % 2 == 1",
                   selectInput("question", "Choisissez une question :", choices = questions),
                   actionButton("submit_code", "Obtenir le code", class = "btn btn-primary"),
                   br(), br(),
                   uiOutput("reponse_code_ui"),
                   actionButton("copy_code", "📋 Copier le code", onclick = "copyCode()", id = "copy-btn"),
                   
                   dashboard_ui("stats_dashboard")
                   
                 )
                 
        )
      )
    )
  )
)

# === Serveur ===
server <- function(input, output, session) {
  data_shared <- reactiveVal(NULL)
  data <- reactiveVal(NULL)
  data_plot <- reactiveVal(NULL)
  data_table <- reactiveVal(NULL)
  code_r_text <- reactiveVal("")
  chat_history <- reactiveVal(list())
  
  # === Langue ===
  get_prompt_language <- reactive({
    if (input$lang == "English") {
      list(
        simple = function(msg) paste("Answer in English:", msg),
        with_data = function(msg, head_text) paste("Here is a dataset preview:\n", head_text, "\nAnswer this:\n", msg),
        code = function(msg, head_text) paste("Here is a dataset preview:\n", head_text, "\nWrite R code using ggplot2 for:\n", msg)
      )
    } else {
      list(
        simple = function(msg) paste("Réponds en français :", msg),
        with_data = function(msg, head_text) paste("Voici un aperçu des données :\n", head_text, "\nAnalyse et réponds :\n", msg),
        code = function(msg, head_text) paste("Voici un aperçu des données :\n", head_text, "\nÉcris du code R (avec ggplot2 si graphique) pour :\n", msg)
      )
    }
  })
  
  # === Chargement de données ===
  observeEvent(input$dataset, {
    ext <- tools::file_ext(input$dataset$name)
    df <- switch(ext,
                 csv = read.csv(input$dataset$datapath),
                 xlsx = read_excel(input$dataset$datapath),
                 xls = read_excel(input$dataset$datapath),
                 dta = read_dta(input$dataset$datapath),
                 NULL)
    
    # Réduction des catégories
    library(forcats)
    if (!is.null(df)) {
      # Remplace "groupe" par ta variable à beaucoup de niveaux
      if ("groupe" %in% names(df)) {
        df$groupe <- fct_lump(df$groupe, n = 10)
      }
      data(df)
      data_shared(df)
    }
    
    showNotification("✅ Données chargées", type = "message")
  })
  
  output$data_preview <- renderTable({ req(data()); head(data()) })
  
  output$summary_table <- renderTable({
    req(data())
    tbl_summary(data()) %>% as_gt() %>% as.data.frame()
  })
  
  # === Analyse IA ===
  observeEvent(input$send_question, {
    msg <- input$user_message
    if (msg == "") return()
    updateTextInput(session, "user_message", value = "")
    df <- data()
    
    new_history <- chat_history()
    new_history <- append(new_history, list(list(role = "user", content = msg)))
    chat_history(new_history)
    
    # IA sans base
    if (is.null(df)) {
      prompt <- get_prompt_language()$simple(msg)
      res <- POST("https://openrouter.ai/api/v1/chat/completions",
                  add_headers(Authorization = paste("Bearer", openai_key),
                              "Content-Type" = "application/json"),
                  body = toJSON(list(model = "openai/gpt-3.5-turbo",
                                     messages = list(list(role = "user", content = prompt)),
                                     temperature = 0.7), auto_unbox = TRUE))
      if (status_code(res) == 200) {
        reply <- content(res, as = "parsed")$choices[[1]]$message$content
      } else {
        reply <- "❌ Erreur OpenAI."
      }
      
    } else {
      # IA avec base de données
      head_text <- paste(capture.output(str(head(df))), collapse = "\n")
      
      # Analyse (Groq)
      prompt_groq <- get_prompt_language()$with_data(msg, head_text)
      res_groq <- POST("https://api.groq.com/openai/v1/chat/completions",
                       add_headers(Authorization = paste("Bearer", api_key_groq),
                                   "Content-Type" = "application/json"),
                       body = toJSON(list(model = "llama3-8b-8192",
                                          messages = list(list(role = "user", content = prompt_groq)),
                                          temperature = 0.7), auto_unbox = TRUE))
      reply <- content(res_groq, as = "parsed")$choices[[1]]$message$content
      
      # Génération de code
      prompt_code <- get_prompt_language()$code(msg, head_text)
      res_code <- POST("https://openrouter.ai/api/v1/chat/completions",
                       add_headers(Authorization = paste("Bearer", openai_key),
                                   "Content-Type" = "application/json"),
                       body = toJSON(list(model = "openai/gpt-3.5-turbo",
                                          messages = list(list(role = "user", content = prompt_code)),
                                          temperature = 0.7), auto_unbox = TRUE))
      code_raw <- content(res_code, as = "parsed")$choices[[1]]$message$content
      code_clean <- str_match(code_raw, "
r\\s*(.*?)
")[,2]
      code_clean <- ifelse(is.na(code_clean), code_raw, code_clean)
      code_r_text(code_clean)
      
      output$code_r_output <- renderText(code_clean)
      
      tryCatch({
        env <- new.env()
        env$df <- df
        result <- eval(parse(text = code_clean), envir = env)
        
        if (inherits(result, "gg")) data_plot(result)
        if (is.data.frame(result)) data_table(result)
      }, error = function(e) {
        output$code_r_output <- renderText(paste("❌ Erreur code :", e$message))
      })
    }
    
    chat_history(append(chat_history(), list(list(role = "assistant", content = reply))))
  })
  
  # === UI de chat stylisé ===
  output$chat_ui <- renderUI({
    msgs <- chat_history()
    ui_list <- lapply(msgs, function(msg) {
      role_class <- if (msg$role == "user") "user-msg" else "bot-msg"
      content <- msg$content
      content <- gsub("\\*\\*(.*?)\\*\\*", "<b>\\1</b>", content)
      content <- gsub("\\*(.*?)\\*", "<i>\\1</i>", content)
      content <- gsub("
r\\s*([\\s\\S]*?)
", "<pre><code class='r'>\\1</code></pre>", content)
      div(class = paste("chat-card", role_class), HTML(content))
    })
    do.call(tagList, ui_list)
  })
  
  # === Graphique ===
  output$auto_plot <- renderPlotly({
    p <- data_plot()
    if (inherits(p, "gg")) ggplotly(p)
  })
  
  # === Téléchargements ===
  output$download_code <- downloadHandler("code.R", function(file) writeLines(code_r_text(), file))
  output$download_csv <- downloadHandler("resultats.csv", function(file) write.csv(data_table(), file))
  output$download_chat <- downloadHandler("chat.txt", function(file) writeLines(sapply(chat_history(), function(x) paste0(x$role, ": ", x$content)), file))
  output$download_report <- downloadHandler("rapport.html", function(file) {
    rmarkdown::render("rapport.Rmd", output_file = file, params = list(data = data(), chat = chat_history())) })
  
  observeEvent(input$reset_chat, {
    chat_history(list())
    output$code_r_output <- renderText("")
    output$auto_plot <- renderPlotly(NULL)
    showNotification("💬 Chat réinitialisé", type = "message")
  })
  observeEvent(input$show_stats, {
    updateTabsetPanel(session, "main_tabs", selected = "📊 Statistiques")
    
    
  })
  dashboard_server("stats_dashboard", data = reactive({ data_shared() }))
  # Fonction pour copier le code
  output$code_essentiel_ui <- renderUI({
    tagList(
      selectInput("question_ce", "Choisissez une question :", choices = questions),
      actionButton("submit_ce", "Obtenir le code", class = "btn btn-info"),
      br(), br(),
      tags$textarea(id = "code_result_ce", class = "reponse-code", rows = 3, ""),
      actionButton("copy_ce", "📋 Copier le code", onclick = "copyCode()", id = "copy-btn")
    )
  })
  # Fonction pour copier le code
  observeEvent(input$submit_ce, {
    idx <- match(input$question_ce, questions)
    code <- reponses[idx]
    updateTextInput(session, "user_message", value = code)  # si tu veux pré-remplir aussi
    runjs(sprintf("document.getElementById('code_result_ce').value = %s;", code))
  })
  # Fonction pour copier le code
  observeEvent(input$submit_code, {
    idx <- match(input$question, questions)
    code <- reponses[idx]
    output$reponse_code_ui <- renderUI({
      tags$textarea(id = "code-result", class = "reponse-code", rows = 3, code)
    })
  })
  
}


shinyApp(ui = ui, server = server)



