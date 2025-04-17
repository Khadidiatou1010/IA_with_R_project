# R/api_config.R

#' Configurer vos clés API dans ~/.Renviron
#'
#' Cette fonction ajoute ou met à jour vos clés API DeepSeek, OpenRouter et Groq
#' dans le fichier d'environnement `~/.Renviron`.
#' Vous devez redémarrer RStudio après exécution.
#'
#' @param deepseek   Clé API DeepSeek (optionnel)
#' @param openrouter Clé API OpenRouter / ChatGPT (optionnel)
#' @param groq       Clé API Groq (optionnel)
#' @export

api_config <- function(deepseek = NULL, openrouter = NULL, groq = NULL) {
  # chemin vers le .Renviron de l'utilisateur
  renv <- path.expand("~/.Renviron")
  if (!file.exists(renv)) file.create(renv)

  # on lit l'existant
  lines <- readLines(renv, warn = FALSE)

  # on retire toutes les anciennes variables concernées
  pattern <- "^(DEEPSEEK_API_KEY|OPENROUTER_API_KEY|GROQ_API_KEY)="
  lines <- lines[!grepl(pattern, lines)]

  # on prépare les nouvelles lignes
  new <- character()
  if (!is.null(deepseek))   new <- c(new, paste0("DEEPSEEK_API_KEY=", deepseek))
  if (!is.null(openrouter)) new <- c(new, paste0("OPENROUTER_API_KEY=", openrouter))
  if (!is.null(groq))       new <- c(new, paste0("GROQ_API_KEY=", groq))

  if (length(new) == 0) {
    stop("Veuillez fournir au moins une clé parmi `deepseek`, `openrouter` ou `groq`.")
  }

  # on écrit le tout
  writeLines(c(lines, new), renv)

  message("✅ Clés API mises à jour dans ", renv,
          "\n▶️ Redémarrez RStudio pour que les variables d'environnement soient prises en compte.")
}
