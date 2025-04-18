% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/api_config.R
\name{api_config}
\alias{api_config}
\title{Configurer vos clés API dans ~/.Renviron}
\usage{
api_config(deepseek = NULL, openrouter = NULL, groq = NULL)
}
\arguments{
\item{deepseek}{Clé API DeepSeek (optionnel)}

\item{openrouter}{Clé API OpenRouter / ChatGPT (optionnel)}

\item{groq}{Clé API Groq (optionnel)}
}
\description{
Cette fonction ajoute ou met à jour vos clés API DeepSeek, OpenRouter et Groq
dans le fichier d'environnement \verb{~/.Renviron}.
Vous devez redémarrer RStudio après exécution.
}
