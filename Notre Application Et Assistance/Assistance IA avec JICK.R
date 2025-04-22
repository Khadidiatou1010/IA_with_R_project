#Installation des packages

packages <- c("httr", "jsonlite", "rstudioapi", "tcltk")

for (pack in packages){
  if(!requireNamespace(pack, quietly = TRUE)){
    install.packages(pack)
  }
  library(pack, character.only = TRUE)
}

# Assistance_Deepseek IA


# Fonction pour appeler l'API DeepSeek
call_deepseek_api <- function(question) {
  api_key <- "Clé API"  # Remplacez par votre clé API DeepSeek
  api_url <- "https://openrouter.ai/api/v1/chat/completions"
  
  # Historique des messages
  history <- list(
    list(role = "user", content = question)
  )
  
  # Corps de la requête
  body <- list(
    model = "deepseek/deepseek-chat-v3-0324:free",  # Remplacez par le modèle approprié
    messages = history,
    temperature = 0.7
  )
  
  # Tentative d'appel de l'API
  response <- tryCatch({
    POST(
      url = api_url,
      add_headers(Authorization = paste("Bearer", api_key),
                  "Content-Type" = "application/json"),
      body = toJSON(body, auto_unbox = TRUE),
      encode = "json"
    )
  }, error = function(e) {
    message("Erreur lors de l'appel à l'API DeepSeek : ", e$message)
    return(NULL)
  })
  
  # Vérification du code de statut HTTP
  if (!is.null(response) && status_code(response) != 200) {
    message("Erreur API : Code HTTP ", status_code(response))
    return("Erreur lors de l'appel à l'API.")
  }
  
  # Vérification de la réponse de l'API
  if (is.null(response)) {
    return("Une erreur est survenue avec l'API DeepSeek.")
  }
  
  content <- content(response, "parsed", encoding = "UTF-8")
  
  # Si l'API renvoie une réponse valide
  if (!is.null(content$choices)) {
    return(content$choices[[1]]$message$content)
  } else {
    return("Aucune réponse générée par l'IA DeepSeek.")
  }
}

# Fonction pour poser une question directement dans la console et demander si l'utilisateur a d'autres questions
Assistance_Groq <- function() {
  repeat {
    question <- readline(prompt = "Entrez votre question : ")
    
    if (nchar(question) == 0) {
      message("Aucune question posée.")
      return(NULL)
    }
    
    # Appeler l'API DeepSeek avec la question
    response <- call_deepseek_api(question)
    
    # Afficher la réponse dans la console
    message("Réponse de l'IA : ", response)
    
    # Demander à l'utilisateur s'il a d'autres questions
    another_question <- readline(prompt = "Avez-vous d'autres questions ? (oui/non) : ")
    if (tolower(another_question) != "oui") {
      message("Merci d'avoir solliciter notre assistance IA ! Deepseek est fièr de toi! À bientôt.")
      break  # Quitter la boucle si l'utilisateur ne veut plus poser de questions
    }
  }
}


# Assistanc groq

# Fonction pour appeler l'API DeepSeek
call_groq_api <- function(question) {
  api_key <- "Clé API"  # Remplacez par votre clé API DeepSeek
  api_url <- "https://api.groq.com/openai/v1/chat/completions"
  
  # Historique des messages
  history <- list(
    list(role = "user", content = question)
  )
  
  # Corps de la requête
  body <- list(
    model = "llama3-8b-8192",  # Remplacez par le modèle approprié
    messages = history,
    temperature = 0.7
  )
  
  # Tentative d'appel de l'API
  response <- tryCatch({
    POST(
      url = api_url,
      add_headers(Authorization = paste("Bearer", api_key),
                  "Content-Type" = "application/json"),
      body = toJSON(body, auto_unbox = TRUE),
      encode = "json"
    )
  }, error = function(e) {
    message("Erreur lors de l'appel à l'API groq : ", e$message)
    return(NULL)
  })
  
  # Vérification du code de statut HTTP
  if (!is.null(response) && status_code(response) != 200) {
    message("Erreur API : Code HTTP ", status_code(response))
    return("Erreur lors de l'appel à l'API.")
  }
  
  # Vérification de la réponse de l'API
  if (is.null(response)) {
    return("Une erreur est survenue avec l'API groq.")
  }
  
  content <- content(response, "parsed", encoding = "UTF-8")
  
  # Si l'API renvoie une réponse valide
  if (!is.null(content$choices)) {
    return(content$choices[[1]]$message$content)
  } else {
    return("Aucune réponse générée par l'IA groq.")
  }
}

# Fonction pour poser une question directement dans la console et demander si l'utilisateur a d'autres questions
Assistance_Groq <- function() {
  repeat {
    question <- readline(prompt = "Entrez votre question : ")
    
    if (nchar(question) == 0) {
      message("Aucune question posée.")
      return(NULL)
    }
    
    # Appeler l'API DeepSeek avec la question
    response <- call_groq_api(question)
    
    # Afficher la réponse dans la console
    message("Réponse de l'IA : ", response)
    
    # Demander à l'utilisateur s'il a d'autres questions
    another_question <- readline(prompt = "Avez-vous d'autres questions ? (oui/non) : ")
    if (tolower(another_question) != "oui") {
      message("Merci d'avoir solliciter notre assistance IA ! Groq est fièr de toi! À bientôt.")
      break  # Quitter la boucle si l'utilisateur ne veut plus poser de questions
    }
  }
}




# Assistance_Open IA



# Fonction pour appeler l'API DeepSeek
call_open_api <- function(question) {
  api_key <- "clé API"  # Remplacez par votre clé API DeepSeek
  api_url <- "https://openrouter.ai/api/v1/chat/completions"
  
  # Historique des messages
  history <- list(
    list(role = "user", content = question)
  )
  
  # Corps de la requête
  body <- list(
    model = "gpt-3.5-turbo",  # Remplacez par le modèle approprié
    messages = history,
    temperature = 0.7
  )
  
  # Tentative d'appel de l'API
  response <- tryCatch({
    POST(
      url = api_url,
      add_headers(Authorization = paste("Bearer", api_key),
                  "Content-Type" = "application/json"),
      body = toJSON(body, auto_unbox = TRUE),
      encode = "json"
    )
  }, error = function(e) {
    message("Erreur lors de l'appel à l'API open IA : ", e$message)
    return(NULL)
  })
  
  # Vérification du code de statut HTTP
  if (!is.null(response) && status_code(response) != 200) {
    message("Erreur API : Code HTTP ", status_code(response))
    return("Erreur lors de l'appel à l'API.")
  }
  
  # Vérification de la réponse de l'API
  if (is.null(response)) {
    return("Une erreur est survenue avec l'API groq.")
  }
  
  content <- content(response, "parsed", encoding = "UTF-8")
  
  # Si l'API renvoie une réponse valide
  if (!is.null(content$choices)) {
    return(content$choices[[1]]$message$content)
  } else {
    return("Aucune réponse générée par l'IA groq.")
  }
}

# Fonction pour poser une question directement dans la console et demander si l'utilisateur a d'autres questions
Assistance_Chatgpt <- function() {
  repeat {
    question <- readline(prompt = "Entrez votre question : ")
    
    if (nchar(question) == 0) {
      message("Aucune question posée.")
      return(NULL)
    }
    
    # Appeler l'API DeepSeek avec la question
    response <- call_open_api(question)
    
    # Afficher la réponse dans la console
    message("Réponse de l'IA : ", response)
    
    # Demander à l'utilisateur s'il a d'autres questions
    another_question <- readline(prompt = "Avez-vous d'autres questions ? (oui/non) : ")
    if (tolower(another_question) != "oui") {
      message("Merci d'avoir solliciter notre assistance IA ! Open est fièr de toi! À bientôt.")
      break  # Quitter la boucle si l'utilisateur ne veut plus poser de questions
    }
  }
}



