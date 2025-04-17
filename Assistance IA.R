

packages <- c("httr", "jsonlite", "rstudioapi", "tcltk")

for (pack in packages){
  if(!requireNamespace(pack, quietly = TRUE)){
    install.packages(pack)
  }
  library(pack, character.only = TRUE)
}



## formating-----------

format_markdown_console <- function(md_text) {
  # Titres → majuscules
  md_text <- gsub("^#+\\s*(.*)", toupper("\\1"), md_text)
  
  # Gras → **Texte** → Texte en MAJ
  md_text <- gsub("\\*\\*(.*?)\\*\\*", toupper("\\1"), md_text)
  
  # Italique → *Texte* → Texte en minuscule
  md_text <- gsub("\\*(.*?)\\*", "\\1", md_text)
  
  # Puces → listes simples
  md_text <- gsub("^\\s*\\-\\s*", "• ", md_text)
  
  # Code blocs → indentation
  md_text <- gsub("```", "", md_text)
  md_text <- gsub("`([^`]*)`", "'\\1'", md_text)
  
  # Lignes vides multiples → une seule
  md_text <- gsub("\n{2,}", "\n", md_text)
  
  return(md_text)
}



#______________________________________________________________________________________________

# Assistance_Deepseek IA----

#______________________________________________________________________________________________


# Variable globale pour conserver l'historique de la conversation
deepseek_history <- list()

# Fonction pour appeler l'API DeepSeek en prenant en compte l'historique
call_deepseek_api <- function(history) {
  api_key <- "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"  # Remplacez par votre clé API DeepSeek
  api_url <- "https://openrouter.ai/api/v1/chat/completions"
  
  body <- list(
    model = "deepseek/deepseek-chat-v3-0324:free",  # Remplacez par le modèle approprié
    messages = history,
    temperature = 0.7
  )
  
  response <- tryCatch({
    POST(
      url = api_url,
      add_headers(
        Authorization = paste("Bearer", api_key),
        "Content-Type" = "application/json"
      ),
      body = toJSON(body, auto_unbox = TRUE),
      encode = "json"
    )
  }, error = function(e) {
    message("Erreur lors de l'appel à l'API DeepSeek : ", e$message)
    return(NULL)
  })


  
  if (is.null(response) || status_code(response) != 200) {
    message("Erreur API : Code HTTP ", status_code(response))
    return("Erreur lors de l'appel à l'API.")
  }
  
  result <- content(response, "parsed", encoding = "UTF-8")
  
  if (!is.null(result$choices)) {
    return(result$choices[[1]]$message$content)
  } else {
    return("Aucune réponse générée par l'IA DeepSeek.")
  }
}





# Fonction pour lire une entrée multi-lignes avec un marqueur de fin
# On tape plusieurs lignes, et on termine la dernière ligne par un "/"
read_multiline_input <- function(prompt = "Entrez votre question (terminez la dernière ligne par \"/\" ) :") {
  cat(prompt, "\n")
  lines <- character()
  repeat {
    line <- readline()
    # si la ligne se termine par "/" → on retire ce "/" et on sort
    if (grepl("/$", line)) {
      line <- sub("/$", "", line)
      lines <- c(lines, line)
      break
    }
    lines <- c(lines, line)
  }
  paste(lines, collapse = "\n")
}





# Fonction principale améliorée d'assistance DeepSeek
Assistance_deepseek <- function() {
  message("Bienvenue dans l'assistance DeepSeek !\n")
  message("Pour poser une question multi-lignes, tapez '__END__' sur une nouvelle ligne pour terminer.")
  message("Tapez '0' seul pour quitter.")
  
  repeat {
    # Lecture de la question multi-lignes
    question <- read_multiline_input()
    
    # Si l'utilisateur tape exactement "0", on quitte la conversation
    if (trimws(question) == "0") {
      message("\nMerci d'avoir utilisé notre assistance DeepSeek ! À bientôt.")
      break
    }
    
    if (nchar(trimws(question)) == 0) {
      message("⚠️ Aucune question posée, réessayez.\n")
      next
    }
    
    # Ajoute la question de l'utilisateur à l'historique
    deepseek_history <<- append(deepseek_history, list(list(role = "user", content = question)))
    
    # Appel à l'API avec l'ensemble de l'historique
    response <- call_deepseek_api(deepseek_history)
    
    # Ajoute la réponse de l'IA à l'historique
    deepseek_history <<- append(deepseek_history, list(list(role = "assistant", content = response)))
    
    # Affiche la réponse dans la console en utilisant la fonction de mise en forme Markdown
    cat("\n📨 Réponse de l'IA :\n")
    cat(format_markdown_console(response), "\n\n")
  }
}










#______________________________________________________________________________________________

# Assistance_Groq IA--------------------

#______________________________________________________________________________________________


# Fonction pour appeler l'API GROQ
call_groq_api <- function(question) {
  api_key <- "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"  # Remplacez par votre clé API DeepSeek
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
    cat("\n📨 Réponse de l'IA :\n\n")
    cat(format_markdown_console(response), "\n\n")
    
    # Demander à l'utilisateur s'il a d'autres questions
    another_question <- readline(prompt = "Avez-vous d'autres questions ? (oui/non) : ")
    if (tolower(another_question) != "oui") {
      message("Merci d'avoir solliciter notre assistance IA ! Groq est fièr de toi! À bientôt.")
      break  # Quitter la boucle si l'utilisateur ne veut plus poser de questions
    }
  }
}





#______________________________________________________________________________________________

# Assistance_Open IA---------------------

#______________________________________________________________________________________________



# Fonction pour appeler l'API 
call_open_api <- function(question) {
  api_key <- "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"  # Remplacez par votre clé API DeepSeek
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
    cat("\n📨 Réponse de l'IA :\n\n")
    cat(format_markdown_console(response), "\n\n")
    
    # Demander à l'utilisateur s'il a d'autres questions
    another_question <- readline(prompt = "Avez-vous d'autres questions ? (oui/non) : ")
    if (tolower(another_question) != "oui") {
      message("Merci d'avoir solliciter notre assistance IA ! Open est fièr de toi! À bientôt.")
      break  # Quitter la boucle si l'utilisateur ne veut plus poser de questions
    }
  }
}



