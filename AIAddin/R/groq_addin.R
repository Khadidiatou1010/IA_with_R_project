# R/api_groq.R

#' @noRd
call_groq_api <- function(history) {
  api_key <- Sys.getenv("GROQ_API_KEY")
  if (identical(api_key, "")) {
    stop("Merci de définir la variable d'environnement GROQ_API_KEY avec votre clé Groq.")
  }

  resp <- httr::POST(
    url = "https://api.groq.com/openai/v1/chat/completions",
    httr::add_headers(
      Authorization = paste("Bearer", api_key),
      "Content-Type" = "application/json"
    ),
    body = jsonlite::toJSON(
      list(
        model       = "llama3-8b-8192",
        messages    = history,
        temperature = 0.7
      ),
      auto_unbox = TRUE
    ),
    encode = "json"
  )

  if (httr::status_code(resp) != 200) {
    return("Erreur Groq API")
  }

  parsed <- httr::content(resp, "parsed", encoding = "UTF-8")
  parsed$choices[[1]]$message$content
}

# R/utils.R

#' @noRd
format_markdown_console <- function(md_text) {
  md_text <- gsub("^#+\\s*(.*)", toupper("\\1"), md_text)
  md_text <- gsub("\\*\\*(.*?)\\*\\*", toupper("\\1"), md_text)
  md_text <- gsub("\\*(.*?)\\*", "\\1", md_text)
  md_text <- gsub("^\\s*\\-\\s*", "• ", md_text)
  md_text <- gsub("```", "", md_text)
  md_text <- gsub("`([^`]*)`", "'\\1'", md_text)
  gsub("\n{2,}", "\n", md_text)
}

# R/groq_addin.R

#' Assistance Groq Addin
#'
#' Ouvre un gadget Shiny dans le Viewer pane pour poser des questions à l'IA Groq.
#' @return Lance le gadget Shiny dans le Viewer pane.
#' @export
groq_addin <- function() {
  ui <- miniUI::miniPage(
    miniUI::gadgetTitleBar("Assistance Groq"),
    miniUI::miniContentPanel(
      shiny::tags$style(shiny::HTML("
        .chat-container {
          overflow-y: auto;
          height: 300px;
          padding: 10px;
          background: #f9f9f9;
        }
        .user-chat, .bot-chat {
          margin: 12px 0;
          white-space: pre-wrap;
          padding: 12px 16px;
          border-radius: 8px;
          box-shadow: 0 2px 4px rgba(0,0,0,0.1);
          line-height: 1.4;
        }
        .user-chat {
          background: #e0f7fa;
          border-left: 4px solid #0288d1;
          text-align: right;
        }
        .bot-chat {
          background: #fffde7;
          border-left: 4px solid #fbc02d;
          text-align: left;
        }
        .chat-separator {
          border: none;
          border-top: 1px dashed #ccc;
          margin: 8px 0;
        }
      ")),
      shiny::div(
        class = "chat-container",
        shiny::uiOutput("chat")
      ),
      shiny::textAreaInput(
        inputId    = "q",
        label      = NULL,
        placeholder = "Pose ta question…",
        width      = "100%",
        height     = "100px"
      ),
      shiny::actionButton(
        inputId = "send",
        label   = "Envoyer",
        class   = "btn-warning"
      )
    )
  )

  server <- function(input, output, session) {
    rv <- shiny::reactiveValues(h = list())

    shiny::observeEvent(input$send, {
      shiny::req(input$q)
      rv$h <- c(rv$h, list(list(role = "user", content = input$q)))

      res <- call_groq_api(rv$h)
      rv$h <- c(rv$h, list(list(role = "assistant", content = res)))

      shiny::updateTextAreaInput(session, "q", value = "")

      output$chat <- shiny::renderUI({
        msgs <- rv$h
        elems <- lapply(seq_along(msgs), function(i) {
          msg <- msgs[[i]]
          cls <- if (msg$role == "user") "user-chat" else "bot-chat"
          bubble <- shiny::div(
            class = cls,
            format_markdown_console(msg$content)
          )
          if (msg$role == "user" && i < length(msgs)) {
            list(
              bubble,
              shiny::tags$hr(class = "chat-separator")
            )
          } else {
            bubble
          }
        })
        htmltools::tagList(elems)
      })
    })

    shiny::observeEvent(input$done, {
      shiny::stopApp()
    })
  }

  shiny::runGadget(
    shiny::shinyApp(ui, server),
    viewer = rstudioapi::viewer
  )

}
