
mod_05_llms_serv <- function(id, submit_button, input_text, selected_dataset_name,
                             api_key, sample_temp, selected_model, logs, ch,
                             counter, api_error_modal, code_error, current_data,
                             current_data_2, run_env, run_env_start, run_result,
                             use_python, send_head) {

  `%||%` <- function(a, b) if (!is.null(a)) a else b

  moduleServer(id, function(input, output, session) {

    llm_prompt <- reactive({
      req(submit_button(), selected_dataset_name(), input_text())
      isolate({
        prep_input(input_text(), selected_dataset_name(), current_data(),
                   use_python(), logs$id, send_head(), current_data_2())
      })
    })

    llm_response <- reactive({
      req(selected_dataset_name() != "Select a Dataset:")
      req(submit_button())

      isolate({
        req(input_text(), llm_prompt(), selected_dataset_name())
        prepared_request <- llm_prompt()

        shinybusy::show_modal_spinner(spin = "orbit", text = sample(jokes, 1), color = "#000000")
        on.exit(shinybusy::remove_modal_spinner(), add = TRUE)

        start_time <- Sys.time()

        response <- update_environment()
        prompt_total <- build_history(prepared_request)
        response <- send_request(prompt_total, prepared_request)

        final_response <- process_response(response, start_time)
        return(final_response)
      })
    })

    build_history <- function(prepared_request) {
      prompt_total <- list(list(role = "system", content = system_role))
      if (length(ch$code_history) > 0) {
        for (i in seq_along(ch$code_history)) {
          prompt_total <- append(prompt_total, list(
            list(role = "user", content = ch$code_history[[i]]$prompt_all),
            list(role = "assistant", content = ch$code_history[[i]]$raw)
          ))
        }
      }
      return(prompt_total)
    }

    update_environment <- function() {
      isolate({
        vars <- as.list(run_env())
        vars$df <- current_data()
        vars$df_name <- selected_dataset_name()
        vars$df2 <- current_data_2()
        run_env(list2env(vars))
        run_env_start(as.list(run_env()))
      })
    }

    send_request <- function(prompt_total, prepared_request) {
      prompt_total <- append(prompt_total, list(list(role = "user", content = prepared_request)))
      tryCatch({
        groq_agent(prompt_total)
      }, error = function(e) {
        return(list(
          error_status = TRUE,
          message = paste("Internal error:", e$message)
        ))
      })
    }

    process_response <- function(response, start_time) {
      error_api <- isTRUE(response$error_status)
      cmd <- if (error_api) NULL else response$choices$message.content
      error_message <- if (error_api) response$message else NULL

      api_time <- as.numeric(difftime(Sys.time(), start_time, units = "secs"))
      pt <- response$usage$prompt_tokens %||% 0
      ct <- response$usage$completion_tokens %||% 0
      counter$tokens_current <- pt + ct
      counter$requests <- counter$requests + 1
      counter$time <- round(api_time, 0)
      counter$costs_total <- counter$costs_total + api_cost(pt, ct, selected_model())

      return(list(
        cmd = polish_cmd(cmd),
        response = response,
        time = round(api_time, 0),
        error = error_api,
        error_message = error_message
      ))
    }

    groq_agent <- function(messages) {
      headers <- c(
        "Authorization" = paste("Bearer", api_key$key),
        "Content-Type" = "application/json"
      )

      body <- list(
        model = selected_model(),
        messages = messages,
        temperature = sample_temp()
      )

      response <- tryCatch({
        httr::POST(
          url = "https://api.groq.com/openai/v1/chat/completions",
          httr::add_headers(.headers = headers),
          body = jsonlite::toJSON(body, auto_unbox = TRUE)
        )
      }, error = function(e) {
        return(list(
          error_status = TRUE,
          message = paste("Connection error:", conditionMessage(e))
        ))
      })

      status <- tryCatch(httr::status_code(response), error = function(e) 500)

      if (status != 200) {
        message <- tryCatch(
          httr::content(response, as = "text"),
          error = function(e) paste("Groq error but failed to parse:", conditionMessage(e))
        )
        return(list(
          error_status = TRUE,
          message = message
        ))
      }

      parsed <- tryCatch(
        httr::content(response, as = "parsed"),
        error = function(e) return(list(error_status = TRUE, message = paste("Parsing error:", conditionMessage(e))))
      )

      if (!is.null(parsed$error_status) && parsed$error_status == TRUE) return(parsed)

      parsed$choices <- data.frame(
        message.content = parsed$choices[[1]]$message$content,
        stringsAsFactors = FALSE
      )

      parsed$error_status <- FALSE
      parsed$message <- NULL

      return(parsed)
    }



    return(list(
      llm_prompt = llm_prompt,
      llm_response = llm_response
    ))
  })
}
