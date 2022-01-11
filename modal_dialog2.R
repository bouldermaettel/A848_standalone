modal_dialog <-
  shiny::modalDialog(
    title = "Edit the Table to your needs",
    div(
      class = "text-center",
      div(
     excelOutput("table"),
    )),
    size = "l",
    easyClose = TRUE,
    footer = div(
      class = "pull-right container",
      shiny::actionButton(
        inputId = "final_edit",
        label = 'Save',
        icon = shiny::icon("edit"),
        class = "btn-success"
      ),
      shiny::actionButton(
        inputId = "dismiss_modal",
        label = "Close",
        class = "btn-danger"
      )
    )
  )
