execute_in_pipeline <- function(.data, .language)
{
  # nice thing is that all !! unquotation for the environment from where we are called is done
  # by enquo either here or one level up
  .language <- enquo(.language)
  # the quosure stores the environment from where it originates
  pipeline_env <- rlang::get_env(.language)
  # Something like ". %>% ..." creates a magrittr functional sequence, which can be called
  # Prepare such a call in our caller's environment
  
  magrittr_call <- quo(`%>%`(., !!.language))
  magrittr_call <- rlang::quo_expr(magrittr_call)
  #alternative
  #magrittr_call <- parse_expr(str_c(". %>% ", quo_text(.language)))
  
  magrittr_call <- rlang::new_quosure(magrittr_call, env=pipeline_env)
  # Call magrittr::%>%, creating a functiona sequence
  fseq <- rlang::eval_tidy(magrittr_call)
  # Call the sequence with our .data parameter
  rlang::invoke(fseq, list(.data))
}