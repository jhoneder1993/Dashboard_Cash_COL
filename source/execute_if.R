execute_if <- function(.data, .predicate, .language)
{
  .predicate <- enquo(.predicate)
  if (rlang::eval_tidy(.predicate, data = .data))
  {
    .language <- enquo(.language)
    execute_in_pipeline(.data, !!.language)
  }
  else
    .data
}