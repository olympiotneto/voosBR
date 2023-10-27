formatar_numero <- function(x, acc = 1){
  scales::number(
    x,
    accuracy = acc,
    big.mark = ".",
    decimal.mark = ","
  )
}

contar_linhas <- function(tab){
  tab |>
    summarise(
      n = n()
    ) |>
    pull(n)
}
