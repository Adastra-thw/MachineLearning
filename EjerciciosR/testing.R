preg <- function() {
  R <-0; while (R != 3.1415) { # Inicio del loop (while)
    cat("Escriba el valor de PI hasta el 4to. decimal: ")
    R <- readLines(n = 1) # Lee la respuesta del usuario
    R <- as.numeric(R) # Convierte la respuesta en numeric
    if (R == 3.1415) # Condicional 1
      break # Corta y termina el loop
    if (R > 3.1415) {
      cat(" -- ese valor es muy ALTO ... intente de nuevo!\n")
    } else {
      cat(" -- ese valor es muy BAJO ... intente de nuevo!\n")
    }
  }
  cat("¡Es correcto!\n")
}