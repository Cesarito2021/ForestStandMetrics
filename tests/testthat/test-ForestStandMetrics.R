library(testthat)

test_that("StemVolumeCalculator works correctly", {
  # Crea un esempio di data frame con le colonne richieste
  data <- data.frame(DBH = c(30),
                     Height = c(15),
                     Species = c("scots pine"))

  # Applica la funzione StemVolumeCalculator
  result <- StemVolumeCalculator(data, dbh_col = "DBH", th_col = "Height", specie_col = "Species")

  # Verifica che il risultato sia un numero
  expect_is(result, "numeric")

  # Aggiungi altre verifiche in base alle tue necessità
})
