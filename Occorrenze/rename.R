# Imposta la directory di lavoro
cartella <- "percorso/della/cartella"  # Cambia con il tuo percorso
setwd(cartella)

# Ottieni la lista dei file nella cartella
file_list <- list.files(pattern = "vecchio_suffisso$")  # Cambia con il suffisso da sostituire

# Loop per rinominare i file
for (file in file_list) {
  nuovo_nome <- sub("vecchio_suffisso$", "nuovo_suffisso", file)
  file.rename(file, nuovo_nome)
}

cat("Rinominati", length(file_list), "file con il nuovo suffisso.\n")
