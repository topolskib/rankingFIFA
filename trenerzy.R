trenerzy_raw = read.table("trenerzy.txt", sep = " ", header = F, stringsAsFactors = F)

colnames(trenerzy_raw) = c("Imie", "Nazwisko", "Lata", "Ilosc_meczy", "Bilans", "Poczatek", "Koniec")

trenerzy <- trenerzy_raw %>%
  mutate(Poczatek = as.POSIXct(strptime(paste0(Poczatek, ".01"), "%d.%m.%Y.%I"), tz = "GMT"),
         Koniec = as.POSIXct(strptime(paste0(Koniec, ".01"), "%d.%m.%Y.%I"), tz = "GMT"),
         Trener = paste0(Imie," ", stri_trans_totitle(Nazwisko))) %>%
  select(Trener, Poczatek, Koniec)

data = trenerzy_raw$Koniec[13]

rm(trenerzy_raw)
