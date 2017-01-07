library(rvest)
library(stringi)

html = read_html("https://pl.wikipedia.org/wiki/Lista_mecz%C3%B3w_reprezentacji_Polski_w_pi%C5%82ce_no%C5%BCnej_m%C4%99%C5%BCczyzn_(1921%E2%80%932000)")
tables = html_nodes(html,".wikitable")

html2 = read_html("https://pl.wikipedia.org/wiki/Lista_mecz%C3%B3w_reprezentacji_Polski_w_pi%C5%82ce_no%C5%BCnej_m%C4%99%C5%BCczyzn")
tables2 = html_nodes(html2,".wikitable")

make_table = function(html_tbl){
  raw_df = html_table(html_tbl)
  
  colnames(raw_df)[5] <- "Rok"
  colnames(raw_df)[7] <- "Wynik"
  colnames(raw_df)[6] <- "Gosc"
  colnames(raw_df)[8] <- "Bramki"
  
  for(i in 1:nrow(raw_df)){
    if(raw_df[i,5] == "–"){
      raw_df[i,5] = raw_df[i-1, 5]
    }
  }
  wyniki = stri_match_first_regex(raw_df$Wynik, "([0-9]+):([0-9]+)")
  
  raw_df = raw_df %>%
    mutate(Miejsce = stri_trim_both(Miejsce),
           Gospodarz = stri_trim_both(Gospodarz),
           Gosc = stri_trim_both(Gosc)) %>%
    mutate(Wynik = wyniki[,1],
           Wynik_nasz = as.numeric(ifelse(Gosc == "Polska", wyniki[,3], wyniki[,2])),
           Wynik_ich = as.numeric(ifelse(Gosc == "Polska", wyniki[,2], wyniki[,3])),
           Data = paste0(Data, ".", Rok),
           MS = stri_detect_regex(Bramki, "^MŚ"),
           ME = stri_detect_regex(Bramki, "^ME"),
           eMS = stri_detect_regex(Bramki, "^eMŚ"),
           eME = stri_detect_regex(Bramki, "^eME")) %>%
    filter(is.na(as.numeric(Miejsce))) %>%
    filter(as.numeric(Rok) < 2017) %>%
    mutate(Wygrana = sign(Wynik_nasz - Wynik_ich),
           Data = as.POSIXct(strptime(paste0(stri_replace_all_regex(Data,  c("(\\.)([0-9])(\\.)", "(^)([0-9])(\\.)"), "$10$2$3", vectorize_all = F), ".01"), "%d.%m.%Y.%I"), tz = "GMT"),
           Rok = as.numeric(Rok)) %>%
    mutate(Miesiac = as.numeric(format(Data, "%m"))) %>%
    select(Data, Rok, Miesiac,  Miejsce, Gospodarz, Gosc, Wynik, Wynik_nasz, Wynik_ich, Wygrana, MS, ME, eMS, eME)
  
  return(raw_df)
}



table1 = do.call(rbind, lapply(tables, make_table))
table2 = do.call(rbind, lapply(tables2, make_table))
mecze = rbind(table1, table2)


rm(table1, table2, html, html2, tables, tables2, make_table)
