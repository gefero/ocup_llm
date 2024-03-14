library(tidyverse)
library(haven)

dir <- './data/raw/'

files <- list.files(dir, pattern=" personas ", full.names = TRUE)

df <- files %>%
        map_df(., ~read_spss(.)) %>%
        rename(ocupacion = ocupación)


change_chars <- function(string){
        str_replace_all(string, 'Ã‘', 'Ñ')
}


cod_publicados <- read_sav('../PISAC_ENES/data/ENES_Personas_version_final_GSF_PIMSA.sav') %>%
        select(nocues:miembro,v182caes, v183ciuo, v183cno)


df <- df %>%
        distinct(orden, .keep_all = TRUE) %>%
        mutate(across(rama:tecno, ~change_chars(.x))) %>%
        mutate(across(c(carac:calif,ciuo), ~as.character(factor(.x)))) %>%
        select(orden:tecno)

orden <- read_sav('./data/raw/orden personas.sav') %>%
        rename(orden = caso)


df_final <- df %>%
        left_join(orden) %>%
        select(orden, nocues, nhog, miembro, everything())

df_final <- df_final %>%
        left_join(cod_publicados)

df_final <- df_final %>%
        mutate(across(v182caes:v183cno, ~as.character(zap_labels(.x)))) %>%
        mutate(carac = str_sub(v183cno, start=1, end=2),
               jerarq = str_sub(v183cno, start=3, end=3), 
               tecn = str_sub(v183cno, start=4, end=4),
               calif = str_sub(v183cno, start=5, end=5),
               ocup_final = str_c(ocupacion, tareas, tecno, sep=". "))


write_csv(df, './data/proc/rama_ocupaciones.csv')
