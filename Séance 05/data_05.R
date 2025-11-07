library(readxl)
library(curl)
library(gt)
library(tidyverse)
library(ggplot2)
library(janitor)
library(insee)
library(rdbnomics)
library(eurostat)
library(readr)


calcul_part<-function(x){
  part=100*(x/sum(x))
  return(part)
}

# Tableau: dépense publique par poste ----

url<-"https://www.insee.fr/fr/statistiques/fichier/8068626/t_3301.xlsx"

curl_download(url,
              destfile = example<-tempfile("data",fileext = ".xlsx"))

data_graph<-read_xlsx(example,sheet = 2)|>
  select(-c(1,2))|>
  filter(row_number()<85)|>
  filter(row_number()>2)|>
  janitor::row_to_names(1)|>
  rename(var=1)|>
  mutate(nbcar=if_else(substr(var,1,2)=="GF",nchar(var),NA))|>
  filter(nbcar==6)|>
  select(-c(nbcar,var))|>
  pivot_longer(cols=-c(Annuel),
               names_to = "date",
               values_to="obs")|>
  mutate(obs=as.numeric(obs))|>
  filter(date==max(as.numeric(date)))

data_pib<-insee::get_idbank_list("CNA-2020-PIB")|>
  filter(OPERATION %in% "PIB")|>
  filter(PRIX_REF %in% "VAL")|>
  pull(idbank)|>
  get_insee_idbank()|>
  select(TIME_PERIOD,OBS_VALUE)|>
  rename(date=TIME_PERIOD,
         PIB=OBS_VALUE)|>
  mutate(PIB=PIB/1000)

data_graph<-merge(data_graph,data_pib,by="date")|>
  mutate(val=100*(obs/PIB))|>
  select(Annuel,val)

data_graph|>
  arrange(-val)|>
  mutate(val=round(val,digits=1))|>
  filter(row_number()<=10)|>
  mutate(rang=row_number())|>
  relocate(rang)|>
  gt()|>
  cols_align(columns=c(rang,val),align="center")|>
  cols_label(rang=md("Rang"),
             Annuel=md("Poste COFOG"),
             val="Dép. (en pts. de PIB)")|>
  tab_source_note(source_note = md("Source: Insee")) |>
  tab_header(
    title = md("10 premiers postes de la dépense pub. par fonction"),
    subtitle = md("Niveau (2023)")
  )

# Tableau: dynamique de la dépense publique par poste ----

data_graph<-read_xlsx(example,sheet = 2)|>
  select(-c(1,2))|>
  filter(row_number()<85)|>
  filter(row_number()>2)|>
  janitor::row_to_names(1)|>
  rename(var=1)|>
  mutate(nbcar=if_else(substr(var,1,2)=="GF",nchar(var),NA))|>
  filter(nbcar==6)|>
  select(-c(nbcar,var))|>
  pivot_longer(cols=-c(Annuel),
               names_to = "date",
               values_to="obs")|>
  mutate(obs=as.numeric(obs))

data_pib<-insee::get_idbank_list("CNA-2020-PIB")|>
  filter(OPERATION %in% "PIB")|>
  filter(PRIX_REF %in% "VAL")|>
  pull(idbank)|>
  get_insee_idbank()|>
  select(TIME_PERIOD,OBS_VALUE)|>
  rename(date=TIME_PERIOD,
         PIB=OBS_VALUE)|>
  mutate(PIB=PIB/1000)

data_graph<-merge(data_graph,data_pib,by="date")|>
  mutate(val=100*(obs/PIB))|>
  select(Annuel,val,date)|>
  filter(as.numeric(date) %in% c(min(as.numeric(date)),max(as.numeric(date))))|>
  group_by(Annuel)|>
  arrange(date)|>
  mutate(val2=val-val[1])|>
  ungroup()|>
  filter()|>
  filter(as.numeric(date) %in% c(max(as.numeric(date))))

data_graph|>
  select(-c(date))|>
  arrange(-val2)|>
  filter(row_number()<=10)|>
  mutate(rang=row_number())|>
  relocate(rang)|>
  gt()|>
  cols_align(columns=c(rang,val,val2),align="center")|>
  cols_label(rang=md("Rang"),
             Annuel=md("Poste COFOG"),
             val=md("Dép. (en pts. de PIB)"),
             val2= md("$\\Delta$ Dép. pub (en pts de PIB)"))|>
  tab_source_note(source_note = md("Source: Insee")) |>
  tab_header(
    title = md("10 premiers postes de la dépense pub. par fonction"),
    subtitle = md("Evolution (1995-2023)")
  )|>
  fmt_number(decimals = 1)

# Tableau - Part de la VA par branche dans le PIB (Insee) ----

url<-'https://www.insee.fr/fr/statistiques/fichier/8574681/T_6201d_6203d.xlsx'

curl_download(url,
              destfile = example<-tempfile("data",fileext = ".xlsx"))

data_tab<-read_xlsx(example,
                    sheet="T_6201d en niveau")|>
  filter(row_number()>2)|>
  filter(row_number()<114)|>
  filter(row_number()!=2)|>
  janitor::row_to_names(1)|>
  rename(branche=1,
         branche_label=2)|>
  pivot_longer(cols=-c(branche,branche_label),
               values_to = "values",
               names_to ="date")|>
  mutate(values=as.numeric(values))|>
  group_by(date)|>
  mutate(values=100*(values/values[branche=="_T"]))|>
  ungroup()|>
  mutate(keep=1*(substr(branche,1,3)=="A38"))|>
  filter(keep==1)|>
  filter(date=="2023")


data_tab|>
  select(branche_label,values)|>
  arrange(-values)|>
  filter(row_number()<=11)|>
  gt()|>
  cols_align(columns = "values",
             align="center")|>
  fmt_number(decimals=1)|>
  cols_label(branche_label=md("Branche"),
             values=md("VA (en pts de la VA totale)"))|>
  tab_header(title=md("Part de la VA de la branche dans la VA totale française"),
             subtitle=md("Année 2023"))|>
  tab_source_note(source_note = md("Source: Insee. Comptes annuel de branche."))


# Part de la branche dans l'emploi total ----

url<-'https://www.insee.fr/fr/statistiques/fichier/8574683/T_6208d.xlsx'

curl_download(url,
              destfile = example<-tempfile("data",fileext = ".xlsx"))

data_tab<-read_xlsx(example,
                    sheet="T_6208d en niveau")|>
  filter(row_number()>2)|>
  filter(row_number()<114)|>
  filter(row_number()!=2)|>
  janitor::row_to_names(1)|>
  rename(branche=1,
         branche_label=2)|>
  pivot_longer(cols=-c(branche,branche_label),
               values_to = "values",
               names_to ="date")|>
  mutate(values=as.numeric(values))|>
  group_by(date)|>
  mutate(values=100*(values/values[branche=="_T"]))|>
  ungroup()|>
  mutate(keep=1*(substr(branche,1,3)=="A38"))|>
  filter(keep==1)|>
  filter(date=="2023")

data_tab|>
  select(branche_label,values)|>
  arrange(-values)|>
  filter(row_number()<=11)|>
  gt()|>
  cols_align(columns = "values",
             align="center")|>
  fmt_number(decimals=1)|>
  cols_label(branche_label=md("Branche"),
             values=md("VA (en pts de la VA totale)"))|>
  tab_header(title=md("Part de la VA de la branche dans la VA totale française"),
             subtitle=md("Année 2023"))|>
  tab_source_note(source_note = md("Source: Insee. Comptes annuel de branche."))


# Comparaison internationale ----

dataset<-"DSD_SHA@DF_SHA"

df <- rdb("OECD", dataset, mask = ".A.EXP_HEALTH.PT_B1GQ.HF1+_T._Z._T._T._T._Z._Z._Z")|>
  select(period,value,REF_AREA,`Financing scheme`)|>
  filter(period==max(period))

ordre_pays<-df|>
  filter(`Financing scheme`=="Total")|>
  arrange(-value)|>
  pull(REF_AREA)

df<-df|>
  mutate(REF_AREA=factor(REF_AREA,levels=ordre_pays))|>
  pivot_wider(names_from=`Financing scheme`,
              values_from = value)

ggplot(data=df,
       aes(x=REF_AREA))+
  geom_col(aes(y=Total,
               fill="Total"))+
  geom_col(aes(y=`Government/compulsory schemes`,
               fill="Government/compulsory schemes"))+
  
  theme_minimal()+
  theme(legend.position = "bottom")+
  labs(title="Dépenses en santé",
       subtitle="En pts. de PIB",
       x=NULL,
       y=NULL,
       fill=NULL)+
  theme(axis.text.x = element_text(angle = 90))

# Prise en charge de la CSBM ----

url<-"https://drees.solidarites-sante.gouv.fr/sites/default/files/2024-12/CNS2024%20-%20Partie%202%20Financement%20de%20la%20CSBMMAJ191224.xlsx"

curl_download(url,
              destfile = example<-tempfile("data",fileext = ".xlsx"))

data_graph<-read_xlsx(example,sheet = "F16 - Tab 1")|>
  filter(row_number()>=3)|>
  filter(row_number()<=8)|>
  janitor::row_to_names(1)|>
  rename(`Composante de la CSBM`=1)|>
  mutate_at(c("Sécurité sociale","Total","État","Organismes complémentaires","Ménages"),as.numeric)|>
  mutate(`Sécurité sociale`=100*(`Sécurité sociale`/Total),
         État = 100*(État/Total),
         `Organismes complémentaires`=100*(`Organismes complémentaires`/Total),
         Ménages=100*(Ménages/Total))|>
  select(-c(Total))


data_graph|>
  gt()|>
  fmt_number(decimals=1)|>
  cols_align(columns=-c(1),
             align="center")|>
  tab_header(title=md("Prise en charge de la CSBM"),
             subtitle=md("Année 2023"))|>
  tab_source_note(source_note=md("[Source: DREES (2025)](https://drees.solidarites-sante.gouv.fr/sites/default/files/2025-02/Les%20d%C3%A9penses%20de%20sant%C3%A9%20en%202023_MEL2ok.pdf)"))

# Dépenses en santé dans le PIB ----

dataset<-"DSD_SHA@DF_SHA"

df <- rdb("OECD", dataset, mask = ".A.EXP_HEALTH.PT_B1GQ._T._Z._T._T._T._Z._Z._Z")|>
  select(period,value,REF_AREA)|>
  filter(REF_AREA %in% c("DEU","GBR", "FRA","USA"))

df|>
  ggplot()+
  aes(x=period,
      y=value,
      color=REF_AREA)+
  geom_line()+
  theme_minimal()+
  theme(legend.position = "bottom")+
  labs(title="Dépenses en santé",
       subtitle="En pts. de PIB",
       x=NULL,
       y=NULL,
       color=NULL,
       caption="Source: OCDE")


