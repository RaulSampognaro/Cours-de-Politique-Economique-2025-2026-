library(readxl)
library(curl)
library(gt)
library(tidyverse)
library(ggplot2)
library(janitor)
library(insee)
library(rdbnomics)
library(eurostat)

calcul_part<-function(x){
  part=100*(x/sum(x))
  return(part)
}

# Tableau: principaux postes de la dépense publique ----

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


# Tableau: postes plus dynamiques de la dépense publique ----

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
  # mutate(val2=round(val2,digits=1))|>
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


# Graphique comparaison internationale ----

dataset<-"DSD_PAG@DF_PAG"

df <- rdb("OECD", dataset, mask = ".A.PEP.PT_B1GQ._Z._Z._Z")

data<-df|>
  filter(period==max(period))|>
  select(REF_AREA,`Reference area`,value)|>
  arrange(-value)|>
  mutate(REF_AREA=if_else(REF_AREA=="OECD_REP","OCDE",REF_AREA))

data|>
  ggplot()+
  aes(x=fct_reorder(as.factor(REF_AREA),-value),
      y=value,
      fill=if_else(REF_AREA=="FRA","FRA",if_else(REF_AREA=="OCDE","OCDE","OTHER")))+
  geom_col()+
  theme_minimal()  +
  theme(axis.text.x = element_text(angle = 90))+
  scale_fill_manual(values = c("blue4", "coral3","grey")) +
  labs(title="Dépenses publiques en pensions",
       subtitle="En pts. de PIB",
       x=NULL,
       y=NULL,
       fill=NULL)+
  theme(legend.position="none")

# Graphique: taux de pauvreté ----

dat <- get_eurostat("ilc_li02", stringsAsFactors = TRUE)

geo_ZE<-c("BE","PT","ES","FR","IE","NL","DE","AT","LX",
          "SL","HR","EL","SK","EE","LT","LU","FI","FR","CY","MT")

data_long<-dat|>
  filter(sex=="T")|>
  filter(age %in% c("TOTAL","Y_GE65"))|>
  filter(TIME_PERIOD==max(TIME_PERIOD))|>
  filter(unit=="PC")|>
  filter(indic_il=="LI_R_MD60")|>
  filter(geo %in% geo_ZE)|>
  select(geo,age,values)|>
  mutate(age=if_else(age=="TOTAL",age,"+ de 65 ans"))

data<-data_long|>
  pivot_wider(names_from=age,
              values_from = values)

ordre_pays <- data %>%
  arrange(`+ de 65 ans`) %>%
  pull(geo)

data <- data %>%
  mutate(geo = factor(geo, levels = ordre_pays))

ggplot(data_long, aes(x = geo, y = values, color = age)) +
  # Segments reliant les deux catégories
  geom_segment(data = data,
               aes(x = geo, xend = geo,
                   y = TOTAL, yend = `+ de 65 ans`),
               inherit.aes = FALSE,
               color = "grey50",
               arrow = arrow(length = unit(0.2, "cm"))) +
  # Points
  geom_point(size = 3) +
  labs(
    x = "Pays",
    y = "Taux de pauvreté (%)",
    title = "Taux de pauvreté : ensemble de la population vs + 65 ans",
    color = NULL,
    caption="Source: Eurostat",
    subtitle="2024"
  ) +
  theme_minimal() +
  theme(
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank()
  )+
  theme(legend.position='bottom')


# Tableau: inégalités ----

url<-"https://www.insee.fr/fr/statistiques/fichier/8242355/FPORSOC24-F17.xlsx"

curl_download(url,
              destfile = example<-tempfile("data",fileext = ".xlsx"))

data_graph<-read_xlsx(example,sheet = "Figure 2")|>
  filter(row_number()<12)|>
  filter(row_number()>2)|>
  janitor::row_to_names(1)|>
  rename(`Statut d'activité`=1)|>
  mutate(across(-1, ~ as.numeric(.x)))

data_graph|>
  gt()|>
  cols_align(columns=-1,
             align="center")|>
  tab_style(
    style = cell_text(weight = "bold"),
    locations = cells_body(
      rows = c(5, 8)  # lignes à mettre en gras
    )
  )|>
  tab_spanner(
    label = md("Niveau de vie"),
    columns = 2:5
  )|>
  tab_spanner(
    label = md("Rapports interdéciles"),
    columns = 6:8
  )|>
  fmt_number(
    columns = 2:5,
    decimals = 0,
    use_seps = TRUE,
    sep_mark = " "
  )|>
  fmt_number(
    columns = 6:8,
    decimals = 1,
    use_seps = FALSE
  )|>
  tab_source_note(source_note = md("[Source: France Portrait Social 2024](https://www.insee.fr/fr/statistiques/8242355?sommaire=8242421)"))


# Situation d'activité par âge ----

url<-"https://dares.travail-emploi.gouv.fr/sites/default/files/4b4f0d7a250987ab2279cac337ca33ec/Dares%20R%C3%A9sultats%20Les%20seniors%20sur%20le%20march%C3%A9%20du%20travail%20en%202024.xlsx"

curl_download(url,
              destfile = example<-tempfile("data",fileext = ".xlsx"))


data <- read_excel(example, 
                   sheet = "Graphique 2")|>
  filter(row_number()>26)|>
  janitor::row_to_names(1)|>
  pivot_longer(cols=-c(Âge),names_to="situation",values_to="val")|>
  mutate(val=as.numeric(val),
         Âge=as.integer(Âge))

data|>
  ggplot()+
  aes(x=Âge,
      y=val,
      fill=situation)+
  geom_col()+
  theme_minimal()+
  labs(title=" Situation d’activité et de retraite des seniors par âge détaillé en 2024",
       caption = "Source : Insee, enquête Emploi 2024 ; calculs Dares.",
       x=NULL,
       y="En %",
       fill=NULL)+
  theme(legend.position="bottom")+
  guides(fill=guide_legend(nrow=2, byrow=TRUE))
