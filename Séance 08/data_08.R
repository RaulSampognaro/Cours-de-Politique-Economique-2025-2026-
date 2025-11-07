library(insee)
library(tidyverse)
library(ggplot2)
library(curl)
library(readxl)
library(rdbnomics)
library(eurostat)

# Graphique Chomage ----

source<-"CHOMAGE-TRIM-NATIONAL"
idbank_list <- get_idbank_list(source)
age = c("00-")
sexe= c("0")
nat = c("TAUX")
ind = c("CTTXC")
area = c("FM")

test<-idbank_list|>
  filter(AGE %in% age,
         SEXE %in% sexe,
         NATURE %in% nat,
         INDICATEUR %in% ind,
         REF_AREA %in% area)|>
  select(idbank)|>
  pull(idbank)

data.chom = 
  get_insee_idbank(test)|>
  split_title()|>
  add_insee_metadata()|>
  select(DATE,OBS_VALUE,REF_AREA,TITLE_FR2,TITLE_FR3)|>
  arrange(DATE)

caption.graph=paste0("Source: Insee. Dernier point: T",quarter(max(data.chom$DATE))," ",year(max(data.chom$DATE)))


ggplot()+
  geom_line(data=data.chom,aes(x=DATE,y=OBS_VALUE,color=TITLE_FR3),show.legend = T)+
  labs(
    title="Taux de chômage (France Métropolitaine)",
    subtitle="% de la population active",
    caption=caption.graph,
    y=NULL,
    x=NULL,
    color=NULL)+
  theme_minimal()+
  theme(legend.position="none")+
  scale_x_date(expand=c(0,0))+
  scale_y_continuous(labels=scales::label_number(decimal.mark=","))


# Graphique DEFM ----

source<-"DEMANDES-EMPLOIS-NATIONALES"

series.defm<-insee::get_idbank_list(source)|>
  filter(REF_AREA %in% area)|>
  filter(SEXE %in% sexe)|>
  filter(AGE %in% age)|>
  filter(FREQ %in% "T")|>
  filter(CORRECTION %in% "CVS-CJO")|>
  filter(ANCIENNETE %in% "SO")|>
  filter(str_length(DEMANDEURS_EMPLOI)==1)|>
  select(IDBANK=idbank,DEMANDEURS_EMPLOI_label_fr)


data.defm<-series.defm|>
  pull(IDBANK)|>
  get_insee_idbank()|>
  select(DATE,OBS_VALUE,IDBANK)|>
  merge(series.defm,by="IDBANK")|>
  select(-c(IDBANK))

caption.graph=paste0("Source: Insee. Dernier point: T", quarter(max(data.defm$DATE))," ",year(max(data.chom$DATE)))

data.defm|>
  ggplot()+
  aes(x=DATE,
      y=OBS_VALUE/1000,
      color=DEMANDEURS_EMPLOI_label_fr)+
  geom_line()+
  theme_minimal()+
  labs(title="Demandeurs d'emploi en fin de mois",
       subtitle="France Métropolitaine - En millions",
       x=NULL,
       y=NULL,
       color=NULL,
       caption=caption.graph)+
  theme(legend.position="bottom")+
  guides(colour = guide_legend(nrow = 3))


# Chômage, Halo et sous-emploi ----

source<-"CHOMAGE-TRIM-NATIONAL"
idbank_list <- get_idbank_list(source)
age = c("15-64")
sexe= c("0")
ind = c("PCONTR1","PCONTR2","PCONTR3")
area = c("FR-D976")

test<-idbank_list%>%
  filter(AGE %in% age,
         SEXE %in% sexe,
         INDICATEUR %in% ind,
         REF_AREA %in% area)%>%
  select(idbank)%>%
  pull(idbank)

data.chom = 
  get_insee_idbank(test) %>%
  split_title()%>% 
  add_insee_metadata()%>%
  select(DATE,OBS_VALUE,REF_AREA,TITLE_FR1)%>%
  arrange(DATE)


label.graphique<- data.frame(
  TITLE_FR1   = row.names(table(data.chom$TITLE_FR1)),
  label.graph = c("Chômage","Chômage + halo","Chômage + halo + sous-emploi")) 

data.graph <- merge(data.chom,label.graphique,by="TITLE_FR1")

caption.graph=paste0("Source: Insee. Dernier point: T",quarter(max(data.graph$DATE))," ",year(max(data.graph$DATE)))

ggplot()+
  geom_line(data=data.graph,aes(x=DATE,y=OBS_VALUE,color=label.graph),show.legend = T)+
  labs(
    title="Du chômage à la contrainte sur l’offre de travail : parts parmi les participants  au marché du travail",
    subtitle="France hors Mayotte, personnes de 15 à 64 ans vivant en logement ordinaire",
    caption=caption.graph,
    y="En %",
    x=NULL,
    color=NULL)+
  theme_minimal()+
  theme(legend.position="bottom")+
  scale_x_date(expand=c(0,0))+
  scale_y_continuous(labels=scales::label_number(decimal.mark=","))

rm(data.chom,data.graph,idbank_list,label.graphique)
  
# Taux d'emploi Total ----

source<-"DSD_LFS@DF_LFS"

df<- rdb("OECD", source, mask = ".POP.PS._T.Y15T64.EMP+POP")|>
  filter(period==max(period))|>
  select(LABOUR_FORCE_STATUS,REF_AREA,original_value)|>
  mutate(original_value=as.numeric(original_value))|>
  pivot_wider(names_from="LABOUR_FORCE_STATUS",
              values_from = "original_value")|>
  mutate(tx=EMP/POP*100)|>
  select(REF_AREA,tx)|>
  mutate(REF_AREA=as.factor(REF_AREA))|>
  mutate(REF_AREA=fct_reorder(REF_AREA,tx))

table(df$REF_AREA)

iso3_ocde <- c(
  "AUS","AUT","BEL","CAN","CZE","DNK","EST","FIN","FRA",
  "DEU","GRC","HUN","ISL","IRL","ITA","JPN","KOR","LVA","LTU","LUX",
  "NLD","NZL","NOR","POL","PRT","SVK","SVN","ESP","SWE","CHE",
  "GBR","USA"
)

df|>
  filter(REF_AREA %in% c(iso3_ocde,"OECD"))|>
  ggplot()+
  aes(x=REF_AREA ,
      y=tx,
      fill=if_else(REF_AREA=="FRA","FRA",if_else(REF_AREA=="OECD","OECD","OTHER")))+
  geom_col()+
  theme_minimal()+
  labs(title="Taux d'emploi (2024)",
       subtitle="Ensemble de la population entre 15 et 64 ans",
       x=NULL,
       y=NULL,
       fill=NULL,
       caption="Source: OCDE")+
  scale_fill_manual(values = c("blue4", "coral3","grey"))+
  theme(legend.position = "none")

# Taux d'emploi par âge ----

source<-"EMPLOI-BIT-TRIM"

idbank_list <- get_idbank_list(source)
sexe= c("0")
area = c("FR-D976")
nat  = c("TAUX")
ind  = c("CTTE15")
age  = c("15-24","25-49","15-64","50-64")

test<-idbank_list%>%
  filter(SEXE %in% sexe,
         REF_AREA %in% area,
         NATURE %in% nat,
         INDICATEUR %in% ind,
         AGE %in% age)%>%
  select(idbank)%>%
  pull(idbank)

data.emploi = 
  get_insee_idbank(test) %>%
  split_title()%>% 
  add_insee_metadata()%>%
  select(DATE,OBS_VALUE,AGE_label_fr)%>%
  arrange(DATE)

caption.graph=paste0("Source: Insee. Dernier point: T",quarter(max(data.emploi$DATE))," ",year(max(data.emploi$DATE)))


ggplot()+
  geom_line(data=data.emploi,
            aes(
              x = DATE,
              y = OBS_VALUE,
              color = AGE_label_fr
            ),
            show.legend = T)+
  labs(
    title="Taux d'emploi",
    subtitle="% de la population active",
    caption=caption.graph,
    y=NULL,
    x=NULL,
    color=NULL)+
  theme_minimal()+
  theme(legend.position="bottom")+
  scale_x_date(expand=c(0,0))+
  scale_y_continuous(labels=scales::label_number(decimal.mark=","))

rm(data.emploi,idbank_list)


# Taux d'emploi par niveau de diplôme

country_list<-c("FR","DE","EA20")

data_graph<-get_eurostat("lfsi_educ_a")|>
  filter(TIME_PERIOD==max(TIME_PERIOD))|>
  filter(sex %in% "T")|>
  filter(age %in% "Y15-64")|>
  filter(unit %in% "PC_POP")|>
  select(geo,isced11,values)|>
  filter(geo %in% country_list)

data_graph|>
  ggplot()+
  aes(x=geo,
      y=values,
      fill=if_else(geo=="FR","FR","OT")
      )+
  geom_col()+
  scale_fill_manual(values = c("blue4","grey"))+
  facet_wrap(vars(isced11))+
  theme_minimal()+
  labs(x=NULL,
       y="En % de la population",
       title="Taux d'emploi selon le dernier diplôme obtenu",
       fill=NULL,
       subtitle="Population de 15 à 64 ans",
       caption="Source: Eurostat")+
  theme(legend.position = "off")


# Coût salarial unitaire ----

source="Eurostat"
base = "NAMA_10_LP_A21"

data<-rdb(provider_code = source,
          dataset_code = base,
          ids="A.I15.C.NULC_HW.FR+DE")|>
  select(geo,original_period,original_value)|>
  mutate(original_period=as.integer(original_period),
         original_value=as.numeric(original_value))|>
  group_by(geo)|>
  mutate(original_value=original_value/original_value[original_period==2000]*100)|>
  ungroup()

data|>
  filter(original_period>=2000)|>
  ggplot()+
  aes(x=original_period,
      y=original_value,
      color=geo)+
  geom_line()+
  theme_minimal()+
  labs(title="Cout salarial unitaire",
       subtile="Industrie manufacturière",
       x=NULL,
       y="Indice base 100 en 2000",
       color=NULL,
       caption="Source: Eurostat")+
  theme(legend.position="bottom")+
  geom_hline(yintercept=100,alpha=0.2)


# Taux implicite de cotisations sociales employeurs ----

sect<-c("S11","S12")
op<-c("D1","D121")

df_idbank_list_selected <-
  get_idbank_list("CNT-2020-CSI")%>%
  filter(SECT_INST %in% sect)%>%
  filter(OPERATION %in% op)%>%
  pull(idbank)

nom.var<-get_idbank_list("CNT-2020-CSI")%>%
  filter(SECT_INST %in% sect)%>%
  filter(OPERATION %in% op)%>%
  select(idbank,OPERATION)%>%
  rename(IDBANK=idbank)

data.cs<-get_insee_idbank(df_idbank_list_selected)%>%
  select(DATE,OBS_VALUE,IDBANK)%>%
  merge(nom.var,by="IDBANK")%>%
  arrange(DATE,OPERATION)%>%
  group_by(DATE,OPERATION)%>%
  summarise(OBS_VALUE=sum(OBS_VALUE))%>%
  ungroup()%>%
  arrange(DATE)%>%
  group_by(DATE)%>%
  mutate(CS=100*OBS_VALUE[OPERATION=="D121"]/OBS_VALUE[OPERATION=="D1"])%>%
  filter(row_number()==1)%>%
  ungroup()

data.gr<-data.cs%>%
  select(DATE,CS)

last.obs<-max(data.gr$DATE)
caption.graph<-paste0("Source: Insee. Dernier point: T", lubridate::quarter(last.obs), " ",lubridate::year(last.obs))


data.gr|>
  filter(year(DATE)>=1990)|>
  ggplot()+
  aes(x=DATE,y=CS)+
  geom_line()+
  labs(title="Taux implicite de cotisations sociales employeurs effectives (D121)", 
       subtitle="Sociétés non financières et sociétés financières",
       caption=caption.graph, 
       x=NULL,
       y="En % de la masse salariale")+
  theme_minimal()+
  scale_y_continuous(
    labels = scales::number_format(decimal.mark = ','))


