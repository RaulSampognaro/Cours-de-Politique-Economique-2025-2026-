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
library(rsdmx)
library(patchwork)

calcul_part<-function(x){
  part=100*(x/sum(x))
  return(part)
}


# Système fiscal français ----

dataset<-"DSD_REV_OECD@DF_REVFRA"

tax_list<-c("_T","T_1100","T_1200","T_2100","T_2200","T_3000","T_4000","T_5000")


df <- rdb("OECD", dataset,mask="FRA.TAX_REV.S13.._T.EUR.A")|>
  filter(year(period) %in% c(1980,1990,2000,2013,2023))|>
  filter(STANDARD_REVENUE %in% tax_list)|>
  mutate(val2=if_else(STANDARD_REVENUE=="_T",value,-value))|>
  select(period,`Revenue category`,value,val2)


aut<-df|>group_by(period)|>
  summarise(value=sum(val2))|>
  ungroup()|>
  mutate(`Revenue category`="Other")

data<-rbind(df|>
              select(-val2),
            aut)

data_tot<-data|>
  filter(`Revenue category`%in% c("Total tax revenue"))|>
  rename(total=value)|>
  select(period,total)

data_det<- data|>
  filter(!(`Revenue category`%in% c("Total tax revenue")))|>
  merge(data_tot,by="period")|>
  mutate(part=100*(value/total))|>
  select(period,`Revenue category`,part)|>
  mutate(annee=as.character(year(period)))|>
  select(-c(period))|>
  pivot_wider(names_from="annee",
              values_from="part")

data_det|>
  gt()|>
  cols_label(`Revenue category`="")|>
  cols_align(starts_with(c("1","2")),align="center")|>
  fmt_number(decimals=1)|>
  tab_header(title=md("Structure de la fiscalité française"))|>
  tab_source_note(source_note=md("Source: OCDE"))


# Comparaison internationale ----


url <- "https://sdmx.oecd.org/public/rest/data/OECD.CTP.TPS,DSD_REV_COMP_GLOBAL@DF_RSGLOBAL,2.1/..S13.T_1110..PT_B1GQ.A?startPeriod=2023&dimensionAtObservation=AllDimensions"

# Lecture du flux SDMX
sdmx <- readSDMX(url)

# Conversion en data.frame
df <- as.data.frame(sdmx)

iso3_ocde <- c(
  "AUS","AUT","BEL","CAN","CHL","COL","CRI","CZE","DNK","EST","FIN",
  "FRA","DEU","GRC","HUN","ISL","IRL","ISR","ITA","JPN","KOR","LVA",
  "LTU","LUX","MEX","NLD","NZL","NOR","POL","PRT","SVK","SVN","ESP",
  "SWE","CHE","TUR","GBR","USA"
)

data_graph<-df|>
  select(REF_AREA,obsValue)|>
  filter(REF_AREA %in% iso3_ocde)|>
  mutate(REF_AREA=as.factor(REF_AREA))

data_graph|>
  ggplot()+
  aes(x=fct_reorder(REF_AREA,-obsValue),
      y=obsValue,
      fill=if_else(REF_AREA=="FRA","FRA","OTHER"))+
  geom_col()+
  scale_fill_manual(values = c("blue4","grey"))+
  theme_minimal()+
  theme(axis.text.x = element_text(angle = 90))+
  theme(legend.position="none")+
  labs(title="Impôts sur le revenu des personnes physiques",
       subtitle="En pts. de PIB",
       caption="Source: OCDE",
       y=NULL,
       x=NULL)

# Evolution IS et CSG ----

url<-"https://www.insee.fr/fr/statistiques/fichier/8574705/t_3217.xlsx"

curl_download(url, destfile = example<-tempfile("data",fileext = ".xlsx"))

data_graph<-read_xlsx(example)|>
  filter(row_number()<111)|>
  filter(!(row_number() %in% c(1)))|>
  select(-c(1))|>
  janitor::row_to_names(1)|>
  rename(var=1)|> 
  filter(!is.na(var))|>
  filter(row_number() %in% c(62,66:68))|>
  mutate_all(as.character)|>
  pivot_longer(cols=-c(var),
               names_to="date",
               values_to="recettes")|>
  mutate(recettes=if_else(is.na(recettes),0,as.numeric(recettes)))|>
  mutate(impot=if_else(var=="Contribution sociale généralisée (CSG) (**)","CSG","IR"))|>
  group_by(date,impot)|>
  summarise(recettes=sum(recettes))|>
  ungroup()



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
  mutate(val=100*(recettes/PIB))|>
  select(date,impot,val)

data_graph|>
  ggplot()+
  aes(x=as.integer(date),
      y=val,
      color=impot)+
  geom_line()+
  theme_minimal()+
  labs(title="Importance des impôts collectés sur les revenus des ménages",
       subtitle="En pts. de PIB",
       x=NULL,
       y=NULL,
       color=NULL,
       caption="Source: Insee")+
  theme(legend.position="bottom")


