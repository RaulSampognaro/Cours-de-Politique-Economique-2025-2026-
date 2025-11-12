library(insee)
library(tidyverse)
library(ggplot2)
library(readxl)
library(janitor)
library(rdbnomics)
library(patchwork)
library(eurostat)
library(rsdmx)
library(gt)
library(curl)

# Ratios des SNF ----

url<-"https://www.insee.fr/fr/statistiques/fichier/8574689/T_7101.xlsx"

curl_download(url, destfile = example<-tempfile("data",fileext = ".xlsx"))

data_snf<-read_xlsx(path=example,sheet=2)|>
  filter(row_number()>2)|>
  filter(row_number()<91)|>
  janitor::row_to_names(row_number = 1)|>
  rename(var=1,
         label_var=2)|>
  filter(var %in% c("B1G","B2G","P51G"))|>
  select(-c(label_var))|>
  distinct(var,.keep_all = TRUE)|>
  pivot_longer(cols=-c(var),
               names_to  = "date",
               values_to = "val")|>
  mutate(val=as.numeric(val))|>
  pivot_wider(names_from="var",
              values_from="val")|>
  mutate(`Tx. de marge`=100*B2G/B1G,
         `Tx. d'investissement`=100*P51G/B1G)|>
  select(date,starts_with("Tx"))|>
  pivot_longer(cols=-c(date),
               names_to="var",
               values_to="val")

data_snf|>
  mutate(date=as.integer(date))|>
  ggplot()+
  aes(x=as.integer(date),
      y=val,
      color=var)+
  geom_line()+
  theme_minimal()+ 
  ylim(10, 40)+
  labs(title="Ratios des SNF",
       subtitle="En pts de la VA des SNF",
       x=NULL,
       y=NULL,
       color=NULL,
       caption="Source: Insee")+
  theme(legend.position="bottom")

# Capacité d'autofinancement ----

url<-"https://www.insee.fr/fr/statistiques/fichier/8574689/T_7101.xlsx"

curl_download(url, destfile = example<-tempfile("data",fileext = ".xlsx"))


data_snf<-read_xlsx(path=example,sheet=2)|>
  filter(row_number()>2)|>
  filter(row_number()<91)|>
  janitor::row_to_names(row_number = 1)|>
  rename(var=1,
         label_var=2)|>
  filter(var %in% c("B8G","P51G"))|>
  select(-c(label_var))|>
  distinct(var,.keep_all = TRUE)|>
  pivot_longer(cols=-c(var),
               names_to  = "date",
               values_to = "val")|>
  mutate(val=as.numeric(val))|>
  pivot_wider(names_from="var",
              values_from="val")|>
  mutate(`Tx. d'autofinancement`=100*B8G/P51G)|>
  select(date,starts_with("Tx"))|>
  pivot_longer(cols=-c(date),
               names_to="var",
               values_to="val")

data_snf|>
  mutate(date=as.integer(date))|>
  ggplot()+
  aes(x=as.integer(date),
      y=val,
      color=var)+
  geom_line()+
  theme_minimal()+
  labs(title="Capacité d'autofinancement des ENF",
       subtitle="En % de l'investissement",
       x=NULL,
       y=NULL,
       caption="Source: Insee")+
  theme(legend.position="none")

# Le système fiscal français----

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

url <- "https://sdmx.oecd.org/public/rest/data/OECD.CTP.TPS,DSD_REV_COMP_GLOBAL@DF_RSGLOBAL,2.1/..S13.T_1200..PT_B1GQ.A?startPeriod=2023&dimensionAtObservation=AllDimensions"

sdmx <- readSDMX(url)

df <- as.data.frame(sdmx)

iso3_ocde <- c(
  "AUS","AUT","BEL","CAN","CHL","COL","CRI","CZE","DNK","EST","FIN",
  "FRA","DEU","GRC","HUN","ISL","IRL","ISR","ITA","JPN","KOR","LVA",
  "LTU","LUX","MEX","NLD","NZL","POL","PRT","SVK","SVN","ESP",
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
  labs(title="Impôts sur le revenu des sociétés",
       subtitle="En pts. de PIB",
       caption="Source: OCDE",
       y=NULL,
       x=NULL)


# Impôts collectés sur les SNF ----

url<-"https://www.insee.fr/fr/statistiques/fichier/8574689/T_7101.xlsx"

curl_download(url, destfile = example<-tempfile("data",fileext = ".xlsx"))

data_graph<-read_xlsx(example,sheet=2)|>  
  filter(row_number() %in% c(3,11,24,63))|>
  select(-c(2))|>
  janitor::row_to_names(1)|>
  rename(var=1)|> 
  mutate_all(as.character)|>
  pivot_longer(cols=-c(var),
               names_to="date",
               values_to="obs")|>
  mutate(date=as.integer(date))|>
  mutate(obs=as.numeric(obs))|>
  group_by(date)|>
  mutate(obs=100*obs/obs[var=="B1G"])|>
  ungroup()|>
  filter(var!="B1G")|>
  mutate(label_var=if_else(var=="D51","IS","Impôts de production"))

data_graph|>
  ggplot()+
  aes(x=as.integer(date),
      y=obs,
      color=label_var)+
  geom_line()+
  theme_minimal()+
  labs(title="Impôts collectés sur les SNF",
       subtitle="En pts. de la VA des SNF",
       x=NULL,
       y=NULL,
       color=NULL,
       caption="Source: Insee")+
  theme(legend.position="bottom")


# Impôts collectés sur les SNF (détail) ----

url<-"https://www.insee.fr/fr/statistiques/fichier/8574689/T_7101.xlsx"

curl_download(url, destfile = example<-tempfile("data",fileext = ".xlsx"))

data_graph<-read_xlsx(example,sheet=2)|>  
  filter(row_number() %in% c(3,11,25,26,63))|>
  select(-c(2))|>
  janitor::row_to_names(1)|>
  rename(var=1)|> 
  mutate_all(as.character)|>
  pivot_longer(cols=-c(var),
               names_to="date",
               values_to="obs")|>
  mutate(date=as.integer(date))|>
  filter(date>=1978)|>
  mutate(obs=as.numeric(obs))|>
  group_by(date)|>
  mutate(obs=100*obs/obs[var=="B1G"])|>
  ungroup()|>
  filter(var!="B1G")|>
  mutate(label_var=if_else(var=="D51","IS",if_else(var=="D291","Impôts sur les salaires et la main d’œuvre","Impôts divers sur la production")))

data_graph|>
  ggplot()+
  aes(x=as.integer(date),
      y=obs,
      color=label_var)+
  geom_line()+
  theme_minimal()+
  labs(title="Impôts collectés sur les SNF",
       subtitle="En pts. de la VA des SNF",
       x=NULL,
       y=NULL,
       color=NULL,
       caption="Source: Insee")+
  theme(legend.position="bottom")

# Comparaison internationale ----

data_snf<-get_eurostat("nasa_10_nf_tr", stringsAsFactors = TRUE)|>
  filter(sector %in% "S11")|>
  filter(na_item %in% c("B1G","D29"))|>
  filter(year(TIME_PERIOD)==max(year(TIME_PERIOD))-1)|>
  filter(unit %in% "CP_MEUR")|>
  filter(!(direct %in% "RECV"))|>
  select(na_item,geo,values)|>
  pivot_wider(names_from = "na_item",
              values_from = "values")|>
  mutate(taux=100*D29/B1G)|>
  select(geo,taux)


data_snf|>
  filter(geo!="EU27_2020")|>
  mutate(geo=as.factor(geo))|>
  mutate(geo=fct_reorder(geo,-taux))|>
  ggplot()+
  aes(x=geo,
      y=taux,
      fill=if_else(geo=="FR","FR",if_else(geo=="EA20","EA20","OTHER")))+
  geom_col()+
  theme_minimal()+
  labs(title="Impôts sur la production payées par les SNF (2023)",
       subtitle="En % de la VA des SNF",
       x=NULL,
       y=NULL,
       caption="Source: Eurostat")+
  scale_fill_manual(values = c( "coral3","blue4","grey"))+
  theme(legend.position="none")

# Importance des multinationales ----

url <- "https://www.insee.fr/fr/statistiques/fichier/7678556/ENTFRA23-F16.xlsx"

curl_download(url, destfile = example<-tempfile("data",fileext = ".xlsx"))

read_xlsx(example,sheet="Figure 1")|>
  filter(row_number() %in% c(4,8:11))|>
  select(1,3,5,7)|>
  rename(type=1,
         n=2,
         l=3,
         va=4)|>
  mutate(across(-type, as.numeric))|>
  gt()|>
  cols_label(type=md("Type d'entreprise"),
             n=md("Nbre. d'entreprises"),
             l=md("Emploi en France"),
             va=md("VA au coût des facteurs"))|>
  fmt_number(decimals=1,dec_mark = ",")|>
  tab_source_note(source_note = md("[Source: Insee](https://www.insee.fr/fr/statistiques/7678556?sommaire=7681078#:~:text=Les%20firmes%20multinationales%20sous%20contr%C3%B4le%20%C3%A9tranger%20emploient%202%2C2%20millions,le%20commerce%20(26%20%25).)"))|>
  tab_style(
    style = cell_text(style = "italic"),
    locations = cells_body(
      rows = c(4, 5)  
    )
  )|>
  tab_style(
    style = cell_text(weight = "bold"),
    locations = cells_body(
      rows = c(3)  
    )
  )|>
  cols_align(columns=-c(type),
             align = "center")

