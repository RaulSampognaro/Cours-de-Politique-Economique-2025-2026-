library(OECD)
library(insee)
library(tidyverse)
library(readxl)
library(curl)
library(janitor)
library(gt)
library(globalmacrodata)
library(rsdmx)
library(rdbnomics)

calcul_part<-function(x){
  part=100*(x/sum(x))
  return(part)
}

# Graphique - Dépenses publiques en pts de PIB (Insee) ----

url<-'https://www.insee.fr/fr/statistiques/fichier/8574705/t_3201.xlsx'

curl_download(url,
              destfile = example<-tempfile("data",fileext = ".xlsx"))

data_dep<-read_xlsx(example)|>
  filter(row_number() %in% c(2,26))|>
  select(-c(1,2))|>
  janitor::row_to_names(1)|>
  mutate(var="Dépenses")|>
  pivot_longer(cols=-c(var),
               values_to = "values",
               names_to ="date")


data_pib<-insee::get_idbank_list("CNA-2020-PIB")|>
  filter(OPERATION %in% "PIB")|>
  filter(PRIX_REF %in% "VAL")|>
  pull(idbank)|>
  get_insee_idbank()|>
  select(TIME_PERIOD,OBS_VALUE)|>
  rename(date=TIME_PERIOD,
         PIB=OBS_VALUE)|>
  mutate(PIB=PIB/1000)

data<-merge(data_dep,data_pib,by="date")|>
  mutate(dep=100*(values/PIB))|>
  select(date,dep)|>
  mutate(date=as.integer(date))

data|>
  ggplot()+
  aes(date,
      dep)+
  geom_line()+
  theme_minimal()+
  labs(title="Dépeneses publiques totales",
       subtitle="En pts. de PIB",
       x=NULL,
       y=NULL,
       caption = "Source: Insee")

# Graphique - Dépenses publiques en pts de PIB (OCDE) ----

pays<-c("FRA","USA","GBR","OECD")

url<-"https://sdmx.oecd.org/public/rest/data/OECD.ECO.MAD,DSD_EO@DF_EO,1.3/.YPGTQ.A?startPeriod=1978&dimensionAtObservation=AllDimensions"

data_OCDE <- as.data.frame(readSDMX(url))|>
  filter(TIME_PERIOD>=1978)|>
  select(TIME_PERIOD,obsValue,REF_AREA)|>
  filter(REF_AREA %in% pays)

data_OCDE|>
  ggplot()+
  aes(x=as.integer(TIME_PERIOD),
      y=as.numeric(obsValue),
      color=REF_AREA)+
  geom_line()+
  theme_minimal()+
  labs(title="Dépenses publiques totales",
       subtitle="En pts. de PIB",
       x=NULL,
       y=NULL,
       color=NULL,
       caption= 'Source: OCDE')+
  theme(legend.position="bottom")


# Graphique historique sur très longue période ----

pays<-c("FRA","USA","DEU")

df <- gmd(country = pays)|>
  select(year,countryname,govexp_GDP)|>
  filter(year>=1789)|>
  filter(year<=2024)

df|>
  ggplot()+
  aes(x=year,
      y=govexp_GDP,
      color=countryname)+
  geom_line()+
  theme_minimal()+
  labs(title="Dépenses publiques (en pts. de PIB)",
       x=NULL,
       y=NULL,
       color=NULL,
       caption="Source: Global Macro Database (Müller, Xu, Lehbib et Chen (2025)")+
  theme(legend.position="bottom")

# Tableau - Part de la dépense faite par sous-secteur des APU (Insee) ----


url<-"https://www.insee.fr/fr/statistiques/fichier/8574705/t_3215.xlsx"

curl_download(url,
              destfile = example<-tempfile("data",fileext = ".xlsx"))

data_soussecteur<-read_xlsx(example)|>
  filter(row_number() %in% c(2,31,34,38))|>
  janitor::row_to_names(1)|>
  select(-c(1,2))|>
  mutate(secteur=c("APUL","APUSS","APU"))|>
  pivot_longer(cols=-c(secteur),
               values_to="values",
               names_to="date")|>
  pivot_wider(names_from="secteur",
              values_from="values")|>
  mutate(APUC=APU-APUL-APUSS)|>
  rename(`Adm. centrales`=APUC,
         Total = APU,
         `Adm. locales`= APUL,
         `Adm. Sécu. soc`=APUSS)|>
  mutate_if(is.numeric,~.x/Total*100)|>
  select(-c(Total))|>
  pivot_longer(cols=-c(date),
               names_to = "Secteur",
               values_to="Part")

data_soussecteur|>
  filter(date==min(as.numeric(date)) | date==max(as.numeric(date)))|>
  pivot_wider(names_from="date",
              values_from="Part")|>
  gt()|>
  cols_align(align=c('center'),
             columns = -c(Secteur))|>
  fmt_number(decimals=1)


# Graphique  Dépenses et recettes publiques (Insee) ----


url<-"https://www.insee.fr/fr/statistiques/fichier/8574705/t_3215.xlsx"

curl_download(url,
              destfile = example<-tempfile("data",fileext = ".xlsx"))

data_deficit<-read_xlsx(example)|>
  filter(row_number() %in% c(2,38,39))|>
  janitor::row_to_names(1)|>
  select(-c(1,2))|>
  mutate(var=c("Dépenses","Recettes"))|>
  pivot_longer(cols=-c(var),
               values_to="values",
               names_to="date")

data_deficit|>
  ggplot()+
  aes(x=as.integer(date),
      y=values,
      color=var)+
  geom_line()+
  theme_minimal()+
  labs(title="Dépenses et Recettes publiques",
       subtitle="En pts. de PIB",
       x=NULL,
       y=NULL,
       color=NULL,
       caption="Source: Insee")+
  theme(legend.position="bottom")

# Graphique- Déficit public (Insee) ----

data_deficit<-data_deficit|>
  pivot_wider(values_from = "values",
              names_from = "var")|>
  mutate(Solde=Recettes-Dépenses)|>
  select(date,Solde)

data_deficit|>
  ggplot()+
  aes(x=as.integer(date),
      y=Solde)+
  geom_line()+
  theme_minimal()+
  labs(title="Solde public",
       subtitle="En pts. de PIB",
       x=NULL,
       y=NULL,
       color=NULL,
       caption="Source: Insee")+
  theme(legend.position="bottom")+
  geom_hline(yintercept=0,alpha=0.2)

# Graphique - Dette publique (Insee) ----

url<-'https://www.insee.fr/fr/statistiques/fichier/8574703/t_3101.xlsx'

curl_download(url,
              destfile = example<-tempfile("data",fileext = ".xlsx"))

data_dette<-read_xlsx(example,sheet="T_3101")|>
  filter(row_number() %in% c(3,14))|>
  select(-c(1,2))|>
  janitor::row_to_names(1)|>
  mutate(var="Dette")|>
  pivot_longer(cols=-c(var),
               values_to = "values",
               names_to ="date")


data_dette|>
  ggplot()+
  aes(x=as.integer(date),
      y=as.integer(values))+
  geom_line()+
  theme_minimal()+
  labs(title="Dette publique",
       subtitle="En pts. de PIB",
       x=NULL,
       y=NULL,
       color=NULL,
       caption="Source: Insee")

# Graphique - Dette publiques (OCDE) ----

pays<-c("JPN","GRC","ITA","USA","FRA","ESP","GBR","EA17","DEU","DNK")

url<-"https://sdmx.oecd.org/public/rest/data/OECD.ECO.MAD,DSD_EO@DF_EO,1.3/.GGFLQ.A?startPeriod=2022&dimensionAtObservation=AllDimensions"

data_OCDE <- as.data.frame(readSDMX(url))|>
  filter(TIME_PERIOD==2024)|>
  select(TIME_PERIOD,obsValue,REF_AREA)|>
  filter(REF_AREA %in% pays)

data_OCDE|>
  ggplot()+
  aes(x=fct_reorder(as.factor(REF_AREA), -obsValue),
      y=obsValue)+
  geom_col()+
  theme_minimal()+
  labs(title="Dette publique",
       subtitle="En pts. de PIB",
       x=NULL,
       y=NULL,
       color=NULL,
       caption= 'Source: OCDE')+
  theme(legend.position="bottom")

# Dette historique ----

url<-"https://www.macrohistory.net/app/download/9834512569/JSTdatasetR6.xlsx?t=1720600182"

curl_download(url,
              destfile = example<-tempfile("data",fileext = ".xlsx"))

data_dette<-read_xlsx(example)|>
  filter(iso=="FRA")|>
  select(year,debtgdp)|>
  mutate(debtgdp=debtgdp*100)

data_dette|>
  ggplot()+
  aes(x=year,
      y=debtgdp)+
  geom_line()+
  theme_minimal()+
  labs(title="Dette publique (en pts. de PIB)",
       x=NULL,
       y=NULL,
       caption="Source: Macrohistory (Jordà, Schularick et Taylor (2017)")

# Graphique - Déficit par sous-secteur (Insee) ----

url <- "https://www.insee.fr/fr/statistiques/fichier/8574703/t_3106.xlsx"

curl_download(url,
              destfile = example<-tempfile("data",fileext = ".xlsx"))

data_deficit_ss<-read_xlsx(example)|>
  filter(row_number() %in% c(2,13,16,17))|>
  select(-c(1))|>
  janitor::row_to_names(1)|>
  rename(agent=1)|>
  mutate(across(everything(), as.character))|>
  pivot_longer(cols=-c(agent),
               values_to = "values",
               names_to ="date")|>
  mutate(values=as.numeric(values))


data_deficit_ss|>
  ggplot()+
  aes(x=as.integer(date),
      y=values,
      color=agent)+
  geom_line()+
  theme_minimal()+
  labs(title="Solde public par sous-secteur des APU",
       subtitle="En pts. de PIB",
       x=NULL,
       y=NULL,
       color=NULL,
       caption="Source: Insee")+
  geom_hline(yintercept=0,
             alpha=0.2)+
  theme(legend.position = "bottom")

# Graphique - Dépenses pas fonction (Insee) ----

url <- "https://www.insee.fr/fr/statistiques/fichier/8068626/t_3307.xlsx"

curl_download(url,
              destfile = example<-tempfile("data",fileext = ".xlsx"))



data_depenses_ss<-read_xlsx(example,sheet="T_3307")|>
  filter(row_number()>2)|>
  row_to_names(1)|>
  rename(secteur=1,
         secteur_label=2,
         fonction=3,
         fonction_label=4)|>
  filter(row_number()>1)|>
  filter(grepl('S13',secteur))|>
  mutate(test=str_length(fonction))|>
  filter(test==4)|>
  select(-c(test))|>
  pivot_longer(cols=c(starts_with("2")),
               names_to="date",
               values_to="value")|>
  select(date,secteur,value,fonction_label)|>
  mutate(value=as.numeric(value))|>
  mutate(value=if_else(is.na(value),0,value))|>
  filter(date==max((date)))|>
  select(-c(date))|>
  filter(!(secteur %in% c("S13111","S13112")))|>
  pivot_wider(names_from=secteur,
              values_from = value)|>
  mutate_if(is.numeric,calcul_part)

data_depenses_ss|>
  gt()|>
  fmt_number(decimals=0)|>
  cols_align(align = "center",
             columns=-c(fonction_label))|>
  cols_label(fonction_label = "",
             S13 = "TOTAL",
             S1311 = "Adm. centrales",
             S1313 = "Adm. locales",
             S1314 = "Adm. Sécu. Soc.")%>%
  tab_style(
    style = cell_text(weight = "bold"),
    locations = list(
      cells_column_labels(columns = c(S13)),
      cells_body(columns = c(S13))
    )
  )|>
  tab_header(title="Dépenses publiques par fonction (COFOG)",
             subtitle="Année 2023")|>
  tab_source_note(source_note = "Source: Insee")


# Dépense publique par fonction (Eurostat) ----

countries    <- c("FR","DE","IT","ES","DK","EA19")

url_country  <- paste0(countries, collapse = "+")
source<- "Eurostat"
base<-"GOV_10A_EXP"


variables      <-c("GF01","GF02","GF03","GF04","GF05","GF06","GF07","GF08","GF09","GF10","TOTAL")


url_variables  <- paste0(variables, collapse = "+")
filter <- paste0("A.PC_GDP.S13.",url_variables,".TE.",url_country)



out.data <- rdb(source,base,mask = filter)%>%
  filter(original_period==max(original_period))%>% 
  select(value,
         geo,
         `Classification of the functions of government (COFOG 1999)`) %>%
  spread(geo, value)|>
  relocate(`Classification of the functions of government (COFOG 1999)`,countries)

out.data|>
  gt()|>
  cols_align(align="center",
             columns=-c(1))|>
  cols_label(`Classification of the functions of government (COFOG 1999)`="")|>
  tab_style(
    style = cell_text(weight = "bold"),
    locations = list(
      cells_column_labels(columns = c(FR)),
      cells_body(columns = c(FR))
    )
  )|>
  tab_header(
    title = md("Dépense publique par fonction (2023)"),
    subtitle = md("En pts. de PIB")
  )|>
  tab_source_note(source_note = "Source: Eurostat")


# Tableau - Recettes (OCDE) ----

liste<-c("1100","1200","2000","3000","4000","5110","5120")

label.liste<-data.frame(
  REVENUE_CODE=liste,
  label.REVENUE_CODE=c(
    "Taxes on income, profits and capital gains of individuals","Taxes on income, profits and capital gains of corporations","Social security contributions (SSC)",
    "Taxes on payroll and workforce",
    "Taxes on property",
    "General taxes",
    "Taxes on specific goods and services")
)

pays<-c("JPN","GRC","ITA","USA","FRA","ESP","GBR","EA17","DEU","DNK")

url<-"https://sdmx.oecd.org/public/rest/data/OECD.CTP.TPS,DSD_REV_OECD@DF_REVFRA,1.1/..S13+S1311+S1313+S1314....A?startPeriod=2022&dimensionAtObservation=AllDimensions"

data_OCDE <- as.data.frame(readSDMX(url))|>
  filter(REVENUE_CODE %in% liste)|>
  filter(as.numeric(TIME_PERIOD)==max(as.numeric(TIME_PERIOD)))|>
  select(REVENUE_CODE,SECTOR,obsValue)|>
  mutate(obsValue=as.numeric(obsValue))


data_OCDE_total<-as.data.frame(readSDMX(url))|>
  filter(REVENUE_CODE %in% "TOTALTAX")|>
  filter(as.numeric(TIME_PERIOD)==max(as.numeric(TIME_PERIOD)))|>
  select(REVENUE_CODE,SECTOR,obsValue)|>
  mutate(obsValue=as.numeric(obsValue))

last_line<-data_OCDE|>
  group_by(SECTOR)|>
  summarise(temp=sum(obsValue))|>
  ungroup()|>
  merge(data_OCDE_total,by="SECTOR")|>
  mutate(obsValue=obsValue-temp,
         REVENUE_CODE="Autres")|>
  select(-c(temp))|>
  relocate(REVENUE_CODE,SECTOR,obsValue)



data_OCDE<-rbind(data_OCDE,
                 last_line,
                 data_OCDE_total)

data_OCDE<-data_OCDE|>
  group_by(SECTOR)|>
  mutate(obsValue=100*(obsValue/obsValue[REVENUE_CODE=="TOTALTAX"]))|>
  ungroup()|>
  pivot_wider(names_from="SECTOR",
              values_from='obsValue')|>
  merge(label.liste,by="REVENUE_CODE")

data_OCDE|>
  relocate(label.REVENUE_CODE,S13,S1311,S1313,S1314)|>
  arrange(REVENUE_CODE)|>
  select(-c(REVENUE_CODE))|>
  gt()|>
  fmt_number(decimals=0)|>
  cols_align(align = "center",
             columns=-c(label.REVENUE_CODE))|>
  cols_label(label.REVENUE_CODE~"")|>
  tab_style(
    style = cell_text(weight = "bold"),
    locations = list(
      cells_column_labels(columns = c(S13)),
      cells_body(columns = c(S13))
    )
  )|>
  cols_label(S13 ~ md("Total"),
             S1311 ~ md("Adm. centrales"),
             S1313 ~ md("Adm. locales"),
             S1314 ~ md("Adm. Sécu. Soc."))

# Principaux PO (Insee) ----

url<-"https://www.insee.fr/fr/statistiques/fichier/8574705/t_3217.xlsx"

curl_download(url,
              destfile = example<-tempfile("data",fileext = ".xlsx"))

data<-read_xlsx(example)|>
  select(-c(1))|>
  filter(row_number()<100)|>
  filter(row_number()>1)|>
  row_to_names(row_number = 1)|>
  rename(var=1)|>
  select(var,last_col())|>
  rename(recettes=2)|>
  filter(!is.na(var))|>
  arrange(-recettes)|>
  filter(row_number()>1)|>
  filter(row_number()!=2)|>
  filter(row_number()!=4)|>
  filter(row_number()!=5)|>
  filter(row_number()!=6)|>
  filter(row_number()!=6)|>
  filter(row_number()!=8)|>
  filter(row_number()!=8)|>
  filter(row_number()<=10)

test<-read_xlsx(example)|>
  select(last_col())|>
  filter(row_number()==107)|>
  pull()

round(sum(data$recettes)/test*100,digits=0)

data|>
  gt()|>
  cols_label(recettes=md("Recettes (en mds.)"),
             var="")|>
  fmt_number(decimals=0)|>
  cols_align(columns=c("recettes"),
             align="center")|>
  tab_source_note(source_note="Source: Insee")|>
  tab_header(title="Les 10 principaux prélèvements obligatoires (2024)")


# Destination des PO (OCDE) ----

source<-"OECD"
base<-"DSD_REV_COMP_GLOBAL@DF_RSGLOBAL" 


list_countries<-c(
  "AUT","BEL","CAN","CHE","DEU","DNK","ESP","FIN","FRA","GBR","GRC",
  "IRE","ITA","JPN","KOR","LUX","MEX","NLD",
  "PRT","SWE","TUR","USA")

url_countries<-paste0(list_countries,collapse ="+")

url_variables<-"TAX_REV.S13+S1314._T._T.PT_B1GQ.A"

filter <- paste0(url_countries,".",url_variables)

data <- rdb(source,base,mask = filter)|>
  filter(original_period==max(original_period))|>
  select(REF_AREA,original_value,`Reference area`,SECTOR)|>
  mutate(original_value=as.numeric(original_value))|>
  pivot_wider(names_from="SECTOR",
              values_from = "original_value")|>
  mutate(Autres=S13-S1314)|>
  rename(Total=S13,
         `Sécurité Sociale`=S1314)|>
  mutate(REF_AREA=fct_reorder(as.factor(REF_AREA),-Total))

data|>
  select(REF_AREA,`Sécurité Sociale`,Autres)|>
  pivot_longer(cols=-c(REF_AREA),
               names_to="var",
               values_to = "val")|>
  ggplot()+
  aes(x=REF_AREA,
      y=val,
      fill=var)|>
  geom_col()+
  theme_minimal()+
  labs(title="Taux de prélèvements obligatoires (2022)",
       subtitle="En pts. de PIB",
       x=NULL,
       y=NULL,
       fill=NULL,
       caption="Source: OCDE")+
  theme(legend.position = "bottom")+
  geom_hline(yintercept=0,
             alpha=0.1)+ 
  annotate(geom="text", x=data$REF_AREA, y=data$Total+3,
           label=(round(data$Total,digits=0)))

