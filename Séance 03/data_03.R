library(rdbnomics)
library(OECD)
library(insee)
library(tidyverse)
library(readxl)
library(curl)
library(janitor)
library(gt)

calcul_part<-function(x){
  part=100*(x/sum(x))
  return(part)
}

# Graphique 1 - Dépenses et recettes publiques en pts de PIB (Insee) ----

url<-'https://www.insee.fr/fr/statistiques/fichier/8574705/t_3201.xlsx'

curl_download(url,
              destfile = example<-tempfile("data",fileext = ".xlsx"))

data_fipu<-read_xlsx(example)|>
  filter(row_number() %in% c(2,26,57,65))|>
  select(-c(1,2))|>
  janitor::row_to_names(1)|>
  mutate(var=c("Dépenses","Recettes","Solde"))|>
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

data<-merge(data_fipu,data_pib,by="date")|>
  mutate(values=100*(values/PIB))|>
  select(date,var,values)|>
  mutate(date=as.integer(date))


data|>
  ggplot()+
  aes(x=date,
      y=values,
      color=var)+
  geom_line()+
  theme_minimal()+
  labs(title="Dépeneses et recettes publiques totales",
       subtitle="En pts. de PIB",
       x=NULL,
       y=NULL,
       color=NULL,
       caption = "Source: Insee")+
  geom_hline(yintercept = 0,alpha=0.2)+
  theme(legend.position="bottom")


# Graphique 2 - Déflateurs (Insee) ----


url<-"https://www.insee.fr/fr/statistiques/fichier/8574657/T_1101_1103.xlsx"
curl_download(url,
              destfile = example<-tempfile("data",fileext = ".xlsx"))

data_def<-read_xlsx(example,sheet="T_1103 en niveau")|>
  select(-c(1))|>
  filter(row_number()>2)|>
  janitor::row_to_names(1)|>
  rename(var=1)|>
  filter(row_number()%in% c(3,8,9,13))|>
  pivot_longer(cols=-c(var),
               values_to = "values",
               names_to ="date")|>
  filter(!is.na(var))|>
  filter(!is.na(values))|>
  mutate(values=as.numeric(values),
         date=as.integer(date))|>
  filter(date>=2000)|>
  group_by(var)|>
  arrange(date)|>
  mutate(values=100*(values/values[1]))|>
  ungroup()

data_def|>
  ggplot()+
  aes(x=date,
      y=values,
      color=var)+
  geom_line()+
  theme_minimal()+
  labs(x=NULL,
       y="100=2000",
       color=NULL,
       title="Déflateur de différentes composantes du PIB",
       caption= "Source: Insee")+
  theme(legend.position="bottom")+guides(color=guide_legend(nrow=2,byrow=TRUE))


# Graphique 3 Charge d'intérêts (en pts de PIB) ---- 


url<-'https://www.insee.fr/fr/statistiques/fichier/8574705/t_3201.xlsx'

curl_download(url,
              destfile = example<-tempfile("data",fileext = ".xlsx"))

data_charge<-read_xlsx(example)|>
  select(-c(1))|>
  filter(row_number() %in% c(2,11))|>
  janitor::row_to_names(1)|>
  rename(var=1)|>
  pivot_longer(cols=-c(var),
               names_to = "date",
               values_to="obs")



data_pib<-insee::get_idbank_list("CNA-2020-PIB")|>
  filter(OPERATION %in% "PIB")|>
  filter(PRIX_REF %in% "VAL")|>
  pull(idbank)|>
  get_insee_idbank()|>
  select(TIME_PERIOD,OBS_VALUE)|>
  rename(date=TIME_PERIOD,
         PIB=OBS_VALUE)|>
  mutate(PIB=PIB/1000)



data_graph<-merge(data_charge,data_pib,by="date")|>
  mutate(val=100*(obs/PIB))|>
  select(date,val)


data_graph|>
  ggplot()+
  aes(x=as.integer(date),
      y=val)+
  geom_line()+
  theme_minimal()+
  labs(title="Charge d'intérêts",
       subtitle="en pts. de PIB",
       x=NULL,
       y=NULL,
       caption="Source: Insee")


# Graphique 4 Charge d'intérêts réelle ---- 


url<-"https://www.insee.fr/fr/statistiques/fichier/8574657/T_1101_1103.xlsx"
curl_download(url,
              destfile = example<-tempfile("data",fileext = ".xlsx"))

data_def<-read_xlsx(example,sheet="T_1103 en niveau")|>
  select(-c(1))|>
  filter(row_number()>2)|>
  janitor::row_to_names(1)|>
  rename(var=1)|>
  filter(row_number()%in% c(8))|>
  pivot_longer(cols=-c(var),
               names_to = "date",
               values_to="def")|>
  select(date,def)

data_graph<-merge(data_charge,data_def,by="date")|>
  mutate(def=as.numeric(def))|>
  mutate(Volume=100*(obs/def))|>
  rename(Valeur=obs)|>
  select(date,Volume,Valeur)|>
  pivot_longer(cols=-c(date),
                       names_to="var",
                       values_to="val")

data_graph|>
  ggplot()+
  aes(x=as.integer(date),
      y=val,
      color=var)+
  geom_line()+
  theme_minimal()+
  labs(title="Charge d'intérêts réelle",
       subtitle="Déflatée par le prix de la consommation",
       x=NULL,
       y=NULL,
       color=NULL)+
  theme(legend.position="bottom")

data_graph|>
  mutate(date=as.integer(date))|>
  filter(date %in% c(2005,max(date)))|>
  group_by(var)|>
  arrange(date)|>
  mutate(val=100*(val/val[1]-1))|>
  ungroup()|>
  filter(date==max(date))

# Graphique 5 Solde public et solde primaire (en pts de PIB) ---- 

url<-'https://www.insee.fr/fr/statistiques/fichier/8574705/t_3201.xlsx'

curl_download(url,
              destfile = example<-tempfile("data",fileext = ".xlsx"))

data_solde<-read_xlsx(example)|>
  select(-c(1))|>
  filter(row_number() %in% c(2,11,65))|>
  janitor::row_to_names(1)|>
  rename(var=1)|>
  mutate(var2=c("D41","B9NF"))|>
  select(-c(var))|>
  pivot_longer(cols=-c(var2),
               names_to = "date",
               values_to="obs")|>
  pivot_wider(names_from="var2",
              values_from="obs")|>
  mutate(`Solde primaire`=B9NF+D41)|>
  rename(`Solde`=B9NF)|>
  select(-c(D41))|>
  pivot_longer(cols=-c(date),
               values_to="obs",
               names_to="var")



data_pib<-insee::get_idbank_list("CNA-2020-PIB")|>
  filter(OPERATION %in% "PIB")|>
  filter(PRIX_REF %in% "VAL")|>
  pull(idbank)|>
  get_insee_idbank()|>
  select(TIME_PERIOD,OBS_VALUE)|>
  rename(date=TIME_PERIOD,
         PIB=OBS_VALUE)|>
  mutate(PIB=PIB/1000)



data_graph<-merge(data_solde,data_pib,by="date")|>
  mutate(val=100*(obs/PIB))|>
  select(date,var,val)

data_graph|>
  ggplot()+
  aes(x=as.integer(date),
      y=val,
      color=var)+
  geom_line()+
  geom_hline(yintercept=0,alpha=0.2)+
  theme_minimal()+
  theme(legend.position="bottom")+
  labs(title="Solde public et solde primaire",
       subtitle="En pts. de PIB",
       x=NULL,
       y=NULL,
       color=NULL,
       caption="Source: Insee")

# Graphique 6 Dépenses publiques et cycle économique ---- 

url<-'https://www.insee.fr/fr/statistiques/fichier/8574705/t_3201.xlsx'

curl_download(url,
              destfile = example<-tempfile("data",fileext = ".xlsx"))

data_dep<-read_xlsx(example)|>
  select(-c(1))|>
  # filter(row_number() %in% c(2,4,11,14,22,26))|>
  filter(row_number() %in% c(2,4,11,14,22))|>
  janitor::row_to_names(1)|>
  rename(var=1)|>
  pivot_longer(cols=-c(var),
               names_to = "date",
               values_to="obs")


data_pib<-insee::get_idbank_list("CNA-2020-PIB")|>
  filter(OPERATION %in% "PIB")|>
  filter(PRIX_REF %in% "VAL")|>
  pull(idbank)|>
  get_insee_idbank()|>
  select(TIME_PERIOD,OBS_VALUE)|>
  rename(date=TIME_PERIOD,
         PIB=OBS_VALUE)|>
  mutate(PIB=PIB/1000)



data_graph<-merge(data_dep,data_pib,by="date")|>
  mutate(val=100*(obs/PIB))|>
  select(date,var,val)|>
  mutate(date=as.integer(date))|>
  filter(date>=1970)

pics <-c(1974,1980,1992,2008,2019)
creux<-c(1975,1981,1993,2009,2020)


data_graph|>
  ggplot()+
  aes(x=as.integer(date),
      y=val,
      color=var)+
  geom_line()+
  geom_hline(yintercept=0,alpha=0.2)+
  theme_minimal()+
  theme(legend.position="bottom")+
  labs(title="Dépenses publiques et cycle économique",
       subtitle="En pts. de PIB",
       x=NULL,
       y=NULL,
       color=NULL,
       caption="Source: Insee, Comité de datation des cycles économiques (AFSE)")+ 
  annotate("rect", xmin = pics, xmax = creux, ymin = 0, ymax = 40,
                                          alpha = .2)


# Graphique 7 Recettes publiques et cycle économique ---- 

url<-'https://www.insee.fr/fr/statistiques/fichier/8574705/t_3201.xlsx'

curl_download(url,
              destfile = example<-tempfile("data",fileext = ".xlsx"))

data_rec<-read_xlsx(example)|>
  select(-c(1))|>
  # filter(row_number() %in% c(2,4,11,14,22,26))|>
  filter(row_number() %in% c(2,45,46,49))|>
  janitor::row_to_names(1)|>
  rename(var=1)|>
  pivot_longer(cols=-c(var),
               names_to = "date",
               values_to="obs")


data_pib<-insee::get_idbank_list("CNA-2020-PIB")|>
  filter(OPERATION %in% "PIB")|>
  filter(PRIX_REF %in% "VAL")|>
  pull(idbank)|>
  get_insee_idbank()|>
  select(TIME_PERIOD,OBS_VALUE)|>
  rename(date=TIME_PERIOD,
         PIB=OBS_VALUE)|>
  mutate(PIB=PIB/1000)



data_graph<-merge(data_rec,data_pib,by="date")|>
  mutate(val=100*(obs/PIB))|>
  select(date,var,val)|>
  mutate(date=as.integer(date))|>
  filter(date>=1970)

pics <-c(1974,1980,1992,2008,2019)
creux<-c(1975,1981,1993,2009,2020)


data_graph|>
  ggplot()+
  aes(x=as.integer(date),
      y=val,
      color=var)+
  geom_line()+
  geom_hline(yintercept=0,alpha=0.2)+
  theme_minimal()+
  theme(legend.position="bottom")+
  labs(title="Recettes publiques et cycle économique",
       subtitle="En pts. de PIB",
       x=NULL,
       y=NULL,
       color=NULL,
       caption="Source: Insee, Comité de datation des cycles économiques (AFSE)")+ 
  annotate("rect", xmin = pics, xmax = creux, ymin = 0, ymax = 25,
           alpha = .2)



# Graphique 8 Solde public primaire et cycle économique (en pts de PIB) ---- 

url<-'https://www.insee.fr/fr/statistiques/fichier/8574705/t_3201.xlsx'

curl_download(url,
              destfile = example<-tempfile("data",fileext = ".xlsx"))

data_solde<-read_xlsx(example)|>
  select(-c(1))|>
  filter(row_number() %in% c(2,11,65))|>
  janitor::row_to_names(1)|>
  rename(var=1)|>
  mutate(var2=c("D41","B9NF"))|>
  select(-c(var))|>
  pivot_longer(cols=-c(var2),
               names_to = "date",
               values_to="obs")|>
  pivot_wider(names_from="var2",
              values_from="obs")|>
  mutate(`Solde primaire`=B9NF+D41)|>
  rename(`Solde`=B9NF)|>
  select(-c(D41))|>
  pivot_longer(cols=-c(date),
               values_to="obs",
               names_to="var")



data_pib<-insee::get_idbank_list("CNA-2020-PIB")|>
  filter(OPERATION %in% "PIB")|>
  filter(PRIX_REF %in% "VAL")|>
  pull(idbank)|>
  get_insee_idbank()|>
  select(TIME_PERIOD,OBS_VALUE)|>
  rename(date=TIME_PERIOD,
         PIB=OBS_VALUE)|>
  mutate(PIB=PIB/1000)



data_graph<-merge(data_solde,data_pib,by="date")|>
  mutate(val=100*(obs/PIB))|>
  select(date,var,val)

pics <-c(1974,1980,1992,2008,2019)
creux<-c(1975,1981,1993,2009,2020)



data_graph|>
  filter(var!="Solde")|>
  filter(as.integer(date)>=1970)|>
  ggplot()+
  aes(x=as.integer(date),
      y=val,
      color=var)+
  geom_line()+
  geom_hline(yintercept=0,alpha=0.2)+
  theme_minimal()+
  theme(legend.position="bottom")+
  labs(title="Solde public primaire",
       subtitle="En pts. de PIB",
       x=NULL,
       y=NULL,
       color=NULL,
       caption="Source: Insee, Comité de datation des cycles économiques (AFSE)")+ 
  annotate("rect", xmin = pics, xmax = creux, ymin = -10, ymax = 3,
           alpha = .2)

# Graphique 9 - Solde public, solde structurel, solde corrigé du cycle ---- 

vars<-c("AMECO/UBLGAP/FRA.1.0.319.0.UBLGAP","AMECO/UBLGAPS/FRA.1.0.319.0.UBLGAPS","AMECO/UBLG/FRA.1.0.319.0.UBLG",
        "AMECO/UBLGA/FRA.1.0.319.0.UBLGA")

label_data<-data.frame(
  dataset_code=c("UBLGAP","UBLGAPS","UBLG","UBLGA"),
  dataset_label=c("Solde corrigé du cycle",
                  "Solde structurel",
                  "Solde",
                  "Solde corrigé du cycle (PIB tendanciel)")
)

data_graph <- rdb(ids = vars)|>
  select(dataset_code,original_period,original_value)|>
  merge(label_data,by="dataset_code")|>
  mutate(original_value=as.numeric(if_else(original_value=="NA",NA,original_value)),
         original_period=as.integer(original_period))

data_graph|>
  filter(original_period>=2010)|>
  filter(original_period<=2024)|>
  ggplot()+
  aes(x=original_period,
      y=original_value,
      color=dataset_label)+
  geom_line()+
  theme_minimal()+
  theme(legend.position="bottom")+
  labs(title="Solde public, solde corrigé du cycle et solde structurel",
       subtitle="En pts de PIB (potentiel)",
       x=NULL,
       y=NULL,
       color=NULL)+
  geom_hline(yintercept=0,alpha=0.2)

# Graphique 10 - Solde public, solde structurel, solde corrigé du cycle (PIB tendanciel) ---- 


data_graph|>
  filter(dataset_code %in% c("UBLG","UBLGA"))|>
  filter(original_period>=1970)|>
  filter(original_period<=2024)|>
  ggplot()+
  aes(x=original_period,
      y=original_value,
      color=dataset_label)+
  geom_line()+
  theme_minimal()+
  theme(legend.position="bottom")+
  labs(title="Solde public, solde corrigé du cycle et solde structurel",
       subtitle="En pts de PIB (potentiel)",
       x=NULL,
       y=NULL,
       color=NULL)+
  geom_hline(yintercept=0,alpha=0.2)+ 
  annotate("rect", xmin = pics, xmax = creux, ymin = -10, ymax = 3,
           alpha = .2)


# Graphique 11 et 12 - Solde public, Variation patrimoine ----  

url<-'https://www.insee.fr/fr/statistiques/fichier/8574705/t_3201.xlsx'

curl_download(url,
              destfile = example<-tempfile("data",fileext = ".xlsx"))

data_solde<-read_xlsx(example)|>
  select(-c(1))|>
  filter(row_number() %in% c(2,65))|>
  janitor::row_to_names(1)|>
  rename(var=1)|>
  pivot_longer(cols=-c(var),
               names_to = "date",
               values_to="Solde")|>
  select(-c(var))


url<-"https://www.insee.fr/fr/statistiques/fichier/8068638/T_8204.xlsx"

curl_download(url,
              destfile = example<-tempfile("data",fileext = ".xlsx"))


data_pat<-read_xlsx(example,sheet="T_8204")|>
  select(-c(1))|>
  filter(row_number() %in% c(3,62))|>
  janitor::row_to_names(1)|>
  rename(var=1)|>
  pivot_longer(cols=-c(var),
               names_to = "date",
               values_to="vn")|>
  mutate(date=as.integer(date),
         vn=as.numeric(vn))|>
  filter(!is.na(vn))|>
  arrange(date)|>
  mutate(dvn=vn-lag(vn))|>
  select(-c(var))

data_pib<-insee::get_idbank_list("CNA-2020-PIB")|>
  filter(OPERATION %in% "PIB")|>
  filter(PRIX_REF %in% "VAL")|>
  pull(idbank)|>
  get_insee_idbank()|>
  select(TIME_PERIOD,OBS_VALUE)|>
  rename(date=(TIME_PERIOD),
         PIB=OBS_VALUE)|>
  mutate(PIB=PIB/1000,
         date=as.integer(date))

data_graph<-merge(data_pat,data_pib,by="date")|>
  merge(data_solde,by="date")|>
  mutate(vn=100*(vn/PIB),
         dvn=100*(dvn/PIB),
         Solde=100*(Solde/PIB))|>
  select(date,vn,dvn,Solde)|>
  pivot_longer(cols=-c(date),
               names_to = "var",
               values_to="val")

data_graph|>
  filter(var!="vn")|>
  ggplot()+
  aes(x=date,
      y=val,
      color=var)+
  geom_line()+
  theme_minimal()+
  labs(title="Solde public et variation du patrimoine net",
       subtitle="En pts. de PIB",
       x=NULL,
       y=NULL,
       color=NULL,
       caption="Source: Insee")+
  theme(legend.position = "bottom")

data_graph|>
  filter(var=="vn")|>
  ggplot()+
  aes(x=date,
      y=val,
      color=var)+
  geom_line()+
  theme_minimal()+
  labs(title="Patrimoine net des APU",
       subtitle="En pts. de PIB",
       x=NULL,
       y=NULL,
       color=NULL,
       caption="Source: Insee")+
  theme(legend.position = "none")
