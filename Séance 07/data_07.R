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

# Comparaison européenne ----

url<-"https://ec.europa.eu/taxation_customs/document/download/1c4386fc-1086-45ba-9df4-65db9d25d28d_en"

curl_download(url, destfile = example<-tempfile("data",fileext = ".xlsx"))

data_graph<-read_xlsx(example,sheet="Table 53")|>
  filter(row_number()<33)|>
  filter(row_number()>1)|>
  row_to_names(row_number = 1)|>
  rename(country=1)|>
  select(country,starts_with("2"))|>
  select(country,tax=last_col())|>
  mutate(tax=as.numeric(tax))|>
  filter(!is.na(tax))|>
  filter(country!="EU-27")

data_graph|>
  ggplot()+
  aes(x=fct_reorder(country,-tax),
      y=tax,
      fill=if_else(country=="France","FRA",if_else(country=="EA-20","EA-20","OTHER")))+
  geom_col()+
  scale_fill_manual(values = c("orange","blue4","grey"))+
  theme_minimal()+
  theme(axis.text.x = element_text(angle = 90))+
  theme(legend.position="none")+
  labs(title="Impôts sur le capital",
       subtitle="En pts. de PIB",
       caption="Source: Commission Européenne",
       y=NULL,
       x=NULL)


# Effets de comportement ----

url<-"https://www.insee.fr/fr/statistiques/fichier/8574695/T_7401.xlsx"

curl_download(url, destfile = example<-tempfile("data",fileext = ".xlsx"))


data_graph<-read_xlsx(example,sheet=2)|>
  filter(row_number() %in% c(3,45,54))|>
  select(-c(1))|>
  row_to_names(row_number = 1)|>
  rename(var=1)|>
  pivot_longer(cols=-c(var),names_to="year",
               values_to="val")|>
  mutate(val=as.numeric(val))|>
  pivot_wider(names_from="var",
              values_from = "val")|>
  mutate(Dividendes=`Revenus distribués des sociétés`/`Solde brut des revenus primaires`*100)|>
  select(year,Dividendes)|>
  mutate(year=as.integer(year))|>
  filter(year>=2010)|>
  filter(year<=2019)

data_graph|>
  ggplot()+
  aes(x=year,
      y=Dividendes)+
  geom_col(fill="lavender",
           color="blue")+
  theme_minimal()+
  scale_x_continuous(breaks = seq(2010, 2019, by = 1))+
  labs(title="Dividendes reçus par les ménages",
       subtitle="En pts. de revenu primaire",
       x=NULL,
       y=NULL,
       caption="Source: Insee")+ 
  geom_hline(yintercept=0,linewidth=0.1)+
  geom_vline(xintercept = c(2012.5,2017.5),linetype="dashed")

