# Importation et nettoyage des données EESSAQ fournies par l'IRDA ----
# Autrice : Jeanne Durivage
# Période : Version octobre 2024
# Université Laval, laboratoires de Marie-Élise Samson et de Thiago Gumiere 


setwd("~/Maitrise/EESSAQ")
library(readxl)
library(tidyverse)
rm(list=ls())


# Importation des fichiers ----

META_variables <- read_excel("Donnees_EESSAQ/DICO_EESSAQ_JD.xlsx")
  # MÉTADONNÉES : Définition des variables des autres tables excel
META_physicochimie <- read_excel("Donnees_EESSAQ/ListVarsPhysicoChimiques_EESSAQ.xlsx")
  # MÉTADONNÉES : Description détaillée de la provenance des variables en physichimie
rdt_raw <- read_excel("Donnees_EESSAQ/Rendement_EESSAQ.xlsx")
  # Mesure détaillée du rendement.
sondage_raw <- read_excel("Donnees_EESSAQ/SiteAnnee_EESSAQ.xlsx")
  # Sondage agronomique envoyé aux producteurs + cote STIR + indice de compaction
pedo_raw <- read_excel("Donnees_EESSAQ/Sites.xlsx")
  # Données pédologiques
physique_raw <- read_excel("Donnees_EESSAQ/HZN_Physique_EESSAQ_donnees_sup.xlsx",
                           na = "NA")
  # Résultats des tests de physique des sols
micro_raw <- read_excel("Donnees_EESSAQ/HZN_Microbiome_EESSAQ.xlsx")
  # Indices de microbio + qPCR + agit comme référence métadonnées pour la table d'ASV
chimie_raw <- read_excel("Donnees_EESSAQ/HZN_Chimie_EESSAQ.xlsx")
  # Résultats des tests de chimie et biologie-fertilité des sols
climat_raw <- read_tsv("Donnees_EESSAQ/Meteo_EESSAQ.tsv", show_col_types = FALSE)
  # Données climatiques


# # Datasets de microbio : fichiers très lourds
# ASV_table_16S <- read_csv("Donnees_EESSAQ/listBiome_ASV_EMP.csv") 
# '820700ListTaxon_BITS.tsv' # ITS table d'attribution taxonomique
# '820700ListTaxon_EMP.tsv' # 16S table d'attribution taxonomique

# Visualisation des variables, choix de variables ----
## Stratégie : Dans un premier temps, identifier les variables à conserver. 
          ##   Dans un deuxième temps, les sélectionner dans les jeux de données 
          ##   et faire les transformations nécessaires

# Observation des variables dans les jeux de données 
str(chimie_raw)
str(micro_raw)
str(pedo_raw)
str(physique_raw)
str(rdt_raw)
str(sondage_raw)
str(climat_raw)

vars_dispo <- c(
  colnames(chimie_raw),
  colnames(micro_raw),
  colnames(pedo_raw),
  colnames(physique_raw),
  colnames(rdt_raw),
  colnames(sondage_raw),
  colnames(climat_raw)
) 
provenance <- c(
  rep("chimie", ncol(chimie_raw)),
  rep("micro", ncol(micro_raw)),
  rep("pedologie", ncol(pedo_raw)),
  rep("physique", ncol(physique_raw)),
  rep("rdt", ncol(rdt_raw)),
  rep("sondage", ncol(sondage_raw)),
  rep("climat", ncol(climat_raw))
  
)

df <- data.frame(matrix(provenance, 1, length(vars_dispo), byrow = TRUE)) 
colnames(df) <- vars_dispo 
df2 <- t(df)
write.csv2(df2, 'Analyses/1_gestion_nettoyage/variables_fournies_par_lIRDA.csv')
rm(df, df2)

## Observation détaillées de certaines variables
unique(sondage_raw$CulturePrincipale) # Maïs grain est écrit de 2 façons, cultures manquantes
unique(sondage_raw$CultureCouverture)
unique(sondage_raw$CC_Implantation) # Ne sera pas retenu
unique(sondage_raw$CC_Autres)
unique(sondage_raw$CC_Graminees)
unique(sondage_raw$CC_Legumineuses)
unique(sondage_raw$CC_Cruciferes)
unique(sondage_raw$CC_Melange)
unique(sondage_raw$AutreCulture) # Ne sera pas retenu
unique(sondage_raw$Details_AEM_1) # Ne sera pas retenu
unique(sondage_raw$TypeAEO_App_1) # Retenu
unique(sondage_raw$SourceAEO_App_1) 
unique(sondage_raw$SourceAEO_App_2)
unique(sondage_raw$SourceAEO_App_3) # Retenu. Les catégories semblent ok.
unique(sondage_raw$Periode_App_1) # Ne sera probablement pas utile pcq peu précis
unique(sondage_raw$Periode_AEM_1) # Ne sera probablement pas utile pcq peu précis


# Identification des variables retenues
variables_retenues <- c(
  
  # Identifiants
  "Horizon_id", "PE_id", "Site_id", "Horizon", "PE",
  
  'DadteEchantillonnage',
  
  # Variables en chimies
    "pH_Eau", 
    
    "P_Meh3", "K_Meh3", "Ca_Meh3", "Mg_Meh3", "Al_Meh3", "Na_Meh3",                   
    "Cu_Meh3", "Fe_Meh3", "Mn_Meh3", "Zn_Meh3",  "Mo_Meh3", "S_Meh3",   
    
    "AzoteTotal",  
    "N_NH4_0jours", "N_NO3_0jours", "N_NH4_14jours", "N_NO3_14jours", # a voir
    "N_mineralise", "N_organique",
    "CO2_0_3jours", "CO2_4_14jours", "CO2_0_14jours", # on garde les 3 pour le moment... a voir
    "CarboneTotal", "CarboneActif",  
    "Rapport_CN", 
  # "pH_SMP", "CEC_Calcul", "ESP", "CarboneActif/CarboneTotal", "MOS_PAF",                  
    
    # Variables en physique
  'Sable', 'Limon', 'Argile', 
  'MVA3', 'ConductiviteHydraulique', 
  'TEEVolumetriqueChamp', 'TEAirVolumetriqueChamp', 
  'TEEVolumetriqueSaturation', 'Macroporosite', 
  'Agregats_4_8mm', 'Agregats_2_4mm', 'Agregats_1_2mm', 'Agregats_0,5_1mm', 
  'Agregats_0,25_0,5mm', 'ValeurVESS',
  # 'CodeTexture', 'GroupeTextSISCan', 'SommeGravierSable', 'DiamMoyenPondere',
  # 'Debit',  'VESS',
  
    # Variables en pédologie
  'TypeSite', 'Serie', 'EESSAQGroup',
  'MateriauParental', 'Sortesol', 'ClasseDrainage', 'GroupeHydrologique', 
  'ModeDepot1',
  'SousGroupePedo', 'SousRegionPedo', 'OrdrePedo', 'ModeleTerrain', 
  'NaturePetrographique', 
  'ProvincePedologique',
  'RegionPedologique', 'SousRegionPedologique',
  # 'FacteurAlpha', 'CodeCANSIS',
  
    # Variables en microbiologie
  # 'Shannon_Emp', 'Pielou_Emp', 'Shannon_Bits', 'Pielou_Bits', 
  'Features_Bits', 'Features_Emp',
  'BT_AmplifUnits', 'CT_AmplifUnits',
  
    # Variables agronomiques
  'Annee', 'AnneEchantillonage',
  'CulturePrincipale', 'CultureCouverture', 'Culture_BDPPAD',
  'NbAppFongicides', 'NbAppInsecticides', 'NbAppHerbicides', 'SemenceEnrobes',
  'Nb_AEM', 'Nb_AEO', 'SourceAEO_App_1', 'SourceAEO_App_2', 'SourceAEO_App_3',
  'Nb_Chaux',
  # 'Date_AEM_1', 'Date_AEM_2', 'Date_AEM_3', 'Date_AEM_4', 'Date_AEM_5',
  # 'DateApp_1', 'DateApp_2', 'DateApp_3', 
  # 'Periode_AEM_1', , 'Periode_AEM_2',  'Periode_AEM_3', 'Periode_AEM_4', 'Periode_AEM_5', 
  # 'Periode_App_1','Periode_App_2', 'Periode_App_3','Periode_AppChaux_1',
  'TypeAEO_App_1', 'TypeAEO_App_2', 'TypeAEO_App_3',
  'CC_Autres', 'CC_Graminees','CC_Legumineuses','CC_Cruciferes','CC_Melange',
  'STIR_Calcule', 'IndiceCompaction', 'IndiceBiodiversite',
  
    # Variables rendement
  'Annee', 'Culture', 
  'RendementSec', 'PoidSpecifique', 'PoidSpecifiqueGrade',
  
  # Variables de climat
  'PPT', 'Gini', 'GDD'
  )


# Sélection des variables retenues et corrections aux jeux de données ----


     ## Chimie ----
        chimie1 <- chimie_raw %>% 
          select(any_of(variables_retenues)) %>% 
          mutate(across(where(is.character), as.factor)) %>% 
          select(-TypeSite)
        str(chimie1)
        
     ## Physique ----
        physique1 <- physique_raw %>% 
          select( any_of(variables_retenues) ) %>% 
          select(-TypeSite, -DadteEchantillonnage) %>% 
          rename( Agregats_0.5_1mm = `Agregats_0,5_1mm`,
                  Agregats_0.25_0.5mm = `Agregats_0,25_0,5mm`,
                  TEAirEch = TEAirVolumetriqueChamp,
                  TEEauEch = TEEVolumetriqueChamp,
                  PorositeTotale = TEEVolumetriqueSaturation) %>% 
          mutate(MVA3 = as.numeric(MVA3),                
                  ConductiviteHydraulique = as.numeric(ConductiviteHydraulique),
                 TEEauEch = as.numeric(TEEauEch),
                 TEAirEch = as.numeric(TEAirEch),
                 PorositeTotale = as.numeric(PorositeTotale),
                 Macroporosite = as.numeric(Macroporosite),
                 Agregats_4_8mm = as.numeric(Agregats_4_8mm),
                 Agregats_2_4mm = as.numeric(Agregats_2_4mm),
                 Agregats_1_2mm = as.numeric(Agregats_1_2mm),
                 Agregats_0.5_1mm = as.numeric(Agregats_0.5_1mm),
                 Agregats_0.25_0.5mm = as.numeric(Agregats_0.25_0.5mm),
                 ValeurVESS = as.numeric(ValeurVESS))

        str(physique1)
           # OK
        

     ## Pédologie ----
        pedo1 <- pedo_raw %>%
          select(any_of(variables_retenues))  %>%
          mutate(across(where(is.character), as.factor))
        str(pedo1)
        
        unique(pedo1$RegionPedologique)
        unique(pedo1$SousRegionPedologique)
        unique(pedo1$NaturePetrographique)
              # Il y a la valeur '-'. Regardons plus loin...
           ggplot(pedo1, aes(x=NaturePetrographique)) +
             geom_bar() +
             theme(axis.text.x = element_text(angle=45, hjust=1))
                ## La plupart des sites ont la valeur '-'.
                ## On peut garder cette var pour l'instant, mais on va
                ## la convertir en NA parce que c'est ce que '-' représente.
        
        # Certaines valeurs ont des noms trop longs
        pedo2 <- pedo1 %>% 
          mutate(
            
          NaturePetrographique = ifelse(NaturePetrographique == '-', NA, as.character(NaturePetrographique)),
          
          MateriauParentalAcr = as.factor(case_when(
            MateriauParental == "Matériaux sableux à squelettique" ~ 'SableuxSquelettique',
            MateriauParental == "Matériaux loameux" ~ 'Loameux',
            MateriauParental == "Matériaux glaciaires (tills)" ~ 'Till',
            MateriauParental == "Matériaux argileux" ~ 'Argileux',
            MateriauParental == "Matériaux organiques" ~ 'Organique')),
          
          RegionPedoNo = as.factor(case_when(
            RegionPedologique == "(A1) Plaine de Montréal (<60 m)" ~ "A1",
            RegionPedologique == "(A3) Plaine du lac St-Jean (120-180 m)" ~ "A3",
            RegionPedologique == "(B6) Monts Notre-Dame (300-1300 m)" ~ "B6",
            RegionPedologique == "(A4) Hautes-terrasses du St-Laurent (60-180 m)" ~ "A4",
            RegionPedologique == "(B7) Bas-plateau de la baie des Chaleurs" ~ "B7",
            RegionPedologique == "(B4) Bas plateau de Compton (180-700 m)" ~ "B4",
            RegionPedologique == "(A2) Plaine littorale et les îles du Saint-Laurent (<140 m)" ~ "A2",
            RegionPedologique == "(B2) Basses et moyennes collines des Appalaches orientales (180-500 m)" ~ "B2",
            RegionPedologique == "(B3) Basses et moyennes collines des Appalaches occidentales (180-500 m)" ~ "B3",
            RegionPedologique == "(D1) Plaine de l'Abitibi (125-525 m)" ~ "D1",
            RegionPedologique == "(C1) Hautes-terres des Laurentides (180-600 m)" ~ "C1",
            RegionPedologique == "(B1) Monts Sutton (300-1000 m)" ~ "B1" )),
          
          SousRegionPedoNo = as.factor(case_when(
            SousRegionPedologique == "(A) BASSES-TERRES DU ST-LAURENT (<180M)" ~ 'A',
            SousRegionPedologique == "(B) LES APPALACHES (180-1300 m)" ~ 'B',
            SousRegionPedologique == "(D) LES BASSES-TERRES DE L'ABITIBI ET DE LA BAIE DE JAMES (30-525M)" ~ 'D',
            SousRegionPedologique == "(C) LES LAURENTIDES (180-1200 m)" ~ 'C' )) ) %>% 
          
          select(-c(MateriauParental, SousRegionPedologique, RegionPedologique))
         
        # Mettre en ordre les niveaux de facteurs
        levels(pedo2$EESSAQGroup) <- c(1:7, '8-A', '8-B', 9:25)
           x <- levels(pedo2$ClasseDrainage)
        levels(pedo2$ClasseDrainage) <- c(x[10],x[9],x[1:2],x[8],x[4],x[3],x[6],x[5],x[7],x[11])
        
        # Vérifier structure obtenue
        str(pedo2)
           ## OK!
        
        
        
        
        # Créer métadonnées pour les acronymes et no de variables créés.
        pedo_description_valeurs <-
          matrix(
          c("MateriauParentalAcr", "Matériaux sableux à squelettique", 'SableuxSquelettique',
        "MateriauParentalAcr", "Matériaux loameux", 'Loameux',
        "MateriauParentalAcr", "Matériaux glaciaires (tills)", 'Till',
        "MateriauParentalAcr", "Matériaux argileux", 'Argileux',
        "MateriauParentalAcr", "Matériaux organiques", 'Organique',
        "RegionPedoNo", "(A1) Plaine de Montréal (<60 m)", "A1",
        "RegionPedoNo", "(A3) Plaine du lac St-Jean (120-180 m)", "A3",
        "RegionPedoNo", "(B6) Monts Notre-Dame (300-1300 m)", "B6",
        "RegionPedoNo", "(A4) Hautes-terrasses du St-Laurent (60-180 m)", "A4",
        "RegionPedoNo", "(B7) Bas-plateau de la baie des Chaleurs", "B7",
        "RegionPedoNo", "(B4) Bas plateau de Compton (180-700 m)", "B4",
        "RegionPedoNo", "(A2) Plaine littorale et les îles du Saint-Laurent (<140 m)", "A2",
        "RegionPedoNo", "(B2) Basses et moyennes collines des Appalaches orientales (180-500 m)", "B2",
        "RegionPedoNo", "(B3) Basses et moyennes collines des Appalaches occidentales (180-500 m)", "B3",
        "RegionPedoNo", "(D1) Plaine de l'Abitibi (125-525 m)", "D1",
        "RegionPedoNo", "(C1) Hautes-terres des Laurentides (180-600 m)", "C1",
        "RegionPedoNo", "(B1) Monts Sutton (300-1000 m)", "B1",
        "SousRegionPedoNo", "(A) BASSES-TERRES DU ST-LAURENT (<180M)", 'A',
        "SousRegionPedoNo", "(B) LES APPALACHES (180-1300 m)", 'B',
        "SousRegionPedoNo", "(D) LES BASSES-TERRES DE L'ABITIBI ET DE LA BAIE DE JAMES (30-525M)", 'D',
        "SousRegionPedoNo", "(C) LES LAURENTIDES (180-1200 m)", 'C'),
            byrow=TRUE, 
            ncol = 3) %>% 
          as.data.frame() 
        
          ## Nommer les colonnes
             colnames(pedo_description_valeurs) <- c("Variable", "Description", "Valeur")  
        
          ## Changer l'ordre des colonnes
             pedo_description_valeurs <- pedo_description_valeurs %>% select(Variable, Valeur, Description)
        
        rm(pedo1)
        



     ## Microbiologie ----
        micro1 <- micro_raw %>% 
          select(any_of(variables_retenues))  %>%
          mutate(across(where(is.character), as.factor)) %>% 
          select(-c(TypeSite, Serie, EESSAQGroup))
        str(micro1)
        


     ## Rendements ----
        rdt1 <- rdt_raw %>% 
          select(any_of(variables_retenues)) %>% 
          mutate(Culture = replace(Culture, Culture == 'Maïs grain', 'Maïs-grain'),
                 PoidSpecifique = as.numeric(PoidSpecifique),
                 across(where(is.character), as.factor),
                 Annee = as.factor(Annee)) %>% 
          select(-c(PE, TypeSite))
        
        # Investiger avertissement de NA introduits pour Poids spécifique
        rdt_NAs <-
          rdt1 %>% 
          select(Site_id,Culture, PoidSpecifique, PoidSpecifiqueGrade) %>% 
          filter(if_any(everything(), is.na)) %>% 
          filter(Culture == 'Maïs') %>% 
          group_by(Site_id) %>% 
          summarise(NombrePtEch = n()) %>% 
          print()
           # Seulement le mais a un poids spécifique, donc l'avertissement est normal
           # Par contre, pour le mais, 6 sites ont des PoidSpecifique manquants dans
           # tous leurs points d'échantillonnages (4 éch par site).
        
        unique(rdt1$Culture)
          ## OK
        str(rdt1)
          ## OK
        
        rm(rdt_NAs)

     ## Climat ----
        climat1 <- climat_raw %>% 
          select(any_of(variables_retenues))  %>%
          mutate(across(where(is.character), as.factor)) 
        str(climat1)

     ## ids ----
        ids1 <- chimie_raw %>% 
        select(Horizon_id, PE_id, Site_id) %>% 
        mutate(Horizon = str_split_fixed(Horizon_id, "_", 2)[, 2],
               PE = substr(chimie_raw$PE_id, 8, 8),
               across(where(is.character), as.factor)) %>% 
        full_join(physique_raw %>% 
                    select(Horizon_id, DadteEchantillonnage)) %>% 
        mutate(DateEch = as.Date(DadteEchantillonnage)) %>% 
        select(-DadteEchantillonnage)
        str(ids1)  

     ## Sondage agronomique ----
        sondage1 <- sondage_raw %>% 
          select(any_of(variables_retenues))
        
        sondage1 %>% 
          filter(CulturePrincipale == 'Manquant' ) %>% 
          select(Site_id, CulturePrincipale, Culture_BDPPAD)
        
        sondage2 <- sondage1 %>% 
          mutate(CulturePrincipale = 
                   coalesce(na_if(CulturePrincipale, 'Manquant'), Culture_BDPPAD, 'Manquant'),
                 CulturePrincipale = 
                   replace(CulturePrincipale, 
                           CulturePrincipale == 'Maïs grain', 'Maïs-grain'),
                 
                 across(where(is.character), as.factor)) %>% 
          select(-Culture_BDPPAD)
        
        sondage_stir_na <- sondage2[is.na(sondage2$STIR_Calcule),]
        sondage_stir_na
        sondage_compaction_na <- sondage2[is.na(sondage2$IndiceCompaction),]
        sondage_compaction_na
            ## Valeurs NA lorsqu'aucun travail de sol : on va remplacer avec 0, mais, en plus, certains sites semblent ne pas avoir de réponses au sondage. A gérer plus tard.
        
        sondage3 <- sondage2 %>% 
          mutate(STIR_Calcule = ifelse(is.na(STIR_Calcule), 0, STIR_Calcule),
                 IndiceCompaction= ifelse(is.na(IndiceCompaction), 0, IndiceCompaction)) %>% 
          
          mutate(NbAppFongicides2 = as.character(NbAppFongicides)) %>%
          mutate(NbAppFongicides3 = na_if(NbAppFongicides2, "Manquant")) %>%
          mutate(NbAppFongicides = as.integer(NbAppFongicides3)) %>% 
          
          mutate(NbAppInsecticides2 = as.character(NbAppInsecticides)) %>%
          mutate(NbAppInsecticides3 = na_if(NbAppInsecticides2, "Manquant")) %>%
          mutate(NbAppInsecticides = as.integer(NbAppInsecticides3)) %>% 
          
          mutate(NbAppHerbicides2 = as.character(NbAppHerbicides)) %>%
          mutate(NbAppHerbicides3 = na_if(NbAppHerbicides2, "Manquant")) %>%
          mutate(NbAppHerbicides = as.integer(NbAppHerbicides3)) %>% 
          
          select(-c(NbAppHerbicides2, NbAppHerbicides3, NbAppInsecticides3, 
                    NbAppInsecticides2, NbAppFongicides3, NbAppFongicides2))
        str(sondage3)
        
        
        
        rm(sondage_compaction_na, sondage_stir_na)
rm(provenance, vars_dispo, x)
rm(chimie_raw, micro_raw, pedo_raw, physique_raw, climat_raw, sondage_raw)


# Sites à conserver pour le projet ------------------------------------------------------------------------

## 1) On ne veut que les sites cultivés
sites_cultives <-pedo2 %>% 
  filter(TypeSite == 'Cultivé') %>% 
  select(Site_id)

## 2) On veut des sites avec grandes cultures dans la rotation, donc avec soya ou maïs au moins une fois dans les 5 années recensées
sites_GCmixtes <- sondage3 %>% 
  filter(CulturePrincipale %in% c(
  'Maïs grain', 'Maïs ensilage', 'Maïs-grain',
  "Prairie suivie de soya", 'Soya')) %>% 
  select(Site_id) %>% 
  unique()

## 3) Jointure des 2 jeux de données
sites_selectionnes <- 
  inner_join(sites_cultives, sites_GCmixtes) %>% 
  as.vector() %>% 
  unlist()
length(sites_selectionnes)  
          ## On a 113 sites.


## 4) J'aimerais savoir combien de sites sont en grandes cultures "pures", soit mais ou soya, avec ou sans céréale seulement
sites_rejetes_GCstrict <- 
  sondage3 %>% 
  filter(CulturePrincipale %in% c(
    "Prairie mixte", "Prairie de graminées", "Pomme de terre", "Canola", "Prairie de légumineuses", 
    "Manquant", "Bord de clôture", "Culture maraîchère", "Maïs sucré", "Verger/Vigne", 
    "Prairie implantation", "Pâturage", "Friche", "Plantation", "Prairie suivie de soya", 
    "Boisé", "Millet perlé", "Haie brise-vent", "Bord de champ", "Prairie 1 coupe puis pâturage", 
    "Gazon", "Jachère", "Sorgho", "Luzerne/Avoine/Pois", "Pelouse", "Système agroforestier", "Pois")) %>% 
  select(Site_id) %>% 
  unique()

nrow(sites_rejetes_GCstrict)

sites_selectionnes_GCstrict <- sondage3 %>% 
  filter(!Site_id %in% sites_rejetes_GCstrict$Site_id) %>% 
  select(Site_id) %>% 
  unique() %>% 
  inner_join(sites_cultives) %>% 
  as.vector() %>% 
  unlist()

length(sites_selectionnes_GCstrict)
     # On a 79 sites si on prend seulement ce qui est en grandes cultures (mais ou soya, avec ou sans céréales, seulement).

rm(sites_cultives, sites_rejetes_GCstrict, sites_GCmixtes)

     ## Trier les jeux de données avec sites sélectionnés ---------------------------
        chimie_sites <- chimie1 %>% 
          filter(Site_id %in% sites_selectionnes) 
        
        physique_sites <- physique1 %>% 
          filter(Site_id %in% sites_selectionnes) 
        
        pedo_sites <-pedo2 %>% 
          filter(Site_id %in% sites_selectionnes) %>% 
          select(-TypeSite)
        
        micro_sites <- micro1 %>% 
          filter(Site_id %in% sites_selectionnes) 
        
        rdt_sites <- rdt1 %>% 
          filter(Site_id %in% sites_selectionnes)
        
        sondage_sites <- sondage3 %>% 
          filter(Site_id %in% sites_selectionnes)
        
        climat_sites <- climat1 %>% 
          filter(Site_id %in% sites_selectionnes)
        
        ids_sites <- ids1 %>% 
          filter(Site_id %in% sites_selectionnes)
        
        
        rm(physique1, chimie1,pedo2, micro1, rdt1, sondage1, sondage2, sondage3, climat1, ids1)

     ## Vérification présence de tous les sites dans chaque jeu de données ----
       
        ids.horizons <- sort(unique(as.character(ids_sites$Horizon_id)))
        ids.PE <- sort(unique(as.character(ids_sites$PE_id)))
        ids.sites <- sort(unique(as.character(ids_sites$Site_id)))
        
        
        # all(sort(unique(chimie_sites$Horizon_id)) == ids.horizons) 
        # all(sort(unique(chimie_sites$PE_id)) == ids.PE)
        # all(sort(unique(chimie_sites$Site_id)) == ids.sites)
        # 
        # all(sort(unique(climat_sites$Site_id)) == ids.sites) # FALSE
        # 
        # all(sort(unique(physique_sites$Horizon_id)) == ids.horizons) # FALSE
        # all(sort(unique(physique_sites$PE_id)) == ids.PE) # FALSE
        # all(sort(unique(physique_sites$Site_id)) == ids.sites) # FALSE
        # 
        # all(sort(unique(sondage_sites$Site_id)) == ids.sites)
        # 
        # all(sort(unique(rdt_sites$PE_id)) == ids.PE) 
        # all(sort(unique(rdt_sites$Site_id)) == ids.sites)
        # 
        # all(sort(unique(pedo_sites$Site_id)) == ids.sites)
        # 
        # all(sort(unique(micro_sites$Horizon_id)) == ids.horizons) # FALSE
        # all(sort(unique(micro_sites$Site_id)) == ids.sites) # FALSE
        
        #### Correction sites manquants climat -----
        climat_manquants <-
          setdiff(ids.sites, climat_sites$Site_id) %>% 
          print()
        
        climat_manquants_df <- tibble(
          Site_id = climat_manquants,
          PPT = NA_real_,
          Gini = NA_real_,
          GDD = NA_real_)
        
        climat_sites2 <- bind_rows(climat_sites, climat_manquants_df)
        
        
        rm(climat_sites, climat_manquants_df, climat_manquants)
        
        #### Correction sites manquants physique -----
        physique_manquants <-
          setdiff(ids.horizons, physique_sites$Horizon_id) %>% 
          print()
        
        physique_manquants_df <- 
          tibble(
            Horizon_id = physique_manquants,
            Sable = NA_real_,
            Limon = NA_real_,
            Argile = NA_real_,
            MVA3 = NA_real_,
            ConductiviteHydraulique = NA_real_,
            TEEauEch = NA_real_,
            TEAirEch = NA_real_,
            PorositeTotale = NA_real_,
            Macroporosite = NA_real_,
            Agregats_4_8mm = NA_real_,
            Agregats_2_4mm = NA_real_,
            Agregats_1_2mm = NA_real_,
            Agregats_0.5_1mm = NA_real_,
            Agregats_0.25_0.5mm = NA_real_,
            ValeurVESS = NA_real_ ) %>% 
          mutate(PE_id = sub("_.*", "", Horizon_id),
                 Site_id = str_sub(PE_id, 1, -2))
        
        physique_sites2 <- bind_rows(physique_sites, physique_manquants_df)
 
        
        rm(physique_sites, physique_manquants, physique_manquants_df)
        
        #### Correction sites manquants microbio -----
        micro_manquants <-
          setdiff(ids.horizons, micro_sites$Horizon_id) %>% 
          print()
        
        micro_manquants_df <- 
          tibble(
            Horizon_id = micro_manquants,
            Features_Bits = NA_real_,
            Features_Emp = NA_real_,
            BT_AmplifUnits = NA_real_,
            CT_AmplifUnits = NA_real_ ) %>% 
          mutate(PE_id = sub("_.*", "", Horizon_id),
                 Site_id = str_sub(PE_id, 1, -2))
        
        micro_sites2 <- bind_rows(micro_sites, micro_manquants_df)
        
        
        rm(micro_sites, micro_manquants, micro_manquants_df)

  rm(ids.sites, ids.horizons, ids.PE)
        

# Inspection/corrections --------------------------------------------------------------------------------------------------------
      ## ids ----
          str(ids_sites)
          summary(ids_sites)
                # On a des NAs pour la date d'échantillonnage (logique : provient du df physique)
                # Il n'y a pas le même nombre de PE pour A, B, C et D.
                # Il n'y a pas le même nombre de Ap1 ou B vs Ap2 (probablement qq sites sans Ap2)
          
          # Inspection pour les horizons
          horizons_manquants <-
            ids_sites %>% 
            group_by(PE_id) %>% 
            summarise(Nb.Horizons = n()) %>% 
            filter(Nb.Horizons != 3) %>% 
            print()
               
                # Regardons si ce sont les Ap2 qui sont manquants
                  ids_sites %>% 
                  filter(PE_id %in% horizons_manquants$PE_id) %>% 
                  pull(Horizon) %>% 
                  unique()
                     # Il y a en effet seulement Ap1 et B. C'était des sites sans Ap2.
                
                # Création d'un df qui répertorie les sites sans Ap2
                  sites_sans_Ap2 <-
                    ids_sites %>% 
                    filter(PE_id %in% horizons_manquants$PE_id) %>%
                    group_by(Site_id) %>% 
                    summarise(Nb_PE_sans_Ap2 = n()) %>% 
                    print(n=Inf)
                  
                    
          # Inspection pour les PE
          PE_manquants <-
            ids_sites %>% 
            select(PE_id, Site_id) %>% 
            distinct() %>% 
            group_by(Site_id) %>% 
            summarise(Nb.PE = n()) %>% 
            filter(Nb.PE != 4) %>% 
            print()
                # Il n'y a aucun PE manquant. Le nombre inégal de PE entre A, B
                # C et D découle du fait qu'il n'y a pas de Ap2 pour certains PE 
                # dans quelques sites.

          
          
          ids <- ids_sites
          
          
          rm(horizons_manquants, PE_manquants)
          
      ## sondage ----
          sondage_sites %>% 
            filter(CultureCouverture == "Ne s'applique pas") %>% 
            select(Site_id, CulturePrincipale, CultureCouverture, CC_Autres:CC_Melange) %>% 
            print(n=100)
               ## OK pour les prairies, inaproprié pour les annuelles
          
          sondage_CC_corr <-
          sondage_sites %>% 
            filter(CultureCouverture == "Ne s'applique pas") %>% 
            mutate(CultureCouverture = ifelse( (grepl('Prairie', CulturePrincipale)), "Ne s'applique pas", 'Non')) %>% 
            print(n=1000)
            
          agro <- sondage_sites %>% 
            filter(CultureCouverture != "Ne s'applique pas" | is.na(CultureCouverture)) %>% 
            rbind(sondage_CC_corr)
            
          
          str(agro)
          summary(agro)

          rm(sondage_CC_corr)

      ## rendement ----------------------------------------------------------------------------------------------------------------------
          rdt_sites
          summary(rdt_sites)
                  # J'ai le poids spécifique complet seulement pour le Mais grain.
          
          ### - NAs - ###
          ## Vérification que les grades de poids spécifiques sont OK
          rdt_sites %>% 
            filter(Culture %in% c('Maïs', 'Maïs-grain')) %>% 
            select(PoidSpecifiqueGrade) %>% 
            unique()
          # ECH est un peu comme l'équivalent d'un grade 6. Donc, c'est ok
          # Il y a des NA
          
          ## --- Cultures principales --- ###
          unique(rdt_sites$Culture)
                  # Répétition de orge vs orge grainée
                         # Après vérification ds tableau agronomie, orge = orge grainée
                  # Répétition Mais - Mais grain - Mais ensilage
                  # J'ai un doute par rapport à si cultures ds 'rdt' = cultures ds 'sondage'...
          
          verif_cultures <- 
            sondage_sites %>% 
            filter(Annee==AnneEchantillonage) %>% 
            select(Site_id, CulturePrincipale) %>% 
            right_join(rdt_sites) %>% 
            select(-c(PE_id, Annee)) %>% 
            mutate(CulturePrincipale = as.character(CulturePrincipale),
                   Culture = as.character(Culture)) %>% 
            filter(CulturePrincipale != Culture) %>% 
            arrange(CulturePrincipale)
            
          print(verif_cultures, n=300)
                  # Les données du sondage agronomique semblent plus précises. Elles seront conservées.
                  # Cas ambigus : Site 18PVD04 - Blé d'automne selon sondage, de printemps selon rdt. 
                                # Site 17SJU06 - Mais grain ds sondage, ensilage selon le rdt.
                                # A regarder avec l'IRDA...
          
          ## Changement culture rdt avec culture des données du sondage
          rdt_sites2 <- 
            sondage_sites %>% 
            filter(Annee==AnneEchantillonage) %>% 
            select(Site_id, CulturePrincipale) %>% 
            right_join(rdt_sites) %>% 
            select(-c(Culture, PoidSpecifique))
                   
          
          # Vérification données rdt sec         
          ggplot(rdt_sites2, aes(CulturePrincipale, RendementSec)) +
            geom_boxplot() +
            theme(axis.text.x = element_text(angle = 45, hjust = 1))
              ## Il y a un problème avec le maïs ensilage...
          
          rdt_sites2 %>% filter(RendementSec > 40)
          rdt_raw %>% filter(Site_id=='18SSL12') 
             ## Je pense que c'est le rendement sur base humide.
             ## Pour l'instant, on va les retirer....
          
          rdt <- rdt_sites2 %>% 
            mutate(RendementSec = if_else(Site_id == '18SSL12', NA, RendementSec))
          
          
          rm(rdt_raw, rdt_sites2, verif_cultures)

      ## pédologie --------------------------------------------------------------------------------------------------------
          str(pedo_sites)
          summary(pedo_sites)
          
          unique(pedo_sites$Sortesol)
                            # Aucun sol organique n'a été conservé. Cette valeur n'est plus utile.
          unique(pedo_sites$ModeleTerrain)
                            # OK
          unique(pedo_sites$NaturePetrographique)
                            # OK... on verra
          unique(pedo_sites$ProvincePedologique)
          unique(pedo_sites$SousRegionPedo)
          unique(pedo_sites$SousRegionPedoNo)
          unique(pedo_sites$Serie)
                           # OK
          unique(pedo_sites$EESSAQGroup)
          unique(pedo_sites$MateriauParentalAcr)
          unique(pedo_sites$Serie)
          
          pedo <- pedo_sites %>% 
            select(-c(Sortesol, SousRegionPedo))
          
          
      ## climat --------------------------------------------------------------------------------------------------------
          climat <- climat_sites2 %>% 
            full_join( 
              data.frame(Site_id = sites_selectionnes) )
          

      ## Inspection/corrections chimie + répertorier les NAs --------------------------------------------------------------------------------------------------------
          str(chimie_sites)
          summary(chimie_sites)
             # Tout est beau
          
          chimie <- chimie_sites
          

      ## microbio --------------------------------------------------------------------------------------------------------
          str(micro_sites2)
          summary(micro_sites2)
             # Il manque la colonne 'PE_id'.
          
          micro <- micro_sites2 %>% 
            mutate(PE_id = sub('_.*', "", Horizon_id))


      ## physique --------------------------------------------------------------------------------------------------------
          str(physique_sites2)
          summary(physique_sites2)
             # Tout est beau
          
          physique <- physique_sites2
          
          
  rm(chimie_sites, physique_sites2, climat_sites2, pedo_sites, micro_sites2,
     rdt_sites, sondage_sites, ids_sites)    

  
# Exportation ---------------------------------------------------------------------------------------------------------------------                  

## Répertoire d'enregistrement
dossier <- "~/Maitrise/EESSAQ/Analyses/donnees_filtrees/donnees_corrigees/metadonnees"

if (!dir.exists(dossier)) {
  dir.create(dossier) }
setwd("~/Maitrise/EESSAQ/Analyses/donnees_filtrees/donnees_corrigees")


write_csv2(pedo, 'pedologie.csv')
write_csv2(micro, 'microbiologie.csv')
write_csv2(chimie, 'chimie.csv')
write_csv2(physique, 'physique.csv')
write_csv2(climat, 'climat.csv')
write_csv2(rdt, 'rendements.csv')
write_csv2(ids, 'ids.csv')

# Il faut encore travailler sur agro
write_csv2(agro, "~/Maitrise/EESSAQ/Analyses/donnees_filtrees/donnees_en_correction/agro.csv")

# Métadonnees créées
pedo_meta <- pedo_description_valeurs
write_csv2(pedo_meta, "metadonnees/pedo_meta.csv")
write_csv2(sites_sans_Ap2, "metadonnees/ids_absence_Ap2.csv")

rm(pedo, micro, chimie, physique, rdt, agro, climat, sites_selectionnes, ids, 
   pedo_description_valeurs, sites_sans_Ap2,
   sites_selectionnes_GCstrict, dossier, pedo_meta)
                  
# Création d'un fichier de metadonnées ------------------------------------------

META_variables2 <- META_variables %>% 
  rename(Variable = Variable_francais,
         `Description de la variable` = Description)

META_filt1 <- bind_rows(META_physicochimie, META_variables2) %>% 
  filter(Variable %in% variables_retenues) 

      # Certaines variables sont dans les deux fichiers de métadonnées fournis par l'ÉÉSSAQ
      META_duplicats <- META_filt1[duplicated(META_filt1$Variable),]
      META_duplicats2 <- META_filt1 %>% 
        filter(Variable %in% META_duplicats$Variable) %>% 
        arrange(Variable)   
          # Il est possible de combiner les entrées sans perdre d'informations
      
      # Fusionner les entrées en utilisant reframe pour chaque colonne, en gardant les valeurs non-NA
      META_merged <- META_duplicats2 %>%
        group_by(Variable) %>%
        reframe(across(
          .cols = everything(),
          .fns = ~ ifelse(Variable == first(Variable) & cur_column() == "Description de la variable", 
                          first(.), 
                          na.omit(.)[1]),
          .names = "{col}"
        )) %>% 
        distinct()
      
      # Afficher le résultat
      head(META_merged)


write_csv2(META_merged, file = '~/Maitrise/EESSAQ/Analyses/1_gestion_nettoyage/META_travail.csv')

META_manquantes <- variables_retenues[!variables_retenues %in% META_merged$Variable]
META_manquantes
write_csv2(META_merged, '~/Maitrise/EESSAQ/Analyses/1_gestion_nettoyage/META_manquantes.csv')

rm(META_variables, META_variables2, META_physicochimie, META_filt1, 
   META_duplicats, META_duplicats2, 
   variables_retenues)






# Pour la reproductibilité        
sessionInfo()

