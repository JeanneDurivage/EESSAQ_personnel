# Création des indices agronomiques ----
# Autrice : Jeanne Durivage 
# Période : Été 2024
# Université Laval, laboratoires de Marie-Élise Samson et de Thiago Gumiere 

    # Setup
    library(readxl)
    library(ggfortify) # PCA
    library(tidyverse)
    library(psych)
    library(ggpubr)
    library(readr)
    theme_set(theme_bw())
    
    rm(list=ls())
    setwd("~/Maitrise/EESSAQ")
    load('~/Maitrise/EESSAQ/Analyses/donnees_filtrees/Donnees_en_correction/donnees_brutes_nettoyees_et_filtrees.RData') # Jeux de données filtrés et formatés
    load('~/Maitrise/EESSAQ/Analyses/donnees_filtrees/Donnees_en_correction/NAs_sauf_agro.RData') # Répertorie toutes les données manquantes
    sondage_raw <- read_excel("Donnees_EESSAQ/SiteAnnee_EESSAQ.xlsx") # Données initiales, pour faire des vérifications
    rm(chimie, micro, pedo, physique, rdt, sites_selectionnes)

# Sites sans réponse au sondage -------------------------------------------------------------------------------------------
na_CC <- agro %>% 
  filter_at(vars(CC_Autres:CC_Melange),any_vars(is.na(.))) %>% 
  print(n=100)
# View(na_CC)
      ## Les sites suivants sont rejetés : '17DBY09', '17BET07'
sites_incomplets <- data.frame(Site_id = c('17DBY09', '17BET07'),
                               ChampIncomplet = 'Tout')


agro_nas1 <- agro %>% 
  filter(Site_id %in% c('17DBY09', '17BET07')) 
agro_nas1[,5:25] <- NA
agro <- agro %>% filter(!Site_id %in% c('17DBY09', '17BET07')) %>% 
  rbind(agro_nas1)


na_culture <- agro %>% 
  filter_at(vars(Site_id:CultureCouverture),any_vars(is.na(.))) %>% 
  print(n=100)
# sondage_raw %>% filter(Site_id == '18SSL27') %>% View()
     ## Pour le site '18SSL27', les 2 premières années sont manquantes au sondage (sauf culture principale).
sites_incomplets <- sites_incomplets %>% 
  add_row(Site_id = '18SSL27', ChampIncomplet = '2PremieresAnnees')

agro_nas2 <- agro %>% 
  filter(Site_id=='18SSL27' & Annee %in% c(2015, 2016)) 
agro_nas2[,5:25] <- NA
agro <- agro %>% filter(!Site_id=='18SSL27'|!Annee %in% c(2015, 2016)) %>% 
  rbind(agro_nas2)

na_ferti <- agro %>% 
  filter_at(vars(Nb_AEM:Nb_AEO, Nb_Chaux),any_vars(is.na(.)))
# View(na_ferti)
     ## Ce sont tous des sites qui n'ont aucune données pour la ferti minérale, 
     ## mais le reste a été rempli (probablement approximativement, mais au moins, est rempli).
sites_incomplets <- na_ferti %>% 
  select(Site_id) %>% 
  unique() %>% 
  filter(!Site_id %in% c('18SSL27', '17BET07', '17DBY09')) %>% 
  mutate(ChampIncomplet = 'EngraisMineral') %>% 
  rbind(sites_incomplets)


na_pesticides <- agro %>% 
  filter_at(vars(NbAppFongicides:NbAppHerbicides),any_vars(is.na(.)))
# View(na_pesticides)
sites_incomplets <- sites_incomplets %>% 
  add_row(Site_id = '17SDM01', ChampIncomplet = 'Pesticides')

# Tableau résumé des problématiques liées aux réponses incomplètes au sondage
sites_incomplets
rm(na_pesticides, na_ferti, na_CC, na_culture, agro_nas1, agro_nas2)


# Biodiversité -----------------------------------------------------------------------------------------------------------------------
  ## DF rotation -------------------------------------------------------------------------------------------------------------------
## Ajout variable : Nombre d'années avant l'échantillonnage
agro <-  agro %>% 
  mutate(AnneeRelatif = AnneEchantillonage - Annee)

unique(agro$AnneeRelatif)
head(select(agro, AnneeRelatif, Annee, AnneEchantillonage))
### OK


## Ajout cultures précédentes au nouveau df
rotation <- agro %>% 
  mutate(AnneeRelatifTxt = paste('NoAnneesPassees', AnneeRelatif, sep = '' )) %>% 
  select(Site_id, CulturePrincipale, AnneeRelatifTxt) %>% 
  spread(AnneeRelatifTxt, CulturePrincipale) %>% 
  print(n=1000)



  ## DF prairie -------------------------------------------------------------------------------------
f_prairie <-
  agro %>% 
  mutate(Present = ifelse( grepl('Prairie', CulturePrincipale) == 'TRUE', 'Oui', 'Non')) %>% 
  select(Site_id, Present) %>%
  group_by(Site_id) %>% 
  summarise(Prairie = any(Present=='Oui')) %>% 
  mutate(Prairie = ifelse( Prairie == 'TRUE', 'Oui', 'Non')) 

pPrairie <- 
  ggplot(f_prairie, aes(Prairie)) +
  geom_bar() +
  ggtitle('Proportion de sites avec au moins 1 année en prairie')
pPrairie
ggsave('Analyses/1_gestion_nettoyage/Sondage/Graphs_sondage/qqt_Prairies.png', plot = pPrairie)

rm(pPrairie)



  ## Engrais vert ------------------------------------------------------------------------------------------------------

# Sous-df avec les données sur les CC
CC_sondage <- agro %>% 
  select(Site_id,CultureCouverture, CC_Autres:CC_Melange)
CC_sondage

# Calcul du nombre d'années en CC par site
CC1 <- agro %>% 
  mutate(CultureCouverture = as.character(CultureCouverture),
    CC = case_when(
    CultureCouverture == 'Oui' ~ 1,
    CultureCouverture == 'Non' ~ 0,
    CultureCouverture == "Ne s'applique pas" ~ 0)) %>% 
  group_by(Site_id) %>% 
  summarise(CC_freq = sum(CC)) %>% 
  ungroup()

    ggplot(CC1, aes(CC_freq)) +
      geom_histogram(bins=4)

    
CC1 <- CC1 %>% 
  mutate(CC_freq_cat = as.factor(case_when(CC_freq == 0 ~ 'Jamais',
                             CC_freq %in% c(1, 2) ~ 'Parfois',
                             CC_freq %in% c(3,4) ~ 'Souvent', 
                             CC_freq == 5 ~ 'Toujours')))
CC1

      pCC <- ggplot(CC1, aes(CC_freq)) +
        geom_histogram() +
        geom_vline(xintercept = c(0.5, 2.5), linetype =2, colour='firebrick', size=1) +
        labs(title = "Fréquence d'utilisation d'engrais verts",
             caption = "Catégories : Jamais / Parfois / Souvent") +
        xlab("Nombre d'années avec engrais verts") +
        ylab("Nombre de sites")
      pCC  
      ggsave("Analyses/1_gestion_nettoyage/Sondage/Graphs_sondage/categories_CC.png", plot = pCC)

     ### Création d'un tableau de référence des catégories
        ref_categ_CC <- data.frame(Catégorie = c('Jamais','Parfois','Souvent','Toujours'),
                                  CC_freq = c('0','1-2','3-4','5'))
        ref_categ_CC



# Déterminer quelles espèces ont été utilisées + Nombre total d'especes
  
CC2 <- CC_sondage %>%
  mutate(across(CC_Autres:CC_Melange, ~ case_when(
    CultureCouverture %in% c('Non', "Ne s'applique pas") ~ 'no', TRUE ~ .  )),
  CultureCouverture = if_else(CultureCouverture == 'Oui', 1, 0),
  CC_Autres = if_else(CC_Autres == 'yes', 1, 0),
  CC_Graminees = if_else(CC_Graminees == 'yes', 1, 0),
  CC_Melange = if_else(CC_Melange == 'yes', 3, 0),
  CC_Legumineuses = if_else(CC_Legumineuses == 'yes', 1, 0),
  CC_Cruciferes = if_else(CC_Cruciferes == 'yes', 1, 0)) %>%
  group_by(Site_id) %>%
  summarise(CC_Cruciferes = max(CC_Cruciferes),
    CC_Autres = max(CC_Autres),
    CC_Graminees = max(CC_Graminees),
    CC_Melange = max(CC_Melange),
    CC_Legumineuses = max(CC_Legumineuses),
    CC_NbSpp = sum(CC_Legumineuses, CC_Graminees, CC_Melange, CC_Cruciferes, CC_Autres)) %>% 
  ungroup() 

  CC2
 
  
  CC3 <- CC2 %>%
    mutate(CC_Autres = case_when(CC_Autres == 1 ~ 'AutresCC', .default = NA),
           CC_Graminees = case_when(CC_Graminees == 1 ~ 'GramineesCC', .default = NA),
           CC_Legumineuses = case_when(CC_Legumineuses == 1 ~ 'LegumineusesCC', .default = NA),
           CC_Cruciferes = case_when(CC_Cruciferes == 1 ~ 'CruciferesCC', .default = NA),
           CC_Melange = case_when(CC_Melange == 3 ~ 'MelangeCC', .default = NA)) %>% 
    rowwise() %>%
    mutate(CC_Spp = paste(na.omit(c_across(CC_Cruciferes:CC_Legumineuses)), collapse = ' + '),
           CC_Spp = case_when(CC_Spp == "CruciferesCC + AutresCC + GramineesCC + LegumineusesCC" ~ 'MelangeCC',
                              is.na(CC_NbSpp) == TRUE ~ NA_character_,
                              CC_Spp == '' ~ 'Aucun',
                              .default = CC_Spp)) %>% 
    select(Site_id, CC_Spp, CC_NbSpp)
    
  CC3
  
  
          pCC_NbSpp <- CC3 %>% 
            ggplot(aes(CC_NbSpp)) +
            geom_bar() +
            ggtitle("Diversité d'engrais verts utilsées au cours\n des 5 ans de sondage à chaque site") +
            xlab("Nombre de familles utilisées") +
            ylab('Nombre de sites')
          pCC_NbSpp
          ggsave("Analyses/1_gestion_nettoyage/Sondage/Graphs_sondage/CC_Nbfamilles.png", plot = pCC_NbSpp)

                pCC_fam <- CC3 %>%
                  filter(!is.na(CC_Spp)) %>%
                  ggplot(aes(fct_infreq(CC_Spp))) +
                  geom_bar() +
                  theme(axis.text.x = element_text(angle = 25, hjust=1, size=7)) +
                  ggtitle("Espèces et combinaisons d'engrais verts\n utilisées dans un même années") +
                  xlab("Groupe d'engrais vert") +
                  ylab('Nombre de sites')
                pCC_fam
                ggsave("Analyses/1_gestion_nettoyage/Sondage/Graphs_sondage/CC_familles-especes.png", plot = pCC_fam)
          

f_CC <- inner_join(CC1, CC3)
f_CC

rm(CC1, CC2, CC3, CC_sondage, pCC_fam, pCC_NbSpp, pCC)


  ## Rotation cultures ----------------------------------------------------------------------------------
### df qui liste la valeur de chaque type de culture principale
cultures <- 
  agro %>% 
  select(CulturePrincipale) %>% 
  unique() %>% 
  print(n=100)

cultures <- cultures %>% 
  mutate(NbSpp = case_when(
    CulturePrincipale == "Prairie de graminées" ~ 4,
    CulturePrincipale == "Prairie mixte" ~ 6,
    CulturePrincipale == "Prairie implantation" ~ 4,
    CulturePrincipale == "Prairie suivie de soya" ~ 6,
    CulturePrincipale == "Luzerne/Avoine/Pois" ~ 3,
    CulturePrincipale == "Jachère" ~ 7,
    CulturePrincipale == "Prairie de légumineuses" ~ 4,
    .default = 1),
    
    Especes = case_when(
      CulturePrincipale == "Pomme de terre" ~ 'PdT',
      CulturePrincipale == "Seigle d'automne" ~ 'Seigle',
      CulturePrincipale == "Culture maraîchère" ~ 'Maraicher',
      CulturePrincipale == "Autre petite céréale" ~ 'Autre_cereale',
      CulturePrincipale == "Jachère" ~ 'J1 J2 J3 J4 J5 J6 J7',
      CulturePrincipale == "Prairie de graminées" ~ 'G1 G2 G3 G4',
      CulturePrincipale == "Prairie mixte" ~ 'G1 G2 G3 G4 L1 L2',
      CulturePrincipale == "Prairie de légumineuses" ~ 'L1 L2 L3 L4',
      CulturePrincipale == "Prairie implantation" ~ 'G1 G2 L1 L2',
      CulturePrincipale == "Prairie suivie de soya" ~ 'Soya G1 G2 G3 L1 L2',
      CulturePrincipale == "Luzerne/Avoine/Pois" ~ 'Avoine Pois L1',
      CulturePrincipale %in% c('Maïs sucré', 'Maïs ensilage', 'Maïs-grain') ~ 'Maïs',
      CulturePrincipale %in% c('Blé de printemps', "Blé d'automne") ~ 'Blé',
      .default = CulturePrincipale),
  
  GrCultural = case_when(
    CulturePrincipale %in% c("Pomme de terre", "Culture maraîchère", 'Pois') ~ 'Maraicher',
    CulturePrincipale %in% c("Prairie de graminées", "Prairie mixte", 
                             "Prairie de légumineuses", "Prairie implantation",
                             "Luzerne/Avoine/Pois") ~ 'Prairie',
    CulturePrincipale %in% c("Prairie suivie de soya", 'Soya') ~ 'Soya',
    CulturePrincipale %in% c('Maïs sucré','Maïs ensilage','Maïs-grain') ~ 'Maïs',
    CulturePrincipale %in% c('Blé de printemps', "Blé d'automne", "Autre petite céréale",
                             "Seigle d'automne", 'Orge', 'Sorgho', 'Avoine') ~ 'Petite céréale',
    .default = CulturePrincipale)) 

print(cultures, n=100)

ref_culturesSpp <- cultures


  ## Biodiversité générale intraannuelle --------------

# Nb sps *cultivées* moyen par année
## Étape 1: Remplacer les valeurs des colonnes NoAnneesPassees0 à NoAnneesPassees4 par les valeurs de NbSpp
rotation_nb_spp <- rotation %>%
  mutate(across(starts_with("NoAnneesPassees"), 
                ~ cultures$NbSpp[match(., cultures$CulturePrincipale)]))

## Moyen des colonnes pour chaque Site_id
biodiv1 <- rotation_nb_spp %>%
  rowwise() %>%
  mutate(Cultures_NbMoyAnnuel = mean(c_across(starts_with("NoAnneesPassees")), na.rm = TRUE)) %>%
  select(Site_id, Cultures_NbMoyAnnuel)

print(biodiv1)


ggplot(biodiv1, aes(Cultures_NbMoyAnnuel)) +
  geom_histogram() 

# Ajout du nb d'espèces d'EV cultivées 
biodiv2 <- biodiv1 %>% 
  left_join(f_CC) %>% 
  mutate(biodivintraannuelle = CC_freq + Cultures_NbMoyAnnuel)

biodiv2


        pBiodiv1 <- ggplot(biodiv2, aes(biodivintraannuelle)) +
          geom_bar() +
          xlab("Nombre moyen d'espèces par années (incluant engrais verts)") +
          ylab("Nombre de sites") +
          labs(title="Biodiversité intraannuelle",
               caption = "Catégories : Faible / Moyen / Élevé") +
          geom_vline(xintercept = c(1.2,2.9), linetype=2, linewidth=1, colour='firebrick')
        pBiodiv1
        ggsave('Analyses/1_gestion_nettoyage/Sondage/Graphs_sondage/categories_biodiversite_intraannuelle.png', 
               plot = pBiodiv1)

biodiv2 <- biodiv2 %>% 
  mutate(biodivintraannuelle = case_when(biodivintraannuelle == 1 ~ 'Minimale',
                                             biodivintraannuelle <3 ~ 'Moyen',
                                             biodivintraannuelle >= 3 ~ 'Élevé'))
        
ref_biodivintraannuelle <- data.frame(Catégorie = c('Minimale', 'Moyen', 'Élevé'),
                                     biodivintraannuelle = c('[0-1]', ']1-3[', '>=3'))



  ## Biodiversité générale interannuelle ----
biodiv3 <- agro %>%
  select(Site_id, CulturePrincipale) %>% 
  mutate(Especes = cultures$Especes[match(CulturePrincipale, cultures$CulturePrincipale)],
         GrCultural = cultures$GrCultural[match(CulturePrincipale, cultures$CulturePrincipale)]) %>% 
  separate_rows(Especes, sep = " ") %>% 
  group_by(Site_id) %>% 
  summarise(Culture_NbEspecesTot = length(unique(Especes)),
            Culture_NbGrTot = length(unique(GrCultural))) %>% 
  ungroup() %>% 
  left_join(biodiv2) %>% 
  mutate(biodivinterannuelle_spp = Culture_NbEspecesTot + CC_NbSpp,
         biodivinterannuelle_gr = Culture_NbGrTot + CC_NbSpp,
         biodivinterannuelle_spp_cat = case_when(biodivinterannuelle_spp == 1 ~ 'Minimale',
                                                 biodivinterannuelle_spp < 3 ~ 'Faible',
                                                 biodivinterannuelle_spp < 5 ~ 'Moyen',
                                                 biodivinterannuelle_spp >= 5 ~ 'Élevé'),
         biodivinterannuelle_gr_cat =  case_when(biodivinterannuelle_gr == 1 ~ 'Minimale',
                                                 biodivinterannuelle_gr < 3 ~ 'Faible',
                                                 biodivinterannuelle_gr < 4 ~ 'Moyen',
                                                 biodivinterannuelle_gr >= 4 ~ 'Élevé')) 
print(biodiv3)
f_biodiv <- biodiv3

## Tableau de référence pour les catégories
ref_biodiv <- data.frame(Catégorie = c('Minimale','Faible','Moyen','Élevé'),
                         biodivinterannuelle_spp = c('[0-1]', ']1-3[', '[3-5[', '>=5'),
                         biodivinterannuelle_gr = c('[0-1]', ']1-3[', '[3-4[', '>=4')) %>% 
  left_join(ref_biodivintraannuelle)
ref_biodiv
          
          ## Illustrations graphique
            pBiodiv2 <-
            ggplot(biodiv3, aes(biodivinterannuelle_spp)) +
              geom_bar() +
              labs(title="Biodiversité interannuelle",
                   subtitle = "Inclue les cultures principales et les engrais verts",
                   caption = "Catégorie : Minimale / Faible / Moyen") +
              ylab('Nombre de sites') +
              xlab("Nombre d'**espèces** différentes cultivées en 5 ans") +
              geom_vline(xintercept = c(1.5, 2.5, 3.5), linetype=2, linewidth=1, colour= 'firebrick')
            pBiodiv2
            ggsave('Analyses/1_gestion_nettoyage/Sondage/Graphs_sondage/categories_biodiversite_interannuelle_especes.png', 
                   plot = pBiodiv2)
            
            pBiodiv3 <-
            ggplot(biodiv3, aes(biodivinterannuelle_gr)) +
              geom_bar() +
              labs(title="Biodiversité interannuelle",
                   subtitle = "Inclue les cultures principales et les engrais verts",
                   caption = "Catégorie : Minimale / Faible / Moyen") +
              ylab('Nombre de sites') +
              xlab("Nombre de **groupes culturaux ou de groupes d'engrais verts**\n différents cultivés en 5 ans") +
              geom_vline(xintercept = c(1.5, 2.5, 4.5), linetype=2, linewidth=1, colour= 'firebrick')
            pBiodiv3
            ggsave('Analyses/1_gestion_nettoyage/Sondage/Graphs_sondage/categories_biodiversite_intrerannuelle_groupescult.png', 
                   plot = pBiodiv3)
            

            ## Graph de fréquence des cultures 
            biodiv4 <- agro %>%
              select(Site_id, CulturePrincipale) %>% 
              mutate(Especes = cultures$Especes[match(CulturePrincipale, cultures$CulturePrincipale)],
                     GrCultural = cultures$GrCultural[match(CulturePrincipale, cultures$CulturePrincipale)]) 
              
            pCultures <-
              ggplot(biodiv4, aes(fct_infreq(CulturePrincipale))) +
              geom_bar() +
              theme(axis.text.x = element_text(angle=45, hjust=1)) +
              ggtitle("Fréquence des *cultures principales*\n dans les 5 années recencées")   +
              ylab("Nombre d'occurences") +
              xlab('')
            pCultures
            ggsave('Analyses/1_gestion_nettoyage/Sondage/Graphs_sondage/freq_CulturesPrincipales.png', 
                   plot = pCultures)
            
            pEspeces <-
              biodiv4 %>% 
              ggplot(aes(fct_infreq(Especes))) +
              geom_bar() +
              theme(axis.text.x = element_text(angle=45, hjust=1)) +
              ggtitle("Fréquence des *espèces* cultivées\n dans les 5 années recencées")  +
              ylab("Nombre d'occurences") +
              xlab('')
            pEspeces
            ggsave('Analyses/1_gestion_nettoyage/Sondage/Graphs_sondage/freq_EspecesCultivees.png', 
                   plot = pEspeces)
            
            pTypes <-
            ggplot(biodiv4, aes(fct_infreq(GrCultural))) +
              geom_bar() +
              theme(axis.text.x = element_text(angle=45, hjust=1)) +
              ggtitle("Fréquence des *groupes culturaux* cultivées\n dans les 5 années recencées") +
              ylab("Nombre d'occurences")  +
              xlab('')
            pTypes
            ggsave('Analyses/1_gestion_nettoyage/Sondage/Graphs_sondage/freq_GroupesCultures.png', 
                   plot = pTypes)
            
            
            
   rm(biodiv1, biodiv2, biodiv3, pCultures, pEspeces, pTypes, cultures, rotation_nb_spp,
      biodiv4, pBiodiv1, pBiodiv2, pBiodiv3, ref_biodivintraannuelle)

# STIR, compaction -------------------------------------------------------------------------------------------------------------------
sondage_cotes <- agro %>%
  select(Site_id, STIR_Calcule, IndiceCompaction) %>% 
  group_by(Site_id) %>% 
  summarise(STIR = mean(STIR_Calcule),
            Compaction = mean(IndiceCompaction))

     ### Vérification réponses au sondage ----
summary(sondage_cotes)
## Problème avec certaines vars

sondage_cotes %>% 
  pivot_longer(c(STIR, Compaction)) %>% 
  ggplot(aes(name, value)) +
  geom_boxplot()

STIR_anormale_high <-
  sondage_raw %>% 
  select(Site_id, CulturePrincipale,
         Travail_1:NbPassages_Travail_5, STIR_Calcule) %>% 
  
  right_join(sondage_cotes) %>% 
  filter(STIR >= 180 | is.na(STIR) )
STIR_anormale_high
## OK


Compaction_anormale_high <-
  sondage_raw %>% 
  select(Site_id, CulturePrincipale, Travail_1:NbPassages_Travail_5, 
         STIR_Calcule, IndiceCompaction) %>% 
  right_join(sondage_cotes) %>% 
  filter(Compaction >= 50 | is.na(Compaction) )
Compaction_anormale_high
## OK


STIR_anormale_low <-
  sondage_raw %>% 
  select(Site_id, CulturePrincipale, 
         Travail_1:NbPassages_Travail_5, STIR_Calcule) %>% 
  right_join(sondage_cotes) %>% 
  filter(STIR <= 35 | is.na(STIR) )
STIR_anormale_low
## OK


Compaction_anormale_low <-
  sondage_raw %>% 
  select(Site_id, CulturePrincipale, Travail_1:NbPassages_Travail_5, 
         STIR_Calcule, IndiceCompaction) %>% 
  right_join(sondage_cotes) %>% 
  filter(Compaction <= 10 | is.na(Compaction) )
Compaction_anormale_low

## Les sites suivants n'ont pas recu des réponse de sondage pour le travail du sol : '22SUB01', '17BET07', '17DBY09')

agro <- agro %>% 
  mutate(across(24:25, ~ ifelse(Site_id %in% c('22SUB01', '17BET07', '17DBY09'), NA, .)))

sites_incomplets <- sites_incomplets %>% 
  add_row(Site_id = rep('22SUB01', 2), ChampIncomplet = c('STIR', 'Compaction'))
#q_STIR <- fivenum(sondage_cotes$STIR, na.rm=TRUE)
q_STIR <- c(0,30,80,110,275)
q_STIR

pSTIR <- ggplot(sondage_cotes, aes(x=STIR)) +
  geom_histogram(bins=25) +
  geom_vline(xintercept = (q_STIR[2:4]), color = 'firebrick', linewidth = 1, linetype = 2) +
  labs(title = "Cote d'intensité de travail du sol (STIR)",
         caption = 'Catégories : Faible / Moyen / Élevé / Extrême') +
  xlab("STIR") +
  ylab("Nombre de sites")
pSTIR

ggsave('Analyses/1_gestion_nettoyage/Sondage/Graphs_sondage/categories_STIR.png', plot = pSTIR)



#q_compaction <- fivenum(sondage_cotes$Compaction, na.rm=TRUE)
q_compaction <- c(0,10,20,30,100)
q_compaction
pCompaction <- ggplot(sondage_cotes, aes(x=Compaction)) +
  geom_histogram(bins=30) +
  geom_vline(xintercept = (q_compaction[2:4]), color = 'firebrick', size = 1, linetype = 2) +
  scale_x_continuous(breaks = seq(0,85, by=10))  +
  labs(title = "Indice de risque de compaction",
       caption = 'Catégories : Faible / Moyen / Élevé / Extrême') +
  xlab("Indice de risque de compaction") +
  ylab("Nombre de sites")
pCompaction

ggsave('Analyses/1_gestion_nettoyage/Sondage/Graphs_sondage/categories_compaction.png', plot = pCompaction)





f_STIRcompaction <- sondage_cotes %>% 
  mutate(STIR_cat = case_when(STIR < q_STIR[2] ~ 'Faible',
                              STIR < q_STIR[3] ~ 'Moyen',
                              STIR < q_STIR[4] ~ 'Élevé',
                              STIR < q_STIR[5] ~ 'Extrême'),
         Compaction_cat = case_when(Compaction < q_compaction[2] ~ 'Faible',
                                    Compaction < q_compaction[3] ~ 'Moyen',
                                    Compaction < q_compaction[4] ~ 'Élevé',
                                    Compaction < q_compaction[5] ~ 'Extrême'))




### Création d'un tableau de référence des catégories STIR et compaction
q_STIR1 <- q_STIR %>% round(1)
q_STIR2 <- c(q_STIR1[2:4])
q_STIR3 <- paste('[', q_STIR1[1:3], '-', q_STIR2, '[', sep = '')
q_STIR3[4] <-  paste('>=',q_STIR1[4])
q_STIR3  

q_compaction1 <- q_compaction %>% round(1)
q_compaction2 <- q_compaction1[2:4]
q_compaction3 <- paste('[', q_compaction1[1:3], '-', q_compaction2, '[', sep = '')
q_compaction3[4] <- paste('>=', q_compaction1[4])
q_compaction3

ref_categ_STIRCompaction <- data.frame(Catégorie = c('Faible','Moyen','Élevé','Extrême'),
                                       STIR = q_STIR3,
                                       Compaction = q_compaction3) 


rm(Compaction_anormale_high, Compaction_anormale_low, STIR_anormale_high, STIR_anormale_low,
   q_STIR,q_STIR2, q_STIR1, q_STIR3,q_compaction, q_compaction1, q_compaction2, q_compaction3,
   sondage_cotes, pSTIR, pCompaction)  

# Indice fertilisation -------------------------------------------------------------------------------------------------------------------
  ## Fréquence d'application ----
    agro_ferti <- agro %>% 
      select(Site_id, Nb_AEM, Nb_AEO, SourceAEO_App_1, SourceAEO_App_2, SourceAEO_App_3,
             TypeAEO_App_1, TypeAEO_App_2, TypeAEO_App_3)
    
    ferti_freq <- 
      agro_ferti %>%
      group_by(Site_id) %>% 
      summarise(AEM_freq = mean(Nb_AEM, na.rm = TRUE),
                AEO_freq = mean(Nb_AEO, na.rm = TRUE)) %>% 
      ungroup()
    
    ggplot(ferti_freq, aes(AEO_freq)) +
      geom_bar(width = .1)
    ggplot(ferti_freq, aes(AEM_freq)) +
      geom_bar(width = .1)
    
    ferti_freq <- ferti_freq %>% 
      mutate(AEM_cat = as.factor(case_when(AEM_freq == 0 ~ 'Jamais',
                                           AEM_freq < 1 ~ 'Parfois',
                                           AEM_freq < 2 ~ 'Souvent',
                                           AEM_freq >=2 ~ 'Très souvent')),
             AEO_cat = as.factor(case_when(AEO_freq == 0 ~ 'Jamais',
                                           AEO_freq < 1 ~ 'Parfois',
                                           AEO_freq >= 1 ~ 'Souvent')))
    
             
    pAEO_freq <- ggplot(ferti_freq, aes(AEO_freq)) +
      geom_bar(width = .1) +
      geom_vline(xintercept = c(0.09, 0.9), color = 'firebrick', linetype=2, size=1) +
      labs(title = "Fréquence d'application d'engrais organiques",
           caption = "Catégories : Jamais / Parfois / Souvent") +
      xlab("Nombre d'applications par années") +
      ylab("Nombre de sites")
    pAEO_freq         
    ggsave("Analyses/1_gestion_nettoyage/Sondage/Graphs_sondage/categories_AEO_freq.png", plot = pAEO_freq)
    
    pAEM_freq <- ggplot(ferti_freq, aes(AEM_freq)) +
      geom_bar(width = .1) +
      geom_vline(xintercept = c(0.09, 0.9, 1.9), color = 'firebrick', linetype=2, size=1) +
      labs(title = "Fréquence annuelle d'application d'engrais minéraux",
           caption = "Catégories : Jamais / Parfois / Souvent / Très souvent") +
      xlab("Nombre d'applications par année") +
      ylab("Nombre de sites")
    pAEM_freq         
    ggsave("Analyses/1_gestion_nettoyage/Sondage/Graphs_sondage/categories_AEO_freq.png", plot = pAEM_freq)
    
    

  ## Type d'application ----
    # Combinaisons Type et Source possibles
    pAEO_typesource <- ggplot(agro_ferti, aes(SourceAEO_App_1, TypeAEO_App_1)) +
      geom_point() + 
      theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
      labs(title="Types d'apports organiques recencés") 
    pAEO_typesource
    ggsave("Analyses/1_gestion_nettoyage/Sondage/Graphs_sondage/AEO_Combinaisons_Type-Source.png", plot = pAEO_typesource)
    
    # Nouvel indice : Source + Type AEO
    agro_ferti <- agro_ferti %>% 
      mutate(AEO1 = ifelse(is.na(SourceAEO_App_1) | is.na(TypeAEO_App_1), NA, paste(SourceAEO_App_1, TypeAEO_App_1, sep = ' - ')), 
             AEO2 = ifelse(is.na(SourceAEO_App_2) | is.na(TypeAEO_App_2), NA, paste(SourceAEO_App_2, TypeAEO_App_2, sep = ' - ')),
             AEO3 = ifelse(is.na(SourceAEO_App_3) | is.na(TypeAEO_App_3), NA, paste(SourceAEO_App_3, TypeAEO_App_3, sep = ' - ')))
    
    # Groupement de toutes les applications sur 5 ans en une seule catégorie.
    AEO_TypesSources <- agro_ferti %>%
      group_by(Site_id) %>%
      summarise(
        AEO_sources = paste(sort(unique(c(SourceAEO_App_1, SourceAEO_App_2, SourceAEO_App_3)[!is.na(c(SourceAEO_App_1, SourceAEO_App_2, SourceAEO_App_3))])), collapse = " + "),
        AEO_types = paste(sort(unique(c(TypeAEO_App_1, TypeAEO_App_2, TypeAEO_App_3)[!is.na(c(TypeAEO_App_1, TypeAEO_App_2, TypeAEO_App_3))])), collapse = " + "),
        AEO_sourcestypes = paste(sort(unique(c(AEO1, AEO2, AEO3)[!is.na(c(AEO1, AEO2, AEO3))])), collapse = " + "),
        AEO_freq = mean(Nb_AEO, na.rm = TRUE)) %>%
      ungroup() %>% 
      mutate(across(AEO_sources:AEO_sourcestypes, ~ifelse(. == "", 'Aucun', .)),
             across(AEO_sources:AEO_sourcestypes, ~ifelse(is.na(AEO_freq), NA, .)))
    
    AEO_TypesSources
    
    
    pAEO1 <- ggplot(AEO_TypesSources) +
      geom_bar(aes(fct_infreq(AEO_sources))) +
      theme(axis.text.x = element_text(angle=45, hjust=1)) +
      scale_y_continuous(breaks = seq(0,35,5)) +
      labs(title = "Ensemble des sources d'engrais organiques\n groupés pour chaque site",
           subtitle = "Nombre de sites par catégorie") 
    pAEO1
    ggsave("Analyses/1_gestion_nettoyage/Sondage/Graphs_sondage/AEO_Sources.png", plot = pAEO1, 
           height = 5)
    
    pAEO2 <- ggplot(AEO_TypesSources) +
      geom_bar(aes(fct_infreq(AEO_types))) +
      theme(axis.text.x = element_text(angle=45, hjust=1)) +
      scale_y_continuous(breaks = seq(0,50,5)) +
      labs(title = "Ensemble des types d'engrais organiques\n groupés pour chaque site",
           subtitle = "Nombre de sites par catégorie")
    pAEO2
    ggsave("Analyses/1_gestion_nettoyage/Sondage/Graphs_sondage/AEO_Types.png", plot = pAEO2,
           height = 4)
    
    pAEO3 <- ggplot(AEO_TypesSources) +
      geom_bar(aes(fct_infreq(AEO_sourcestypes))) +
      theme(axis.text.x = element_text(angle=45, hjust=1)) +
      scale_y_continuous(breaks = seq(0,35,5)) +
      labs(title = "Ensemble des sources et types d'engrais organiques\n groupés pour chaque site",
           subtitle = "Nombre de sites par catégorie")
    pAEO3  
    ggsave("Analyses/1_gestion_nettoyage/Sondage/Graphs_sondage/AEO_TypesSources.png", plot = pAEO3,
           height = 5)
    

       #### Dfs et plots avec la fréquence d'application des sources et types, sans avoir groupé les différents types par site ----
          combine_aeo <- function(data, aeo_column) {
            data %>%
              group_by(Site_id, !!sym(aeo_column)) %>%
              summarise(freq = n(), .groups = 'drop') %>%
              filter(!is.na(!!sym(aeo_column))) %>%
              rename(AEO = !!sym(aeo_column))
          }
          
          AEO_combine_list <- c("AEO1", "AEO2", "AEO3")
          AEO_combine <- lapply(AEO_combine_list, combine_aeo, data = agro_ferti) %>%
            bind_rows() %>%
            group_by(Site_id, AEO) %>%
            summarise(AEO_combine_nb = sum(freq)/5, .groups = 'drop')
          AEO_combine
          
          pfreqAEO_sourcetype <- ggplot(AEO_combine, aes(x=AEO, AEO_combine_nb))+
            geom_boxplot() +
            theme(axis.text.x = element_text(angle = 35, hjust=1, size=7)) +
            labs(title="Fréquence d'utilisation des engrais organiques") +
            ylab("Nombre d'applications par année")
          pfreqAEO_sourcetype
          ggsave("Analyses/1_gestion_nettoyage/Sondage/Graphs_sondage/AEO_Freq_Type-Source_sepSites.png", plot = pfreqAEO_sourcetype)
          
          
          AEO_source_list <- c("SourceAEO_App_1", "SourceAEO_App_2", "SourceAEO_App_3")
          AEO_source <- lapply(AEO_source_list, combine_aeo, data = agro_ferti) %>%
            bind_rows() %>%
            group_by(Site_id, AEO) %>%
            summarise(AEO_source_nb = sum(freq)/5, .groups = 'drop')
          AEO_source
          
          pfreqAEO_source <- AEO_source %>% 
            ggplot(aes(x=AEO, AEO_source_nb))+
            geom_boxplot() +
            theme(axis.text.x = element_text(angle = 35, hjust=1, size=7)) +
            labs(title="Fréquence d'utilisation des engrais organiques") +
            ylab("Nombre d'applications par année")
          pfreqAEO_source
          ggsave("Analyses/1_gestion_nettoyage/Sondage/Graphs_sondage/AEO_Freq_SourcesepSites.png", plot = pfreqAEO_source)
          
          
          
          
          AEO_type_list <- c("TypeAEO_App_1", "TypeAEO_App_2", "TypeAEO_App_3")
          AEO_type <- lapply(AEO_type_list, combine_aeo, data = agro_ferti) %>%
            bind_rows() %>%
            group_by(Site_id, AEO) %>%
            summarise(AEO_type_nb = sum(freq), .groups = 'drop')
          AEO_type
          
          pfreqAEO_type <- ggplot(AEO_type, aes(x=AEO, AEO_type_nb))+
            geom_boxplot() +
            theme(axis.text.x = element_text(angle = 35, hjust=1, size=7)) +
            labs(title="Fréquence d'utilisation des engrais organiques") +
            ylab("Nombre d'applications par année")
          pfreqAEO_type
          ggsave("Analyses/1_gestion_nettoyage/Sondage/Graphs_sondage/AEO_Freq_SourcesepSites.png", plot = pfreqAEO_source)
          
          

  ## Combinaison des indices et catégories créés
  f_ferti <- inner_join(ferti_freq, AEO_TypesSources)
  f_ferti
  dim(ferti_freq); dim(AEO_TypesSources); dim(f_ferti) ## OK



rm(AEO_combine, AEO_source, AEO_type, agro_ferti, ferti_freq, pAEM_freq, pAEO_freq, 
   pAEO_typesource, pfreqAEO_source, pfreqAEO_sourcetype, pfreqAEO_type, combine_aeo, 
   AEO_combine_list, AEO_source_list, AEO_type_list, AEO_TypesSources, pAEO1, pAEO2, 
   pAEO3)

# Indice phytosanitaire ---------------------------------------------------------------------------------------------------------------------
pesticides_num <- agro %>% 
  mutate(SemenceEnrobes = case_when(grepl('Prairie',CulturePrincipale)==TRUE ~ 'Non',
                                    CulturePrincipale=='Jachère' ~ 'Non',
                                    is.na(NbAppFongicides) == FALSE & is.na(SemenceEnrobes) == TRUE ~ 'Non',
                                    .default =SemenceEnrobes) ) %>% 
  group_by(Site_id) %>% 
  summarise(NbMoyHerbicide = mean(NbAppHerbicides),
            NbMoyFongicides = mean(NbAppFongicides),
            NbMoyInsecticides = mean(NbAppInsecticides),
            NbMoySemenceEnrobee = sum(ifelse(SemenceEnrobes == 'Oui', 1, 0))/5,
            NbMoyPesticidesTot = sum(NbMoyHerbicide, NbMoyFongicides, NbMoyInsecticides, NbMoySemenceEnrobee))

head(pesticides_num)
summary(pesticides_num)

pesticides_num %>%  
  select(-Site_id) %>% 
  pairs.panels(method = 'spearman', 
               stars = TRUE, 
               scale = TRUE)

pesticides_long <- pesticides_num %>%
  select(-Site_id)  %>%
  gather(type, quantite) %>% 
  mutate(type = factor(type, levels = c("NbMoyFongicides", "NbMoyInsecticides", 
                                        "NbMoyHerbicide", "NbMoySemenceEnrobee", "NbMoyPesticidesTot")))

pPesticides1 <- 
  ggplot(pesticides_long, aes(type, quantite)) +
  geom_boxplot() +
  labs(title = "Fréquence d'utilisation de pesticides") +
  ylab("Nombre d'applications par année")
pPesticides1
ggsave("Analyses/1_gestion_nettoyage/Sondage/Graphs_sondage/Pesticides_freq_bx.png", plot = pPesticides1)


ggplot(pesticides_long) +
  geom_histogram(aes(quantite)) +
  facet_grid( ~ type, scales = 'free') 

pesticides_pca <- pesticides_num %>% 
  select(NbMoyHerbicide:NbMoyPesticidesTot) %>% 
  mutate( NbFongicides = log1p(NbMoyFongicides), 
    NbInsecticides = log1p(NbMoyInsecticides),
    NbPesticidesTot = log1p(NbMoyPesticidesTot) ) %>% 
  replace(is.na(.), 0) %>% 
  prcomp(scale=TRUE)

autoplot(pesticides_pca, data=pesticides_num, colour="NbPesticidesTot", 
         loadings=TRUE, loadings.label=TRUE)

# Création de catégories :
f_pesticides <- pesticides_num %>% 
  mutate(Herbicide_cat = as.factor(case_when(NbMoyHerbicide == 0 ~ 'Jamais',
                                       NbMoyHerbicide < 0 ~ 'Parfois',
                                       NbMoyHerbicide < 2 ~ 'Souvent',
                                       NbMoyHerbicide >=2 ~ 'Très souvent')),
         Fongicide_cat = as.factor(case_when(NbMoyFongicides == 0 ~ 'Jamais',
                                             NbMoyFongicides < 1 ~ 'Parfois')),
         Insecticide_cat = as.factor(case_when(NbMoyInsecticides == 0 ~ 'Jamais',
                                             NbMoyInsecticides < 1 ~ 'Parfois')),
         SemencesEnrobees_cat = as.factor(case_when(NbMoySemenceEnrobee == 0 ~ 'Jamais',
                                             NbMoySemenceEnrobee < 1 ~ 'Parfois',
                                             NbMoySemenceEnrobee == 1 ~ 'Toujours')),
         PesticidesTot_cat = as.factor(case_when(NbMoyPesticidesTot == 0 ~ 'Jamais',
                                             NbMoyPesticidesTot < 2 ~ 'Parfois',
                                             NbMoyPesticidesTot < 3 ~ 'Souvent',
                                             NbMoyPesticidesTot >=3 ~ 'Très souvent')))

vlines <- tibble(
    type = c("NbMoyFongicides", "NbMoyInsecticides", "NbMoyHerbicide", 
             "NbMoyPesticidesTot", "NbMoySemenceEnrobee"),
    xintercept = list(c(.1), c(.1), c(.1, .9, 1.9), c(.1, 1.9, 2.9), c(.1, .9)))
vlines_long <- vlines %>%
    unnest(cols = c(xintercept))

pPesticides2 <-
ggplot(pesticides_long, aes(quantite)) +
    geom_histogram(bins=25) + 
    facet_grid(~ type, scales = 'free') +
    geom_vline(data = filter(vlines_long, type %in% c("NbMoyFongicides", 'NbMoyInsecticides')), 
               aes(xintercept = xintercept), colour = 'firebrick', linetype = 2, size = .8) + 
    geom_vline(data = filter(vlines_long, type == "NbMoyHerbicide"), 
               aes(xintercept = xintercept), colour = 'firebrick', linetype = 2, size = .8) +
    geom_vline(data = filter(vlines_long, type == "NbMoyPesticidesTot"), 
               aes(xintercept = xintercept), colour = 'firebrick', linetype = 2, size = .8) +
    geom_vline(data = filter(vlines_long, type == "NbMoySemenceEnrobee"), 
               aes(xintercept = xintercept), colour = 'firebrick', linetype = 2, size = .8) +
    labs(title = "Fréquence d'utilisation de pesticides",
         caption = "Catégories fongicides et insecticides : Jamais / Parfois
Categories herbicides et pesticides totaux : Jamais / Parfois / Souvent / Très souvent
Categories enrobage de semences : Jamais / Parfois / Toujours") +
    theme(plot.caption = element_text(hjust = 0)) +
  xlab("Nombre d'applications par année") +
  ylab("Nombre de sites")
pPesticides2 

ggsave("Analyses/1_gestion_nettoyage/Sondage/Graphs_sondage/categories_pesticides.png", plot = pPesticides2,
       width = 11, height = 4)


rm(pesticides_long, pesticides_num, pesticides_pca, vlines, vlines_long, pPesticides1, pPesticides2)


# Indices chaulage --------------------------------------------------------------------------------------------------------------------
f_chaux <- agro %>% 
  group_by(Site_id) %>% 
  summarise(NbChauxTot = sum(Nb_Chaux),
            PresenceChaux = ifelse(sum(Nb_Chaux) >= 1, "Oui", "Non"),
            AnneesDepuisChaux =  ifelse(any(Nb_Chaux != 0), min(AnneeRelatif[Nb_Chaux != 0]), NA_real_)) %>%   
  # Les sites qui n'ont pas de chaux ont la valeur NA pour AnneesDepuisChaux. Pour corriger cela :
  mutate(AnneesDepuisChaux = ifelse(NbChauxTot == 0 & is.na(AnneesDepuisChaux), ">5", as.character(AnneesDepuisChaux)))

head(f_chaux)

pChaux1 <- ggplot(f_chaux, aes(NbChauxTot)) +
  geom_bar() +
  labs(title="Nombre total d'applications\n de chaux en 5 ans") +
  theme(plot.title = element_text(size=10))
pChaux2 <- ggplot(f_chaux, aes(PresenceChaux)) +
  geom_bar() +
  labs(title="Proportions des sites ayant\n appliqué de la chaux")+
  theme(plot.title = element_text(size=10))
pChaux3 <- ggplot(f_chaux, aes(AnneesDepuisChaux)) +
  geom_bar() +
  labs(title="Nombre d'années depuis la\n dernière application de chaux")+
  theme(plot.title = element_text(size=10))

pChaux <- ggarrange(pChaux1, pChaux2, pChaux3)
pChaux
ggsave('Analyses/1_gestion_nettoyage/Sondage/Graphs_sondage/chaux.png', plot = pChaux)

rm(pChaux1, pChaux2, pChaux3, pChaux)

# Intégration des données générées par ce code --------------------------------------------------------------------------------------------

    ## Regroupement en un jeu de données unique ----
    # 1) Nouveau df avec seulement 1 entrée par site. 
      # Ajout des dates d'échantillonage du même coup.
        f_df <- agro %>% 
          filter(Annee == AnneEchantillonage) %>% 
          select(Site_id, AnneEchantillonage, CulturePrincipale) %>% 
          left_join(ids %>% 
                      select(Site_id, DateEch) %>% 
                      distinct()) %>% 
          print()
                    # Certains sites ont été échantillonnés sur 2 jours consécutifs. 
                # Puisque ce n'est pas très important pour moi, je vais garder la
                # date du premier jour.
                  
                  f_df_unique <- f_df %>%
                    group_by(Site_id, AnneEchantillonage, CulturePrincipale) %>%
                    slice_min(DateEch, n = 1) %>% # Garde la date la plus ancienne pour chaque Site_id
                    ungroup()
                  
    
    # 2) Fusion de tous les df créés
    agro_indices <- f_df_unique %>%
      left_join(f_STIRcompaction) %>% 
      left_join(f_chaux) %>% 
      left_join(f_prairie) %>% 
      left_join(f_CC)%>% 
      left_join(f_ferti) %>% 
      left_join(f_biodiv) %>% 
      mutate(across( 
        c(PresenceChaux:AnneesDepuisChaux, STIR_cat, Compaction_cat, Prairie, 
          AEO_sources:AEO_sourcestypes, CC_freq_cat, AnneEchantillonage,
          biodivinterannuelle_spp_cat, biodivinterannuelle_gr_cat, CC_Spp,
          biodivintraannuelle), as.factor),
        DateEch = as.Date(DateEch))
    
        levels(agro_indices$CC_freq_cat) <- c('Jamais','Parfois','Souvent','Toujours')
        levels(agro_indices$STIR_cat) <- c('Faible', 'Moyen', 'Élevé', 'Extrême')
        levels(agro_indices$Compaction_cat) <- c('Faible', 'Moyen', 'Élevé', 'Extrême')
        levels(agro_indices$AEO_cat) <- c('Jamais','Parfois','Souvent')
        levels(agro_indices$AEM_cat) <- c('Jamais','Parfois','Souvent', 'Très souvent')
        levels(agro_indices$biodivinterannuelle_spp_cat) <- c('Minimale','Faible','Moyen', 'Élevé')
        levels(agro_indices$biodivinterannuelle_gr_cat) <- c('Minimale','Faible','Moyen', 'Élevé')
        levels(agro_indices$biodivintraannuelle) <- c('Minimale','Faible','Moyen', 'Élevé')

        # Correction des NaN
        agro_indices <- agro_indices %>%
          mutate(across(everything(), ~ replace(., is.nan(.), NA)))
        
        # Dernières vérifications
        str(agro_indices)
        summary(agro_indices)
        
        # On va maintenant le renommer pour être cohérent avec d'autres scripts
        agro <- agro_indices
        
    ## Intégration des NA au répertoire des NA pour tous les jeux de données ----
       load('~/Maitrise/EESSAQ/Analyses/donnees_filtrees/donnees_en_correction/NAs_sauf_agro.RData') 
            Valeurs_NA_autres <- Valeurs_NA
            # Répertorie toutes les données manquantes des autres données EESSAQ
      
        agro_NAs <- agro_indices %>% 
          mutate(across(AnneEchantillonage:biodivinterannuelle_gr_cat, as.character)) %>%
          pivot_longer(AnneEchantillonage:biodivinterannuelle_gr_cat, 
                       names_to = 'Variable_NA') %>% 
          filter(if_any(everything(), is.na)) %>% 
          mutate(JeuDeDonnees = 'SondageAgronomique',
                 PE_id = NA,
                 Horizon_id = NA) %>%
          select(-value) %>% 
          print(n=3)
        
        Valeurs_NA <- Valeurs_NA_autres %>% 
          full_join(agro_NAs) %>% 
          print(n=3)
        
        # Vérification de l'intégrité de la fusion (doit retourner)
        nrow(agro_NAs) + nrow(Valeurs_NA_autres) == nrow(Valeurs_NA)
      
      
      
    ## Création d'un répertoire de métadonnées sur le sondage ----
        sondage_meta <- list(ref_biodiv, ref_categ_CC, ref_categ_STIRCompaction, 
                             ref_culturesSpp)
        
        
    ## Exportation ----
        setwd("~/Maitrise/EESSAQ/Analyses/donnees_filtrees/donnees_corrigees")

        # Jeux de données
        write_csv2(agro, 'agro.csv')
        
        # Données complémentaires
        agro_meta <- sondage_meta
        
        saveRDS(agro_meta, 'metadonnees/sondage_meta.RDS')
        write_csv2(Valeurs_NA, 'metadonnees/Repertoire_Valeurs_NAs.csv')
        
        

        
# Pour la reproductibilité        
sessionInfo()