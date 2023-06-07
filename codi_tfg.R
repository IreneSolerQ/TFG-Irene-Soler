####################################
##############PAQUETS###############
###################################
options(repos = "https://cran.rediris.es/")
library(readxl)#llegir excel

library(tidyverse);library(dplyr)#tractament dades

#alpha crobatch

library(psych)
#mapes:
library(sf)
library(rgdal)
#variabilitat dades
library(caret)
#taules
library(knitr)
library(kableExtra)
#gràfics
library(ggplot2)
#matriu de grafics
library(gridExtra)


####################################
##############CARREGA DADES########
###################################
bbdd_cat <- read_excel("bbdd_cat.xlsx", 
                       sheet = "Datos sin procesar", col_names = FALSE)
bbdd_caste <- read_excel("bbdd_caste.xlsx", 
                         sheet = "Datos sin procesar", col_names = FALSE)

colnames(bbdd_caste)<- bbdd_caste[1,]
bbdd_caste <- bbdd_caste[-1,-c(2:18, 95:101)]#eliminem la primera fila que eren els noms de les columnes
#també hem eliminat columnes que no interessen, que són propiament de l'aplicació
colnames(bbdd_cat)<- bbdd_cat[1,]
bbdd_cat <- bbdd_cat[-1,-c(2:18, 95:101)]



####################################
##############RECODIFICACIONS########
###################################

colnames(bbdd_caste) <- colnames(bbdd_cat) <- c("ID", "Politica_Privacitat", "Empresa", "Propietari?", "Directiu?",
                                                "Genere_directiu", "Genere_directiu_altres", "Any_naixement_directiu","Any_acces_carrec_directiu", "Manera_acces_carrec_directiu", "Manera_acces_carrec_directiu_altres", 
                                                "N_propietaris_directiu", "Genere_propietaris_directiu","Genere_propietari_directiu", 
                                                "Genere_1prop", "Any_naixement_1prop", "Any_acces_propietat_1prop", "Manera_acces_1prop", "Manera_acces_1prop_altres", "N_propietaris_+1prop", "Genere_propietaris_+1prop", "Genere_+1prop", "Any_naixement_+1prop", "Any_acces_propietat_+1prop", "Manera_acces_propietat_+1prop","Manera_acces_propietat_+1prop_altres","Barri", "Barri_altres", "Codi_postal","Codi_postal_altres", "Any_fundacio", "N_assalariats", "Treballadors_1", "Treballadors_2", "Treballadors_3", "Treballadors_4", "Sector", "Importacio_Nacional", "Importacio_Europa","Importacio_intercontinental", "Importacio_No", "Exportacio_Nacional", "Exportacio_Europa","Exportacio_intercontinental", "Exportacio_No","N_sucursals", "Sucursal_fora_principal", "Pandemia", "Erto", "COVID_1", "COVID_2", "COVID_3", "COVID_4", "COVID_5", "COVID_6", "COVID_7", "COVID_8", "COVID_9", "Esforc_1", "Esforc_2", "Esforc_3", "Esforc_4", "Esforc_5", "Esforc_6", "Esforc", "Fills", "Situacio_sentimental", "Situacio_sentimental_altres", "Estudis_ESO", "Estudis_Batx", "Estudis_FP", "Estudis_dipl_uni", "Estudis_llic_uni", "Estudis_postgrau_doctorat", "Estudis_no_reglats","Estudis_altres", "Hores_benestar")

bbdd <- full_join(bbdd_caste,bbdd_cat)

bbdd <- bbdd %>%
  mutate(Genere_resposta = coalesce(`Genere_+1prop`, Genere_directiu, Genere_1prop),Any_naixement_resposta = coalesce(Any_naixement_directiu, `Any_naixement_+1prop`, Any_naixement_1prop), Any_acces = coalesce(`Any_acces_propietat_+1prop`, Any_acces_propietat_1prop, Any_acces_carrec_directiu))


#eliminem les persones que no han acceptat politica de privacitat i/o els que no han posat nom d'empresa.


bbdd <- bbdd %>%filter(!is.na(Barri))

#També eliminem els que han respost que no són ni propietaris ni directius. 

bbdd <- bbdd%>%filter(!(`Propietari?`=="No"&`Directiu?`=='No'))


#CANVI ESTUDIS
estudis <- as.numeric(c(nrow(bbdd)))

altres <- c(which(bbdd$Estudis_altres!=is.na(bbdd$Estudis_altres)))
no_reglats <- c(which(bbdd$Estudis_no_reglats!=is.na(bbdd$Estudis_no_reglats)))
ESO <- c(which(bbdd$Estudis_ESO!=is.na(bbdd$Estudis_ESO)))
Batx <- c(which(bbdd$Estudis_Batx!=is.na(bbdd$Estudis_Batx)))
FP <- c(which(bbdd$Estudis_FP!=is.na(bbdd$Estudis_FP)))
uni <- c(which(bbdd$Estudis_dipl_uni!=is.na(bbdd$Estudis_dipl_uni)),which(bbdd$Estudis_llic_uni!=is.na(bbdd$Estudis_llic_uni)))
#FP_uni <- c(FP_uni,which(bbdd$Estudis_FP!=NA))
postgrau <- c(which(bbdd$Estudis_postgrau_doctorat!=is.na(bbdd$Estudis_postgrau_doctorat)))

estudis[altres] <- "Altres"; estudis[no_reglats]  <- "No_reglats"; estudis[ESO] <- "ESO"; estudis[Batx] <- "Batx"; estudis[FP] <- "FP";estudis[uni] <- "Uni";estudis[postgrau] <- "Postgrau_master_doctorat" 
bbdd$Estudis <- estudis



bbdd <- bbdd %>% mutate(Genere_resposta = recode(Genere_resposta,"Masculino" = "M", "Masculí" = "M", "Femenino" = "F", "Femení" = "F"),Propietaris = recode(`Propietari?`, "Si, comparteixo l'empresa amb altres persones"="+1prop","Sí, comparto la empresa con otras personas"="+1prop","Si, sóc l'únic/a propietari/ària"= "1prop" , "Sí, soy el/la única propietario/a"="1prop", "No"="Directiu"),N_propietaris_directiu = recode(N_propietaris_directiu, "La empresa pertenece a varias personas" = "+1prop","L'empresa pertany a diverses persones" ="+1prop", "La empresa pertenece a una sola persona"="1prop", "L'empresa pertany a una sola persona"="1prop"),Treballadors_1 = recode(Treballadors_1, "De acuerdo" = "D'acord", "Totalmente en desacuerdo" = "Totalment en desacord", "Totalmente deacuerdo" = "Totalment d'acord", "En desacuerdo" = "En desacord", "Ni de acuerdo ni en desacuerdo" = "Ni d'acord ni en desacord"),  Treballadors_2 = recode(Treballadors_2, "De acuerdo" = "D'acord", "Totalmente en desacuerdo" = "Totalment en desacord", "Totalmente deacuerdo" = "Totalment d'acord", "En desacuerdo" = "En desacord", "Ni de acuerdo ni en desacuerdo" = "Ni d'acord ni en desacord"), Treballadors_3 = recode(Treballadors_3, "De acuerdo" = "D'acord", "Totalmente en desacuerdo" = "Totalment en desacord", "Totalmente deacuerdo" = "Totalment d'acord", "En desacuerdo" = "En desacord", "Ni de acuerdo ni en desacuerdo" = "Ni d'acord ni en desacord"), Treballadors_4 = recode(Treballadors_4, "De acuerdo" = "D'acord", "Totalmente en desacuerdo" = "Totalment en desacord", "Totalmente deacuerdo" = "Totalment d'acord", "En desacuerdo" = "En desacord", "Ni de acuerdo ni en desacuerdo" = "Ni d'acord ni en desacord"), Sector = recode(Sector, "Comercio" = "Comerç", "Función pública" = "Funció pública", "Servicios de salud" = "Serveis de salut", "Servicios financieros, servicios profesionales" = "Serveis financers, serveis professionals", "Hotelería, restauración, turismo" = "Hoteleria, restauració, turisme", "Educación"  ="Educació", "Textiles, calzado" = "Tèxtils, calçat","Alimentación, bebidas, tabaco" = "Alimentació, begudes, tabac", "Construcción"  ="Construcció", "Producción de metales básicos" = "Producció de metalls bàsics","Medios de comunicación, cultura, industria gráfica" = "Mitjans de comunicació, cultura, indústria gràfica", "Ingeniería mecánica y eléctrica" = "Ingenyeria mecànica i elèctrica", "Transporte" = "Transport", "Agricultura, plantaciones, otros sectores rurales" = "Agricultura, plantacions, altres sectors rurals", "Industrias químicas" = "Indústries químiques", "Silvicultura, madera, celulosa, papel" = "Silvicultura, fusta, cel·lulosa, paper","Fabricación de material de transporte" = "Fabricació de material de transport", "Hotelería, restauració, turisme" ="Hoteleria, restauració, turisme" ), Importacio_Europa = recode(Importacio_Europa, "Europeo" = "Europeu"), Exportacio_Europa = recode(Exportacio_Europa, "Europeo" = "Europeu"), N_sucursals = recode(N_sucursals, "11 o más" = "11 o més"))

bbdd <- bbdd %>% mutate(Sucursal_fora_principal = recode(Sucursal_fora_principal, "Sí, en el resto de España" = "Espanya", "Si, a la resta d'Espanya"  ="Espanya", "Sí, en Cataluña" ="Catalunya", "Si, a Catalunya" = "Catalunya", "Sí, en Europa" ="Europa", "Si, a Europa"  ="Europa", "Sí, en el resto del mundo" = "Món"), COVID_1 = recode(COVID_1, "De acuerdo" = "D'acord", "Totalmente en desacuerdo" = "Totalment en desacord", "Totalmente de acuerdo" = "Totalment d'acord", "En desacuerdo" = "En desacord", "Ni de acuerdo ni en desacuerdo" = "Ni d'acord ni en desacord"),COVID_2 = recode(COVID_2, "De acuerdo" = "D'acord", "Totalmente en desacuerdo" = "Totalment en desacord", "Totalmente de acuerdo" = "Totalment d'acord", "En desacuerdo" = "En desacord", "Ni de acuerdo ni en desacuerdo" = "Ni d'acord ni en desacord"),COVID_3 = recode(COVID_3, "De acuerdo" = "D'acord", "Totalmente en desacuerdo" = "Totalment en desacord", "Totalmente de acuerdo" = "Totalment d'acord", "En desacuerdo" = "En desacord", "Ni de acuerdo ni en desacuerdo" = "Ni d'acord ni en desacord"),COVID_4 = recode(COVID_4, "De acuerdo" = "D'acord", "Totalmente en desacuerdo" = "Totalment en desacord", "Totalmente de acuerdo" = "Totalment d'acord", "En desacuerdo" = "En desacord", "Ni de acuerdo ni en desacuerdo" = "Ni d'acord ni en desacord"),COVID_5 = recode(COVID_5, "De acuerdo" = "D'acord", "Totalmente en desacuerdo" = "Totalment en desacord", "Totalmente de acuerdo" = "Totalment d'acord", "En desacuerdo" = "En desacord", "Ni de acuerdo ni en desacuerdo" = "Ni d'acord ni en desacord"),COVID_6 = recode(COVID_6, "De acuerdo" = "D'acord", "Totalmente en desacuerdo" = "Totalment en desacord", "Totalmente de acuerdo" = "Totalment d'acord", "En desacuerdo" = "En desacord", "Ni de acuerdo ni en desacuerdo" = "Ni d'acord ni en desacord"),COVID_7 = recode(COVID_7, "De acuerdo" = "D'acord", "Totalmente en desacuerdo" = "Totalment en desacord", "Totalmente de acuerdo" = "Totalment d'acord", "En desacuerdo" = "En desacord", "Ni de acuerdo ni en desacuerdo" = "Ni d'acord ni en desacord"),COVID_8 = recode(COVID_8, "De acuerdo" = "D'acord", "Totalmente en desacuerdo" = "Totalment en desacord", "Totalmente de acuerdo" = "Totalment d'acord", "En desacuerdo" = "En desacord", "Ni de acuerdo ni en desacuerdo" = "Ni d'acord ni en desacord"),COVID_9 = recode(COVID_9, "De acuerdo" = "D'acord", "Totalmente en desacuerdo" = "Totalment en desacord", "Totalmente de acuerdo" = "Totalment d'acord", "En desacuerdo" = "En desacord", "Ni de acuerdo ni en desacuerdo" = "Ni d'acord ni en desacord"),Esforc_1 = recode(Esforc_1, "De acuerdo" = "D'acord", "Totalmente en desacuerdo" = "Totalment en desacord", "Totalmente de acuerdo" = "Totalment d'acord", "En desacuerdo" = "En desacord", "Ni de acuerdo ni en desacuerdo" = "Ni d'acord ni en desacord"),Esforc_2 = recode(Esforc_2, "De acuerdo" = "D'acord", "Totalmente en desacuerdo" = "Totalment en desacord", "Totalmente de acuerdo" = "Totalment d'acord", "En desacuerdo" = "En desacord", "Ni de acuerdo ni en desacuerdo" = "Ni d'acord ni en desacord"),Esforc_3 = recode(Esforc_3, "De acuerdo" = "D'acord", "Totalmente en desacuerdo" = "Totalment en desacord", "Totalmente de acuerdo" = "Totalment d'acord", "En desacuerdo" = "En desacord", "Ni de acuerdo ni en desacuerdo" = "Ni d'acord ni en desacord", "Totalmente de acuerdo" = "Totalment d¡acord"),Esforc_4 = recode(Esforc_4, "De acuerdo" = "D'acord", "Totalmente en desacuerdo" = "Totalment en desacord", "Totalmente de acuerdo" = "Totalment d'acord", "En desacuerdo" = "En desacord", "Ni de acuerdo ni en desacuerdo" = "Ni d'acord ni en desacord"),Esforc_5 = recode(Esforc_5, "De acuerdo" = "D'acord", "Totalmente en desacuerdo" = "Totalment en desacord", "Totalmente de acuerdo" = "Totalment d'acord", "En desacuerdo" = "En desacord", "Ni de acuerdo ni en desacuerdo" = "Ni d'acord ni en desacord"))

bbdd <- bbdd %>% mutate(Esforc_6 = recode(Esforc_6, "De acuerdo" = "D'acord", "Totalmente en desacuerdo" = "Totalment en desacord", "Totalmente de acuerdo" = "Totalment d'acord", "En desacuerdo" = "En desacord", "Ni de acuerdo ni en desacuerdo" = "Ni d'acord ni en desacord"),Situacio_sentimental = recode(Situacio_sentimental, "Con pareja" = "Amb parella", "Prefiero no contestar" = "Prefereixo no contestar", "Sin pareja"  ="Sense parella"), N_assalariats = recode(N_assalariats, "0" = 0, "1 - 9" = 1, "10 - 49" = 2,  "50 - 199" = 3, "200 - 249"=4, "Más de 250" =5, "Més de 250" = 5), Importacio_Nacional = if_else(is.na(Importacio_Nacional), 0, 1),Importacio_Europa = if_else(is.na(Importacio_Europa), 0, 1),Importacio_intercontinental = if_else(is.na(Importacio_intercontinental), 0, 1), Importacio_No = if_else(is.na(Importacio_No),0,1),Exportacio_Nacional = if_else(is.na(Exportacio_Nacional), 0, 1),Exportacio_Europa = if_else(is.na(Exportacio_Europa), 0, 1),Exportacio_intercontinental = if_else(is.na(Exportacio_intercontinental), 0, 1), Exportacio_No = if_else(is.na(Exportacio_No),0,1))

bbdd <- bbdd %>% mutate(Hores_benestar = recode(Hores_benestar, "Más de 20h" = "Més de 20h"), Sucursal_fora_principal = recode(Sucursal_fora_principal, "Sí, en Cataluña" = "Catalunya"), Estudis_uni = coalesce(Estudis_dipl_uni, Estudis_llic_uni))

# elimino estudis llicenciatura i estudis diplomatura
bbdd <- bbdd %>% select(-Estudis_llic_uni, - Estudis_dipl_uni)



# eliminem anys de naixement de propietaris...també els anys d'acces
bbdd <- bbdd %>% select(- c(Any_naixement_directiu, `Any_acces_propietat_+1prop`, Any_naixement_1prop, Any_acces_propietat_1prop, `Any_naixement_+1prop`, `Any_acces_propietat_+1prop`, Any_acces_carrec_directiu,`Directiu?`))

#També eliminem Politica de Privacitat i Empresa
bbdd <- bbdd %>%select(-c(Politica_Privacitat, Empresa, `Propietari?`))


#CANVIS SECTORS
bbdd <- bbdd %>% mutate(Sector = recode(Sector,"Alimentació, begudes, tabac" = "C",
                                        "Serveis de salut" = "Q",
                                        "Comerç" = "G",
                                        "Tèxtils, calçat" = "C" ,
                                        "Construcció" = "F",
                                        "Hoteleria, restauració, turisme" = "I",
                                        "Educació" = "P",
                                        "Serveis financers, serveis professionals" = "K",
                                        "Producció de metalls bàsics" = "C",
                                        "Mitjans de comunicació, cultura, indústria gràfica" = "J",
                                        "Ingenyeria mecànica i elèctrica" =  "M",
                                        "Transport" = "H",
                                        "Agricultura, plantacions, altres sectors rurals" = "A",
                                        "Indústries químiques" = "C",
                                        "Silvicultura, fusta, cel·lulosa, paper" = "A",
                                        "Funció pública" = "O",
                                        "Servicios  públicos (agua, gas, electricidad)" = "O",
                                        "Fabricació de material de transport" = "C" ,
                                        "Medis de comunicació, cultura, indústria gràfica" = "J",
                                        "Ingeniería mecànica i elèctrica" =  "M",z = "O"))

bbdd <- bbdd %>% mutate(Sector = recode(Sector, "A" = "Agricultura", "C" = "Fabricació", "F" = "Construcció", "G" = "Serveis de mercat", "H" = "Serveis de mercat", "I" = "Serveis de mercat", "J" = "Serveis de mercat", "K" = "Serveis de mercat", "M" = "Serveis de mercat", "O" ="Serveis de no mercat", "P" = "Serveis de no mercat", "Q" = "Serveis de no mercat", "Servicios  públicos (agua, gas, electricidad)" = "Serveis de no mercat"))


#MIREM DUPLICATS
duplicados <- duplicated(bbdd)
bbdd[duplicados,]


###########################################
######AJUNTAR VARIABLES I CATEGORIES########
##########################################

bbdd$Sucursal_fora_principal <- ifelse(bbdd$N_sucursals == "0.0", "No", bbdd$Sucursal_fora_principal)


#nombre propietaris

bbdd$N_propietaris <- ifelse(bbdd$Propietaris == "Directiu",
                             ifelse(bbdd$N_propietaris_directiu == "+1prop", "+1prop", "1prop"),
                             bbdd$Propietaris)
bbdd <- bbdd %>% select(-N_propietaris_directiu)

#estudis
bbdd <- bbdd %>% mutate(Estudis = recode(Estudis, "FP" = "FP_Batx", "Batx" = "FP_Batx"))
bbdd <- bbdd %>% select(-c(Estudis_altres, Estudis_Batx, Estudis_FP, Estudis_postgrau_doctorat, Estudis_no_reglats, Estudis_uni, Estudis_ESO))


#ajuntem el genere de l'empresa
bbdd <- bbdd %>% mutate(Genere_empresa = coalesce(Genere_propietaris_directiu, Genere_propietari_directiu, `Genere_propietaris_+1prop`,Genere_1prop),Genere_empresa = recode(Genere_empresa, "Más hombres que mujeres" = "Més homes que dones", "Más mujeres que hombres" = "Més dones que homes"),Genere_empresa = ifelse(Propietaris=="1prop"& !is.na(Genere_resposta), Genere_resposta,Genere_empresa), Genere_empresa = recode(Genere_empresa, "Més dones que homes"  ="F", "Més homes que dones" = "M", "Proporcional" = "Mixt", "Masculí" ="M", "Masculino" ="M", "Femenino" ="F", "Femení"="F"))

#eliminem les variables "Genere_propietaris_+1prop", "Genere_propietari_directiu"
#eliminem les variables "Genere_1prop", "Genere_+1prop", "Genere_directiu"
#eliminem manera acces a la propietat 1prop i +1prop
bbdd <- bbdd %>% select(-c(Genere_propietari_directiu, Genere_propietaris_directiu,`Genere_propietaris_+1prop`,`Genere_+1prop`, Genere_1prop, Genere_directiu, Genere_directiu_altres))


table(bbdd$Barri)
indices_other <- which(tolower(bbdd$Barri) == "other")
indices_centre <- c(grep("(?i)Centre", bbdd$Barri_altres[indices_other]),grep("(?i)Centro", bbdd$Barri_altres[indices_other]),grep("(?i)Gavatxons", bbdd$Barri_altres[indices_other]),grep("(?i)Rambla", bbdd$Barri_altres[indices_other]))
bbdd$Barri[indices_other[indices_centre]] <- "Centre"

indices_esport <- grep("(?i)Esportiva", bbdd$Barri_altres[indices_other])
bbdd$Barri[indices_other[indices_esport]] <- "Poble Nou - Zona Esportiva"
bbdd$Barri_altres[indices_other[indices_esport]] <- NA


bbdd <- bbdd %>% mutate(Barri = recode(Barri, "Can Boada Casc Antic" = "Can Boada", "Can Boada del Pi" = "Can Boada") )

# ho ajuntem a districtes
bbdd <- bbdd %>% mutate(Districte = recode(Barri, "Antic Poble de Sant Pere" = "D1", "Ca n'Anglada" = "D2", "Ca n'Aurell" = "D4", "Can Boada" = "D5", "Can Jofresa" = "D3", "Can Palet" = "D3", "Can Palet de Vista Alegre" = "D4", "Can Parellada" = "D7", "Can Roca" = "D5", "Can Tusell" = "D6", "Cementiri Vell" = "D1", "Centre" = "D1", "Disseminats - D4" = "D4", "Ègara" = "D6", "La Cogullada" = "D4", "La Maurina" = "D4", "Les Arenes - La Grípia - Can Montllor" = "D6", "Les Fonts" ="D7", "P.I. Can Parellada" = "D3", "P.I. Can Petit" = "D6", "P.I. Catalunya - Esc. Industrial" = "D1", "P.I. Els Bellots" = "D3", "P.I. Santa Eulàlia" = "D3", "P.I. Santa Margarida" = "D3", "Pla del Bon Aire" = "D5", "Poble Nou - Zona Esportiva" = "D5", "Roc Blanc" = "D4", "Sant Llorenç" = "D6", "Sant Pere" = "D5", "Sant Pere Nord" = "D6", "Segle XX" = "D3", "Torrent d'en Pere Parres" = "D5", "Vallparadís" = "D1", "Montserrat" = "D2", "P.I. Puigbarral"  ="D6", "Disseminats - D2" ="D2", "Disseminats - D6" ="D6", "Torre Sana" = "D2"))

bbdd <- bbdd %>% select(- Barri, - Codi_postal)

# eliminem la variable barri_altres

bbdd <- bbdd %>% select(-Barri_altres, - Codi_postal_altres)


table(bbdd$Situacio_sentimental_altres)
indices_other <- which(tolower(bbdd$Situacio_sentimental) == "other")
indices_viuda <- c(grep("(?i)viuda", bbdd$Situacio_sentimental_altres[indices_other]),grep("(?i)viudo", bbdd$Situacio_sentimental_altres[indices_other]))

bbdd$Situacio_sentimental[indices_other[indices_viuda]] <- "Sense parella"
indices_separada <- c(grep("(?i)Separada", bbdd$Situacio_sentimental_altres[indices_other]))
bbdd$Situacio_sentimental[indices_other[indices_separada]] <- "Sense parella"
bbdd$Situacio_sentimental_altres[indices_other[indices_separada]] <- NA
bbdd$Situacio_sentimental_altres[indices_other[indices_viuda]] <- NA


#eliminem la opció altres
bbdd <- bbdd %>% select(-Situacio_sentimental_altres)


bbdd <- bbdd %>% mutate(Fills_binari = ifelse(Fills>0, "1", "0"))
class(bbdd$Fills_binari)
table(bbdd$Fills_binari)
198/(55+198)
table(bbdd$Fills)


table(bbdd$Manera_acces_carrec_directiu_altres)

indices_other <- c(which(tolower(bbdd$Manera_acces_carrec_directiu) == "altres"),which(tolower(bbdd$Manera_acces_carrec_directiu) == "other"))

indices_familia <- c(grep("(?i)Familiar", bbdd$Manera_acces_carrec_directiu_altres[indices_other]), grep("(?i)Marido", bbdd$Manera_acces_carrec_directiu_altres[indices_other]),grep("(?i)Parentesco", bbdd$Manera_acces_carrec_directiu_altres[indices_other]),grep("(?i)Coneixement directe", bbdd$Manera_acces_carrec_directiu_altres[indices_other]),grep("(?i)Conocian", bbdd$Manera_acces_carrec_directiu_altres[indices_other]))

bbdd$Manera_acces_carrec_directiu[indices_other[indices_familia]] <- "Familiar"

bbdd$Manera_acces_carrec_directiu_altres[indices_other[indices_familia]] <- NA

bbdd <- bbdd %>% select(-Manera_acces_carrec_directiu_altres)

# Passa el mateix amb la manera d'accès dels que tenen propietat compartida
table(bbdd$Manera_acces_1prop_altres)
table(bbdd$`Manera_acces_propietat_+1prop_altres`)
indices_other <- which(tolower(bbdd$`Manera_acces_propietat_+1prop`) == "other")

indices_familia <- c(grep("(?i)Familiar", bbdd$`Manera_acces_propietat_+1prop_altres`[indices_other]),grep("(?i)Mujer", bbdd$`Manera_acces_propietat_+1prop_altres`[indices_other]),grep("(?i)Esposa", bbdd$`Manera_acces_propietat_+1prop_altres`[indices_other]))

bbdd$`Manera_acces_propietat_+1prop`[indices_other[indices_familia]] <- "Familiar"

indices_compra <- c(grep("(?i)Compra", bbdd$`Manera_acces_propietat_+1prop_altres`[indices_other]))

bbdd$`Manera_acces_propietat_+1prop`[indices_other[indices_compra]] <- "Adquisició"


bbdd$`Manera_acces_propietat_+1prop_altres`[indices_other[indices_compra]] <- NA
bbdd$`Manera_acces_propietat_+1prop_altres`[indices_other[indices_familia]] <- NA


bbdd <- bbdd %>% mutate(Manera_acces_propietat = coalesce(Manera_acces_1prop, `Manera_acces_propietat_+1prop`))


bbdd <- bbdd %>% mutate(Manera_acces_propietat = recode(Manera_acces_propietat, "Adquirí la empresa" = "Adquisició", "Fusión/Adquisición de la empresa" = "Fusió", "He fundado la empresa" = "Fundació", "He fundat la empresa"  ="Fundació", "Herdé la empresa" = "Herència","Heredé la empresa" = "Herència", "Other" ="Altres", "Heredé participaciones en la empresa" = "Herència", "La empresa la fundamos varios/as socios/as" = "Fundació","Soy el fundador principal de la empresa" ="Fundació", "L'empresa la vam fundar diversos/es socis/es" ="Fundació", "Vaig adquirir l'empresa" ="Adquisició","Vaig heretar participaciones en l'empresa" ="Herència", "Sóc el fundador principal de l'empresa" ="Fundació"), Manera_acces_carrec_directiu = recode(Manera_acces_carrec_directiu, "Promoción interna" = "Promoció interna", "Proceso selección" = "Procés de selecció", "Other" = "Altres"))

bbdd <- bbdd %>% select(- c(Manera_acces_1prop_altres, Manera_acces_1prop, `Manera_acces_propietat_+1prop`, `Manera_acces_propietat_+1prop_altres`))



######################################
##########CANVIS FACTOR A NUMERIC#####
######################################

afactor<-select(bbdd,-ID, -Any_naixement_resposta, -Any_acces, -Any_fundacio, -Esforc, -Fills, - `N_propietaris_+1prop`)
#Vector var factor
names<-names(afactor)
#Cambio a factor
bbdd[names]<-lapply(bbdd[names], factor)


num<-select(bbdd, Any_naixement_resposta, Any_acces, Any_fundacio, Esforc, Fills)
nom <- names(num)
bbdd[nom] <- lapply(bbdd[nom], as.numeric)


######################################
##########ALPHA CRONBACH #############
######################################

#COVID
bbdd <- bbdd %>% mutate(COVID_1 = recode(COVID_1, "Totalment en desacord" = "1", "En desacord" = "2", "Ni d'acord ni en desacord" ="3", "D'acord" = "4", "Totalment d'acord" ="5") ,COVID_2 = recode(COVID_2, "Totalment d'acord" = "1", "D'acord" = "2", "En desacord" = "4", "Totalment en desacord" = "5", "Ni d'acord ni en desacord" = "3"),COVID_3 = recode(COVID_3, "Totalment en desacord" = "1", "En desacord" = "2", "Ni d'acord ni en desacord" ="3", "D'acord" = "4", "Totalment d'acord" ="5"),COVID_4 = recode(COVID_4, "Totalment en desacord" = "1", "En desacord" = "2", "Ni d'acord ni en desacord" ="3", "D'acord" = "4", "Totalment d'acord" ="5"),COVID_5 = recode(COVID_5, "Totalment en desacord" = "1", "En desacord" = "2", "Ni d'acord ni en desacord" ="3", "D'acord" = "4", "Totalment d'acord" ="5"),COVID_6 = recode(COVID_6, "Totalment d'acord" = "1", "D'acord" = "2", "En desacord" = "4", "Totalment en desacord" = "5", "Ni d'acord ni en desacord" = "3"),COVID_7 = recode(COVID_7, "Totalment en desacord" = "1", "En desacord" = "2", "Ni d'acord ni en desacord" = "3", "D'acord" = "4", "Totalment d'acord" = "5"),COVID_8 = recode(COVID_8, "Totalment d'acord" = "1", "D'acord" = "2", "En desacord" = "4", "Totalment en desacord" = "5", "Ni d'acord ni en desacord" = "3"),COVID_9 = recode(COVID_9, "Totalment en desacord" = "1", "En desacord" = "2", "Ni d'acord ni en desacord" = "3", "D'acord" = "4", "Totalment d'acord" = "5"))
covid <- data.frame(bbdd$COVID_1,bbdd$COVID_2,bbdd$COVID_3,bbdd$COVID_4,bbdd$COVID_5,bbdd$COVID_6,bbdd$COVID_7,bbdd$COVID_8,bbdd$COVID_9)

covid <- covid[complete.cases(covid),]
covid <- covid %>% mutate(bbdd.COVID_1 = as.numeric(bbdd.COVID_1),bbdd.COVID_2 = as.numeric(bbdd.COVID_2),bbdd.COVID_3 = as.numeric(bbdd.COVID_3),bbdd.COVID_4 = as.numeric(bbdd.COVID_4),bbdd.COVID_5 = as.numeric(bbdd.COVID_5),bbdd.COVID_6 = as.numeric(bbdd.COVID_6),bbdd.COVID_7 = as.numeric(bbdd.COVID_7),bbdd.COVID_8 = as.numeric(bbdd.COVID_8),bbdd.COVID_9 = as.numeric(bbdd.COVID_9))

#re codificar les respostes per a que les inverses coincideixin i es puguin comparar.

#posar categories en 1 2 3 4 5 i calcular la covariança. Ha de sortir positiva tota. Abans s'ha de fer els canvis de 1-5.

bbdd <- bbdd %>% mutate(Esforc_1 = recode(Esforc_1, "Totalment en desacord" = "1", "En desacord" = "2", "Ni d'acord ni en desacord" ="3", "D'acord" = "4", "Totalment d'acord" ="5"), Esforc_2 = recode(Esforc_2,"Totalment en desacord" = "1", "En desacord" = "2", "Ni d'acord ni en desacord" ="3", "D'acord" = "4", "Totalment d'acord" ="5"), Treballadors_1 = recode(Treballadors_1, "Totalment en desacord" = "1", "En desacord" = "2", "Ni d'acord ni en desacord" ="3", "D'acord" = "4", "Totalment d'acord" ="5"), Treballadors_4 = recode(Treballadors_4,"Totalment d'acord" = "1", "D'acord" = "2", "En desacord" = "4", "Totalment en desacord" = "5", "Ni d'acord ni en desacord" = "3"), Esforc_3 = recode(Esforc_3, "Totalment en desacord" = "1", "En desacord" = "2", "Ni d'acord ni en desacord" ="3", "D'acord" = "4", "Totalment d'acord" ="5"), Esforc_4 = recode(Esforc_4, "Totalment en desacord" = "1", "En desacord" = "2", "Ni d'acord ni en desacord" ="3", "D'acord" = "4", "Totalment d'acord" ="5"), Esforc_5 = recode(Esforc_5, "Totalment d'acord" = "1", "D'acord" = "2", "En desacord" = "4", "Totalment en desacord" = "5", "Ni d'acord ni en desacord" = "3"), Treballadors_2 = recode(Treballadors_2, "Totalment d'acord" = "1", "D'acord" = "2", "En desacord" = "4", "Totalment en desacord" = "5", "Ni d'acord ni en desacord" = "3"), Treballadors_4 = recode(Treballadors_4, "Totalment en desacord" = "1", "En desacord" = "2", "Ni d'acord ni en desacord" ="3", "D'acord" = "4", "Totalment d'acord" ="5"), Esforc_6 = recode(Esforc_6, "Totalment en desacord" = "5", "En desacord" = "4", "Ni d'acord ni en desacord" ="3", "D'acord" = "2", "Totalment d'acord" ="1"), Treballadors_3 = recode(Treballadors_3,"Totalment en desacord" = "1", "En desacord" = "2", "Ni d'acord ni en desacord" ="3", "D'acord" = "4", "Totalment d'acord" ="5"))

 #ESFORÇ
Esforc_treballadors <- select(bbdd,Esforc_1, Esforc_2, Esforc_3, Esforc_4, Esforc_5, Esforc_6, Treballadors_1, Treballadors_2, Treballadors_3, Treballadors_4) 
Esforc_treballadors <- subset(Esforc_treballadors, complete.cases(Esforc_treballadors))
Esforc_treballadors <- Esforc_treballadors %>% mutate(Treballadors_1 = as.numeric(Treballadors_1),Treballadors_2 = as.numeric(Treballadors_2),Treballadors_3 = as.numeric(Treballadors_3),Treballadors_4 = as.numeric(Treballadors_4),Esforc_1 = as.numeric(Esforc_1),Esforc_2 = as.numeric(Esforc_2),Esforc_3 = as.numeric(Esforc_3),Esforc_4 = as.numeric(Esforc_4),Esforc_5 = as.numeric(Esforc_5), Esforc_6 = as.numeric(Esforc_6))

# amb treballadors
round(cov(Esforc_treballadors),2) #variable Esforc_5[-5] negatius 
round(cov(Esforc_treballadors[-5]),2) #matriu treballadors_4[10] i Esforc_3[3]
round(cov(Esforc_treballadors[-c(3,5)]),2) #sense esforc_3 ni esforc_5
round(cov(Esforc_treballadors[-c(5,10)]),2) #sense treballadors_4 ni esforc_5

alpha_esforc_treballadors_510 <- psych::alpha(round(cov(Esforc_treballadors[-c(5,10)]),2)) #eliminant esforc_5 i treballadors_4
alpha_esforc_treballadors_510$total$raw_alpha
alpha_esforc_35 <- psych::alpha(round(cov(Esforc_treballadors[-c(3, 5)]),2))# eliminant esforc_3 i esforc_5
alpha_esforc_35$total$raw_alpha

  Esforc_no_treballadors <- subset(bbdd,  N_assalariats !=0,select = c("Esforc_1", "Esforc_2", "Esforc_3", "Esforc_4", "Esforc_5", "Esforc_6"))

Esforc_no_treballadors <- Esforc_no_treballadors[complete.cases(Esforc_no_treballadors),]

Esforc_no_treballadors <- Esforc_no_treballadors %>% mutate(Esforc_1 = as.numeric(Esforc_1),Esforc_2 = as.numeric(Esforc_2),Esforc_3 = as.numeric(Esforc_3),Esforc_4 = as.numeric(Esforc_4),Esforc_5 = as.numeric(Esforc_5), Esforc_6 = as.numeric(Esforc_6))

#primera covariancia
round(cov(Esforc_no_treballadors),2) #variable Esforc_5[-5]
round(cov(Esforc_no_treballadors[-5]),2)

#segona 
alpha_esforc_no_treballadors_5 <- psych::alpha(Esforc_no_treballadors[-5]) #eliminant esforc_5 
alpha_esforc_no_treballadors_5$total$raw_alpha



# ALPHA COVID
corr <- round(cov(covid),2)

c <- psych::alpha(cov(covid))
c$total$raw_alpha

mean(corr)

#eliminar variables

bbdd <- bbdd %>% select(-c(Treballadors_1, Treballadors_2, Treballadors_3, Treballadors_4, COVID_1, COVID_2, COVID_3, COVID_4, COVID_5, COVID_6, COVID_7, COVID_8, COVID_9, Esforc_1, Esforc_2, Esforc_3, Esforc_4, Esforc_5, Esforc_6, Pandemia, Erto))


##########################################
#############CREACIÓ VARIABLES###########
##############################################
bbdd <- bbdd %>% mutate(Edat = 2023- as.numeric(Any_naixement_resposta))
bbdd <- bbdd %>% mutate(Edat_acces = as.numeric(Any_acces) - as.numeric(Any_naixement_resposta))
bbdd <- bbdd %>% mutate(Anys_empresa =  2023 - as.numeric(bbdd$Any_acces))
bbdd <- bbdd %>% mutate(Antiguitat_empresa =  2023 - as.numeric(bbdd$Any_fundacio))




##########################################
#############VARIABLES POC INFORMATIVES###
##############################################

require(tidyverse)
data_long <- bbdd%>%gather(key = "variable", value = "valor")
data_long %>%group_by(variable) %>%
  dplyr::summarize(porcentaje_NA = 100 * sum(is.na(valor)) / length(valor)) %>%ggplot(aes(x = reorder(variable, desc(porcentaje_NA)), y = porcentaje_NA)) +
  geom_col() +labs(title = "Percentatge de valors perduts",
                   x = "", y = "NA's") +theme_bw()+ theme(axis.text.x=element_text(angle=90))

table(bbdd$Propietaris)
1 - 68/(125+60+68) # directius = 26.88% 


nzv_vars <- nearZeroVar(bbdd)
bbdd[,nzv_vars] 





############################################
###########categories poca representacio####
#########################################


afactor<-select(bbdd,-ID, -Any_naixement_resposta, -Any_acces, -Any_fundacio, -Esforc, -Fills, - `N_propietaris_+1prop`, -Edat, -Any_acces, -Anys_empresa, -Edat_acces)
#Vector var factor
names<-names(afactor)
#Cambio a factor
bbdd[names]<-lapply(bbdd[names], factor)


num<-select(bbdd, Any_naixement_resposta, Any_acces, Any_fundacio, Esforc, Fills)
nom <- names(num)
bbdd[nom] <- lapply(bbdd[nom], as.numeric)


repre <- function(var, perc){
  vector <- numeric()
  for(i in 1:length(var)){
    subs <- subset(bbdd,select=var[i])
    taula <- table(subs)/sum(!is.na(subs)) < perc
    if(any(taula)==TRUE){
      vector <- c(vector, i)
    }
  }
  return(vector)
}
repre(names, 0.05)
names[repre(names, 0.05)]


bbdd <- bbdd %>% mutate(N_assalariats = recode(N_assalariats, "5" = "3", "4" = "3"))

bbdd <- bbdd %>% mutate(N_sucursals = recode(N_sucursals, "11 o més" = "6 o més", "6 - 10" = "6 o més"))

bbdd <- bbdd %>% filter(Sector != "Agricultura")

bbdd$Sector <- droplevels(bbdd$Sector)

bbdd <- bbdd %>% mutate(Estudis = recode(Estudis, "Altres" = NA_character_, "No_reglats" = NA_character_))

bbdd <- bbdd %>% mutate(Situacio_sentimental = recode(Situacio_sentimental, "Other" = NA_character_))

table(bbdd$Manera_acces_propietat)
bbdd <- bbdd %>% mutate(Manera_acces_carrec_directiu = recode(Manera_acces_carrec_directiu, "Familiar" = "Altres"))



###################################
#########valors extrems############
######################################
#any naixement
any_naixement_extrems <- ggplot(bbdd, aes(x=Any_naixement_resposta))+geom_boxplot()+
  geom_point(aes(x = min(bbdd$Any_naixement_resposta), y = 0), size = 1) +
  geom_point(aes(x = max(bbdd$Any_naixement_resposta), y = 0), size = 1) +
  annotate("text", x=min(bbdd$Any_naixement_resposta), y=-0.1, 
           label=paste0(min(bbdd$Any_naixement_resposta)), size=3) +
  annotate("text", x=max(bbdd$Any_naixement_resposta), y=-0.1, 
           label=paste0(max(bbdd$Any_naixement_resposta)), size=3)

#Edat
edat_extrems <- ggplot(bbdd, aes(x=Edat))+geom_boxplot()+
  geom_point(aes(x = min(bbdd$Edat), y = 0), size = 1) +
  geom_point(aes(x = max(bbdd$Edat), y = 0), size = 1) +
  annotate("text", x=min(bbdd$Edat), y=-0.1, 
           label=paste0(min(bbdd$Edat)), size=3) +
  annotate("text", x=max(bbdd$Edat), y=-0.1, 
           label=paste0(max(bbdd$Edat)), size=3)

#Edat acces
# Eliminar filas con valores faltantes
bbdd_edat <- bbdd[!is.na(bbdd$Edat_acces),]

edat_acces_extrem <- ggplot(bbdd_edat, aes(x=bbdd_edat$Edat_acces))+geom_boxplot()+
  geom_point(aes(x = min(bbdd_edat$Edat_acces), y = 0), size = 1) +
  geom_point(aes(x = max(bbdd_edat$Edat_acces), y = 0), size = 1) +
  annotate("text", x=min(bbdd_edat$Edat_acces), y=-0.1, 
           label=paste0(min(bbdd_edat$Edat_acces)), size=3) +
  annotate("text", x=max(bbdd_edat$Edat_acces), y=-0.1, 
           label=paste0(max(bbdd_edat$Edat_acces)), size=3)

#subset(bbdd, Edat_acces_propietat < 16 & !is.na(Edat_acces_propietat))
#un cas de 12 anys


#Any fundacio
any_fundacio_extrem <- ggplot(bbdd, aes(x=Any_fundacio))+geom_boxplot()+
  geom_point(aes(x = min(bbdd$Any_fundacio), y = 0), size = 1) +
  geom_point(aes(x = max(bbdd$Any_fundacio), y = 0), size = 1) +
  annotate("text", x=min(bbdd$Any_fundacio), y=-0.05, 
           label=paste0(min(bbdd$Any_fundacio)), size=3) +
  annotate("text", x=max(bbdd$Any_fundacio), y=-0.05, 
           label=paste0(max(bbdd$Any_fundacio)), size=3)

summary(bbdd$Any_fundacio) #el minim es 2.007, es canvia a 2007
bbdd$Any_fundacio <- replace(bbdd$Any_fundacio,bbdd$Any_fundacio==min(bbdd$Any_fundacio, na.rm = TRUE), 2007)
summary(bbdd$Any_fundacio)


# Fills
fills_extrems <-  ggplot(bbdd, aes(x=bbdd$Fills))+geom_boxplot()+
  geom_point(aes(x = min(bbdd$Fills), y = 0), size = 1) +
  annotate("text", x=min(bbdd$Fills), y=-0.1, 
           label=paste0(min(bbdd$Fills)), size=3) 
# hi ha un cas amb 98 fills, el passem a NA

bbdd$Fills[bbdd$Fills == 98] <- NA
bbdd <- subset(bbdd, !(Edat_acces == 12))


#importacio exportacio
importacio <- select(bbdd, Importacio_Nacional, Importacio_Europa, Importacio_intercontinental, Importacio_No)

importacio <- importacio %>% mutate(Importacio_Nacional = as.numeric(as.character(Importacio_Nacional)), Importacio_Europa = as.numeric(as.character(Importacio_Europa)), Importacio_intercontinental = as.numeric(as.character(Importacio_intercontinental)), Importacio_No = as.numeric(as.character(Importacio_No)))
for(i in 1:nrow(importacio)){
  if(importacio$Importacio_No[i]=="1"){
    s <- sum(importacio[i,])
    if(s!="1")
      print(c(importacio[i,],i))
  }
  
}

exportacio <- select(bbdd, Exportacio_Nacional, Exportacio_Europa, Exportacio_intercontinental, Exportacio_No)
exportacio <- exportacio %>% mutate(Exportacio_Nacional = as.numeric(as.character(Exportacio_Nacional)), Exportacio_Europa = as.numeric(as.character(Exportacio_Europa)), Exportacio_intercontinental = as.numeric(as.character(Exportacio_intercontinental)), Exportacio_No = as.numeric(as.character(Exportacio_No)))

for(i in 1:nrow(exportacio)){
  if(exportacio$Exportacio_No[i]==1){
    s <- sum(exportacio[i,])
    if(s!=1)
      print(c(exportacio[i,],i))
  }
  
}
#columnes de 7 a 14
#cas 179 i 238
bbdd[179,7:14 ] <- NA

# anys acces
subset(bbdd, Any_acces < round(Any_fundacio, 0) & !is.na(Any_acces) & !is.na(Any_fundacio))# hi ha dos casos que varien d'un any amunt 

bbdd <- bbdd %>% mutate(Any_acces = ifelse(Any_acces < round(Any_fundacio, 0) & !is.na(Any_acces) & !is.na(Any_fundacio) , NA, Any_acces))

bbdd <- bbdd %>% mutate(Any_fundacio = ifelse(Any_acces < round(Any_fundacio, 0) & !is.na(Any_acces) & !is.na(Any_fundacio) , NA, Any_fundacio))

bbdd[bbdd$Edat_acces<16,]#valor no útil. No té sentit. No tindrem en compte aquesta resposta per analisis d'anys.

# com tenim files on les dates no tenen sentit(edat_acces<16 , o Any_acces<Any_fundacio), agafem només aquelles que si que tenen sentit

bbdd[bbdd$Any_acces<bbdd$Any_fundacio | bbdd$Edat_acces<16,] #11persones han contestat malament


##########################################################
################# ANÀLISI DESCRIPTIVA######################
#########################################################

#GRAFICS GEOGRAFICS
library(sf)
library(rgdal)

datos_mapa <- readOGR(dsn = "C:/Users/irene/OneDrive/Escritorio/irene/UNI/TFG/pt_dist.shp", layer="pt_dist", verbose= FALSE)

respostes1 <- read_excel("respostes_districtes.xlsx")
respostes1$ordre <- c(3,5,6,4,1,2,7)
respostes <- respostes1[order(respostes1$ordre),]
# Obtener las coordenadas del centro de cada polígono de distrito
centros <- st_centroid(sf::st_as_sf(datos_mapa, coords = c("long", "lat")))
datos_mapa_sf <- st_as_sf(datos_mapa)

# Crear un objeto de datos que contenga los nombres de los distritos y sus coordenadas
nombres_distritos <- data.frame(DISTRICTE = unique(datos_mapa$DISTRICTE),
                                nombre = c("D5", "D6", "D1", "D4","D2", "D3", "D7"),
                                coords = centros)

respostes$percentatges <- round((respostes$Respostes)/(sum(respostes$Respostes))*100,2)
datos_mapa_sf <- datos_mapa_sf[order(datos_mapa_sf$PT_DIST_ID),]

distr_respostes <- ggplot() +
  geom_sf(data = datos_mapa_sf, aes(fill = respostes$Respostes, group = PT_DIST_ID ))+ 
  geom_sf_text(data = datos_mapa_sf, aes(label = nombres_distritos$nombre), color = "black", size = 2, nudge_x = 0, nudge_y = 0) +
  geom_sf_label(data = datos_mapa_sf, aes(label = paste(paste(nombres_distritos$nombre, respostes$percentatges, sep = "\n"), "%")), color = "black", size = 1.7, nudge_x = 0, nudge_y = 0, label.padding = unit(0.2, "lines"), label.r = unit(0.2, "npc")) +
  scale_fill_gradient(low = "white", high = "#8A2BE2") +
  labs(fill = paste("Nombre de", "respostes", sep="\n"), title = "% de les respostes totals segons el districte") +
  theme_minimal() +
  theme(plot.title = element_text(size = 10),
        axis.text.x = element_text(angle = 45, hjust = 1, vjust = 0.5, size = 6),
        axis.title.y = element_blank(),
        axis.title.x = element_blank(),
        legend.title = element_text(size = 8),  # mesura títol llegenda
        axis.text.y = element_text(size = 6))  # mesura etiquetes eix y

########################################
########################################
n.empreses <- data.frame("Districtes" = c("D1", "D2", "D3", "D4", "D5", "D6", "D7"),"N.empreses" = c(1224, 384, 1214,779,686, 730, 286))
n.empreses$perc <- round((n.empreses$N.empreses)/(sum(n.empreses$N.empreses))*100,2)
n.empreses$ordre <- c(3,5,6,4,1,2,7)
n.empreses <- n.empreses[order(n.empreses$ordre),]

empreses <- ggplot() +
  geom_sf(data = datos_mapa_sf, aes(fill = n.empreses$N.empreses, group = DISTRICTE)) +
  geom_sf_text(data = datos_mapa_sf, aes(label = nombres_distritos$nombre), color = "black", size = 2, nudge_x = 0, nudge_y = 0) +
  geom_sf_label(data = datos_mapa_sf, aes(label = paste(paste(nombres_distritos$nombre, n.empreses$perc, sep = "\n"), "%")), color = "black", size = 1.7, nudge_x = 0, nudge_y = 0, label.padding = unit(0.2, "lines"), label.r = unit(0.2, "npc")) +
  scale_fill_gradient(low = "white", high = "#8A2BE2") +
  labs(fill = paste("Nombre", "d'empreses",sep="\n"), title = "% de les empreses totals segons el districte") +
  theme_minimal() +
  theme(plot.title = element_text(size = 10),
        axis.text.x = element_text(angle = 45, hjust = 1, vjust = 0.5, size = 6),
        axis.title.y = element_blank(),
        axis.title.x = element_blank(),
        legend.title = element_text(size = 8),  # mesura títol llegenda
        axis.text.y = element_text(size = 6))  # mesura etiquetes eix y

grid.arrange(empreses,distr_respostes, nrow=1)
#####
#PERFIL ENQUESTATS
#genere resposta
per <- round(table(bbdd$Genere_resposta)/sum(table(bbdd$Genere_resposta)) * 100, 1)
df <- data.frame(Genere = names(per), counts = as.numeric(table(bbdd$Genere_resposta)), per = per)
colorgenere <- c("F" = "#FFDDA1", "M" = "#7AD7B0")

# Crear el gráfico tipo donut
pgen <- ggplot(data = df, aes(x = "", y = counts, fill = Genere)) +
  geom_bar(width = 1, stat = "identity", color = "white") +
  coord_polar(theta = "y", start = 0) +
  ggtitle("Gènere de la persona enquestada") +
  labs(fill = "Gènere")

pgen <- pgen + theme_void() + scale_fill_manual(values = colorgenere) + 
  geom_text(aes(label = paste0(counts, "\n", per, "%")),
            position = position_stack(vjust = 0.5)) + 
  theme(plot.title = element_text(hjust = 0.5, size = 13))


#genere empresa
counts <- table(bbdd$Genere_empresa)
per <- round(counts/sum(counts) * 100, 1)
df <- data.frame(categories = names(counts), counts = as.numeric(counts), per = per)
colorgenereempresa <- c("F" = "#FFDDA1", "M" = "#7AD7B0", "Mixt" = "#E8E391")

gen.empresa <- ggplot(data = df, aes(x = "", y = counts, fill = categories)) + 
  geom_bar(width = 1, stat = "identity", color = "white") +
  coord_polar(theta = "y", start=0) +
  ggtitle("Gènere d'empresa") + labs(fill = "Gènere")

gen.empresa <- gen.empresa + theme_void() +
        scale_fill_manual(values = colorgenereempresa) +
        geom_text(aes(label = paste0(counts, "\n", per, "%")),
                  position = position_stack(vjust = 0.5))+ 
        theme(plot.title = element_text(hjust = 0.5, size = 13))

#edats 
df <- data.frame(bbdd$Edat, bbdd$Genere_resposta)
# Agrupar las edades en intervalos de 10 en 10
df$grupos_edad <- cut(bbdd$Edat, breaks = seq(15, 85, by = 10))
etiquetas <- c("15 a 25", "26 a 35", "36 a 45", "46 a 55", "56 a 65", "66 a 75", "76 a 85")
# Calcular la frecuencia total por grupos de edad
freq_totales <- table(df$grupos_edad)

# Calcular la frecuencia por grupos de edad y género
freq_genero <- table(df$grupos_edad, df$bbdd.Genere_resposta)

# Convertir las tablas de frecuencias en data frames
df_totales <- data.frame(grupos_edad = as.character(names(freq_totales)),
                         freq_tot = as.numeric(freq_totales))
df_genero <- as.data.frame.table(freq_genero)
names(df_genero) <- c("grupos_edad", "Genere_resposta", "freq_gen")

# Crear el gráfico de barras de totales
grafico_totales <- ggplot(data = df_totales, aes(x = grupos_edad, y = freq_tot)) +
  geom_bar(stat = "identity", fill = "#B4DCE7") +
  geom_text(aes(label = freq_tot), vjust = -0.5, color = "black", size = 3) +
  labs(x = "Edad", y = "Frecuencia") +
  ggtitle("Edats de propietaris de 10 en 10") +
  theme_minimal() +
  scale_x_discrete(labels = etiquetas) +  # Cambiar las etiquetas del eje x
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))  # Girar las etiquetas del eje x

# Crear el gráfico de barras separado por género con etiquetas
grafico_genero <- ggplot(data = df_genero, aes(x = grupos_edad, y = freq_gen, fill = Genere_resposta)) +
  geom_bar(stat = "identity", position = "dodge") +
  geom_text(aes(label = freq_gen), position = position_dodge(width = 0.9), vjust = -0.5, color = "black", size = 3) + scale_fill_manual(values = c("F" = "#FFDDA1", "M" = "#7AD7B0"))+
  labs(x = "Edat", y = "Freqüència", fill = "Gènere") +
  ggtitle("Edats dels enquestats de 10 en 10, per gènere") +
  theme_minimal() +
  scale_x_discrete(labels = etiquetas) +  # Cambiar las etiquetas del eje x
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))  # Girar las etiquetas del eje x

# Mostrar los gráficos de barras
grid.arrange(grafico_totales, grafico_genero, nrow = 1)

library(tseries)
shapiro.test(subset(bbdd, Genere_resposta=="F")$Edat)# >0.05 normalidad
jarque.bera.test(subset(bbdd, Genere_resposta=="F")$Edat)#<0.05 no normalitat
jarque.bera.test(subset(bbdd, Genere_resposta=="M")$Edat)# <0.05 no normalitat




#################################
#perfil propietaris

#carrec- propietari o dircetiu
counts <- table(bbdd$Propietaris)
counts[2] <- counts[2]+counts[1];counts <- counts[-1]
per <- round(counts/sum(counts) * 100, 1)
colorduen <- c("Directius" = "#BDFCC9", "Propietaris" = "#FFC2C2")

df <- data.frame(categories = c("Propietaris", "Directius"), counts = as.numeric(counts), per = per)

carrec <- ggplot(data = df, aes(x = "", y = counts, fill = categories)) + 
  geom_bar(width = 1, stat = "identity", color = "white") +
  coord_polar(theta = "y") +
  labs(fill = "Perfil")+
  ggtitle("Càrrec de l'enquestat/da")

carrec <- carrec+
        theme_void() +
        scale_fill_manual(values = colorduen) +
        geom_text(aes(label = paste0(counts, "\n", per, "%")),
                  position = position_stack(vjust = 0.5))


#propietat unica o comaprtida
counts <- table(bbdd$Propietaris)[-3]
per <- round(counts/sum(counts) * 100, 1)
df <- data.frame(categories = names(counts), counts = as.numeric(counts), per = per)
colorprop <- c("1prop" = "#FFD1D1", "+1prop" = "#FFB3B3")

tipus.propietat <- ggplot(data = df, aes(x = "", y = counts, fill = categories)) + 
  geom_bar(width = 1, stat = "identity", color = "white") +
  coord_polar(theta = "y") + labs(fill = "Perfil") +
  ggtitle("Un propietari vs. Conjunt propietaris")

tipus.propietat <- tipus.propietat +
        theme_void() +
        scale_fill_manual(values = colorprop) +
        geom_text(aes(label = paste0(counts, "\n", per, "%")),
                  position = position_stack(vjust = 0.5))


#genere empresa segons tipo de propietat
porcentajes <- bbdd %>%
  filter(Propietaris %in% c("1prop", "+1prop")) %>%
  group_by(Propietaris, Genere_empresa) %>%
  summarise(freq = sum(!is.na(Genere_empresa))) %>%
  group_by(Propietaris) %>%
  mutate(porcentaje = freq / sum(freq) * 100) %>%
  invisible()


empresa.tprop <-   ggplot(porcentajes, aes(x = Propietaris, y = freq, fill = Genere_empresa)) +
    geom_bar(stat = "identity", position = "stack") +
    geom_text(aes(label = paste0(round(porcentaje, 1), "%"), 
                  y = freq, 
                  group = Genere_empresa), 
              position = position_stack(vjust = 0.5),
              size = 3, 
              color = "black") +
    scale_y_continuous(labels = scales::comma) +
    labs(title = "Gènere de l'empresa segons tipus de propietat",
         x = "Tipus de propietat (compartida o no)",
         y = "Nombre d'empreses") +
    scale_fill_manual(values = c("F" = "#B4DCE7", "M" = "#F8D3A5", "Mixt" = "#D6E2EC")) +
    theme_minimal() + labs( fill = "Gènere de l'empresa")

#edats propietaris
prop <- bbdd %>% filter(bbdd$Propietaris %in% c("1prop", "+1prop"))
df <- data.frame(prop$Edat, prop$Genere_resposta)
# Agrupar las edades en intervalos de 10 en 10
df$grupos_edad <- cut(prop$Edat, breaks = seq(15, 85, by = 10))
etiquetas <- c("15 a 25", "26 a 35", "36 a 45", "46 a 55", "56 a 65", "66 a 75", "76 a 85")
# Calcular la frecuencia total por grupos de edad
freq_totales <- table(df$grupos_edad)

# Calcular la frecuencia por grupos de edad y género
freq_genero <- table(df$grupos_edad, df$prop.Genere_resposta)

# Convertir las tablas de frecuencias en data frames
df_totales <- data.frame(grupos_edad = as.character(names(freq_totales)),
                         freq_tot = as.numeric(freq_totales))
df_genero <- as.data.frame.table(freq_genero)
names(df_genero) <- c("grupos_edad", "Genere_resposta", "freq_gen")

# Crear el gráfico de barras de totales
grafico_totales <- ggplot(data = df_totales, aes(x = grupos_edad, y = freq_tot)) +
  geom_bar(stat = "identity", fill = "#B4DCE7") +
  geom_text(aes(label = freq_tot), vjust = -0.5, color = "black", size = 3) +
  labs(x = "Edad", y = "Frecuencia") +
  ggtitle("Edats de propietaris de 10 en 10") +
  theme_minimal() +
  scale_x_discrete(labels = etiquetas) +  # Cambiar las etiquetas del eje x
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))  # Girar las etiquetas del eje x

# Crear el gráfico de barras separado por género con etiquetas
grafico_genero <- ggplot(data = df_genero, aes(x = grupos_edad, y = freq_gen, fill = Genere_resposta)) +
  geom_bar(stat = "identity", position = "dodge") +
  geom_text(aes(label = freq_gen), position = position_dodge(width = 0.9), vjust = -0.5, color = "black", size = 3) + scale_fill_manual(values = c("F" = "#FFDDA1", "M" = "#7AD7B0"))+
  labs(x = "Edat", y = "Freqüència", fill = "Gènere") +
  ggtitle("Edats de propietaris de 10 en 10, per gènere") +
  theme_minimal() +
  scale_x_discrete(labels = etiquetas) +  # Cambiar las etiquetas del eje x
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))  # Girar las etiquetas del eje x

# Mostrar los gráficos de barras
grid.arrange(grafico_totales, grafico_genero, nrow = 1)

shapiro.test(prop$Edat)#<0.05 no normal
shapiro.test(subset(prop, Genere_resposta=="F")$Edat)#>0.05 normal
shapiro.test(subset(prop, Genere_resposta=="M")$Edat)#<0.05 no normal


#genere propietaris
bdfprop <- filter(bbdd, Propietaris %in% c("1prop","+1prop"))
counts <- table(bdfprop$Genere_resposta)
per <- round(counts/sum(counts) * 100, 1)
df <- data.frame(Genere = names(counts), counts = as.numeric(counts), per = per)

# Crear el gráfico tipo donut
genere_prop <- ggplot(data = df, aes(x = "", y = counts, fill = Genere)) +
  geom_bar(width = 1, stat = "identity", color = "white") +
  coord_polar(theta = "y", start = 0) +
  theme_void() +
  scale_fill_manual(values = c("F" = "#FFDDA1", "M" = "#7AD7B0")) +geom_text(aes(label = paste0(counts, "\n", per, "%")),
            position = position_stack(vjust = 0.5)) +
  ggtitle("Gènere dels/de les propietaris/àries enquestats/des") +
  labs(fill = "Gènere") +
  theme(plot.title = element_text(hjust = 0.5, size = 13))


#genere resposta vs genere empresa

prop <- bbdd %>% filter(bbdd$Propietaris != "Directiu")
x <- which(bbdd$Propietaris=="1prop")
y <- which(bbdd$Propietaris=="+1prop")
bdfprop <- filter(bbdd, Propietaris %in% c("1prop","+1prop"))

porcentajes <- prop %>%
  filter(Genere_resposta %in% c("F", "M")) %>%
  group_by(Genere_resposta, Genere_empresa) %>%
  summarise(freq = sum(!is.na(Genere_empresa))) %>%
  group_by(Genere_resposta) %>%
  mutate(porcentaje = freq / sum(freq) * 100) %>%
  invisible()
colorresemp <- c("F" = "#FFDDA1", "M" = "#7AD7B0", "Mixt" = "#E8E391")

presemp <- ggplot(porcentajes, aes(x = Genere_resposta, y = freq, fill = Genere_empresa)) +
  geom_bar(stat = "identity", position = "stack") +
  labs(title = "Distribució del gènere de l'enquestat i el gènere de l'empresa",
       x = "Gènere",
       y = "Respostes", fill = paste("Gènere de", "l'empresa", sep="\n"))

presemp <- presemp + 
  geom_text(aes(label = paste0(round(porcentaje, 1), "%"), 
                y = freq, 
                group = Genere_empresa), 
            position = position_stack(vjust = 0.5),
            size = 3, 
            color = "black") +
  scale_y_continuous(labels = scales::comma) +
  scale_fill_manual(values = colorresemp) +
  theme_minimal()+
  theme(plot.title = element_text(size=10), 
        axis.text.x = element_text(size = 8, hjust = 1))


########
#empresa

#manera acces vs genere propietari
porcentajes <- prop %>%
  filter(Genere_resposta %in% c("F", "M", "Mixt")) %>%
  group_by(Manera_acces_propietat, Genere_resposta) %>%
  summarise(freq = sum(!is.na(Manera_acces_propietat))) %>%
  filter(freq > 0) %>%
  group_by(Genere_resposta) %>%
  mutate(porcentaje = freq / sum(freq) * 100) %>%
  invisible()

# Crear el gráfico de columna apilada
pacces <- ggplot(porcentajes, aes(x = Genere_resposta, y = freq, fill = Manera_acces_propietat)) +
  geom_bar(stat = "identity", position = "stack") +
  labs(title = "Gènere de l'enquestat/da segons la manera d'accés a la propietat",
       x = "Manera d'accés",
       y = "Nombre d'empreses", fill = paste("Manera d'accés", "\n", "a la propietat")) + 
  geom_text(aes(label = paste0(round(porcentaje, 1), "%"), 
                y = freq, 
                group = Manera_acces_propietat), 
            position = position_stack(vjust = 0.5),
            size = 2,  # Ajusta el tamaño de las etiquetas aquí (2 en este ejemplo)
            color = "black") +
  scale_y_continuous(labels = scales::comma) +
  scale_fill_manual(values = c("Adquisició" = "#9376E0", "Altres" = "#E893CF", "Fundació" ="#F3BCC8", "Fusió" = "#F6FFA6","Herència" = "#FFD95A")) +
  theme_minimal() 



#anitguitat empresa
fundacio <- bbdd[bbdd$Manera_acces_propietat=="Fundació",]

df <- data.frame(fundacio$Antiguitat_empresa, fundacio$Genere_resposta)
# Agrupar las edades en intervalos de 10 en 10
df$grupos_edad <- cut(as.numeric(fundacio$Antiguitat_empresa), breaks = seq(0, 50, by = 10))
etiquetas <- c("0 a 10", "11 a 20", "21 a 30", "31 a 40", "41 a 50")
# Calcular la frecuencia total por grupos de edad
freq_totales <- table(df$grupos_edad)

# Calcular la frecuencia por grupos de edad y género
freq_genero <- table(df$grupos_edad, df$fundacio.Genere_resposta)

# Convertir las tablas de frecuencias en data frames
df_totales <- data.frame(grupos_edad = as.character(names(freq_totales)),
                         freq_tot = as.numeric(freq_totales))
df_genero <- as.data.frame.table(freq_genero)
names(df_genero) <- c("grupos_edad", "Genere_resposta", "freq_gen")

# Crear el gráfico de barras de totales
grafico_totales <- ggplot(data = df_totales, aes(x = grupos_edad, y = freq_tot)) +
  geom_bar(stat = "identity", fill = "#B4DCE7") +
  geom_text(aes(label = freq_tot), vjust = -0.5, color = "black", size = 3) +
  labs(x = "Edat", y = "Freqüència") +
  ggtitle("Anys d'antiguitat de les empreses") +
  theme_minimal() +
  scale_x_discrete(labels = etiquetas) +  # Cambiar las etiquetas del eje x
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))  # Girar las etiquetas del eje x

# Crear el gráfico de barras separado por género con etiquetas
grafico_genero <- ggplot(data = df_genero, aes(x = grupos_edad, y = freq_gen, fill = Genere_resposta)) +
  geom_bar(stat = "identity", position = "dodge") +
  geom_text(aes(label = freq_gen), position = position_dodge(width = 0.9), vjust = -0.5, color = "black", size = 3) + scale_fill_manual(values = c("F" = "#FFDDA1", "M" = "#7AD7B0"))+
  labs(x = "Edat", y = "Freqüència", fill = "Gènere") +
  ggtitle("Anys d'antiguitat de les empreses per gènere") +
  theme_minimal() +
  scale_x_discrete(labels = etiquetas) +  # Cambiar las etiquetas del eje x
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))  # Girar las etiquetas del eje x



#sector
counts <- table(bbdd$Sector)
names(counts) <- levels(bbdd$Sector)
per <- round(counts/sum(counts) * 100, 1)
df <- data.frame(Sectors_economics = names(counts), counts = as.numeric(counts), per = per)

sector <- ggplot(data = df, aes(x = "", y = counts, fill = Sectors_economics)) + 
  geom_bar(width = 1, stat = "identity", color = "white") +
  coord_polar(theta = "y") +
  theme_void() +
  scale_fill_manual(values = c("Serveis de mercat" = "#D7C2FF", "Serveis de no mercat" = "#FFF8B3", "Construcció" = "#FFD8B3", "Fabricació" = "#B3E2FF")) +
  geom_text(aes(label = paste0(counts, "\n", per, "%")),
            position = position_stack(vjust = 0.5))+
  ggtitle("Un propietari vs. Conjunt propietaris") + labs(fill = "Sectos econòmics", title = "Sectors econòmics de les empreses")

#sector per districte
porcentajes <- bbdd %>%
  filter(Districte %in% c("D1", "D2", "D3", "D4", "D5", "D6", "D7")) %>%
  group_by(Districte, Sector) %>%
  summarise(freq = sum(!is.na(Sector))) %>%
  group_by(Districte) %>%
  mutate(porcentaje = freq / sum(freq) * 100) %>%
  invisible()

sector.districte<-  ggplot(porcentajes, aes(x = Districte, y = freq, fill = Sector)) +
    geom_bar(stat = "identity", position = "stack") +
    geom_text(aes(label = paste0(round(porcentaje, 1), "%"), 
                  y = freq, 
                  group = Sector), 
              position = position_stack(vjust = 0.5),
              size = 3, 
              color = "black") +
    scale_y_continuous(labels = scales::comma) +
    labs(title = "Distribución de estudios según sector",
         x = "Districte",
         y = "Empreses") +
    scale_fill_manual(values =  c( "Serveis de mercat" = "#D7C2FF", "Serveis de no mercat" = "#FFF8B3", "Construcció" = "#FFD8B3", "Fabricació" = "#B3E2FF")) +
    theme_minimal()+
    theme(plot.title = element_text(size=10), 
          axis.text.x = element_text(size = 8, hjust = 1))


#nsucursal vs nassalariats
porcentajes <- bbdd %>%
  filter(N_assalariats %in% c("0", "1", "2", "3", "4")) %>%
  count(N_assalariats, N_sucursals, name = "freq") %>%
  group_by(N_assalariats) %>%
  mutate(porcentaje = freq / sum(freq) * 100) %>%
  group_by(N_sucursals) %>%
  mutate(porcentaje = porcentaje / sum(porcentaje) * 100) %>%
  ungroup()


passal <- ggplot(porcentajes, aes(x = N_sucursals, y = freq, fill = N_assalariats)) +
  geom_bar(stat = "identity", position = "stack") +
  labs(title = "Nombre d'assalariats segons nombre de sucursals",
       x = "Nombre de sucursals",
       y = "Empreses", fill = paste("Nombre", "\n", "d'assalariats")) 
  
  
passal <-  passal +
  geom_text(aes(label = paste0(round(porcentaje, 1), "%"), 
                y = freq, 
                group = N_assalariats), 
            position = position_stack(vjust = 0.5),
            size = 3, 
            color = "black") +
  scale_y_continuous(labels = scales::comma) +
  scale_fill_manual(values = c("0" = "#AA3333", "1" = "#B9005B", "2" = "#FF7C7C", "3" = "#FEE0C0" )) +
  theme_minimal()

#importacio per nscurusal
import <- matrix(c(table(bbdd$Importacio_No, bbdd$N_sucursals)[2,],                table(bbdd$Importacio_Nacional,bbdd$N_sucursals)[2,],
                   table(bbdd$Importacio_Europa, bbdd$N_sucursals)[2,],                table(bbdd$Importacio_intercontinental, bbdd$N_sucursals)[2,]),
                 byrow=T,ncol=length(table(bbdd$N_sucursals)))
import <- t(import)
# Creación del dataframe a partir de la matriz "expor"
df <- data.frame(
  Categoria = c("0", "1 a 5", "6 o més"),  import_no = import[, 1],
  import_nacional = import[, 2],  import_europa = import[, 3], import_inter = import[,4])

# Convertir el dataframe al formato largo (tidy)
df_long <- tidyr::gather(df, key = import, value = Valor, -Categoria)
# Calcular los porcentajes para cada color
df_long <- df_long %>% 
  group_by(Categoria) %>%  mutate(Percent = Valor / sum(Valor) * 100)
# Definir una paleta de colores personalizada
mis_colores <- c("#F0EABE", "#FAEAB1", "#C99747", "#C58940")
# Gráfico de barras con todas las categorías en una sola barra
pimpor <- ggplot(df_long, aes(x = Categoria, y = Valor, fill = import)) +
  geom_bar(stat = "identity", position = "stack") +   
  ylab("Valor") +  ggtitle("Percentatge de tipus d'importació segons nombre de sucursals") + labs(fill = "Tipus d'importació")


pimpor <- pimpor +
  geom_text(aes(label = paste0(round(Percent,2), "%")), 
            position = position_stack(vjust = 0.5),             color = "black", size = 2) +
  scale_fill_manual(values = mis_colores, labels = c("Europa", "Intercontinental", "Nacional", "No")) + 
  theme_minimal()





#exportacio per nscucursals
# Creación del dataframe a partir de la matriz "expor"
expor <- matrix(c(table(bbdd$Exportacio_No, bbdd$N_sucursals)[2,],                table(bbdd$Exportacio_Nacional,bbdd$N_sucursals)[2,],
                  table(bbdd$Exportacio_Europa, bbdd$N_sucursals)[2,],                table(bbdd$Exportacio_intercontinental, bbdd$N_sucursals)[2,]),
                byrow=T,ncol=length(table(bbdd$N_sucursals)))
expor <- t(expor)
# Creación del dataframe a partir de la matriz "expor"
df <- data.frame(
  Categoria = c("0", "1 a 5", "6 o més"),  expor_no = expor[, 1],
  expor_nacional = expor[, 2],  expor_europa = expor[, 3], expor_inter = expor[,4])

# Convertir el dataframe al formato largo (tidy)
df_long <- tidyr::gather(df, key = export, value = Valor, -Categoria)
# Calcular los porcentajes para cada color
df_long <- df_long %>% 
  group_by(Categoria) %>%  mutate(Percent = Valor / sum(Valor) * 100)
# Definir una paleta de colores personalizada
mis_colores <- c("#F0EABE", "#FAEAB1", "#C99747", "#C58940")
# Gráfico de barras con todas las categorías en una sola barra
ggplot(df_long, aes(x = Categoria, y = Valor, fill = export)) +
  geom_bar(stat = "identity", position = "stack") +  geom_text(aes(label = paste0(round(Percent,2), "%")), 
                                                               position = position_stack(vjust = 0.5),             color = "black", size = 2) +
  scale_fill_manual(values = mis_colores, labels = c("Europa", "Intercontinental", "Nacional", "No")) +  
  ylab("Valor") +  ggtitle("Percentatge de tipus d'exportació segons nombre de sucursals") + labs(fill = "Tipus d'exportació")+
  theme_minimal()


#anys antiguitat per nombre assalariats
antig_assalariats <- ggplot(bbdd, aes(x = factor(N_assalariats), y = as.numeric(Antiguitat_empresa))) +
  geom_boxplot() +
  labs(title = "Anys d'antiguitat de l'empresa segons nombre d'assalariats",
       x = "Nombre d'assalariats",
       y = "Anys d'antiguitat") +
  scale_x_discrete(labels = c("0", "1 a 9", "10 a 49", "Més de 50")) +
  theme_minimal()

############
##preguntes personals

# estudis

bdfprop <- filter(bbdd, Estudis %in% c("ESO","FP_Batx", "Uni", "Postgrau_master_doctorat" ))
counts <- table(bbdd$Estudis)
per <- round(counts/sum(counts) * 100, 1)
df <- data.frame(Estudis = names(counts), counts = as.numeric(counts), per = per)

# Crear el gráfico tipo donut
estu <- ggplot(data = df, aes(x = "", y = counts, fill = Estudis)) +
  geom_bar(width = 1, stat = "identity", color = "white") +
  coord_polar(theta = "y", start = 0) +
  theme_void() +
  scale_fill_manual(values = c("ESO" = "#B9EDDD", "FP_Batx" = "#87CBB9", "Uni" = "#569DAA", "Postgrau_master_doctorat" = "#577D86"),
                    labels = c("ESO" = "Educació obligatòria", "FP_Batx" = "Educació post-obligatòria", "Uni" = paste("Educació","\n", "universitària"), "Postgrau_master_doctorat" = "Educció post-universitària")) 
  estu <- estu +  geom_text(aes(label = paste0(counts, "\n", per, "%")),
            position = position_stack(vjust = 0.5)) +
  ggtitle("Nivell màxim d'estudis") +
  labs(fill = "Estudis") +
  theme(plot.title = element_text(hjust = 0.5, size = 13))
  
#estudis per sector

  porcentajes <- bbdd %>%
    filter(Estudis %in% c("ESO", "FP_Batx", "Uni", "Postgrau_master_doctorat", "Altres", "No_reglats")) %>%
    group_by(Estudis, Sector) %>%
    summarise(freq = sum(!is.na(Sector))) %>%
    group_by(Estudis) %>%
    mutate(porcentaje = freq / sum(freq) * 100) %>%
    invisible()
  
  porcentajes$Estudis <- factor(porcentajes$Estudis, levels = c("ESO", "FP_Batx", "Uni", "Postgrau_master_doctorat"))
  
estudis.sector<- 
    ggplot(porcentajes, aes(x = Estudis, y = freq, fill = Sector)) +
      geom_bar(stat = "identity", position = "stack") 
   estudis.sector <- estudis.sector + geom_text(aes(label = paste0(round(porcentaje, 1), "%"), 
                    y = freq, 
                    group = Sector), 
                position = position_stack(vjust = 0.5),
                size = 2,  # Ajusta el tamaño del texto aquí
                color = "black") +
      scale_y_continuous(labels = scales::comma) +
      labs(title = "Nivell d'estudis segons sector el sector",
           x = "Estudis",
           y = "Freqüència") +
      scale_fill_manual(values = c("Serveis de mercat" = "#D7C2FF", 
                                   "Serveis de no mercat" = "#FFF8B3", 
                                   "Construcció" = "#FFD8B3", 
                                   "Fabricació" = "#B3E2FF")) +
      theme_minimal() +
      theme(plot.title = element_text(size = 10), 
            axis.text.x = element_text(size = 8, angle = 45, hjust = 1),
            axis.text.y = element_text(size = 9)) +
      scale_x_discrete(labels = c("ESO" = "Educació obligatòria", 
                                  "FP_Batx" = "Educació post-obligatòria", 
                                  "Uni" = paste("Educació","\n", "universitària"), 
                                  "Postgrau_master_doctorat" = "Post-universitària"))  # Ajusta los nuevos nombres de las etiquetas aquí



#estudis per isstuacio sentimental
    porcentajes <- bbdd %>%
      filter(Situacio_sentimental %in% c("Amb parella", "Sense parella")) %>%
      group_by(Situacio_sentimental, Estudis) %>%
      summarise(freq = sum(!is.na(Estudis))) %>%
      group_by(Situacio_sentimental) %>%
      mutate(porcentaje = freq / sum(freq) * 100) %>%
      mutate(Situacio_sentimental = factor(Situacio_sentimental, levels = c("Amb parella", "Sense parella"))) %>%
      invisible()
    
sentimental_estudis <- ggplot(porcentajes, aes(x = Situacio_sentimental, y = freq, fill = Estudis)) +
      geom_bar(stat = "identity", position = "stack") +
      geom_text(aes(label = paste0(round(porcentaje, 1), "%"), 
                    y = freq, 
                    group = Estudis), 
                position = position_stack(vjust = 0.5),
                size = 2,
                color = "black") +
      scale_y_continuous(labels = scales::comma) +
      labs(title = "Nivell màxim d'estudis segons situació sentimental",
           x = "Situació sentimental",
           y = "Freqüència") +
      scale_fill_manual(values = c("ESO" = "#F9B1C7", 
                                   "FP_Batx" = "#AED3E9", 
                                   "Uni" = "#B7E4C7",
                                   "Postgrau_master_doctorat" = "#D9B6E3"),
                        labels = c("ESO" = "Educació obligatòria",
                                   "FP_Batx" = "Educació postsecundària",
                                   "Uni" = "Educació universitària",
                                   "Postgrau_master_doctorat" = "Educació postuniversitària")) +
      theme_minimal() +
      theme(plot.title = element_text(size = 10), 
            axis.text.x = element_text(size = 8, hjust = 1, angle = 60),
            axis.text.y = element_text(size = 8))
    


#hores benestar per tipus enquestat
    
    porcentajes <- bbdd %>%
      filter(Hores_benestar %in% c("0 - 4h", "5 - 9h", "10 - 14h", "15 - 19h", "Més de 20h")) %>%
      group_by(Hores_benestar, Propietaris) %>%
      summarise(freq = sum(!is.na(Propietaris))) %>%
      group_by(Hores_benestar) %>%
      mutate(porcentaje = freq / sum(freq) * 100) %>%
      mutate(Hores_benestar = factor(Hores_benestar, levels = c("0 - 4h", "5 - 9h", "10 - 14h", "15 - 19h", "Més de 20h"))) %>%
      invisible()
    
    porcentajes <- bbdd %>%
      filter(Propietaris %in% c("1prop", "+1prop", "Directiu")) %>%
      group_by(Hores_benestar, Propietaris) %>%
      summarise(freq = sum(!is.na(Hores_benestar))) %>%
      group_by(Propietaris) %>%
      mutate(porcentaje = freq / sum(freq) * 100) %>%
      mutate(Propietaris = factor(Propietaris, levels = c("1prop", "+1prop", "Directiu"))) %>%
      invisible()
    

hores_prop <- ggplot(porcentajes, aes(x = Propietaris, y = as.numeric(freq), fill = as.factor(Hores_benestar))) +
      geom_bar(stat = "identity", position = "stack") +
      geom_text(aes(label = paste0(round(porcentaje, 1), "%"), 
                    y = freq, 
                    group = Hores_benestar), 
                position = position_stack(vjust = 0.5),
                size = 3, 
                color = "black") +
      scale_y_continuous(labels = scales::comma) +
      labs(title = "Hores per al propi benestar segon tipus de càrrec",
           x = "Perfil",
           y = "Freqüència", fill= "Hores benestar") +
      scale_fill_manual(values = c("0 - 4h" = "#FBFFB1", 
                                   "5 - 9h" = "#FFEBB4", 
                                   "10 - 14h" = "#FFBFA9", 
                                   "15 - 19h" = "#FFACAC", 
                                   "Més de 20h" = "#D97373")) +
      theme_minimal() +
      theme(plot.title = element_text(size = 10), 
            axis.text.x = element_text(size = 8,  hjust = 1))

####################################
########ANÀLISI ESTADÍSTIC##########
####################################

# Error permès
  Z <- 1.96
n <- 253
N <- 5303
p <- q <- 0.5

E <- sqrt((Z^2*p*q*(N-n))/(n*(N-1)))

#1r model: antiguitat empresa vs nombre assalariats
model1 <- lm(as.numeric(Antiguitat_empresa) ~ N_assalariats, data = bbdd)
smodel1 <- summary(model1)

dones_empresa <- bbdd%>% filter(Genere_empresa=="F")
model1_d <- lm(as.numeric(Antiguitat_empresa) ~ N_assalariats, data = dones_empresa)
smodel1_d <- summary(model1_d)

homes_empresa <- bbdd%>% filter(Genere_empresa=="M")
model1_h <- lm(as.numeric(Antiguitat_empresa) ~ N_assalariats, data = homes_empresa)
smodel1_h <- summary(model1_h)


#2n model: sector vs districte

datos.acs <- table(bbdd$Districte, bbdd$Sector)
addmargins(datos.acs)
library(gplots)

# Convertir los datos en una tabla
dt <- as.table(datos.acs)
dt
dt <- prop.table(dt,margin=1) 
dt
balloonplot(t(dt), 
            main ="Gráfico Opinión Renta", 
            xlab ="Opinión", 
            ylab="Renta",
            label = F, cum.margins=F, 
            label.lines=F, show.margins = FALSE)
# independencia
prueba <- chisq.test(datos.acs)
prueba #pvalor > 0.05

library(FactoMineR) 
#4-1 -> ncp=3
res.ca <- CA(datos.acs,ncp=3,graph=T)
res.ca 
plot.CA(res.ca) # Mapa Simétrico


#3r model: Edat vs N_assalariats
dones_resposta <- bbdd %>% filter(Genere_resposta=="F")
homes_resposta <- bbdd %>% filter(Genere_resposta=="M")

n_general <- lm(Edat ~ N_assalariats, data=bbdd)
sn_general <- summary(n_general)#p-valor = 0.0.189

n_dones <- lm(Edat ~ N_assalariats, data=dones_resposta)
sn_dones <- summary(n_dones)#p-valor = 0.0.189

n_homes <- lm(Edat ~ N_assalariats, data=homes_resposta)
sn_homes <- summary(n_homes)#p-valor = 0.0.189

#4t model: edat acces vs nsucursals i nassalariats
dones_resposta <- bbdd %>% filter(Genere_resposta=="F")
homes_resposta <- bbdd %>% filter(Genere_resposta=="M")

mod <- lm(Edat_acces ~ N_sucursals + N_assalariats, data=bbdd)
smod <- summary(mod)#1 casi

mod_dones <- lm(Edat_acces ~ N_sucursals + N_assalariats, data=dones_resposta)
smod_dones <- summary(mod_dones)#1 significatiu i 1 casi

mod_homes <- lm(Edat_acces ~ N_sucursals + N_assalariats, data=homes_resposta)
smod_homes <- summary(mod_homes)

#5è model: Estudis vs Sector
#general
datos.acs <- table(bbdd$Estudis, bbdd$Sector)
addmargins(datos.acs)
library(gplots)

# Convertir los datos en una tabla
dt <- as.table(datos.acs)
dt
dt <- prop.table(dt,margin=1) 
dt
balloonplot(t(dt), 
            main ="Gráfico Opinión Renta", 
            xlab ="Opinión", 
            ylab="Renta",
            label = F, cum.margins=F, 
            label.lines=F, show.margins = FALSE)
# independencia
prueba <- chisq.test(datos.acs)
prueba #pvalor > 0.05

library(FactoMineR) 

res.ca <- CA(datos.acs,ncp=3,graph=F)
res.ca 
plot.CA(res.ca) # Mapa Simétrico


#dones
dones_resposta <- bbdd %>% filter(Genere_resposta == "F")
datos.acs <- table(dones_resposta$Estudis, dones_resposta$Sector)
addmargins(datos.acs)
library(gplots)

# Convertir los datos en una tabla
dt <- as.table(datos.acs)
dt
dt <- prop.table(dt,margin=1) 
dt
balloonplot(t(dt), 
            main ="Gráfico Opinión Renta", 
            xlab ="Opinión", 
            ylab="Renta",
            label = F, cum.margins=F, 
            label.lines=F, show.margins = FALSE)
# independencia
prueba <- chisq.test(datos.acs)
prueba #pvalor > 0.05

library(FactoMineR) 
#3 filas y 4 columnas min(3,4)-1 -> ncp=2
res.ca <- CA(datos.acs,ncp=2,graph=F)
res.ca 
plot.CA(res.ca) # Mapa Simétrico


#homes
homes_resposta <- bbdd %>% filter(Genere_resposta == "M")
datos.acs <- table(homes_resposta$Estudis, homes_resposta$Sector)
addmargins(datos.acs)
library(gplots)

# Convertir los datos en una tabla
dt <- as.table(datos.acs)
dt
dt <- prop.table(dt,margin=1) 
dt
balloonplot(t(dt), 
            main ="Gráfico Opinión Renta", 
            xlab ="Opinión", 
            ylab="Renta",
            label = F, cum.margins=F, 
            label.lines=F, show.margins = FALSE)
# independencia
prueba <- chisq.test(datos.acs)
prueba #pvalor > 0.05

library(FactoMineR) 
#3 filas y 4 columnas min(3,4)-1 -> ncp=2
res.ca <- CA(datos.acs,ncp=2,graph=F)
res.ca 
plot.CA(res.ca) # Mapa Simétrico


#6è model: Fills_binari vs Sector
fill_sector <- glm(Fills_binari  ~Sector, data=bbdd, family=binomial)
sfill_sector <- summary(fill_sector)

dones_resposta <- bbdd%>% filter(Genere_resposta=="F")
fill_sector_d <- glm(Fills_binari  ~Sector, data=dones_resposta, family=binomial)
sfill_sector_d <- summary(fill_sector_d)

homes_resposta <- bbdd%>% filter(Genere_resposta=="M")
fill_sector_h <- glm(Fills_binari  ~Sector, data=homes_resposta, family=binomial)
sfill_sector_h <- summary(fill_sector_h)

#7è model: Tipus de propietat segons les hores de benestar
propietaris<- bbdd %>% filter(Propietaris %in% c("1prop", "+1prop"))

mod7 <- glm(Propietaris ~Hores_benestar, data=propietaris, family=binomial)
summary(mod7)

prop_dones <- propietaris %>% filter(Genere_resposta=="F")
mod7_d <- glm(Propietaris ~Hores_benestar, data=prop_dones, family=binomial)
summary(mod7_d)

prop_homes <- propietaris %>% filter(Genere_resposta=="M")
mod7_h <- glm(Propietaris ~Hores_benestar, data=prop_homes, family=binomial)
summary(mod7_h)

