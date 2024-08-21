# Détection des évolutions des parcelles cadastrales

Stage de 2ᵉ année réalisé au pôle Référentiel Géographique à la DR 45 Centre-Val de Loire.  
Encadré par Pierre Vernedal et Frédéric Minodier.  
Mes remerciements vont à Pierre Vernedal et Frédéric Minodier pour leur mentorat, ainsi qu'à Joachim Clé et Violaine Simon pour leur soutien.

## Mise en garde

Ce projet est conçu pour être utilisé avec les données du Parcellaire Express de l'IGN.  
Il n'est pas nécessaire de télécharger les données en amont, car une fonction prend déjà en charge cette tâche.

De plus, le projet est conçu pour utiliser un système de base de données PostgreSQL avec l'extension PostGIS.

## Usage

### 1) Projet et installation des packages

Ouvir le projet `cadastre_detection.Rproj` et installer les packages nécessaires via le module prévu à cet effet.
  
**À exécuter :**

```r
source("requirements.R")
```


### 2) Identifiants base de données

**Fichier à modifier : `database/connexion_db.R`**
  
**À faire :**

Remplissez les variables du module avec les identifiants de votre base de données.  
Adaptez les paramètres si vous utilisez un autre système que PostgreSQL.


### 3) Import et enregistrement des données en SQL

**Fichier à modifier : `database/creation_db.R`**
  
Pour faire une comparaison entre 20XX et 20YY pour un département ZZ, vous devez disposer de :  

- Les parcelles de 20XX pour le département ZZ,  
- Les parcelles de 20YY pour le département ZZ,  
- Un fond communal (de l'année la plus récente à priori).

Nous utilisons le fond communal déjà présent dans le même fichier que celui contenant les parcelles. Toutefois, ceci nécessite trois téléchargements différents, dont un redondant si nous utilisons le fond communal de l'année 20XX ou 20YY.  

Il y a un traitement des départements comportant des arrondissements pour que l'arrondissement apparaisse directement dans l'attribut nom_com (utile pour l'application shiny). Un autre traitement est effectué pour rendre les géométries valides. Enfin, un dernier traitement élimine les parcelles ayant un attribut idu (identifiant unique) en doublon.

La dernière fonction permet d'enregistrer les données traitées dans la base SQL.

**À faire :**

Remplissez la variable `params_list` avec la liste des paramètres nécessaires à l'importation :

- Le numéro du département dans `num_departement`,  
- Les deux derniers chiffres de l'année à comparer dans `num_annee`,  
- La variable `indicatrice_parcelle` en fonction de l'import souhaité :

  - Pour l'import des parcelles, mettez `TRUE`.  
  - Pour l'import du fond communal, mettez `FALSE`.

Exécutez l'intégralité du fichier.

**Exemple :**

```r
params_list <- list(
  list(num_departement = ZZ, num_annee = XX, indicatrice_parcelle = TRUE),
  list(num_departement = ZZ, num_annee = YY, indicatrice_parcelle = TRUE),
  list(num_departement = ZZ, num_annee = YY, indicatrice_parcelle = FALSE)
)
# Enregistrement des parcelles des années 20XX et 20YY et du fond communal de l'année 20YY
```

Exécutez l'intégralité du fichier.

**Remarque :** 

Le téléchargement peut être long s'il provient directement du site de l'IGN et non de opendatarchives.  
Si vous souhaitez choisir quelle parcelle doit être conservée en cas de doublon, vous devrez le faire manuellement.

### 4) Application de la méthode de détection

#### A) De manière automatisée

**Fichier à modifier : `traitement_automatique.R`**
  
Ce fichier permet de traiter plusieurs départements ou plusieurs intervalles de temps pour un même département en une seule fois. Un message s'affiche à la fin du traitement de chaque département.  
Si vous souhaitez suivre le processus du début à la fin, reportez-vous à la section `traitement_parcelles.Rmd`.

**À faire :**

Remplissez la variable `params_list` avec la liste des paramètres nécessaires au traitement :

- Le numéro du département dans `num_departement`,  
- Les deux derniers chiffres de la période la plus récente que vous souhaitez comparer dans `temps_apres`,  
- Les deux derniers chiffres de la période la plus ancienne que vous souhaitez comparer dans `temps_avant`.

Exécutez l'intégralité du fichier.

**Exemple :**

```r
params_list <- list(
  list(num_departement = ZZ, temps_apres = YY, temps_avant = XX)
)
```

Exécutez l'intégralité du fichier.

**Remarque :** 

Vous pouvez ajouter plusieurs listes de paramètres dans `params_list` pour traiter plusieurs départements en une seule fois.

Si un message d'alerte indique un traitement partiel dû à un trop grand nombre de parcelles à traiter, cela signifie que trop peu de parcelles ont été détectées comme identiques. Un seuil maximal de 500 000 parcelles a été fixé pour arrêter le traitement à une étape précise et le reprendre là où cela ne pose plus de problème. Plus le seuil est élevé, plus le traitement peut prendre de temps.  
À moins d'avoir beaucoup de temps, il est déconseillé d'augmenter ce seuil, mais plutôt de le réduire (variable `nb_parcelles_seuil` dans la fonction `traitement_parcelles`).

#### B) Pas à pas

**Fichier à modifier : `traitement_parcelles.Rmd`**
  
Si vous souhaitez appliquer la méthode de détection à un département à la fois, et étape par étape, ce fichier markdown permet de comprendre les étapes une par une et d'identifier les éventuels problèmes.

**À faire :**

Dans l'en-tête, remplissez les paramètres nécessaires au traitement :

- Le numéro du département dans `num_departement`,  
- Les deux derniers chiffres de la période la plus récente dans `temps_apres`,  
- Les deux derniers chiffres de la période la plus ancienne dans `temps_avant`.

Lancez les chunks (CTRL + ALT + R pour tous les lancer d'un coup).

**Remarque :** 

Des imports sont placés après chaque traitement pour visualiser les parcelles ajoutées dans chaque table à chaque étape.  
Ils doivent être lancés manuellement (et non via CTRL + ALT + R).

De même, des graphiques sont ajoutées au cours du traitement pour mieux visualiser la classification effectuée. Elles ne peuvent être lancées manuellement qu'à partir des chunks.


### 5) Visualisation 

#### A) Carte à l'échelle départementale

**Fichier à modifier : `carte_departement.R`**
  
Pour visualiser les évolution des parcelles d'un département dans son intégralité via une carte HTML. Elle est automatiquement enregistrée dans l'environnement du projet sous la forme (pour un département ZZ entre 20XX et 20YY où YY est plus grand que XX) : parcelles_ZZ_YY-XX.html.  
La visualisation de la carte doit être fait via un navigateur internet (option 'View in Web Browser' via un click gauche).

**À faire :**

Remplissez la variable `params` avec les paramètres nécessaires :

- Le numéro du département dans `num_departement`,  
- Les deux derniers chiffres de la période la plus récente que vous souhaitez comparer dans `temps_apres`,  
- Les deux derniers chiffres de la période la plus ancienne que vous souhaitez comparer dans `temps_avant`.

Exécutez l'intégralité du module.

#### B) Application Shiny

**Fichier à lancer : `app.R`**

Pour une visualisation par communes (ou plusieurs communes en même temps). Des informations telles que les parcelles avec des géométries vides, des détails sur la classification à l'échelle du département, et les changements au niveau des communes (changement de nom, fusion, scission) sont également disponibles dans des onglets dédiés.

**À faire :**

Exécutez l'intégralité du fichier via le bouton 'Run App'.


## Maintenir le projet dans le temps

**Fonction à modifier: `telechargement_departement` situé dans `fonctions/fonction_creation_db.R`**

La fonction à mettre à jour concerne l'import des données.  
Le Parcellaire Express étant mis à jour trimestriellement, l'URL de téléchargement dépend du mois et de l'année de publication des données.  
Je n'ai pas connaissance de la fréquence exacte de mise à jour d'opendatarchives, mais je sais qu'elle ne conserve pas toutes les publications trimestrielles du Parcellaire Express.

Si vous souhaitez obtenir les données les plus récentes sur le site de l'IGN et que vous constatez qu'elles datent d'après juillet 2024 (ou que la fonction renvoie une erreur du type "URL introuvable"), il sera nécessaire de modifier l'URL en conséquence. Il en va de même pour ajouter des téléchargements suite à des ajouts sur opendatarchives.

```r
# Nous nous situons dans la fonction telechargement_departement

mois_publi_ign <- c(
    "2024" = "07" # Modifier la date si nécessaire (association de l'année avec le mois de publication)
)

# Par exemple, si les données sont mises à jour au 1er octobre 2024,
# Il faut changer "07" par "10" (voire l'année si nous sommes en 2025)

mois_publi_ign_maj <- c(
    "2024" = "10" # Mise à jour liée à la mise à disposition des données plus récentes du site de l'IGN
)

# Si plus tard, le site datarchives ajoute la version du 1er juillet 2024 et que vous souhaitez la récupérer par ce biais, ajoutez une entrée dans le vecteur `mois_publi_archive_pci_version_rec_maj`

mois_publi_archive_pci_version_rec_maj <- c(
    "2024" = "07", # Mise à jour
    "2023" = "07"
)
```

**Attention :** on ne peut pas conserver deux millésimes d'une même année, il faut faire un choix.  
Si les variables `mois_publi_ign` et `mois_publi_archive_pci_version_rec_maj` contiennent une même année mais associée à un mois différent, l'import se fera à partir des données de opendatarchives.