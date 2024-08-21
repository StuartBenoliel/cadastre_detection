# Détection des évolutions des parcelles cadastrales

Stage de 2ᵉ année réalisé au pôle Référentiel Géographique à la DR 45 Centre-Val de Loire.  
Encadré par Pierre Vernedal et Frédéric Minodier.

## Mise en garde

Ce projet est conçu pour être utilisé avec les données du Parcellaire Express de l'IGN.  
Il n'est pas nécessaire de télécharger les données en amont, car une fonction prend déjà en charge cette tâche.

De plus, le projet est conçu pour utiliser un système de base de données PostgreSQL avec l'extension PostGIS.

## Usage

### 1) Identifiants base de données

**Fichier à modifier : `database/connexion_db.R`**
  
**À faire :**

Remplissez les variables du module avec les identifiants de votre base de données.  
Adaptez les paramètres si vous utilisez un autre système que PostgreSQL.


### 2) Import et enregistrement des données en SQL

**Fichier à modifier : `database/creation_db.R`**
  
Pour faire une comparaison entre 20XX et 20YY pour un département ZZ, vous devez disposer de :  

- Les parcelles de 20XX pour le département ZZ,  
- Les parcelles de 20YY pour le département ZZ,  
- Un fond communal (de l'année la plus récente à priori).

Nous utilisons le fond communal déjà présent dans le même fichier que celui contenant les parcelles. Toutefois, ceci nécessite trois téléchargements différents, dont un redondant si nous utilisons le fond communal de l'année 20XX ou 20YY.  

Il y a un traitement des départements comportant des arrondissements pour que l'arrondissement apparaisse directement dans l'attribut nom_com (utile pour l'application shiny). Un autre traitement est effectué pour rendre les géométries valides. Enfin, un dernier traitement élimine les parcelles ayant un attribut idu (identifiant unique) en doublon.

La dernière fonction permet d'enregistrer les données traitées dans la base SQL.

**À faire :**

Remplissez la variable `num_departements` avec les numéros des départements que vous souhaitez traiter.  
Remplissez la variable `num_annees` avec les deux derniers chiffres des années que vous souhaitez comparer.  
Changez la variable `indicatrice_parcelle` en fonction de l'import souhaité :

- Pour l'import des parcelles, mettez `TRUE`.  
- Pour l'import du fond communal, mettez `FALSE`.

Exécutez l'intégralité du fichier.

**Exemple:**

**a) Import des parcelles**

```r
num_departements <- ZZ
num_annees <- c(XX , YY)
indicatrice_parcelle <- TRUE
```

Exécutez l'intégralité du fichier.

**b) Import du fond**

```r
num_departements <- ZZ
num_annees <- XX  # (ou YY)
indicatrice_parcelle <- FALSE
```

Exécutez l'intégralité du fichier.

**Remarque:** 

Le téléchargement peut être long s'il provient directement du site de l'IGN et non de opendatarchives.  
Si vous entrez plusieurs numéros de départements et plusieurs années, alors pour chaque département, l'import sera effectué pour toutes les années du vecteur `num_annees`.  
Si vous souhaitez choisir quelle parcelle doit être conservée en cas de doublon, vous devrez le faire manuellement.

### 3) Application de la méthode de détection

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

De même, des cartes sont ajoutées au cours du traitement pour mieux visualiser la classification effectuée. Elles ne peuvent être lancées manuellement qu'à partir des chunks.


### 4) Visualisation 

**Fichier à modifier : `cartes_departement.R`**
  
Pour visualiser un département dans son intégralité via une carte HTML. Elle est automatiquement enregistrée dans l'environnement du projet.

**À faire :**


**Fichier à lancer : `app.R`**

Pour une visualisation par communes (ou plusieurs communes en même temps). Des informations telles que les parcelles avec des géométries vides, des détails sur la classification à l'échelle du département, et les changements au niveau des communes (changement de nom, fusion, scission) sont également disponibles dans des onglets dédiés.

**À faire :**

Exécutez l'intégralité du fichier via le bouton 'Run App'.


## Maintenir le projet

**Fonction à modifier: `telechargement_departement` situé dans `fonctions/fonction_creation_db.R`**

La fonction nécessaire à mettre à jour est celle pour l'import des données.  
Le Parcellaire Express étant mise à jour trimestriellement, l'url de téléchargement dépend du mois (et jour) de publications des données.  
Je n'ai pas connaissance de la date de fréquence de mise à jour d'opendatarchives mais je sais qu'elle ne conserve pas toutes les publications trimestrielles du Parcellaire Express.

Si vous souhaitez obtenir les données les plus fraiches du site de l'IGN et que vous constatez sur le site qu'elle date d'après Juillet 2024 (ou la fonction renvoie URL introuvable), il est nécessaire de faire des modifications dans l'URL.

```r
# Nous nous situons dans la fonction telechargement_departement

if (num_annee == 2024) {
    url <- paste0("https://data.geopf.fr/telechargement/download/PARCELLAIRE-EXPRESS/PARCELLAIRE-EXPRESS_1-1__SHP_",
                  sys_projection(num_departement),
                  "_D",
                  num_departement,
                  "_2024-07-01/PARCELLAIRE-EXPRESS_1-1__SHP_",
                  sys_projection(num_departement),
                  "_D",
                  num_departement,
                  "_2024-07-01.7z")
  }
  
# Il faut changer 2024-07-01 par la date qui convient
# Par exemple, si les données sont mise à jour au 1er Octobre 2024
  
url_maj <- paste0("https://data.geopf.fr/telechargement/download/PARCELLAIRE-EXPRESS/PARCELLAIRE-EXPRESS_1-1__SHP_",
                  sys_projection(num_departement),
                  "_D",
                  num_departement,
                  "_2024-10-01/PARCELLAIRE-EXPRESS_1-1__SHP_", # changement ici
                  sys_projection(num_departement),
                  "_D",
                  num_departement,
                  "_2024-10-01.7z") # changement ici
```

Si vous souhaitez obtenir les données les plus fraiches du site de l'IGN et que vous constatez sur le site qu'elle date d'après Juillet 2024 (ou la fonction renvoie URL introuvable), il est nécessaire de faire des modifications dans l'URL.