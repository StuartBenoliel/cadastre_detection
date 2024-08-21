# Détection des évolutions des parcelles cadastrales

Stage 2A réalisé au pôle Référentiel Géographique à la DR 45 Centre-Val de Loire.
Encadré par Pierre Vernedal et Frédéric Minodier.

## Mise en garde

Ce projet est fait pour être utilisé avec les données du Parcellaire Express de l'IGN.
Il n'est pas utile de télécharger les données en amont car une fonction remplit déjà cette tâche.

De plus, le projet est conçut pour utiliser un système de base de donnée PostGre avec l'extension PostGIS.

## Usage

1) Identifiants base de données

  -> database/connexion_db.R
  
A faire:

Remplissez les variables du module database/connexion_db.R par vos identifiants de votre base de données.
A adapter si il s'agit d'un système autre que PostGre


2) Import et enregistrement des données en SQL

  -> database/creation_db.R
  
Pour faire une comparaison entre 20XX et 20YY pour un département ZZ, vous devez avoir :
- Les parcelles de 20XX pour le département ZZ
- Les parcelles de 20YY pour le département ZZ
- Un fond communal (de l'année la plus récente à priori)

Nous utilisons le fond communal se trouvant déjà dans le même fichier que celui où se situe les parcelles.
Toutefois, ceci nécessite trois téléchargements différents dont un redondant si nous utilisons le fond communal de
l'année 20XX ou 20YY.
Il y a un traitement des départements qui comporte des arrondissements pour que l'arrondissement apparaisse directement
dans l'attribut nom_comm (utile pour l'app shiny). Un autre traitement pour rendre les géomètries toutes valides.
Et un dernier pour éliminer les parcelles dont l'attibut idu (identifiant unique) est en doublon ...
Enfin, la dernière fonction sert à enregister les données traitées dans la base SQL.

A faire:

Remplir dans la variable num_departements, les numéros des départements que vous voulez traiter.
Remplir dans la variable num_annees, les deux derniers chiffres des années que vous voulez comparez.
Changer la variable indicatrice_parcelle, selon l'import que vous souhaitez : 
- pour l'import des parcelles, mettez True
- pour l'import du fond communal, mettez False
Lancer l'entièreté du fichier

Exemple:

1) Import des parcelles

num_departements <- ZZ
num_annees <- c(XX , YY)
indicatrice_parcelle <- True

Lancer l'entièreté du fichier

2) Import du fond

num_departements <- ZZ
num_annees <- XX  # (ou YY)
indicatrice_parcelle <- False

Lancer l'entièreté du fichier

Remarque: 

Le téléchargement est long s'il s'agit d'un téléchargement venant directement du site de l'IGN et
non de opendatarchives.

Si vous mettre plusieurs numéros de départements et plusieurs années, alors pour 
chaque département, l'import sera fait pour toutes les années du vecteur num_annees.

Si vous souhaitez choisir laquelles des parcelles supprimées en cas de doublon,
vous devez faire cela manuellement.

3) Application de la méthode de détection

  -> traitement_automatique.R 
  
Le fichier permet de traiter plusieurs départements ou plusieurs intervalles de temps d'un même département 
en un seul coup. 
Un message s'affiche pour indiquer la fin du traitement d'un département.
Si vous voulez suivre le processus du début à la fin aller à la section traitement_parcelles.Rmd.

A faire:

Remplir dans la variable params_list, la liste des paramètres nécessaire à un traitement :
- le numéro du département dans num_departement
- les deux derniers chiffres de la période la plus récente que vous souhaitez comparer dans temps_apres
- les deux derniers chiffres de la période la plus ancienne que vous souhaitez comparer dans temps_avant
Lancer l'entièreté du fichier

Exemple:

params_list <- list(
  list(num_departement = ZZ, temps_apres = YY, temps_avant = XX)
)

Lancer l'entièreté du fichier

Remarque: 

Vous pouvez mettre dans params_list, plusieurs listes comportant les paramètres de plusieurs traitement pour les effectuer en une seule fois.

Si pour un département, un message d'alerte indique un traitement partiel du à un trop nombre de parcelles à traiter à une étape, cela signifie que trop peu de parcelles ont été détecté comme identique. Un seuil de 500 000 parcelles maximal a été placé pour arrêter à une partie précise du traitement et pour qu'il reprenne à partir d'un endroit où cela ne pose plus problème. Plus le seuil est grand, plus il y a de risque que le traitement prenne du temps.
Sauf si vous avez beaucoup de temps devant vous, je ne conseille pas d'augmenter le seuil et plutôt même de le diminuer.
(variable nb_parcelles_seuil dans la fonction traitement_parcelles)


  -> traitement_parcelles.Rmd
  
Si vous souhaitez appliquer la méthode de détection un département à la fois et étape par étape, ce fichier markdown permet de comprendre les étapes une par une et de trouver les problèmes pouvant avoir lieu.

A faire:

Dans l'en-tête, remplir dans params, les paramètres nécessaire à un traitement :
- le numéro du département dans num_departement
- les deux derniers chiffres de la période la plus récente que vous souhaitez comparer dans temps_apres
- les deux derniers chiffres de la période la plus ancienne que vous souhaitez comparer dans temps_avant

Lancer les chunks (CTRL + ALT + R pour tous lancer d'un coup)

Remarque: 

Des imports sont placés après chaque traitement pour voir les parcelles ajoutées dans chaque table à chaque étape.
Ils ne peuvent être lancer que manuellement (et non via CTRL + ALT + R)

De même, des cartes sont placées au cours du traitment pour visualiser mieux la classification effectuée. Elles ne sont lancées dans des chunks que manuellement.


3) Visualisation 

  -> cartes_departement.R
  
Pour une visualisation du département en intégralité via une carte htlm.
Elle est automatiquement enregistrée dans l'environnement du projet.

A faire:

Dans l'en-tête, remplir dans params, les paramètres nécessaire à un traitement :
- le numéro du département dans num_departement
- les deux derniers chiffres de la période la plus récente que vous souhaitez comparer dans temps_apres
- les deux derniers chiffres de la période la plus ancienne que vous souhaitez comparer dans temps_avant

Lancer les chunks (CTRL + ALT + R pour tous lancer d'un coup)

-> app.R

Pour une visualisation par communes (ou plusieurs en même temps). Des informations du type, parcelles avec des géométries vides, détail de la classification sur un département, changement au niveau de la commune (changement de nom, fusion, scission) sont également présent dans des onglets dédiés.

A faire:

Lancer l'entièreté du fichier via le bouton 'Run app'