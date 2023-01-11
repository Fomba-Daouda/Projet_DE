# Projet_DE

DATA ENGINEERING 
ANALYSE TEXTUELLE ET ANALYSE DE SENTIMENTS AVEC TENSORFLOW/KERAS R/SHINY

 
INTRODUCTION 
Ce projet consiste en l’analyse des sentiments. Il est divisé en deux parties ; la première consiste en une analyse universelle de texte c’est-à-dire : en fonction d’un texte passé fourni par l’utilisateur, le programme classifie les mots positifs et négatifs. La deuxième partie illustre la classification de texte à partir de fichiers de texte brut stockés sur disque. On formera un classificateur binaire pour effectuer une analyse des sentiments sur un ensemble de données IMDB. Nous allons entraîner un modèle d'analyse des sentiments pour classer les critiques de films comme positives ou négatives, en fonction du commentaire de l’utilisateur. Il s'agit d'un exemple de classification binaire - ou à deux classes -, un type de problème d'apprentissage automatique important et largement applicable.
On utilisera de données Large Movie Review qui contient le texte de 50 000 critiques de films de la base de données de films Internet. Ceux-ci sont divisés en 25 000 avis pour la formation et 25 000 avis pour les tests. Les ensembles de formation et de test sont équilibrés, ce qui signifie qu'ils contiennent un nombre égal d'avis positifs et négatifs. 
 
 
Figure 2: Nuage de mots
II.	Classification des critiques utilisateurs selon les films
Nous rappelons qu’il s’agit d’une classification binaire, les films sont soit commentés positivement soit négativement.

 
Figure 7: Résultat final
6.	Mise en place et liens utiles
	Lien docker Hub
	Lien GitHub
	docker pull armelsanou/projet_de:tfsentiment_analysis1
	docker run -p 3838:3838 armelsanou/projet_de:tfsentiment_analysis1
	naviguer vers 0.0.0.0 :3838
