Projet : Counting SAT Solutions
Jeanne Coschieri - Corentin Jazeron



##### Instructions de compilation #####

Nous n'avons pas prévu de commande pour tester tous les fichiers à la suite. Nous n'en voyions pas l'intêret puisque calculer le nombre de modèles satisfaisant une formule est très (très) long, notamment pour de grosses formules, comme celles données dans le dossier ./tests, à moins de limiter avec l'option --partial avec une petite limite. Cependant, les formules ont généralement plus de 500 variables, et déjà pour une limite posée à 100, l'appel à CDPLL dure parfois plus de 15 minutes. 
Il y a deux possibilités pour utiliser le solver sur un fichier donné donc, décrites ci-dessous. 

# Avec make

La première solution est la suivante : 
`make option [LIM=n] FILE=chemin/vers/mon_fichier.dimacs` 
où option = basic | partial | comp | cosat,
Dans le cas de partial, on ajoute `LIM=n` où n est la limite entière qu'on instaure. 

# Sans make

Il est également possible de compiler avec la commande : 
`dune exec dpll -- --option [n] chemin/vers/mon_fichier.dimacs`
où option = basic | partial | comp | cosat. 






##### Choix d'implémentation ##### 


Notre code est principalement dans le fichier src/count.ml
et nous avons adapté le fichier src/main.ml pour gérer les options.
 


# Partial

L'option --partial permet de calculer une borne inférieure du nombre de modèles satisfaisant la formule. Elle prend un argument N, qui instaure une limite sur le nombre de variables assignées pendant la recherche de modèles. Ainsi, l'appel avec `--partial N` renverra soit "unsat" si assigner N variables ne suffit pas à satisfaire la formule, soit "sat n" où n est le nombre de modèles qui satisfont la formules en assignant au plus N variables.
Le code est donc très similaire à celui de basic, hormis qu'il limite la taille des modèles. 


# Component

On a fait le choix d'utiliser le type `IntSet.t IntMap.t` pour représenter un graphe. En général, les ensembles (e.g. les composantes connexes) sont représentés par des `IntSet.t` plutôt que par des listes (opérations coûteuses) ou des arrays.
Pour créer le graphe, l'algorithme itère sur chaque couple de sommets dans les différentes clauses avec une boucle double, donnant une complexité en *n^2*, qui pourrait être réduite en *n(n-1)/2*.
L'algorithme est **nettement** plus rapide si on ne lui demande pas de calculer les modèles totaux. En effet, calculer ces derniers nécéssite une sorte de "multiplication-concaténation" des différents modèles des Fk, laquelle est couteuse comme les modèles sont des listes.

# Cosat

On utilise le module Sys pour lancer la commande minisat depuis un terminal virtuel, en jetant toutes les sorties dans `/dev/null`, car seul la valeur de retour (10 ou 20) nous interesse. La méthode est : partitionner -> transformer chaque Fk en un fichier dimacs -> lancer MiniSat dessus -> si aucun ne renvoie unsat, continuer comme dans component.