# Projet Scientifique
Projet EP3 : résolution des équations de NS pour un problème de cavité entrainée.

Pour utiliser l’interface graphique, il est nécessaire de sélectionner les paramètres souhaités, puis de les enregistrer avant de lancer le calcul. La compilation du modèle doit être effectuée une seule fois au début. Les exécutions suivantes peuvent ensuite être lancées directement.

Une fois l’exécution terminée, les graphiques peuvent être affichés depuis l’interface. L’ensemble des fonctionnalités du modèle est accessible via l’interface graphique, sans nécessiter de modification directe du code.

Il est toutefois à noter que l’affichage des graphiques peut présenter des dysfonctionnements sur certains écrans. Dans ce cas, il est recommandé de réduire la fenêtre de l’interface puis de la remettre en plein écran afin de rétablir un affichage correct.


# INSTALLATION & UTILISATION : 

## utilisation de l'IHM pour compiler et executer
python3 INTERFACE_GRAPHIQUE.py pour compiler, executer et visualiser les résultats 


## compilation sans IHM
1 - compilation avec gfortran : gfortran ./optiongraphique.f95 -o resolution_ns.exe
2 - faire tourner, avec ./resolution_ns.exe sachant que le input.data est à adapter selon le cas
3 - python3 INTERFACE_GRAPHIQUE.py pour visualiser les résultats 
