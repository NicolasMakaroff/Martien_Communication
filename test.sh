#!/bin/bash
#fichier de test pour le projet

echo "----------------FICHIER TEST-----------------"
echo "L'objectif est de lancer des séquences de test afin de tester les valeurs retournées pour un chaine de caractères."


echo $(make)

function test()
{
if [ "$#" -ne 3 ]; then
 echo "Le nombre d'arguments est invalide"
else
(echo $1 && echo $2 && echo $3) | echo $(./projet)
fi
}

echo -e "Testons avec le message : AZ et 2 antennes\n"
test 1 2 "AZ"
echo 
echo -e "Testons maintenant avec le message : A Z et 1 antenne\n"
test 2 1 "A Z"
echo
echo -e "Testons l'enchainement : BONJOUR et 3 antennes\n"
test 2 3 "BONJOUR"
echo
echo -e "Un dernier essai : EZCTZBRYCZFQXDS et 5 antennes\n"
test 2 5 "EZCTZBRYCZFQXDS"