#!/bin/bash

# Script : head_all.sh
# Affiche les 5 premières lignes de chaque fichier CSV/TSV/TXT dans les sous-dossiers

echo "=== SCAN DES JEUX DE DONNÉES ==="

# Parcourt récursivement tous les fichiers
find data_full -type f \( -iname "*.csv" -o -iname "*.tsv" -o -iname "*.txt" \) | while read FILE
do
    echo ""
    echo "--------------------------------------------"
    echo "📄 Fichier : $FILE"
    echo "--------------------------------------------"
    head -n 5 "$FILE"
done

echo ""
echo "=== FIN DU SCAN ==="

