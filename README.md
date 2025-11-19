# Web Data Mining & Business Intelligence

Ce dépôt contient les supports de cours (slides **Quarto / reveal.js**), les scripts et les ressources associés au module **Web Data Mining & Business Intelligence** (M2 Marketing Digital – IAE Paris-Est).

L'objectif de ce cours est de maîtriser la chaîne de valeur de la donnée web : de la collecte (APIs, Scraping) à la visualisation (Dataviz, Storytelling) et au pilotage (Business Intelligence, Dashboards), en passant par l'analyse exploratoire et prédictive (Data Mining).

---

## 📅 Séances & Supports

| Séance | Thème | Slides | Scripts et références |
|--------|-------|--------|-----------------------|
| **Séance 1** | **Introduction, Big Data & SQL** <br> *Enjeux des données web, KPIs & requêtes* | [📑 cours_1.html](https://oliviercaron.github.io/web_data_mining_webi/cours_1/cours_1.html) | **Lectures** : [Bit by Bit (Salganik)](https://www.bitbybitbook.com/) · [Data Mining (IBM)](https://www.ibm.com/topics/data-mining) <br> **SQL & R** : [SQL for Data Science (W3Schools)](https://www.w3schools.com/sql/) · [dbplyr (SQL dans R)](https://dbplyr.tidyverse.org/) |
| **Séance 2** | **Data Visualization & Storytelling** <br> *Grammaire graphique, Gestalt & Dashboards* | [📑 cours_2.html](https://oliviercaron.github.io/web_data_mining_webi/cours_2/cours_2.html) | **Théorie** : [Storytelling with Data (Knaflic)](https://www.storytellingwithdata.com/) · [Grammaire des graphiques](https://ggplot2-book.org/) <br> **R** : [ggplot2 (Docs)](https://ggplot2.tidyverse.org/) · [gt (Tableaux)](https://gt.rstudio.com/) · [patchwork (Composition)](https://patchwork.data-imaginist.com/) |
| **Séance 3** | **Data Mining & A/B Testing** <br> *Analyses factorielles, Tests stat & Prédiction* | *(À venir)* | **Exploration** : [ACP & ACM (FactoMineR)](http://factominer.free.fr/index_fr.html) <br> **Inférence** : [Comprendre la p-value](https://www.youtube.com/watch?v=vemZtEM63GY) · [Guide des A/B tests](https://vwo.com/ab-testing/) <br> **Modélisation** : [Arbres de décision](https://explained.ai/decision-tree-viz/) · [Régression Logistique](https://christophebenoit.org/cours-regression-logistique/) |
| **Séance 4** | **Données Géographiques (GIS)** <br> *Cartographie & analyse spatiale* | *(À venir)* | **R** : [sf (Simple Features)](https://r-spatial.github.io/sf/) · [leaflet (Cartes interactives)](https://rstudio.github.io/leaflet/) · [tmap](https://r-tmap.github.io/tmap/) |
| **Séance 5** | **Analyse de Réseaux (SNA)** <br> *Graphes sociaux et communautés* | *(À venir)* | **R** : [igraph](https://r.igraph.org/) · [ggraph](https://ggraph.data-imaginist.com/) · [tidygraph](https://tidygraph.data-imaginist.com/) |

---

## 📊 Visualisation de données (Ressources incontournables)

Pour aller plus loin sur la théorie et la pratique de la Dataviz (références du cours) :

- **Fondamentaux** : [Fundamentals of Data Visualization – Claus O. Wilke](https://clauswilke.com/dataviz/)
- **Pratique R** : [Modern Data Visualization with R – Robert Kabacoff](https://rkabacoff.github.io/datavis/)
- **Sociologie & Viz** : [Data Visualization: A practical introduction – Kieran Healy](https://socviz.co/index.html#preface)
- **Cheatsheets** : [Posit – Cheat Sheets (ggplot2, etc.)](https://rstudio.github.io/cheatsheets/)

---

## 📱 Dashboards & Reporting (Shiny & Quarto)

Outils essentiels pour la partie Business Intelligence et le rendu des projets :

- **Shiny (Web Apps)** :
  - [Mastering Shiny (Hadley Wickham)](https://mastering-shiny.org/) : La bible pour comprendre la réactivité.
  - [Shiny Gallery](https://shiny.posit.co/r/gallery/) : Pour s'inspirer d'exemples existants.
  - [bslib (Thèmes)](https://rstudio.github.io/bslib/) : Pour faire des dashboards modernes et esthétiques.

- **Quarto (Reporting)** :
  - [Documentation officielle](https://quarto.org/)
  - [Quarto Dashboards](https://quarto.org/docs/dashboards/) : Créer des dashboards statiques ou interactifs sans serveur complexe.

---

## 💡 Apprendre à coder (R & Tidyverse)

- **La référence absolue** : [R for Data Science (2e ed)](https://r4ds.hadley.nz/)
- **Manipulation de données** : [Introduction au Tidyverse (Julien Barnier)](https://juba.github.io/tidyverse/)
- **Style de code** : [The Tidyverse Style Guide](https://style.tidyverse.org/)

---

## 🎓 Projet & Évaluation

Le cours est validé par un projet de groupe (2-3 étudiants) consistant en :
1.  **Collecte de données** (Web scraping / API / Open Data).
2.  **Analyse & Visualisation** (Nettoyage, Dataviz, Storytelling, Data Mining).
3.  **Livrable** : Un rapport reproductible (format HTML Quarto) intégrant code et analyse métier.

---

## 🧰 Organisation du dépôt

Le dépôt est organisé par séance.
- `cours_n/` : Contient les slides (`.qmd` et `.html`), les scripts R associés et les images.
- `data_full/` : Contient les jeux de données bruts utilisés dans les exemples (ex: *gapminder*, *ifood*, *A/B testing*).

## 🚀 Utilisation

1.  Clonez ce dépôt ou téléchargez les fichiers.
2.  Ouvrez les fichiers `.qmd` ou `.R` dans **RStudio**.
3.  Assurez-vous d'avoir installé les librairies nécessaires (`tidyverse`, `quarto`, `shiny`, etc.).
4.  Les **slides** sont accessibles directement via GitHub Pages (liens dans le tableau ci-dessus).

---

## 📖 Licence

Ces supports sont mis à disposition pour un usage pédagogique dans le cadre du **M2 Marketing Digital**.  
Toute réutilisation ou diffusion en dehors de ce contexte doit citer l'auteur : **Olivier Caron**.