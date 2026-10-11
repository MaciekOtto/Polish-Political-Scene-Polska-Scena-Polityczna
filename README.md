# Polish Political Scene – Multivariate Analysis in R
# Polska Scena Polityczna – analiza wielowymiarowa w R

🇬🇧 [English](#-english) | 🇵🇱 [Polski](#-polski)

![R](https://img.shields.io/badge/R-276DC3?logo=r&logoColor=white)
![License](https://img.shields.io/badge/license-MIT-green)

---

## 🇬🇧 English

### Overview

An independent university project that applies **multivariate statistical methods** to survey data on how respondents perceive **nine Polish politicians**. Each politician was rated on **17 personality and image traits** (e.g. honest, competent, charismatic, diplomatic, influential). The analysis explores which politicians are perceived as similar, how traits group together, and which traits are associated with which politicians.

The repository contains the full R script, the input data, a written report (PL and EN) and a project poster.

### Methods

| Area | Techniques |
|---|---|
| Descriptive statistics | Respondent profile (gender, education, age), trait rating summaries |
| Categorical association | Contingency tables, χ² test, Cramér's V, Tschuprow's T, (corrected) contingency coefficient |
| Reliability | Cronbach's α, split-half reliability (Spearman–Brown, Guttman) on a 24-item questionnaire |
| Similarity & distance | Jaccard distance/similarity, simple matching (Sokal–Michener), Gower distance for mixed-type variables |
| Cluster analysis | Hierarchical clustering (average and complete linkage), dendrograms, cluster cutting |
| Correlation & factorability | Spearman correlation matrices, Bartlett's test, Kaiser–Meyer–Olkin (KMO) |
| Dimension reduction | Parallel analysis and scree plots, PCA (varimax / oblimin), exploratory factor analysis (ML, principal axis, minres) |
| Correspondence analysis | Row/column profiles, inertia, quality of representation, contributions, biplots (17 traits × 9 politicians) |

### Repository structure

```
├── README.md
├── LICENSE
├── R/
│   └── polish_political_scene_analysis.R   # full, commented analysis script
├── data/
│   ├── ankietowani.xlsx   # respondent characteristics (gender, education, age)
│   ├── cechy.xlsx         # trait ratings of the politicians
│   ├── pytania.xlsx       # questionnaire items x1–x24 (reliability, PCA/EFA)
│   ├── tablice.xlsx       # data for contingency tables (age × answer)
│   ├── zerojeden.xlsx     # binary (0/1) matrix – Jaccard / simple matching
│   ├── mieszane.xlsx      # mixed-type variables – Gower distance
│   └── srednie.xlsx       # mean ratings (traits × politicians) – correspondence analysis
└── report/
    ├── Polish_Political_Scene_EN.pdf                    # report (English)
    ├── Polska Scena Polityczna Raport - Maciej Otto.pdf # report (Polish)
    └── Polska Scena Polityczna Plakat - Maciej Otto.pdf # poster (Polish)
```

### How to run

1. Clone the repository and open it in RStudio (set the working directory to the repository root).
2. Open `R/polish_political_scene_analysis.R`.
3. Run the script section by section. Missing packages are installed automatically
   (`readxl`, `psych`, `tidyverse`, `DescTools`, `corrplot`, `vegan`, `proxy`, `cluster`, `factoextra`, `ca`, and others – see section 0 of the script).

Input file paths are controlled by the `DATA_DIR` variable at the top of the script (default: `data`).

### Results

The full interpretation of results – cluster composition, factor structure, correspondence maps – is available in the report: [`report/Polish_Political_Scene_EN.pdf`](report/Polish_Political_Scene_EN.pdf).

### Notes

- Column names and some category values in the Excel files are in Polish (e.g. `TAK`/`NIE` = yes/no); the script accounts for this.
- This is a student project; the data come from a survey of a limited sample and the results should not be treated as representative of the Polish population.

### Author

**Maciej Otto** – [GitHub](https://github.com/MaciekOtto)

Licensed under the [MIT License](LICENSE).

---

## 🇵🇱 Polski

### Opis projektu

Samodzielny projekt zrealizowany podczas studiów, w którym zastosowano **metody statystycznej analizy wielowymiarowej** do danych ankietowych o tym, jak respondenci postrzegają **dziewięciu polskich polityków**. Każdy polityk był oceniany pod kątem **17 cech wizerunkowych** (m.in. uczciwy, kompetentny, charyzmatyczny, dyplomatyczny, wpływowy). Analiza pokazuje, którzy politycy są postrzegani podobnie, jak cechy grupują się w czynniki oraz które cechy są związane z którymi politykami.

Repozytorium zawiera pełny skrypt w R, dane wejściowe, raport (PL i EN) oraz plakat projektowy.

### Zastosowane metody

| Obszar | Techniki |
|---|---|
| Statystyki opisowe | Profil respondentów (płeć, wykształcenie, wiek), podsumowania ocen cech |
| Zależności między cechami jakościowymi | Tablice kontyngencji, test χ², V Cramera, T Czuprowa, (skorygowany) współczynnik kontyngencji |
| Rzetelność | α Cronbacha, rzetelność połówkowa (Spearman–Brown, Guttman) dla kwestionariusza z 24 pytaniami |
| Podobieństwo i odległość | Odległość/podobieństwo Jaccarda, Simple Matching (Sokala–Michenera), odległość Gowera dla zmiennych mieszanych |
| Analiza skupień | Hierarchiczna analiza skupień (średnie i pełne wiązanie), dendrogramy, wyodrębnianie grup |
| Korelacje i zasadność analizy czynnikowej | Macierze korelacji Spearmana, test Bartletta, miara KMO |
| Redukcja wymiarów | Analiza równoległa i wykresy osypiska, PCA (varimax / oblimin), eksploracyjna analiza czynnikowa (ML, osi głównych, minres) |
| Analiza korespondencji | Profile wierszowe/kolumnowe, inercja, jakość reprezentacji, wkłady, biploty (17 cech × 9 polityków) |

### Struktura repozytorium

```
├── README.md
├── LICENSE
├── R/
│   └── polish_political_scene_analysis.R   # pełny, skomentowany skrypt analizy
├── data/
│   ├── ankietowani.xlsx   # charakterystyka respondentów (płeć, wykształcenie, wiek)
│   ├── cechy.xlsx         # oceny cech polityków
│   ├── pytania.xlsx       # pytania kwestionariusza x1–x24 (rzetelność, PCA/EFA)
│   ├── tablice.xlsx       # dane do tablic kontyngencji (wiek × odpowiedź)
│   ├── zerojeden.xlsx     # macierz zero-jedynkowa – Jaccard / Simple Matching
│   ├── mieszane.xlsx      # zmienne mieszane – odległość Gowera
│   └── srednie.xlsx       # średnie oceny (cechy × politycy) – analiza korespondencji
└── report/
    ├── Polish_Political_Scene_EN.pdf                    # raport (angielski)
    ├── Polska Scena Polityczna Raport - Maciej Otto.pdf # raport (polski)
    └── Polska Scena Polityczna Plakat - Maciej Otto.pdf # plakat (polski)
```

### Jak uruchomić

1. Sklonuj repozytorium i otwórz je w RStudio (katalog roboczy ustaw na główny folder repozytorium).
2. Otwórz plik `R/polish_political_scene_analysis.R`.
3. Uruchamiaj skrypt sekcja po sekcji. Brakujące pakiety instalują się automatycznie
   (`readxl`, `psych`, `tidyverse`, `DescTools`, `corrplot`, `vegan`, `proxy`, `cluster`, `factoextra`, `ca` i inne – zob. sekcja 0 skryptu).

Ścieżka do plików z danymi jest ustawiana zmienną `DATA_DIR` na początku skryptu (domyślnie: `data`).

### Wyniki

Pełna interpretacja wyników – skład grup, struktura czynnikowa, mapy korespondencji – znajduje się w raporcie: [`report/Polska Scena Polityczna Raport - Maciej Otto.pdf`](report/Polska%20Scena%20Polityczna%20Raport%20-%20Maciej%20Otto.pdf).

### Uwagi

- Nazwy kolumn i niektóre wartości kategorii w plikach Excel są po polsku (np. `TAK`/`NIE`); skrypt to uwzględnia.
- To projekt studencki; dane pochodzą z ankiety na ograniczonej próbie, więc wyników nie należy traktować jako reprezentatywnych dla populacji Polski.

### Autor

**Maciej Otto** – [GitHub](https://github.com/MaciekOtto)

Licencja: [MIT](LICENSE).
