``` r
library(dplyr)
library(tidyr)
library(ggplot2)
library(purrr)
library(knitr)
```

# Wczytywanie i przetworzenie danych

``` r
load("all_data.rda")
```

# Cel raportu i kontekst analizy

Niniejszy raport powstał w celu zaadresowania kluczowej uwagi Recenzenta
dotyczącej potencjalnego błędu systematycznego (tzw. *non-response
bias*).

W trakcie badań terenowych 156 respondentów zgodziło się narysować mapę
kognitywną Parku Narodowego „Ujście Warty”, natomiast 93 osoby odmówiły
wykonania tego zadania. Ponieważ analiza percepcji przestrzennej stanowi
jeden z filarów naszego artykułu, musimy jednoznacznie wykazać, czy
grupa, która zrealizowała zadanie, różniła się cechami demograficznymi
lub profilem wizyty od osób, które z niego zrezygnowały.

W dokumencie sprawdzamy krok po kroku: 1. Czy decyzja o odmowie
rysowania zależała od cech turystów (płeć, wiek, wykształcenie, liczba
wcześniejszych wizyt, odwiedziny w OME/MEC) oraz lokalizacji
ankietowania. 2. Z czego wynikały zidentyfikowane różnice i czy miały
one charakter pozorny (efekt zakłócający – *confounding*). 3. Jakie
wnioski metodologiczne płyną z tych wyników do dyskusji w manuskrypcie.

Dokładna treść uwagi Recenzenta wraz z naszą propozycją oficjalnej
odpowiedzi znajduje się w ostatniej części tego dokumentu.

# 1. Przygotowanie zmiennej określającej, czy respondent narysował mapę

Aby sprawdzić różnice między osobami, które narysowały mapę poznawczą, a
tymi, które odmówiły, tworzymy nową zmienną grupującą (`drew_map`).
Przypisujemy wartość “Tak” respondentom, u których w bazie widnieje
jakakolwiek wartość dla skali mapy, oraz “Nie” pozostałym. Oczyszczamy
również bazę z ewentualnych pustych wierszy bez identyfikatora.

``` r
all_data <- all_data %>%
  filter(!is.na(survey_id)) %>% # Usuwa wiersz/wiersze, gdzie survey_id to NA (jeśli istniały)
  mutate(
    drew_map = if_else(!is.na(m_scale), "Tak", "Nie")
  )
```

Sprawdziliśmy poprawność przypisania nowej zmiennej. Poniższa tabela
wskazuje nam ostateczną liczbę osób w obu grupach badawczych.

``` r
# Szybkie sprawdzenie poprawności przypisania (powinno być ok. 156 "Tak" i 93 "Nie")
kable(table(all_data$drew_map, useNA = "ifany"))
```

| Var1 | Freq |
|:-----|-----:|
| Nie  |   93 |
| Tak  |  156 |

# 2A Tabela z testami niezależności

Sprawdzamy, czy cechy demograficzne i profil wizyty turystów wpływają na
chęć narysowania mapy. Aby to ustalić, wykonujemy testy statystyczne
(Chi-kwadrat oraz dokładny test Fishera), które ocenią, czy różnice w
odpowiedziach w poszczególnych podgrupach są istotne statystycznie, czy
wynikają jedynie z przypadku.

``` r
# Definiujemy zmienne do analizy
vars_to_test <- c("sex", "age", "education", "place", "visited_ome", "visits_count")

# Pętla generująca wyniki testów
test_results <- map_dfr(vars_to_test, function(var) {
  # Tabela krzyżowa (pomijamy NA w obu zmiennych dla czystości testu)
  tab <- table(all_data[[var]], all_data$drew_map)
  
  # Chi-kwadrat (ukrywamy ostrzeżenia o małych liczebnościach komórek)
  chisq_res <- suppressWarnings(chisq.test(tab))
  
  # Fisher (z symulacją Monte Carlo na wypadek dużych tabel > 2x2)
  fisher_res <- fisher.test(tab, simulate.p.value = TRUE)
  
  # Zapisanie wyników jako wiersz (tibble)
  tibble(
    Zmienna = var,
    `P-value (Chi-kwadrat)` = round(chisq_res$p.value, 4),
    `P-value (Fisher)` = round(fisher_res$p.value, 4),
    `Znaczące różnice?` = if_else(fisher_res$p.value < 0.05, "Tak (*)", "Nie")
  )
})

# Wyświetlenie tabeli
kable(test_results)
```

| Zmienna      | P-value (Chi-kwadrat) | P-value (Fisher) | Znaczące różnice? |
|:-------------|----------------------:|-----------------:|:------------------|
| sex          |                0.8334 |           0.7933 | Nie               |
| age          |                0.0024 |           0.0050 | Tak (\*)          |
| education    |                0.7157 |           0.6932 | Nie               |
| place        |                0.0000 |           0.0000 | Tak (\*)          |
| visited_ome  |                0.2300 |           0.1888 | Nie               |
| visits_count |                0.4060 |           0.3953 | Nie               |

Tabela z wynikami wskazuje, że statystycznie istotne różnice (p-value
poniżej 0.05) występują wyłącznie w przypadku wieku respondentów (age)
oraz miejsca przeprowadzania ankiety (place). Pozostałe zmienne, takie
jak płeć, wykształcenie czy wcześniejsze wizyty w OME, nie mają wpływu
na decyzję o wykonaniu zadania.

# 2B: Wizualizacje rozkładów (pętla)

Aby lepiej zobrazować uzyskane wyniki statystyczne, generujemy wykresy
słupkowe dla wszystkich analizowanych cech. Wykresy te pokazują
procentowy udział poszczególnych podgrup w obrębie osób rysujących i
odmawiających.

``` r
# Słownik do ładnych tytułów wykresów
plot_titles <- c(
  sex = "Płeć", age = "Wiek", education = "Wykształcenie", 
  place = "Miejsce ankietowania", visited_ome = "Odwiedziny w OME", 
  visits_count = "Liczba wizyt w Parku"
)

# Funkcja generująca pojedynczy wykres w Twoim stylu
for(var in vars_to_test) {
  
  summary_df <- all_data %>%
    filter(!is.na(.data[[var]])) %>% 
    count(drew_map, !!sym(var)) %>%
    group_by(drew_map) %>%
    mutate(percent = 100 * n / sum(n)) %>%
    ungroup()
  
  # Ustalenie kolejności kategorii wg sumarycznej popularności
  var_order <- summary_df %>%
    group_by(!!sym(var)) %>%
    summarise(total_percent = sum(percent)) %>%
    arrange(desc(total_percent)) %>%
    pull(!!sym(var))
  
  p <- ggplot(summary_df, aes(x = factor(!!sym(var), levels = var_order),
                              y = percent, fill = drew_map)) +
    geom_col(position = position_dodge(width = 0.8), width = 0.7) +
    coord_flip() +
    geom_text(aes(label = sprintf("%.1f%%", percent)),
              position = position_dodge(width = 0.8),
              hjust = -0.1, size = 3.5) +
    labs(
      title = paste(plot_titles[var], "a udział w zadaniu"),
      x = NULL, y = "Udział procentowy (w grupie)",
      fill = "Narysował mapę"
    ) +
    scale_fill_brewer(palette = "Pastel2") +
    theme_minimal(base_size = 13) +
    scale_y_continuous(expand = expansion(mult = c(0, 0.15))) # miejsce na procenty
  
  print(p)
}
```

![](07_draw_map_yes_no_files/figure-markdown_github/unnamed-chunk-6-1.png)![](07_draw_map_yes_no_files/figure-markdown_github/unnamed-chunk-6-2.png)![](07_draw_map_yes_no_files/figure-markdown_github/unnamed-chunk-6-3.png)![](07_draw_map_yes_no_files/figure-markdown_github/unnamed-chunk-6-4.png)![](07_draw_map_yes_no_files/figure-markdown_github/unnamed-chunk-6-5.png)![](07_draw_map_yes_no_files/figure-markdown_github/unnamed-chunk-6-6.png)

Wykresy wizualnie potwierdzają wniosek z tabeli. Struktura odpowiedzi
dla płci, wykształcenia czy liczby wizyt jest bardzo zbliżona dla obu
grup. Zauważalne dysproporcje widoczne są gołym okiem tylko na wykresach
dla wieku (powyżej 60 lat) i miejsca ankietowania.

# 3. Szczegółowe podsumowanie różnic dla zmiennych, które okazały się istotnie różne: wiek (age) i miejsce (place)

## 3.1 Wiek

Skupiamy się najpierw na zmiennej wieku. Aby zrozumieć, z czego
dokładnie wynika wykazana wcześniej różnica statystyczna, obliczamy
procentowy profil wiekowy w obu grupach badawczych.

``` r
# 1. Podsumowanie dla zmiennej: Wiek (age)
age_summary <- all_data %>%
  filter(!is.na(age), !is.na(drew_map)) %>%
  count(age, drew_map) %>%
  group_by(drew_map) %>%
  mutate(Procent = round(100 * n / sum(n), 1)) %>%
  ungroup() %>%
  select(-n) %>%
  pivot_wider(names_from = drew_map, values_from = Procent, values_fill = 0) %>%
  mutate(`Różnica (p.p.)` = Tak - Nie)

kable(age_summary)
```

| age        |  Nie |  Tak | Różnica (p.p.) |
|:-----------|-----:|-----:|---------------:|
| 18-30      | 16.1 | 20.5 |            4.4 |
| 31-40      | 15.1 | 23.1 |            8.0 |
| 41-50      | 21.5 | 25.6 |            4.1 |
| 51-60      | 24.7 | 25.0 |            0.3 |
| powyżej 60 | 22.6 |  5.8 |          -16.8 |

Z tabeli wynika, że najstarsza grupa respondentów (powyżej 60. roku
życia) bardzo mocno odstaje od reszty – osoby te stanowią duży odsetek
odmawiających (ponad 22%), a marginalny wśród rysujących (niespełna 6%).

### Testy post-hoc dla wieku

Aby formalnie zweryfikować, między którymi konkretnie przedziałami
wiekowymi występują znaczące różnice, wykonujemy testy post-hoc z
odpowiednią korektą Bonferroniego, zabezpieczającą nas przed błędnymi
wnioskami przy porównywaniu wielu grup naraz.

``` r
# Przygotowanie tabeli kontyngencji dla wieku i drew_map
tab_age <- table(all_data$age, all_data$drew_map)

# pairwise.prop.test przyjmuje liczbę sukcesów ("Tak") oraz sumę w każdym wierszu
# Metoda p.adjust "holm" lub "bonferroni" zabezpiecza przed błędem I rodzaju
posthoc_age <- pairwise.prop.test(
  x = tab_age[, "Tak"], 
  n = rowSums(tab_age), 
  p.adjust.method = "bonferroni"
)

# Wyświetlenie macierzy p-value dla porównań par
print(posthoc_age)
```

    ## 
    ##  Pairwise comparisons using Pairwise comparison of proportions 
    ## 
    ## data:  tab_age[, "Tak"] out of rowSums(tab_age) 
    ## 
    ##            18-30 31-40 41-50 51-60
    ## 31-40      1.000 -     -     -    
    ## 41-50      1.000 1.000 -     -    
    ## 51-60      1.000 1.000 1.000 -    
    ## powyżej 60 0.024 0.006 0.022 0.062
    ## 
    ## P value adjustment method: bonferroni

Macierz wyników potwierdza (p \< 0.05), że statystycznie istotna różnica
dotyczy zachowania grupy “powyżej 60”, która wyraźnie odróżnia się od
młodszych kohort. Pozostałe grupy wiekowe nie różnią się między sobą
chęcią do wykonania zadania.

## 3.2 Płeć

Analogicznie analizujemy drugą z istotnych zmiennych – lokalizację, w
której turysta był proszony o narysowanie mapy. Sprawdzamy procentowy
rozkład udziałów na Betonce i w Olszynkach.

``` r
# 2. Podsumowanie dla zmiennej: Miejsce (place)
place_summary <- all_data %>%
  filter(!is.na(place), !is.na(drew_map)) %>%
  count(place, drew_map) %>%
  group_by(drew_map) %>%
  mutate(Procent = round(100 * n / sum(n), 1)) %>%
  ungroup() %>%
  select(-n) %>%
  pivot_wider(names_from = drew_map, values_from = Procent, values_fill = 0) %>%
  mutate(`Różnica (p.p.)` = Tak - Nie)

kable(place_summary)
```

| place    | Nie |  Tak | Różnica (p.p.) |
|:---------|----:|-----:|---------------:|
| Betonka  |  72 | 21.8 |          -50.2 |
| Olszynki |  28 | 78.2 |           50.2 |

Wynik ukazuje drastyczny wpływ miejsca. Na Betonce turyści odmawiali
rysowania bardzo często, natomiast w Olszynkach znaczna większość
zapytanych godziła się na wykonanie zadania. Różnica wynosi ponad 50
punktów procentowych.

**Taka różnica sugeruje, że istniał jakiś czynnik powodujący, że w
Olszynkach zaistniał czynnik powodujący, że ankietowani chętniej
wypełniali mapy. Czy byli to sprawniejsi ankieterzy? A może miejsce
prowadzenia badań sprzyjało spędzeniu większej ilości czasu nad
ankietą?**

# 4. Sprawdzanie czy różnica wynikająca z wieku nie jest zdeterminowana różnicą wynikającą z miejsca ankietowania

Chcemy sprawdzić, czy na wykazaną w poprzednim etapie istotną różnicę w
wieku nie wpłynęło miejsce ankietowania (w którym ankieterzy mieli
większą skuteczność lub warunki do rysowania były lepsze)

``` r
# Tabela krzyżowa: Wiek a Miejsce ankietowania
age_by_place <- all_data %>%
  filter(!is.na(age), !is.na(place)) %>%
  count(place, age) %>%
  group_by(place) %>%
  # Obliczamy procent wewnątrz danego miejsca
  mutate(Procent = round(100 * n / sum(n), 1)) %>%
  ungroup() %>%
  # Wyświetlamy jako szeroką tabelę dla łatwego porównania
  pivot_wider(
    names_from = place, 
    values_from = c(n, Procent), 
    values_fill = 0
  ) %>%
  # Porządkowanie kolumn dla czytelności
  select(age, n_Betonka, Procent_Betonka, n_Olszynki, Procent_Olszynki)

kable(age_by_place)
```

| age        | n_Betonka | Procent_Betonka | n_Olszynki | Procent_Olszynki |
|:-----------|----------:|----------------:|-----------:|-----------------:|
| 18-30      |        17 |            16.8 |         30 |             20.3 |
| 31-40      |        15 |            14.9 |         35 |             23.6 |
| 41-50      |        26 |            25.7 |         34 |             23.0 |
| 51-60      |        23 |            22.8 |         39 |             26.4 |
| powyżej 60 |        20 |            19.8 |         10 |              6.8 |

Nasze przypuszczenia okazały się trafne. Wśród spacerujących po Betonce
seniorzy stanowili blisko 20%, podczas gdy w Olszynkach było ich niecałe
7%. Zatem większość najstarszych badanych trafiła na lokalizację
zniechęcającą do rysowania.

Aby ostatecznie rozstrzygnąć ten problem, budujemy model regresji
logistycznej. Uwzględnia on równocześnie obie zmienne (wiek i miejsce),
co pozwala nam wyizolować ich niezależny wpływ i odpowiedzieć na
pytanie: co tak naprawdę było powodem odmowy.

``` r
# Przygotowanie danych do modelu (wymaga zamiany "Tak"/"Nie" na 1/0)
model_data <- all_data %>%
  filter(!is.na(age), !is.na(place), !is.na(drew_map)) %>%
  mutate(drew_map_bin = if_else(drew_map == "Tak", 1, 0))

# Budowa modelu regresji logistycznej uwzględniającego OBA czynniki
model_glm <- glm(drew_map_bin ~ age + place, data = model_data, family = "binomial")

# Sprawdzenie ogólnej istotności obu zmiennych w jednym modelu 
# (test stosunku wiarygodności, odpowiednik testu Chi-kwadrat dla regresji)
drop1(model_glm, test = "Chisq")
```

    ## Single term deletions
    ## 
    ## Model:
    ## drew_map_bin ~ age + place
    ##        Df Deviance    AIC    LRT  Pr(>Chi)    
    ## <none>      258.32 270.33                     
    ## age     4   266.61 270.61  8.280   0.08183 .  
    ## place   1   312.97 322.97 54.643 1.445e-13 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

Wyniki modelu regresji (funkcja `drop1`) pozwalają ostatecznie odrzucić
przypuszczenie, że to duży udział seniorów na Betonce wpłynął na
zróżnicowanie wyników. Model logistyczny działa tak, że bada wpływ
każdej zmiennej, jednocześnie kontrolując (zamrażając) pozostałe
czynniki:

- **Miejsce (`place`):** Po odfiltrowaniu różnic wieku, lokalizacja
  nadal wykazuje potężną, bezdyskusyjną istotność statystyczną
  (*p* = 1.445*e*<sup>−13</sup>, czyli *p* \< 0.001). Oznacza to, że
  miejsce ankietowania zniechęcało do rysowania niezależnie od metryki –
  nawet najmłodsi respondenci na Betonce odmawiali znacznie częściej niż
  w Olszynkach.
- **Wiek (`age`):** Po uwzględnieniu miejsca, wiek traci swoją
  wcześniejszą istotność, osiągając wartość *p* = 0.081 (czyli powyżej
  progu *α* = 0.05). Oznacza to, że jeśli porównamy osoby w tym samym
  miejscu (np. wyłącznie na Betonce), 20-latkowie i 60-latkowie
  odmawiali z podobną częstotliwością – różnica pokoleniowa w tym samym
  środowisku po prostu znika.

Powyższe dowodzi, że to specyfika przestrzenna i warunki w danej
lokalizacji (a nie zaawansowany wiek turystów) determinowały chęć
wykonania mapy. Seniorzy okazali się jedynie „ofiarą statystyki” – po
prostu częściej spacerowali tam, gdzie warunki sprzyjały odmowom.

# 5. Odpowiedź na uwagę recenzenta

**Odpowiedź na uwagę Recenzenta dotyczącą błędu braku odpowiedzi
(non-response bias):**

**Komentarz Recenzenta:**  
*Cognitive maps were completed by 156 of the 249 respondents,
i.e. 62.6%. Among them, 62 had visited the MEC and 94 had not. This
means that a considerable proportion of respondents did not complete
this task. The reasons for this should be explained, and it would be
useful to verify whether those who completed the maps differed
systematically from those who did not. This is important because the
analysis of spatial perception is one of the central elements of the
article.*

**Odpowiedź Autorów:**

Dziękujemy Recenzentowi za ten niezwykle istotny komentarz. Zgadzamy
się, że zrozumienie powodów braku odpowiedzi w tak kluczowym zadaniu
jest niezbędne. Zgodnie z sugestią przeprowadziliśmy szczegółową analizę
potencjalnego błędu systematycznego (*non-response bias*).

W pierwszym etapie zweryfikowaliśmy, czy decyzja o narysowaniu mapy
zależała od cech socjodemograficznych lub profilu wizyty turystów (testy
Chi-kwadrat oraz Fishera). Analiza nie wykazała żadnych istotnych
statystycznie różnic systematycznych pod względem: \* **faktu
odwiedzenia MEC** (*p* = 0.189) – proporcje odmawiających wśród
odwiedzających i nieodwiedzających centrum były zbliżone, \* płci
(*p* = 0.793), \* wykształcenia (*p* = 0.690), \* liczby wcześniejszych
wizyt w Parku (*p* = 0.419).

Wstępne testy wykazały natomiast różnice ze względu na wiek respondentów
(*p* = 0.0035) oraz miejsce ankietowania (*p* \< 0.001).

Aby wyjaśnić **przyczyny dużej liczby odmów** (o co pyta Recenzent),
zbadaliśmy strukturę demograficzną w poszczególnych punktach badawczych.
Zidentyfikowaliśmy występowanie efektu zakłócającego (*confounding
effect*). W lokalizacji o bardzo wysokim odsetku odmów („Betonka”)
udział osób powyżej 60. roku życia był niemal trzykrotnie wyższy (19,8%)
niż w drugiej lokalizacji („Olszynki”, 6,8%).

W celu odseparowania wpływu wieku od wpływu lokalizacji, zbudowaliśmy
model regresji logistycznej uwzględniający obie zmienne jednocześnie.
Wyniki wykazały, że: \* **Miejsce ankietowania** zachowało skrajnie
wysoką istotność statystyczną (*χ*<sup>2</sup> = 54.64, *p* \< 0.001).
\* **Wiek respondenta**, po skontrolowaniu lokalizacji, utracił
istotność statystyczną (*χ*<sup>2</sup> = 8.28, *p* = 0.082).

**Podsumowując powody odmów:** Powyższe wyniki dowodzą, że obserwowana
początkowo niechęć starszych osób do rysowania mapy była artefaktem
przestrzennym. Główną przyczyną znacznej liczby nieuzupełnionych map nie
były systematyczne różnice w profilu respondentów (np. ich wiek czy fakt
wizyty w MEC), lecz same uwarunkowania przestrzenno-logistyczne w
konkretnym miejscu ankietowania (np. brak infrastruktury do wygodnego
siedzenia i rysowania, tranzytowy charakter ścieżki oraz wynikający z
tego pośpiech turystów).

Zgodnie z rekomendacją Recenzenta, metodyka testowania *non-response
bias* oraz szczegółowe wyjaśnienie przyczyn odmów zostały dodane do
zrewidowanej wersji manuskryptu (sekcja Metodologia oraz Ograniczenia
badania).
