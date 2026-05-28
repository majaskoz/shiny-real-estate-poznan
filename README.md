# Analiza Ofert Wynajmu Nieruchomości w Poznaniu (Shiny Dashboard)

Interaktywna aplikacja webowa służąca do przestrzennej i statystycznej analizy rynku nieruchomości mieszkaniowych w Poznaniu. Projekt łączy geoprzetwarzanie z dynamiczną wizualizacją danych, ułatwiając identyfikację trendów cenowych w poszczególnych dzielnicach.

## Wersja demonstracyjna (Live)
**[KLIKNIJ TUTAJ, ABY OTWORZYĆ DZIAŁAJĄCĄ APLIKACJĘ](TUTAJ_WKLEJ_LINK_Z_SHINYAPPS_IO)**

---

##  Kluczowe Funkcjonalności Dashboardu
* **Mapa przestrzenna cen za m²:** Interaktywna mapa Poznania oparta na warstwach geoprzestrzennych (`sf` i `leaflet`).
* **Analiza segmentu studenckiego:** Porównanie średnich cen najmu dla studentów oraz pozostałych grup za pomocą wykresów lollipop (`plotly`).
* **Struktura rynku w centrum:** Wykres kołowy typu sunburst obrazujący rozkład liczby pokoi w centralnych dzielnicach.
* **Agregacja tabelaryczna:** Czytelne podsumowanie statystyk dla poszczególnych rejonów wygenerowane przez pakiet `gt`.

---

## Jak uruchomić projekt lokalnie?

1. Sklonuj to repozytorium:
```bash
   git clone [https://github.com/majaskoz/shiny-real-estate-poznan.git](https://github.com/majaskoz/shiny-real-estate-poznan.git)
