Użytkowanie:

`make`

Następnie:
`./interpreter <ścieżka_do_programu>`
lub
`./interpreter`
by ręcznie wpisać ze standardowego wejścia program.

Język obsługiwany przez interpreter stanowi podzbiór języka, który opisuje gramatyka loltalk.cf. Nie zmieniono gramatyki, gdyż w oryginalnej z powodzeniem usunięto wszystkie konflikty i nieużywane reguły, co mogłoby okazać się być trudniejsze w przypadku bieżącej aktualizacji zakresu gramatyki. Ponadto rozszerzona gramatyka umożliwiłaby w przyszłości implementację dodatkowych funkcjonalności.

# Funkcjonalności obecnie nieobsługiwane, lecz zadeklarowane w pierwotnej wersji gramatyki #
* funkcje zagnieżdżone
* tablice
* funkcje wyższego rzędu, funkcje anonimowe, domknięcia
* inkrementacja, dekrementacja
* zwracanie funkcji oraz przekazywanie funkcji jako parametr
* niezmienniki oraz bloki zwalniające z obowiązywania niezmienników

Przewidywane jest również uzupełnienie implementacji o type-checker. Obecnie program nie sprawdza poprawności typów, zakładając, że będzie to stanowić zadanie type-checkera.

Program wymaga funkcji MAIN do uruchomienia.

Zmienne oraz nazwy funkcji są zapisywane wielkimi literami bądź jako liczby. System liczbowy oparty jest o wielokrotność ,,x" oraz ,,D" w słowie ,,xD". Każde "x" odejmuje jedynkę, każde "D" - dodaje.

`xD := 0`
`xxD := -1 `
`xDD := 1 `