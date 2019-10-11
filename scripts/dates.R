# creates for dates in class

### Library
library(tidyverse)
library(knitr)
library(kableExtra) ## for "Awesome HTML Table with knitr::kable"

### 17 Sitzungen (inkl. Vorlesungsfreitage/ -zeit)
Sitzung <- c(1:17)

### Daten für jede Sitzung
Termin <- c()

### für jede Sitzung
for (i in Sitzung){

    ### einen Tag hinzufügen, beginnend von der ersten Sitzung
    Tag <- (as.Date("2019-10-14") + (i*7-7)) %>% format("%d. %B %Y")

    ### den Tag dem Termin hinzufügen
    Termin <- c(Termin, Tag)
}


### Thema für jede Sitzung
Thema <- c(

    ### Sitzung 1
    # Inhalt
    'Orga, Kennenlernen, Wissenschaft, [Folien](./slides/01-orga.html), [Protokoll](./protocols/01-protocol.html)',

    ### Sitzung 2
    '',

    ### Sitzung 3
    '',

    ### Sitzung 4
    '',

    ### Sitzung 5
    '',

    ### Sitzung 6
    '',

    ### Sitzung 7
    '',

    ### Sitzung 8
    '',

    ### Sitzung 9
    '',

    ### Sitzung 10
    '',

    ### Sitzung entfällt
    '',

    ### Sitzung entfällt
    '',

    ### Sitzung entfällt
    '',

    ### Sitzung 11
    '',

    ### Sitzung 12
    '',

    ### Sitzung 13
    '',

    ### Sitzung 14
    '')

### Nr. Termin Thema in einer Tabelle
Sitzungen <- data.frame(Sitzung, Termin, Thema) %>%

    ### Vorlesungsfreie Tage
    filter(

        ### Weihnachten
        Termin != "23. Dezember 2019",

        ### Silvester
        Termin != "30. Dezember 2019",

        ### Hlg. Drei Könige
        Termin != "06. Januar 2020") %>%

    group_by(str_starts(Thema, "Entfällt")) %>%

    mutate(
        Sitzung = 1:n()) %>% ungroup() %>%

    mutate(

        ### Anzahl Sitzungen ohne vorlesungsfreie Tage
        Sitzung = case_when(
            str_starts(Thema, "Entfällt") ~ " ",
            !str_starts(Thema, "Entfällt") ~
                ifelse(
                    str_count(Sitzung) < 2,
                    str_c("0", Sitzung),
                    Sitzung))) %>%

    select('Nr.' = Sitzung, Termin, Thema)


### Tabelle in html für slides
sitzung_html <-
    Sitzungen %>%

    ### delete link to other slides in html table
    mutate(
        Thema = ifelse(

            # when pattern is found
            str_detect(string=Thema, pattern="Folien"),

            # extracts (deletes) everything following the pattern
            str_extract(string=Thema, pattern=".+?(?=, \\[Folien\\])"),
            as.character(Thema))) %>%

    kable(
        format = 'html',
        table.attr = "style='width:100%;'",
        align = c("c","c","l"),
        caption = "Übersicht - Sommersemester 2019"
        ) %>%
    kable_styling(font_size = 14)

### Tabelle in markdown für index.md
sitzung_md <- kable(Sitzungen, format = 'markdown', align = c("c","c","l"))

#rm(day, i, Nr., Thema, Termine)
