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
    'Orga, Kennenlernen, Wissenschaft, [Folien](./slides/01_orga.html), [Protokoll](./protocols/01_protocol.md)',

    ### Sitzung 2
    'Experimente, Forschungsideen, [Folien](./slides/02_start.html), [Protokoll](./protocols/02_protocol.md)',

    ### Sitzung 3
    'Theorie, [Folien](./slides/03_theory.html), [Protokoll](./protocols/03_protocol.md)',

    ### Sitzung 4
    'Forschungsfrage, Preregistrierung und Hypothesen, keine Folien, [Protokoll](./protocols/04_protocol.md)',

    ### Sitzung 5
    'Design, [Protokoll](./protocols/05_protocol.md)',

    ### Sitzung 6
    'Finalisierung, [*Protokoll*](./protocols/06_protocol.md)',

    ### Sitzung 7
    '[*Protokoll*](./protocols/07_protocol.md)',

    ### Sitzung 8
    '[*Protokoll*](./protocols/08_protocol.md)',

    ### Sitzung 9
    '*Auswertung*, [*Protokoll*](./protocols/09_protocol.md)',

    ### Sitzung 10
    '[*Protokoll*](./protocols/11_protocol.md)',

    ### Sitzung entfällt
    '',

    ### Sitzung entfällt
    '',

    ### Sitzung entfällt
    '',

    ### Sitzung 11
    '*Poster*, [*Protokoll*](./protocols/11_protocol.md)',

    ### Sitzung 12
    '[*Protokoll*](./protocols/12_protocol.md)',

    ### Sitzung 13
    '[*Protokoll*](./protocols/13_protocol.md)',

    ### Sitzung 14
    '[*Protokoll*](./protocols/14_protocol.md)')

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

    ### delete link to other slides and protocols in html table
    mutate(
        Thema = str_remove_all(
            Thema,
            # (in "()") | in ", []" | in "[]"
            "(\\([^()]*\\))|(, \\[[^()]*\\])|(\\[[^()]*\\])")) %>%

    kable(
        format = 'html',
        table.attr = "style='width:100%;'",
        align = c("c","c","l"),
        caption = "Übersicht - Wintersemester 2019/20"
        ) %>%
    kable_styling(font_size = 14)

### Tabelle in markdown für index.md
sitzung_md <- kable(Sitzungen, format = 'markdown', align = c("c","c","l"))

#rm(day, i, Nr., Thema, Termine)
