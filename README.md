simulateur
==========

Simulateur de netlists écrit en OCaml.

Pour le compiler :
`ocamlbuild main.byte`

Exemple pour le lancer :
`./main.byte test/fulladder.net`

---

    ./main.byte [-d] [-b {batch.b}] [-r {data.r}] netlist.net

Options :
- `-b {batch.b}`  Batch mode. Lit les entrées dans le fichier `{batch.b}`,
    un fichier texte avec le format suivant :
    - un entier `nb_cycles` = nombres de cycles à simuler,
    - dans l'ordre où elles sont déclarées dans le bloc `INPUT`
        de la netlist, les valeurs des entrées à chaque cycle.
- `-r {data.r}`  Specifie un fichier `{data.r}` qui initialise les ROM et RAM
    (eventuelles) de la netlist.
- `-d`  Debug mode. Affiche les valeurs des variables intermédiaires.

Pour des infos plus détaillées, voir le rapport, dans le dossier `rapport`.

