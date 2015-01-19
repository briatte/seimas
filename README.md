This repository contains code to build cosponsorship networks from bills passed in the [Lithuanian Parliament](http://www.lrs.lt/).

- [interactive demo](http://briatte.org/seimas)
- [static plots](http://briatte.org/seimas/plots.html)

# HOWTO

Replicate by running `make.r` in R.

The `data.r` script downloads information on bills and sponsors. All photos should download fine.

The `build.r` script then assembles the edge lists and plots the networks, with the help of a few routines coded into `functions.r`. Adjust the `plot`, `gexf` and `mode` parameters to skip the plots or to change the node placement algorithm.

# DATA

## Bills

- `legislature` – legislature years
- `uids` – bill unique identifier
- `urls` – bill URL
- `date` – date of introduction
- `text` – short title
- `authors` – first authors
- `cosponsors` – cosponsors

There is no strict separation between authors and cosponsors: when there are many authors, some of them are simply listed on a different page. The separation is technical, not formal.

## Sponsors

The sponsors data have multiple entries for each sponsor (one per legislature in which the sponsor sat).

- `legislature` – legislature years
- `url` – profile URL
- `photo` – photo URL
- `id` – unique identifier, first letter of first name + family name
- `name` – full name (no duplicates)
- `sex` – gender (F/M), imputed from title ("narė/narys")
- `born` – year of birth (int)
- `party` – party affiliation, abbreviated
- `constituency` – sponsor constituency (many missing)
- `mandates` – semicolon-separated mandate years, used to compute the `nyears` seniority variable

Note – the Brazauskas/Paulauskas coalition lists are coded as Social-Liberals (_socialliberalai_).
