# ragno.pl

ragno.pl is a light crawler written in (SWI) Prolog

## USAGE

Start with one or more domains to be used as "seeds".

```bash
DOMAIN=https://it.wikipedia.com/ make run
```

Or using a docker container 

```bash
docker build -t ragnopl .
docker run --rm -ti -v ~/Downloads/ragnopl:/app/data ragnopl run
```

Good lucks!

## SAMPLE OUTPUT

Ragno creates a new file for each domain, for instance data/re/redaelli/https\:www.redaelli.org.json 

```json
{
  "external_links": [
    "http://caicarateb.netsons.org/",
    "http://cognome.alfemminile.com/w/cognomi/cognome-redaelli.html",
    "http://www.alpinicarate.it",
    "http://www.bandacarate.com/",
    "http://www.comune.caratebrianza.mi.it/",
    "http://www.democabrio.it/",
    "http://www.lagora.net",
    "http://www.marciacaratesi.it/",
    "http://www.rifugiocarate.it/",
    "http://www.uscaratese.it/",
    "http://www.vvfcarate.it/",
    "https://matteoredaelli.netlify.app/",
    "https://www.redaelli.org/",
    "https://www.redaelli.org/carlo/",
    "https://www.redaelli.org/marco/",
    "https://www.redaelli.org/matteo/",
    "https://www.redaelli.org/pictures/",
    "https://www.redaelli.org/sara/"
  ],
  "final_url":"https://www.redaelli.org/",
  "headers": {
    "accept_ranges":"bytes",
    "connection":"close",
    "content_length":10590,
    "content_type":"text/html",
    "date":"Wed, 04 May 2022 14:56:44 GMT",
    "etag":"\"5e49912e-295e-5b98e996561e0\"",
    "last_modified":"Sat, 23 Jan 2021 10:16:33 GMT",
    "server":"Apache",
    "x_aruba_cache":"NA"
  },
  "internal_links": [],
  "linked_domains": [
    "http://caicarateb.netsons.org/",
    "http://cognome.alfemminile.com/",
    "http://www.alpinicarate.it/",
    "http://www.bandacarate.com/",
    "http://www.comune.caratebrianza.mi.it/",
    "http://www.democabrio.it/",
    "http://www.lagora.net/",
    "http://www.marciacaratesi.it/",
    "http://www.rifugiocarate.it/",
    "http://www.uscaratese.it/",
    "http://www.vvfcarate.it/",
    "https://matteoredaelli.netlify.app/",
    "https://www.redaelli.org/"
  ],
  "url":"https://www.redaelli.org/"
}
```
