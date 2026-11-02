## Slides PDFs

This directory contains the released slide PDFs, published by `slidemk --release`.

Changes in this directory are reflected on the course website, so **changes to
file names** need to be adjusted in the website repository.

The layout mirrors `slides/`:

```
slides-pdf
├── 01-intro
│   ├── 01-basics.pdf
│   ├── 02-learning.pdf
│   ...
├── 05-featsel
│   ├── 01-intro.pdf
│   ...
│   └── 99-literature.pdf
...
```

Here, `slides-pdf/<chapter>/<NN>-<topic>.pdf` is built from
`slides/<chapter>/<NN>-<topic>.tex`, and `<chapter>/99-literature.pdf` from
`slides/<chapter>/references.bib`.

The top-level PDFs are not built from this repo's decks; they stay because the
`slds-lmu/i2ml` website links them:

- `slides-fs-*.pdf`, `chapter-literature-feature-selection.pdf`: copies from
  `lecture_sl`.
- `slides-preprocessing-*.pdf`, `slides-imputation-*.pdf`: renders of the old
  Rmd sources in `slides/04-preproc/rmds/` and
  `slides/06-imputation/rmds_from_eds_folder/`. Their content now lives,
  restructured, in the `04-preproc` and `06-imputation` decks.
