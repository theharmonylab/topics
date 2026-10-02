## Submission: topics 1.0.1

This release removes the dependency on 'textmineR'.

CRAN notified us (2026-10-01) that 'float' will be archived on 2026-10-21, which
would also archive its strong reverse dependencies. 'topics' was exposed only
through textmineR -> text2vec -> rsparse -> float. 'textmineR' is now removed
from Imports: document-term matrices are built with 'quanteda' (already in
Imports), and the two small helpers previously used from 'textmineR' are
implemented within 'topics'. Results are unchanged (identical document-term
matrices and model outputs on the package's example data).

## R CMD check results

TODO before submitting: run R CMD check --as-cran locally and on win-builder
(devtools::check_win_devel()) and replace this with the results, e.g.
0 errors | 0 warnings | 1 note (the NOTE being the version update).

## Reverse dependencies

'text' imports 'topics' and needs no changes; with this release it no longer
depends on 'float' through 'topics'.
