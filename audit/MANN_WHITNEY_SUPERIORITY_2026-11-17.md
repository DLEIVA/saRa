## Mann-Whitney superiority (2026-11-17)

Wilcoxon: developer confirmed full suite 926 pass, 0 fail/warn/skip,
12.8 s; manual single-pair legend, restoration, HTML/PDF and .omv accepted.

Mann-Whitney manual fixture: A=(3,5,5,8,9), B=(2,4,5,6,7,10,12).
U=16; Z=-0.2453209547; bilateral p with continuity correction=.870087532,
without correction=.806207928. Rank-biserial=.085714286, f=.542857143.
Rank sums A=31, B=47. GUI agrees except superiority=.4290.
There are 15 strict A>B comparisons and 2 ties among 35 comparisons;
P(A>B)+.5*P(A=B)=16/35=.457142857.
Correction uses existing tie-adjusted U divided by group-size product.
Independent pairwise-definition tests cover ties, group reversal, alternatives,
missing data and complete separation. R unavailable here: tests pending.
