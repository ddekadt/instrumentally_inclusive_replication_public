# Standard-error audit of the original 2024 replication archive

**Object audited:** Turnbull-Dugarte & López Ortega (2024), *Instrumentally Inclusive: The Political
Psychology of Homonationalism*, APSR — original Dataverse replication archive, as mirrored at
`https://github.com/ddekadt/instrumentally_inclusive_replication_public` under `clean_replication/`.

**Files audited (all R code in the archive; downloaded as raw text and read line by line):**

| File | Paper outputs (per `Dataverse_files/ReadMe.pdf`) |
|---|---|
| `clean_replication/Dataverse_files/study1.R` | Figures 3, 4, A4, A6, A7, A9; Tables A7, A8 |
| `clean_replication/Dataverse_files/study2.R` | Figures 6, 7, 8, A5; Tables A9, A10, A11 |
| `clean_replication/Dataverse_files/multiverse.R` | Figures A10, A11, A12, A13 |
| `clean_replication/descriptiveplots.R` | Figures A1, A2, A3, A8 |
| `clean_replication/study1_summarystats.R` | Tables A1, A2, A3 |
| `clean_replication/study2_summarystats.R` | Tables A4, A5, A6 |

There are no other `.R`, `.Rmd` or `.qmd` files under `clean_replication/`. The full repo tree was
enumerated via the GitHub trees API before downloading, so nothing was missed.

Detailed row-by-row evidence: `se_call_audit.csv` (73 rows, one per model-estimation call paired
with the reporting call it feeds).

---

## 1. Counts

### 1a. By estimation call (73 rows = every model-estimation call in the archive)

| Category | Count |
|---|---|
| **A. Robust, correctly obtained** | **6** |
| **B. Robust requested but silently ignored** | **11** |
| **C. No robust SEs requested at all** | **36** |
| **D. Not applicable / no SEs reported** | **20** |
| **Total** | **73** |

### 1b. By distinct reporting call (56 = every table/figure/console-print call)

Tables with several models are collapsed to one reporting call here (e.g. Table A9's four `lm()`
calls feed a single `modelsummary()` call).

| Category | Count |
|---|---|
| A | 6 |
| B | 3 |
| C | 27 |
| D | 20 |
| **Total** | **56** |

### 1c. By study (estimation-call rows)

| Study | A | B | C | D | Total |
|---|---|---|---|---|---|
| Study 1 (UK) | 6 | 4 | 14 | 9 | 33 |
| Study 2 (Spain) | 0 | 7 | 22 | 7 | 36 |
| NA (ESS/Eurobarometer descriptives, power sim) | 0 | 0 | 0 | 4 | 4 |
| **Total** | **6** | **11** | **36** | **20** | **73** |

Note the asymmetry: **every** correctly-obtained robust SE in the archive is in Study 1. Study 2 has
none.

---

## 2. Which paper outputs fall in each category

### A — robust SEs actually computed and reported (6 rows / 6 calls)

All in `study1.R`, all `jtools`:

- L47 `summ(model1, robust=TRUE)` — console print of the Figure 3 model
- L104 `summ(modelsub1, robust=TRUE)` — console print, Figure 4 left model
- L108 `summ(modelsub2, robust=TRUE)` — console print, Figure 4 right model
- L117 `effect_plot(model = modelsub1, pred = treat, robust=TRUE, ..., int.width = .90)` — **Figure 4, left panel**
- L140 `effect_plot(model = modelsub2, pred = treat, robust=TRUE, ..., int.width = .90)` — **Figure 4, right panel**
- L196 `summ(model2, robust=TRUE)` — console print of the Figure A4 model

**Only one published output (Figure 4) actually displays robust standard errors.** The other five are
console prints.

### B — robust requested, silently ignored (11 rows / 3 calls)

| Paper output | File:line | Call |
|---|---|---|
| **Table A8** (Study 1) | `study1.R:189` | `modelsummary(models2, star=c('*'=.1, "**"=.05, "***"=.01), output="latex", robust=TRUE)` |
| **Table A11** (Study 2) | `study2.R:248` | `modelsummary(mech, star=c('*'=.1, "**"=.05, "***"=.01), output="latex", robust=TRUE)` |
| Figure 6 model, console print (Study 2) | `study2.R:42` | `summary(modelES, robust=TRUE)` |

`study2.R:42` is base `stats::summary.glm`, not `jtools::summ` — it too silently swallows `robust`
(verified below). It is a console print, not a published table, but it is the same failure mode.

### C — no robust SEs requested anywhere (36 rows / 27 calls)

Published outputs:

- **Table A7** (Study 1) — `study1.R:176-180`
- **Table A9** (Study 2) — `study2.R:224-228`
- **Table A10** (Study 2) — `study2.R:234-238`
- **Figure 3, bottom panel** (conditional ATE) — `study1.R:76-81, 87`
- **Figure A4, bottom panel** — `study1.R:234-239, 245`
- **Figure A6** — `study1.R:260-263`
- **Figure A7** — `study1.R:269-272`
- **Figure 6, bottom panel** — `study2.R:67-72, 78`
- **Figure 7, both panels** — `study2.R:110-112` and `study2.R:132-134` (plus the `summary()` prints at L98, L102)
- **Figure 8, bottom panel** — `study2.R:197-202, 208` (plus `summary(pride1)` at L166)
- **Figure A5, bottom panel** — `study2.R:286-291, 297`
- **Figures A10, A11, A12, A13** — 12 `stability_plot()` calls in `multiverse.R`

### D — not applicable / no SEs reported (20 rows / 20 calls)

- Figure 3 top panel, Figure 6 top panel, Figure 8 top panel, Figure A5 top panel — `interact_plot(..., interval = FALSE)`; no bands drawn
- Figure A4 top panel — predicted-value jitter, and `stat_smooth(method = "lm_robust", fullrange = TRUE, se=FALSE)`: `estimatr::lm_robust` *is* the smoother, but `se=FALSE`, so no SEs are shown
- Figure A9 — three `ritest()` randomisation-inference calls; permutation distributions, no SEs
- Figures A1, A2, A3 — descriptive ESS / Eurobarometer plots
- Figure A8 — Monte Carlo power simulation
- Tables A1, A2, A4, A5 — `datasummary()` descriptives
- Tables A3, A6 — `datasummary_balance()`. Classified D because there is no user model or SE call; for
  completeness, `modelsummary:::DinM` calls `estimatr::difference_in_means`, whose variance estimator
  is heteroskedasticity-robust by construction — so these two tables do carry robust SEs, by package
  default, not by request.

---

## 3. Behavioural test of `modelsummary(model, robust = TRUE)`

Category B was **verified by execution**, not assumed.

**Environment:** R 4.5.2 (2025-10-31 ucrt), Windows. **modelsummary 2.5.0**; jtools 2.3.0;
estimatr 1.0.6; sandwich 3.1.1.

Toy data with deliberate heteroskedasticity: `n = 200`, `x ~ N(0,1)`, `y = 1 + 2x + e`,
`e ~ N(0, exp(x)^2)`; `m <- lm(y ~ x)`. Coefficient / SE for `x`, printed to 12 digits:

```
modelsummary(m)                   2.329449402248 (0.171114612381)
modelsummary(m, robust = TRUE)    2.329449402248 (0.171114612381)
modelsummary(m, vcov = "robust")  2.329449402248 (0.302745175462)
modelsummary(m, vcov = "HC3")     2.329449402248 (0.302745175462)
modelsummary(m, vcov = "HC1")     2.329449402248 (0.296131751714)

robust=TRUE identical to plain?         TRUE
robust=TRUE identical to vcov="robust"? FALSE

Reference SEs:  classical 0.1711146 | HC3 0.3027452 | HC1 0.2961318 | HC2 0.2986629
```

**Result: `modelsummary(m, robust = TRUE)` matches plain `modelsummary(m)` exactly — the classical
OLS SE — and differs from `vcov = "robust"` by a factor of ~1.77.**

Mechanism: `modelsummary()`'s formals are
`models, output, fmt, estimate, statistic, vcov, conf_level, exponentiate, stars, shape, coef_map,
coef_omit, coef_rename, gof_map, gof_omit, gof_function, group_map, add_columns, add_rows, align,
notes, title, escape, ...` — there is **no `robust` argument**. `robust = TRUE` is absorbed by `...`
and discarded. No error and no warning is raised. The package NEWS shows `vcov` shortcuts
(`"robust"`, `"HC1"`, …) were introduced in modelsummary 0.6.5 and there is no record of a `robust`
argument ever existing, so this behaviour is not an artefact of testing a newer version than the
authors used.

Contrast, same session, same model:

- `jtools::summ(m, robust = TRUE)` prints `Standard errors: Robust, type = HC3`, SE(x) = 0.30 — the
  request **is** honoured. Plain `summ(m)` prints `Standard errors: OLS`, SE(x) = 0.17.
- `jtools::effect_plot` has `robust` in its formals (`TRUE`) — the request is honoured.
- `interactions::interact_plot` also has a `robust` formal — but it is never used in the archive.
- Base `summary(glm_object, robust = TRUE)`: `summary(g)$coefficients` and
  `summary(g, robust = TRUE)$coefficients` are `identical()`. Silently ignored.
- `margins::margins.lm` / `.glm` default to `vcov = stats::vcov(model)` — classical. Every AME panel
  in the paper (the lower half of Figures 3, 6, 8, A4, A5) therefore uses classical SEs.
- `starbility::stability_plot` (Figures A10–A13) estimates via `lfe::felm(spec, data, weights)` with
  `cluster` defaulting to `'0'` (no clustering) and reports `broom::tidy()$std.error` — classical
  SEs. starbility is not on CRAN and was not installed locally; this rests on inspection of
  `github.com/AakaashRao/starbility` (`R/builtin_models.R`, `R/create_stability_plot.R`), not on
  execution.

So the authors' description of the mechanism is **correct as far as it goes**: `modelsummary(model,
robust = TRUE)` does run without error and does silently report classical SEs.

---

## 4. Verdict

**The "conflated the two packages" account explains the category-B subset only — 11 of 73 estimation
calls (15%), or 3 of 56 reporting calls: Table A8, Table A11, and one console `summary()` print. It
does not explain the 36 category-C calls, and in particular it does not explain Tables A9 and A10.**

The authors' footnote 3 says the error was "`modelsummary(model, robust = TRUE)`" — a robust request
that the wrong package silently dropped. That story requires a `robust = TRUE` to be present. In
Tables A9 and A10 there is none. A plain-text search of the entire archive for
`robust|vcov|sandwich|coeftest|lm_robust` returns exactly ten hits across all six R files, and none
of them is in the Table A9 or Table A10 blocks.

**Tables A9 and A10 are category C, not category B.** These are the Study 2 (Spain) results tables —
the tabular presentation of the paper's headline finding. Verbatim, `study2.R` lines 222–238:

```r
###TABLE A9###
models1 <- list()
models1 [['Model 1']] <-lm (support ~ treat + imm_1, weight=nationalweight, data=spain)
models1 [['Model 2']] <-lm (support ~ treat*imm_1, weight=nationalweight, data=spain)
models1 [['Model 3']] <-lm (support ~ treat, weight=nationalweight, data=proimmES)
models1 [['Model 4']] <-lm (support ~ treat, weight=nationalweight, data=noproimmES)
modelsummary(models1, star=c('*'=.1, "**"=.05, "***"=.01), output="latex")



###TABLE A10###
models0 <- list()
models0 [['Model 1']] <-lm (lgbtED ~ treat + imm_1, weight=nationalweight, data=spain)
models0 [['Model 2']] <-lm (lgbtED ~ treat*imm_1, weight=nationalweight, data=spain)
models0 [['Model 3']] <-lm (lgbtED ~ treat, weight=nationalweight, data=proimmES)
models0 [['Model 4']] <-lm (lgbtED ~ treat, weight=nationalweight, data=noproimmES)
modelsummary(models0, star=c('*'=.1, "**"=.05, "***"=.01), output="latex")
```

Neither the four `lm()` calls nor the `modelsummary()` call contains a `robust` argument, a `vcov`
argument, or any other request for heteroskedasticity-consistent SEs. There was nothing to conflate.
The same is true of Table A7 in `study1.R:176-180`.

That this is a genuine inconsistency rather than a global convention is settled inside the very same
file. Ten lines below Table A10, `study2.R:248` reads:

```r
modelsummary(mech, star=c('*'=.1, "**"=.05, "***"=.01), output="latex", robust=TRUE)
```

So `robust = TRUE` was supplied for the ancillary mechanism table (A11) and omitted for the two main
results tables (A9, A10). The same split appears in `study1.R`: absent for Table A7 (L180), present
for Table A8 (L189). And across studies: `effect_plot(..., robust = TRUE)` at `study1.R:117` and
`:140` (Figure 4), but `effect_plot(...)` with no `robust` at `study2.R:110` and `:132` (Figure 7) —
the directly parallel analysis.

Summarising the archive's actual behaviour:

- 6 calls report genuinely robust SEs — all in Study 1, and only one of them (Figure 4) reaches a
  published output.
- 11 calls request robust SEs and get classical ones — the failure mode the authors describe.
- 36 calls never request robust SEs at all, including Tables A7, A9 and A10 and every
  `margins()`-based conditional-ATE panel in both studies.
- 20 calls report no SEs at all.

**Conclusion: the archive is not one uniform mistake but at least two distinct patterns. The
authors' account covers the smaller one. Under any reading, the published paper's Study 2 results
tables report classical standard errors because robust standard errors were never asked for.**

---

## 5. What could not be audited, and residual uncertainty

- `descriptiveplots.R` could not be executed: its input `ESS-Data-Wizard-subset-2023-02-08.dta` is
  excluded from the GitHub mirror for size (stated in the repo README) though it is listed in the
  Dataverse ReadMe. This affects Figures A1 and A2 only, both of which are descriptive
  (category D) on the face of the code, so nothing in the SE audit turns on it.
- `study1_summarystats.R:10` reads `read_csv("UKdata_analysis.csv")`, a filename not present in the
  Dataverse archive (which ships `study1_data.csv`). `study2_summarystats.R:38-48` selects a column
  set excluding `nationalweight` and then references and drops it. Both scripts therefore fail as
  distributed. These are reproducibility defects, not SE defects; both scripts produce descriptive
  tables (category D) regardless.
- `starbility` is not on CRAN and was not installed; the category-C classification of the 12
  `stability_plot()` calls (Figures A10–A13) rests on reading the package source, not on running it.
  This is the one classification in the table supported by source inspection rather than execution.
  It does not bear on the A9/A10 finding.
- **No instance was left unclassifiable.** Every category-D row is D for a stated, checkable reason
  (no SEs displayed, or purely descriptive output) rather than because it was ambiguous.

### Minor issues noticed in passing (not SE issues, recorded for completeness)

- `study1.R:265` labels the Figure A6 plot "Continuous outcome linear regression model", but the
  model plotted (`model1`, L46) is the binomial glm; `study1.R:274` labels Figure A7 "Binary outcome
  logstic regression model" but plots `model2` (L195), the OLS model. The two subtitles appear
  swapped.
- `study1.R:301` builds `Rplot3` from `ritest2$betas` where `ritest3$betas` is evidently intended, so
  the third panel of Figure A9 plots the second panel's permutation distribution.
- `Dataverse_files/ReadMe.pdf` lists Table A11 as produced by `study1.R`; no Table A11 code exists in
  `study1.R` (it is in `study2.R:240-248`).
- The ATE values annotated on Figures 4 and 7 (`"ATE=-.06*"`, `"ATE=.10**"`, `"ATE=.11***"`,
  `"ATE=.10***"`) are hard-coded `geom_bracket` text labels, not values computed at run time.
