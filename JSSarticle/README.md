# JSS article

Renders `article.qmd` to `article.pdf` with the JSS Quarto template in `_extensions/`.

## One-time setup

From the repository root, restore the project library:

```
Rscript -e "renv::restore()"
```

You also need Quarto and a LaTeX installation (LuaLaTeX).

## Render

From the repository root:

```
quarto render JSSarticle/article.qmd
```

Or from this folder:

```
quarto render article.qmd
```

The article loads graceDR from the source in the repository root, so it always uses the current
code. It reads the simulation results in `results/`. To rebuild them, run
`Rscript run_simulations.R` from this folder (1,000 trials in each of four settings).

## Replication script

`article.R` in the repository root reproduces the code and output of the article, without the text.
It installs graceDR from GitHub and the other packages at the versions in `renv.lock`, then runs
the code of the article. Run it from the repository root:

```
Rscript article.R
```

It reads the simulation results in `JSSarticle/results/`, and runs `JSSarticle/run_simulations.R`
only when those results are missing. The figures go to `article-figures.pdf`.
