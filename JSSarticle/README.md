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

The folder `replication/` is a self-contained RStudio project that reproduces the code and output
of the article, without the text. See `replication/README.md`.
