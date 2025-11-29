## Details

Reproduction materials for "A Comment on ‘Instrumentally Inclusive: The Political Psychology of Homonationalism’ (Turnbull-Dugarte and López Ortega (2024))"

Author: Daniel de Kadt
Email: d.n.de-kadt@lse.ac.uk
Date: 3 October 2025
R Version: 4.4.0
Platform: x86_64-w64-mingw32/x64
OS: Windows 10 x64 (build 19045)

## Folder Contents

Beside this readme file, this folder contains five files:

1. paper_tables_and_figures.qmd : a quarto notebook to produce the figures and tables for the published note
2. supplementary_materials.qmd : a quarto notebook to produce the supplementary materials and all analyses therein
3. references.bib : reference .bib file
4. study1_data.csv : original data for the UK study
5. study2_data.Rda : original data for the Spain study

## Reproduction Instructions

To reproduce the results in the published note, open file 1 (paper_tables_and_figures.qmd) in RStudio or equivalent and render in Quarto. 

This file will do a few things: 

- install version-controlled packages using groundhog
- execute all code to produce figures and tables
- save those figures and tables in an automatically created 'output' folder as .tex files (tables) and .jpgs (figures)
- produce an .html document that includes all of these main results 

The supplementary materials are included as a fully dynamic document. To reproduce these results open file 2 (supplementary_materials) in RStudio or equivalent and render in Quarto. This will execute all code that underpins the supplementary material, and produce a file called supplementary_materials.pdf that is the supplementary material. 

## Notes

Computational run-time should not be long. Be aware that you should run the setup chunk first to deal with package versioning. Then you may simply render the .qmd. 

Seed is set in the setup chunk of both .qmd files, though this should not be relevant. 

For additional information about the data, please contact the authors of the original study.