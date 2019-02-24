#!/bin/bash
Rscript scripts/01_portfolio_analysis.R analysis_year=2018 mc_cores=4
mv RUN_THIS_SCRIPT_2018.sh done_2018.sh
chmod -x done_2018.sh
