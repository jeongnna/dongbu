#!/bin/bash
Rscript scripts/01_portfolio_analysis.R analysis_year=2017 mc_cores=4
mv RUN_THIS_SCRIPT_2017.sh done_2017.sh
chmod -x done_2017.sh
