#!/bin/bash
Rscript scripts/01_portfolio_analysis.R analysis_year=2016 mc_cores=4
mv RUN_THIS_SCRIPT_2016.sh done_2016.sh
chmod -x done_2016.sh
