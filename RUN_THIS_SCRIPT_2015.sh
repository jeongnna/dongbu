#!/bin/bash
Rscript scripts/01_portfolio_analysis.R analysis_year=2015 mc_cores=4
mv RUN_THIS_SCRIPT_2015.sh done_2015.sh
chmod -x done_2015.sh
