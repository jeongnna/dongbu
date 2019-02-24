#!/bin/bash
Rscript scripts/01_portfolio_analysis.R analysis_year=2014 mc_cores=4
mv RUN_THIS_SCRIPT_2014.sh done_2014.sh
chmod -x done_2014.sh
