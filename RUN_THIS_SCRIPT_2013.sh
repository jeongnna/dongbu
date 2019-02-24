#!/bin/bash
Rscript scripts/01_portfolio_analysis.R analysis_year=2013 mc_cores=4
mv RUN_THIS_SCRIPT_2013.sh done_2013.sh
chmod -x done_2013.sh
