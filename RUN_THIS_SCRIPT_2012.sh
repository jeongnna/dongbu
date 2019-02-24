#!/bin/bash
Rscript scripts/01_portfolio_analysis.R analysis_year=2012 mc_cores=4
mv RUN_THIS_SCRIPT_2012.sh done_2012.sh
chmod -x done_2012.sh
