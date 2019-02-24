#!/bin/bash
Rscript scripts/01_portfolio_analysis.R analysis_year=2011 mc_cores=4
mv RUN_THIS_SCRIPT_2011.sh done_2011.sh
chmod -x done_2011.sh
