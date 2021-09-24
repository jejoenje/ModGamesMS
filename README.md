# ModGamesMS

This is the main repository for the manuscript "**Models as games: a novel approach for 'gamesourcing' parameter data and communicating complex models**"

The main manuscript file is main_ms.Rmd, which "knits" a docx by default.

The manuscript file uses _analysis.R_ as its main analysis script which mainly produces the numbers reported in the main text.

The files _Figure4.R_, _Figure5.R_ and _Figure6.R_ produce the corresponding figures in the text, the others are hard copied from other sources.

The folder _data_ has output files from the Animal&Farm game, which are straight exports from the MySQL tables. These are used to build the numbers and figures in the MS above. They need updating manually if new data is to be incorporated.

Folder _pres_ has the data and output for a short introductory presentation of A&F.

The file _comparing_gmse_GA.R_ is a short attempt at comparing the A&F game data with runs from GMSE using the same parameters, to compare outcomes. While this script should work and produce output, it only outputs the simulated (GMSE) data so far, and does not compare these directly to the A&F game data. GMSE simulation data are in folder _sim_dat_.



