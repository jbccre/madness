This directory contains the code and scripts necessary to operate the CCRE March Madness dashboard.

Dashboard link: https://ccre.shinyapps.io/madness

How it works:

- (n) lovers of March Madness submit brackets for either the March Madness men's or women's tournament using a Qualtrics link. The submissions include the four first four games and the 63 "official" March Madness matches. Responses are transformed into a (n) x 68 csv file using upload_qualtrics.R at the start of each tournament. (67 columns for the selected winners of the 67 matches, and a 68th field containing the player's name.) 

- Every ten minutes during gameplay, a cron-job (from cron-job.org) triggers a GitHub Action (.gitHub/workflows/run-men.yml or .github/workflows/run-women.yml). The action triggers the core script madness2026.R, which (1) sets baseline probabilities for each of the (n) players winning at the start of each tournament; (2) updates probabilities based on any victories that have occurred since last iteration. The probabilities are based on simulating the full March Madness tournament 10,000 times, factoring in completed games. Probabilities of each simulated match is captured using Warren Nolan's ELO ratings (pulled at start of tournament by elo2026.R). This is somewhat naive and does not factor in injuries, travel, or progress during the match. Current data is stored in the men/ and women/ subfolders. The CCRE scoring system is used:

- CCRE scoring system: We use a point system that rewards wins -- especially upsets. You get points for each win, with games in later rounds getting more points than games in earlier rounds. You also get points based on the "seed" (1-16) of the team you correctly select to win. So, you will get more points if you correctly call an "upset" win. Points are as follows.

Example: You correctly pick a 10-seed team to win a Final Four game. You will get 210 points for that game (140 + 7*10)

      - First Four:	20
      - First Round: 20 + seed
      - Second Round:	40 + 2 * seed
      - Sweet Sixteen: 75 + 3 * seed
      - Elite Eight: 100 + 5 * seed
      - Final Four:	140 + 7 * seed
      - Championship Game: 200 + 10 * seed

    Bracket with the most points wins 70% of the pot. Second place gets 20%. Third place gets 10%.

 - Source data (in the men/ and women/ folders)
      - all_standings.csv: All historical standings (used for "madness over time")
      - current_standings.csv: Current standings
      - elo.csv: ELO scores (team strength) for tournament.
      - games_completed.txt: Number of games completed (used to ensure data added only after new matches are completed)
      - gamestate.csv: Current state of the world (bracket structure and which teams won)
      - simulation.csv: 10,000 iterations of March Madness (used for simulating scenarios)
      - submitted_brackets.csv: Submitted brackets, transformed at start of tournament.

- Code for the dashboard itself is in the dashboard/ folder. The dashboard is built in Shiny using R. It contains the main code (app.R) and five R scripts containing code for modules. The dashboard code itself contains no data. When someone logs into the dashboard, it simply pulls the latest state of the world by querying this repository for the latest data. If the dashboard code is edited, a GitHub Action script .github/workflows/deploy_shinyapp_on_push.yml automatically pushes the revised files to shinyapps.io. Ticks on the "madness over time" x-axis are governed by dashboard_ticks.csv, in the main folder. Modules in the dashboard are as follows:

  - Current Standings (as it says).
  
  - Madness Over Time (longitudinal plotting of individual bracket's probabilities of winning first, second, or third, total points, and market value [expected value of winning assuming a $10 price per bracket submission).
  
  - Current Bracket (draws the current state of the world - helps to see which victories are being accounted for).
  
  - Explore Submitted Brackets (draws a bracket for each player).
  
  - Simulate Scenario (this applies filters to the simulated data to get probabilities of victory if certain teams win or lose certain matches).

