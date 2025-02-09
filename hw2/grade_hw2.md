*Sophia Luo*

### Overall Grade: 162/180

### Quality of report: 10/10

-   Is the homework submitted (git tag time) before deadline? Take 10 pts off per day for late submission.  

-   Is the final report in a human readable format (html, pdf)? 

-   Is the report prepared as a dynamic document (Quarto) for better reproducibility?

-   Is the report clear (whole sentences, typos, grammar)? Do readers have a clear idea what's going on and how results are produced by just reading the report? Take some points off if the solutions are too succinct to grasp, or there are too many typos/grammar. 

All good.

### Completeness, correctness and efficiency of solution: 124/130

- Q1 (17/20)

    - Q1.1 Nice summary. `+2.0`
    
    - Q1.2 The memory usage should be mush smaller with about 46 MB by keeping IDs double (or changing them to integer) and converting categorical variables to factor types. `-5.0`

- Q2 (75/80)

    - Q2.1 (10/10) Explain why read_csv cannot ingest labevents.csv.gz
    
    - Q2.2 (10/10) Explain why read_csv cannot ingest labevents.csv.gz
    
    - Q2.3 (15/15) The Bash code should be able to generate a file `labevents_filtered.csv.gz` (166MB). Check the numbers of rows and columns are correct.
    
    - Q2.4 (15/15)
    
    - Q2.5 (10/15)
    
      The rows do not match those of Q2.4 (and Q2.3, which is now okay). You should’ve double-checked your html and pdf files. Without arrange, this mismatch could happen. See related discuttion on Slack. `-5.0`
    
    - Q2.6 (15/15)
    
      The rows do not match those of Q2.4 (and Q2.3, which is now okay). You should’ve double-checked your html and pdf files. Without arrange, this mismatch could happen. See related discuttion on Slack. `-0.0`

- Q3 (32/30) Steps should be documented and reproducible. Check final number of rows and columns.
	    
    Good thoughts. `+2.0`

	    
### Usage of Git: 10/10

-   Are branches (`main` and `develop`) correctly set up? Is the hw submission put into the `main` branch?

-   Are there enough commits (>=5) in develop branch? Are commit messages clear? The commits should span out not clustered the day before deadline. 
          
-   Is the hw2 submission tagged? 

-   Are the folders (`hw1`, `hw2`, ...) created correctly? 
  
-   Do not put auxiliary and big data files into version control. 

All good.

### Reproducibility: 10/10

-   Are the materials (files and instructions) submitted to the `main` branch sufficient for reproducing all the results? Just click the `Render` button will produce the final `html`? 

-   If necessary, are there clear instructions, either in report or in a separate file, how to reproduce the results?

All good.

### R code style: 8/20

For bash commands, only enforce the 80-character rule. Take 2 pts off for each violation. 

-   [Rule 2.6](https://style.tidyverse.org/syntax.html#long-function-calls) The maximum line length is 80 characters. Long URLs and strings are exceptions.  

    Line 255, 319, 365. `-6.0`
    
-   [Rule 2.5.1](https://style.tidyverse.org/syntax.html#indenting) When indenting your code, use two spaces.  

-   [Rule 2.2.4](https://style.tidyverse.org/syntax.html#infix-operators) Place spaces around all infix operators (=, +, -, &lt;-, etc.).  

    Line 179. `-2.0`

-   [Rule 2.2.1.](https://style.tidyverse.org/syntax.html#commas) Do not place a space before a comma, but always place one after a comma.  

    Line 179 (different from above), 440. `-4.0`

-   [Rule 2.2.2](https://style.tidyverse.org/syntax.html#parentheses) Do not place spaces around code in parentheses or square brackets. Place a space before left parenthesis, except in a function call.

Again, double-check R code style right before the submission to avoid these avoidable mistakes.
