*Landi Luo*

### Overall Grade: 96/100

### Quality of report: 10/10

-   Is the homework submitted (git tag time) before deadline?

    Yes. `Feb 2, 2018, 5:57 PM PST`.

-   Is the final report in a human readable format html?

    Yes. `html`.

-   Is the report prepared as a dynamic document (R markdown) for better reproducibility?

    Yes. `Rmd`.

-   Is the report clear (whole sentences, typos, grammar)? Do readers have a clear idea what's going on and how are results produced by just reading the report?

    Yes.

### Correctness and efficiency of solution: 50/50

-   Q1 (10/10)

-   Q2 (20/20)

    \#2. The following implementation (from Dr. Zhou's solution sketch) is fast as it traverses `bim` file only once. The `uniq` command in Linux is useful for counting but takes longer.

    ``` bash
    time awk '
    {chrno[$1]++;} 
    END{ for (c in chrno) print "chr.", c, "has", chrno[c], "SNPs"}'                                   
    /home/m280-data/hw1/merge-geno.bim
    ```

-   Q3 (20/20)

    \#1. `runSim.R`: Use `rcauchy` for the Cauchy distribution.

### Usage of Git: 10/10

-   Are branches (`master` and `develop`) correctly set up? Is the hw submission put into the `master` branch?

    Yes.

-   Are there enough commits? Are commit messages clear?

    Yes.

-   Is the hw1 submission tagged?

    Yes.

-   Are the folders (`hw1`, `hw2`, ...) created correctly?

    Yes.

-   Do not put a lot auxillary files into version control.

    Yes.

### Reproducibility: 8/10

-   Are the materials (files and instructions) submitted to the `master` branch sufficient for reproducing all the results? (-2 pts)

    -   In your `autoTable.R`, the path `"/home/l4luo/biostat-m280-2018-winter/hw1"` is for your own server. Make sure your collaborators can easily run your code. You may use something like

        ``` r
        filenames <- list.files(".", 
                            pattern = "*00.txt")
        ```

        for easier reproducibility.

    -   If necessary, are there clear instructions, either in report or in a separate file, how to reproduce the results?

        Yes.

### Julia code style: 18/20

-   [Rule 3.](https://google.github.io/styleguide/Rguide.xml#linelength) Never place more than 80 characters on a line.

-   [Rule 4.](https://google.github.io/styleguide/Rguide.xml#indentation) 2 spaces for indenting.

-   [Rule 5.](https://google.github.io/styleguide/Rguide.xml#spacing) Place spaces around all binary operators (`=`, `+`, `-`, `<-`, etc.). Exception: Spaces around `=`'s are optional when passing parameters in a function call. (-1 pt)

    Some violations:
    -   `runSim.R`: line 35

-   [Rule 5.](https://google.github.io/styleguide/Rguide.xml#spacing) Do not place a space before a comma, but always place one after a comma. (-1 pt)

    Some violations:
    -   `autoTable.R`: line 14

-   [Rule 5.](https://google.github.io/styleguide/Rguide.xml#spacing) Place a space before left parenthesis, except in a function call. Do not place spaces around code in parentheses or square brackets. Exception: Always place a space after a comma.
