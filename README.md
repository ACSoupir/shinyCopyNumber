# shinyCopyNumber!
**shinyCopyNumber** ([here](https://acsoupir.shinyapps.io/copynumbershiny/)) is a way to perform copy number calculations for sample wig files that were aligned to a genome and then counted using readCounter from [HMMcopy](https://github.com/shahcompbio/HMMcopy). Currently, there are 3 genome versions (GRCh37, hg19, and b37) that work if you aligned to those, and for 4 window sizes (1MB, 100Kb, 10Kb, 1Kb). All that is needed is to input the `wig` file from readCounter.

4 libraries that are needed:
* shiny
* DT
* stringr
* HMMcopy

Genomes Included:
* GRCh37
* hg19
* b37

Windows available:
* 1,000,000 bp
* 100,000 bp
* 10,000 bp
* 1,000 bp

The difficult part of creating the genome GC content and mappability files is already inside of the Shiny App and hopefully will add more in the future.

Additionally, I have changed a few of the functions that are in the HMMcopy package for R to allow for playing with the threshold

___

### Other application use
Currently on pages 2 and 3, there is the ability to merge patient information (or really any table information provided that they share at least 1 column in common). Only 2 tables are an option right now, and merging 2, downloading, reuploaded to remerge may cause issues due to the way I have the application handling duplicate column headings with different information: usually `merge` will give `.x` and `.y` to the overlapping column but I rename the column the file name to keep track of where the column came from. Provided is open source ICGC patient data that can be used to test this.

___

### In the future
* In the future I plan to add more genomes and possibly window sizes. I did edit the perl code for the mappability files to use `bowtie2` instead of `bowtie` and use multithreading but haven't had the opportunity to test it out. With my computer, it took an afternoon to do the three genomes above due to single-threaded nature of the perl scripts.
* Add graphical representations of the copy number fixes done by HMMcopy for samples.
* Add selection of experimental design file(s) to allow comparisons between different groups' copy number changes (patient normal / tumor).
* **Add demo data**
