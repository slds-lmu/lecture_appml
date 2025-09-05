# Steps AppML
1. Downloaded [`update-service.sh`](https://github.com/slds-lmu/lecture_service/blob/main/service/scripts/update-service.sh), placed in `scripts/` and run `bash update-service.sh` (using WSL)
2. Converted [logo](style/logos/applied.png) to pdf and names `logo.pdf`


## Folder sources
SL - 1) feature-selection
AdvML - 1) multitarget, 2) fairness, 3) imbalanced-learning

## Chunks

[x] 01_intro (1 -> 4 chunks)
[x] 03_lifecycle (1 -> 2 chunks)
[x] 04_tuning (2 -> 4) (I have not included the i2ml .tex-s) 
[] 05_feature-preproc
[] 06 adv-feature-selection
- [] slides04-adv-feature-preproc.tex 
[x] 07 imputation (1 -> 2 chunks, Rmds translated)
[x] 08_perf_calibr (1 -> 2 chunks)
[x] 09_imbalancy_corr (1 -> 3 chunks)
[x] 10_ensembles-stacking (forward pass / prediction is an empty slide)
[x] 11_parallelization (2 -> 3 chunks)
[x] 12_perf_bench (didn't split perf-eval-04 - it had 3 scenarios and a conclusion, having them in separate files seemed unnatural (+ the files would be too small)) + (I was unable to find the lift plot (`figure/lift_chart.png`) referenced at `slides04-perf-eval-05-practical`)
[x] 12_time_series
[] fairness
[x] feature_selection
[] multittarget


## Per Chunk

### 12 time series
1. title figure is missing
2. You may want to change the subtitle
3. Some slides have too little text (e.g. simple baseline)
4. Sometimes maybe section-name slides are redundant (e.g. "wrap-up" slide which is followed by only "key-takeaways" slide)
5. Overall it's 18 slides (including section-name slides) but quite compact, I don't think it makes sense to split it into 2 parts 


## ToDo

[] Take into account for \href{https://arxiv.org/abs/2404.19494}{\beamergotobutton{REF}}
[] PDF insert - is not left alligned
[] Fix absolute path
[] Fill in PLACEHOLDERs
[] Some pdfs don't fully fit, but are okay
[] texts go over the footer line

- Moodle [Course](https://moodle.lmu.de/course/view.php?id=39034)
- mlr3slides dir can be treated as an "image dir"
- 05_feature-preproc has pdfs where you need to create a corresponidng tex file (based on an Rmd file, you find it here https://github.com/Essential-Data-Science/eds/tree/master/material/rmd_chapters) -> you need to store the R code separately, create the figures and include them in tex
- Do not need a "master file" that combines all the subchapters
- try to split long chapters into multiple meaningful subchapters (in a separate PR)
- feature selection is here: https://github.com/slds-lmu/lecture_sl/tree/main/slides/feature-selection
- maybe from this other repo also copy and paste these two chapters as well:
  - https://github.com/slds-lmu/lecture_advml/tree/main/slides/multitarget
  - https://github.com/slds-lmu/lecture_advml/tree/main/slides/fairness
  - https://github.com/slds-lmu/lecture_advml/tree/main/slides/imbalanced-learning
- if in the appml repo we haven't used all of t
everything that is included from a pdf_chapters dir, should exist as .Rmd file here: https://github.com/Essential-Data-Science/eds/tree/master/material/rmd_chapters
- \setbeamersize{text margin left=0.3cm,text margin right=0.3cm}


Hayk Tarkhanyan, [01-Aug-25 3:17 PM]
Մեծ ֆայլը բաժանել ըստ սեքշնների, են պդֆ իմպոռտները

Hayk Tarkhanyan, [01-Aug-25 3:27 PM]
All non ml3 nkn integers go to attic

Hayk Tarkhanyan, [01-Aug-25 3:28 PM]
Not slides prefixes should go to attic

Hayk Tarkhanyan, [01-Aug-25 3:30 PM]
Timeseries folder is okay

Hayk Tarkhanyan, [01-Aug-25 3:30 PM]
Ensembel 2 slides also okay

Hayk Tarkhanyan, [01-Aug-25 3:30 PM]
For new old look into moodle

Hayk Tarkhanyan, [01-Aug-25 3:30 PM]
If too large => split

Hayk Tarkhanyan, [01-Aug-25 3:31 PM]
15 is the threshold

Hayk Tarkhanyan, [01-Aug-25 3:31 PM]
But also sematically

Hayk Tarkhanyan, [01-Aug-25 3:32 PM]
Convert all absolute files to relatice

