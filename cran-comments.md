## R CMD check results

0 errors | 0 warnings | 1 note

- This is a new release.
- Resubmission after request from Uwe Ligges:
  - Reduced runtime of tests in ./tests. Tests pass now in just under a minute on winbuilder (release + devel)
  - Fixed accidentally verbose output during tests
- The "possibly misspelled" words in DESCRIPTION are
  - A citation (Ewald et al.)
  - A method's name as it is commonly stylized ("Shapley Additive Global importancE (SAGE)")
- The 403 URL to https://pmc.ncbi.nlm.nih.gov is accessible, but likely not via automated means.
