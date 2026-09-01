# 7/31/2026 - AO
- [bug fix] Corrected Lesion Site not found error, function is not using Event Name column as surrogate Lesion Site Label

# 09/01/2026 - AO
- [update] major app code clean up of unused or commented out code in app.R file
- [update] Data is now read in and formatted to a transferable object already broken down to the patient level to easily select and view patient information
- [update] event parameter file is no longer used/needed
- [update] event summary rows are no longer being derived
  - Maybe come back to this for re-implementation, added as to-do
- [update] update .gitignore with test files

- [feature] Full patient data table added as new tab. can be used to filter patients and select to view in timeline

- [bug fix] legend now shows for lesion level line plot
- [bug fix] input data will be reordered to ensure required columns are in the beginning

- [to do] Maybe come back to this for re-implementation
- [to do] Highlight event not working
- [to do] Turned off hover choice
- [to do] x-axis breaks and unit changing
- [to do] reformat patient selection table
- [to do] add refresh button for API retrieval

- [bug] timeline double loading something different at first
