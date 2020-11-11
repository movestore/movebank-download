# Movebank
MoveApps
 
Github repository: *github.com/movestore/movebank-download*

## Description
Download Movement tracks which are stored in a Movebank study. It is possible to select Animals, a time range and duplicate filtering methods. 

## Documentation
This App allows the direct download of Movement data that are stored in the Movebank Data Base. Those data can be the start of workflows that then filter, visualise and/or analyse them. In an interactive interface the data have to be accessed and selected.

The first step is the Movebank Login. There Movebank accounts can be added with login and password. They are remembered within your MoveApps account. To start the data selection, select one of your valid Movebank logins.

In the next frame the Studies to which you have access under the selected Movebank login are listed. They can be filtered to only those studies to which you have download access (minimum requirement), to which you are collaborator or which you own as data manager. For each study details like the number of animals, the number of locations (event) and the timestamps of the first and the most recent location are provided to support your selection. If the selected study is open by some license agreement, you have to agree to the license agreement that is shown in an intermediary window.

Once a study is selected, in the next frame the Animals for which data shall be downloaded have to be chosen. Be default the top bar `All XX Animals` is selected. When selecting (or unselecting) single Animals below, this selection disappears. Any number of Animals can be selected, note that for large data sets the download might take a long time. For help with the selection, the given nickname, species, ring ID, number of locations (events), number of deployments and timestamps of the first and last location are provided for each animal.

In the next frame (Options) start and end timestamps can (but need not) be selected to download only part of the data set(s). Start and end dates (timestamps) can be selected or left open independently. Another option is how possible duplicate timestamps for same locations/animals shall be handled, as the moveStack output of the App does not allow for duplicated timestamps. The two options presently available are (1) to retain the first occurance of each duplicate timestamp and (2) to merge all data from a timestamp and keep the first non-emtpy entry for each column, thus retaining a maximum of information.

In the final frame and Overview of your data selection are provided with the Movebank login username, the Study ID and name, the number of selected Animals and (if clicking Details) their names, the selected start/end timestamps and the dupliate handling method.

!> Note that Movebank Apps can be repeatedly added to Workflows and data are appended to each other. This way, it is possible to jointly analyse data from different user accounts and/or studies.

### Input data
none or 
moveStack in Movebank format

### Output data
moveStack in Movebank format

### Artefacts
none

### Parameters 
none

### Null or error handling:
**Data:** If one or more Animals without data (in the selected time range) are selected then they are omitted from the return data set with a warning only. However, if all selected Animals have no data to download then NULL is returned, leading to an error.


