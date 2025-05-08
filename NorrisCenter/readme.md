# NorrisCenter

This folder is for data pertaining to the Norris Center Herbarium [more here](https://norriscenter.ucsc.edu/collections/).

The code here was created specifically for the Herbarium Student Curators to explore while they work on their poster presentations, but may be useful to anyone interested in working with large datasets.

Occurrence data published by: UCSC Herbarium, Kenneth S. Norris Center for Natural History (Accessed through
CCH2 Portal Data Portal, https://cch2.org/portal/index.php, 2025-04-08)

## Files

- Had to take down the "Apr8norriscentersnapshot" folder and .zip -- I didn't realize that CCH2 asks people not to distribute data! if you need it, email me or download it yourself from CCH2! the code I have shoudl still work for any date you download for, you may have to adjust dates on axes though, and numbers will likely be slightly different.

- **.gitignore** = ignore this its a github thing (you dont need to download it)

- **Al_data.xlsx** = data provided by Al Keuter (Vascular Plants Collection Curator) tracking changes in the entire collection management

- **Al_data_20250419.xlsx** = data provided by Al Keuter but updated on 19 Apr 2025

- **Apr8norriscentersnapshot.zip** = zip file of the folder with the same name (may be easier to download)

- **CCH2 Portal Data Usage Guidelines** = rules for using CCH2 data!

- **al_data.R** = code exploring the the file Al_data.xlsx

- **snapshot.R** code exploring the data in Apr8norriscentersnapshot (the data straight from CCH2). is mostly me wrangling the data, then trying to make some graphs with it

    - at the very least makes this cute graph:
- ![image](https://github.com/user-attachments/assets/d944309a-9342-4d7f-91a9-692150f56ab2)
