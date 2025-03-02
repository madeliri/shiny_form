### 0.??.?

##### features
- added checkboxes input form;
- added button to reset data in forms;
- added option to export input data to `.docx` format (installed pandoc is required), using `reference.docx` template;
- added new column in `main.xlsx` schema with `required` option: now you can set specifically which forms is required (1 - is required, NA - is not required) - this option now used in input validation (doesn't block saving data yet);
- checking on load if schema changed (comparing to existing db): 
  - if new input form added in schema - adding it also on database (with empty values);
  - if input form deleted - stop app to prevent data loss;
  - if input form was renamed - stop app to prevent data loss;
  - in other cases - show warnings;

##### fixes

- fixed not erasing inputs while loading empty values (with checkboxes, radiobuttons);
- +number input validation

##### changes

- some code refactoring;
- replacing NumberImput to TextInput due to correct implement validation;



### 0.14.1 2024-10-14

##### fixes

- catching crash file due to bug in rhandsometable (fail to export tables with empty rows)



### 0.14 2024-10-14

##### changes

- code rafactoring
- add visual data validation



### 0.13 2024-10-11

##### changes

- moving script to init login db to separate file
- wider inline tables



### 0.12 2024-09-29

##### fixes:

- error while saving tables to db due to wrong data formats

##### changes:

- moving config.yml to configs folder
- moving schemas to configs folder


### 0.11 2024-09-23

##### fixes:
- error while loading table due to change PostrgreSQL driver
- error while export db as .xlsx due to misspelling in button name