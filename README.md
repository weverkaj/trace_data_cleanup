# Data cleaning for TRACE flux data and biogeochemical data. 

Cleaned data is found in the *data_cleaned* folder. Data are described as follows:

    Key Columns: plot, date (year-month-day)

    Column names: varname_unit_depth_source

    For aggregated columns (columns that represent means or measures of variation): varname_measure_unit_depth_source

        varname - name of the variable
        unit - unit
        depth - depth from which the sample was taken, for depth-resolved data. Not included if data not taken across depths
        measure - measure of aggregated data (i.e. mean, SD, etc)
        source - file from which data was taken
