## Approach
"
For each record
    find columns (attributes) that have values
    query_res = run a query to find all records that have values for these attributes
    for each_record in query_res
        compute distance between current_rec and each_record
    for each missing_attribute 
        filter out records from query_res that dont have value for missing_attribute
        value of missing_attribute = weighted average of the value from other records

"
