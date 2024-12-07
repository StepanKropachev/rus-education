# workflow

## commit 1

1. upload data from the file called 'Rus_Census_Labour_force_grouped_by_status_and_education';
2. delete the first 9 raws;
3. add header raw with the following names for the columns:

region
all_pop
ind_edu
phd_edu
hig_edu
mast_edu
spec_edu
bac_edu
not_compl_edu
prof_edu
prof_edu_mid_spec
prof_edu_work
mid_edu
mid_edu_gen
mid_edu_prim
non_edu
not_ind_edu

4. drop all the rows that have "в том числе:" in the first column;
5. add cloumns 'set_type', 'status';
6. allocated regions, status and set_type values to the coresponding columns;
7. Translate russian text into english to better reading.