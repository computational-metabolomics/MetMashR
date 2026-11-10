# Extending MetMashR with custom sources and workflow steps

**Note:** This vignette is built without network access. Online
databases are read from stored copies included with `MetMashR`, and REST
API lookups use stored response caches with `cache_mode = "offline"`, so
a value that is not in a cache is left as `NA` instead of being queried.
To query the online services yourself, use your own cache and the
default `cache_mode = "update"`. The stored copies were made with
`inst/scripts/make_vignette_snapshots.R`.

## Introduction

There is a number software solutions available to annotate an LCMS
metabolomics datasets. We have only included a small number
`annotation_sources` in `MetMashR` as they are the ones we use the most.

In this document we describe how to extend the provided templates to
include new sources bespoke to your requirements.

## Getting started

The latest versions of
*[struct](https://bioconductor.org/packages/3.24/struct)* and `MetMashR`
that are compatible with your current R version can be installed using
BiocManager.

\
`# install BiocManager if not present`\
`if`` ``(``!`[`requireNamespace`](https://rdrr.io/r/base/ns-load.html)`(``"BiocManager"``, quietly ``=`` ``TRUE``)``)`` ``{`\
`    `[`install.packages`](https://rdrr.io/r/utils/install.packages.html)`(``"BiocManager"``)`\
`}`\
\
`# install MetMashR and dependencies`\
`BiocManager``::`[`install`](https://bioconductor.github.io/BiocManager/reference/install.html)`(``"MetMashR"``)`

Once installed you can activate the packages in the usual way:

\
`# load the packages`\
[`library`](https://rdrr.io/r/base/library.html)`(``struct``)`\
[`library`](https://rdrr.io/r/base/library.html)`(`[`MetMashR`](https://computational-metabolomics.github.io/MetMashR/)`)`\
[`library`](https://rdrr.io/r/base/library.html)`(``metabolomicsWorkbenchR``)`\
[`library`](https://rdrr.io/r/base/library.html)`(`[`ggplot2`](https://ggplot2.tidyverse.org)`)`

## Example study

This example uses the first ten rows of metabolite annotations from
Metabolomics Workbench analysis AN000465, stored with the package. We
will create a source reader and add two processing steps: removing
columns that contain only missing values and preparing names for
database searches. We will then retrieve candidate PubChem CIDs and
ConnectivitySMILES strings. At the end, we will display the original
names, search names, selected identifiers and returned SMILES so that
readers can see the result.

The annotations were retrieved from Metabolomics Workbench with the
[`do_query()`](https://rdrr.io/pkg/metabolomicsWorkbenchR/man/do_query.html)
function from the `metabolomicsWorkbenchR` package (`context = "study"`,
`input_item = "analysis_id"`, `input_value = "AN000465"`,
`output_item = "metabolites"`). The full table has 747 rows and 8
columns. For brevity in this vignette we have stored the first 10 rows
in the package.

\
`AN`` ``<-`` `[`readRDS`](https://rdrr.io/r/base/readRDS.html)`(`\
`    `[`system.file`](https://rdrr.io/r/base/system.file.html)`(``"extdata/AN000465_subset.rds"``, package ``=`` ``"MetMashR"``)`\
`)`

We can convert the imported table into an `annotation_table`:

\
`AT`` ``<-`` `[`annotation_table`](https://computational-metabolomics.github.io/MetMashR/reference/annotation_table.md)`(``data ``=`` ``AN``, id_column ``=`` ``NULL``)`

All `annotation_table` objects must have an id for each row in the
table. This is so that later, when examining the outputs of workflow
steps, you can easily trace an annotation through the workflow. The
default `id_column = NULL` will create a new column ‘.MetMashR_id’
containing the row index as an identifier unless provided with an
alternative column name, which should be the name of a column already in
the table.

Two steps needed to clean up this table are not provided by `MetMashR`:

- removal of empty columns
- removal of suffix from duplicate molecule names

We will implement these steps here as an example of how to add new
workflow steps using the
*[struct](https://bioconductor.org/packages/3.24/struct)* package
template system. We will also implement an new `annotation_source` to
import the data from Metabolomics Workbench as part of the workflow.

## Annotation sources

Annotation sources are the mechanism used by `MetMashR` to get tables of
annotation data into R and into a well-defined format. Each extension of
the base `annotation_source` template provides methods to import the raw
annotation table and convert it into a type of `annotation_table`
suitable for the input data. For example, the `cd_source` defines
methods to import annotation data from Compound Discoverer’s Excel
format and convert it into a `lcms_table` object.

## Adding new annotation sources

If we plan to import annotation data from Metabolomics Workbench a lot
we could consider implementing a new `annotation_source` that would
import the annotations as part of a workflow model sequence.

We will do this now as an example of implementing a new
`annotation_source`.

This example creates a new database source by extending
`annotation_database`. We need three pieces: a class definition, a
constructor that creates the object, and a
[`read_database()`](https://computational-metabolomics.github.io/MetMashR/reference/read_database.md)
method that reads the data and returns a data frame. The existing
[`read_source()`](https://computational-metabolomics.github.io/MetMashR/reference/read_source.md)
method puts that data into the source object. We use
[`import_source()`](https://computational-metabolomics.github.io/MetMashR/reference/import_source.md)
to bring the source into a workflow. The processing steps introduced
later use
[`model_apply()`](https://computational-metabolomics.github.io/MetMashR/reference/model_apply.md)
instead.

We will implement each of these steps now.

### The class definition

In this example there are no input and output slots, but they could be
defined using the `slots` parameter, and then differentiated by name
using `.params` and `.outputs` in the prototype.

We have defined `libraries` in the prototype. This is a list of R
package names that are needed to use this object in addition to the
depends/imports of `MetMashR`.

\
`.mwb_source`` ``<-`` ``setClass``(`\
`    ``"mwb_source"``,`\
`    contains ``=`` `[`c`](https://rdrr.io/r/base/c.html)`(``"annotation_database"``)``,`\
`    prototype ``=`` `[`list`](https://rdrr.io/r/base/list.html)`(`\
`        name ``=`` ``"Import from Metabolomics Workbench"``,`\
`        libraries ``=`` ``"metabolomicsWorkbenchR"`\
`    ``)`\
`)`

### The calling function

The calling function is a small function that creates a new instance of
the source object using `new_struct` and provides a mechanism to
initialise values based on input parameters.

The use of ellipsis `...` allows us to pass additional values to the
base `struct` object like name and description without including them in
the function definition, in case we want to override the values defined
in the prototype of the class definition in the previous section.

\
`mwb_source`` ``<-`` ``function``(``...``)`` ``{`\
`    ``# new object`\
`    ``out`` ``<-`` `[`new_struct`](https://rdrr.io/pkg/struct/man/new_struct.html)`(`\
`        ``"mwb_source"``,`\
`        ``...`\
`    ``)`\
`    `[`return`](https://rdrr.io/r/base/function.html)`(``out``)`\
`}`

### The import method

The
[`read_database()`](https://computational-metabolomics.github.io/MetMashR/reference/read_database.md)
method below reads the cached annotation table and returns it as a data
frame. We define this method for the `mwb_source` class using
`setMethod()`.

\
`setMethod``(`\
`    f ``=`` ``"read_database"``,`\
`    signature ``=`` `[`c`](https://rdrr.io/r/base/c.html)`(``"mwb_source"``)``,`\
`    definition ``=`` ``function``(``obj``)`` ``{`\
`        ``## for vignette use locally cached subset`\
`        ``AN`` ``<-`` `[`readRDS`](https://rdrr.io/r/base/readRDS.html)`(`\
`            `[`system.file`](https://rdrr.io/r/base/system.file.html)`(``"extdata/AN000465_subset.rds"``, package ``=`` ``"MetMashR"``)`\
`        ``)`\
\
`        `[`return`](https://rdrr.io/r/base/function.html)`(``AN``)`\
`    ``}`\
`)`

This demonstration always reads `AN000465_subset.rds`. Changing the
`source` parameter does not select a different analysis. To make a
reader for other analyses, replace the cached-file code with a working
query that uses the object’s `source` parameter.

### Using the new source

The new `mwb_source` class and its
[`read_database()`](https://computational-metabolomics.github.io/MetMashR/reference/read_database.md)
method are ready to use. To import the table we can use
[`read_source()`](https://computational-metabolomics.github.io/MetMashR/reference/read_source.md):

\
`# initialise source`\
`SRC`` ``<-`` ``mwb_source``(`\
`    source ``=`` ``"AN000465"`\
`)`\
\
`# import`\
`AT`` ``<-`` `[`read_source`](https://computational-metabolomics.github.io/MetMashR/reference/read_source.md)`(``SRC``)`

Variable `AT` is the imported and cleaned `annotation_database` from
Metabolomics Workbench and can be used as input to `model_apply` for
other models and sequence, e.g. to look up PubChem Ids etc.

## Adding annotation workflow steps

`MetMashR` workflow steps use the `model` template from the
*[struct](https://bioconductor.org/packages/3.24/struct)* package.
Although originally intended for statistical methods, the `model`
template is a flexible way to implement many different kinds of workflow
step.

The *[struct](https://bioconductor.org/packages/3.24/struct)* package
provides convenience functions for “on-the-fly” implementation of new
model objects. Here will will use the convenience functions to implement
the first new model. Examples of this are also included in the
*[struct](https://bioconductor.org/packages/3.24/struct)* package
vignette.

### Empty column removal

To define a new `model` object we can use the `set_struct_obj` function.
The new object is fairly simple in that no input parameters are
required. The only output slot will contain the `annotation_table` after
we have removed the empty columns.

For the `prototype` input we provide a name and a description for our
new model. This is good practice as our intentions for this model are
explicitly stated and stay with the model wherever we use it, helping to
ensure transparency, reproducibility etc. We also defined the default
output of the model by providing `predicted` in the `prototype`.

\
[`set_struct_obj`](https://rdrr.io/pkg/struct/man/set_struct_obj.html)`(`\
`    class_name ``=`` ``"drop_empty_columns"``,`\
`    struct_obj ``=`` ``"model"``,`\
`    params ``=`` `[`character`](https://rdrr.io/r/base/character.html)`(``0``)``,`\
`    outputs ``=`` `[`c`](https://rdrr.io/r/base/c.html)`(``updated ``=`` ``"annotation_source"``)``,`\
`    private ``=`` `[`character`](https://rdrr.io/r/base/character.html)`(``0``)``,`\
`    prototype ``=`` `[`list`](https://rdrr.io/r/base/list.html)`(`\
`        name ``=`` ``"Drop empty columns"``,`\
`        description ``=`` `[`paste0`](https://rdrr.io/r/base/paste.html)`(`\
`            ``"A workflow step that removes columns from an annotation table "``,`\
`            ``"where all rows are NA."`\
`        ``)``,`\
`        predicted ``=`` ``"updated"`\
`    ``)`\
`)`

We can now create an instance of our new model, and use the `show`
method to display information about it.

\
`M`` ``<-`` ``drop_empty_columns``(``)`\
`show``(``M``)`\
`#> A "drop_empty_columns" object`\
`#> -----------------------------`\
`#> name:          Drop empty columns`\
`#> description:   A workflow step that removes columns from an annotation table where all rows are NA.`\
`#> outputs:       updated `\
`#> predicted:     updated`\
`#> seq_in:        data`

Before we can use our new model we need to implement the `model_apply`
method for it. This is the function that actually does all the work. In
this case it will search for columns of missing values and then remove
them from the `annotation_table`. To define this method we use the
`set_obj_method` function.

\
[`set_obj_method`](https://rdrr.io/pkg/struct/man/set_obj_method.html)`(`\
`    class_name ``=`` ``"drop_empty_columns"``,`\
`    method_name ``=`` ``"model_apply"``,`\
`    signature ``=`` `[`c`](https://rdrr.io/r/base/c.html)`(``"drop_empty_columns"``, ``"annotation_source"``)``,`\
`    definition ``=`` ``function``(``M``, ``D``)`` ``{`\
`        ``# search for columns of NA`\
`        ``W`` ``<-`` `[`lapply`](https://rdrr.io/r/base/lapply.html)`(`` ``# for each column`\
`            ``D``$``data``, ``# in the annotation table`\
`            ``function``(``x``)`` ``{`\
`                `[`all`](https://rdrr.io/r/base/all.html)`(`[`is.na`](https://rdrr.io/r/base/NA.html)`(``x``)``)`` ``# return TRUE if all rows are NA`\
`            ``}`\
`        ``)`\
\
`        ``# get index of columns with all rows NA`\
`        ``idx`` ``<-`` `[`which`](https://rdrr.io/r/base/which.html)`(`[`unlist`](https://rdrr.io/r/base/unlist.html)`(``W``)``)`\
\
`        ``# if any found, remove from annotation table`\
`        ``if`` ``(`[`length`](https://rdrr.io/r/base/length.html)`(``idx``)`` ``>`` ``0``)`` ``{`\
`            ``D``$``data``[``, ``idx``]`` ``<-`` ``NULL`\
`        ``}`\
\
`        ``# update model object`\
`        ``M``$``updated`` ``<-`` ``D`\
\
`        ``# return object`\
`        `[`return`](https://rdrr.io/r/base/function.html)`(``M``)`\
`    ``}`\
`)`

Note that in the `signature` we specify that the second input is an
`annotation_table`.

The new model is ready to use. First we try it on a tiny table where
column `b` contains only missing values:

\
`toy`` ``<-`` `[`annotation_table`](https://computational-metabolomics.github.io/MetMashR/reference/annotation_table.md)`(`\
`    data ``=`` `[`data.frame`](https://rdrr.io/r/base/data.frame.html)`(`\
`        a ``=`` `[`c`](https://rdrr.io/r/base/c.html)`(``"x"``, ``"y"``)``,`\
`        b ``=`` `[`c`](https://rdrr.io/r/base/c.html)`(``NA``, ``NA``)``,`\
`        c ``=`` `[`c`](https://rdrr.io/r/base/c.html)`(``1``, ``NA``)`\
`    ``)``,`\
`    id_column ``=`` ``NULL`\
`)`\
\
`# before`\
`toy``$``data`\
`#>   a  b  c .MetMashR_id`\
`#> 1 x NA  1            1`\
`#> 2 y NA NA            2`\
\
`# after`\
[`predicted`](https://rdrr.io/pkg/struct/man/predicted.html)`(`[`model_apply`](https://computational-metabolomics.github.io/MetMashR/reference/model_apply.md)`(``drop_empty_columns``(``)``, ``toy``)``)``$``data`\
`#>   a  c .MetMashR_id`\
`#> 1 x  1            1`\
`#> 2 y NA            2`

Column `b` has been removed. Column `c` is kept because it is only
partly missing.

Now we apply it to the Metabolomics Workbench annotations using the
`model_apply` method, and check that some columns have been removed.

\
`M`` ``<-`` `[`model_apply`](https://computational-metabolomics.github.io/MetMashR/reference/model_apply.md)`(``M``, ``AT``)`

The number of columns before was:

\
[`ncol`](https://rdrr.io/r/base/nrow.html)`(``AT``$``data``)`\
`#> [1] 8`

The number of columns afterwards is:

\
[`ncol`](https://rdrr.io/r/base/nrow.html)`(``M``$``updated``$``data``)`\
`#> [1] 5`

### Suffix removal

The second model removes the suffix from the molecule names. The is
necessary if we want to use the molecule names to e.g search for PubChem
identifiers using the REST API; we wont get a match if the suffix is
part of the molecule name.

As we did for `drop_empty_columns` we use the `set_struct_obj` and
`set_obj_method` function to create our new workflow step.

\
`# define new model object`\
[`set_struct_obj`](https://rdrr.io/pkg/struct/man/set_struct_obj.html)`(`\
`    class_name ``=`` ``"remove_suffix"``,`\
`    struct_obj ``=`` ``"model"``,`\
`    params ``=`` `[`c`](https://rdrr.io/r/base/c.html)`(``clean ``=`` ``"logical"``, column_name ``=`` ``"character"``)``,`\
`    outputs ``=`` `[`c`](https://rdrr.io/r/base/c.html)`(``updated ``=`` ``"annotation_source"``)``,`\
`    prototype ``=`` `[`list`](https://rdrr.io/r/base/list.html)`(`\
`        name ``=`` ``"Remove suffix"``,`\
`        description ``=`` `[`paste0`](https://rdrr.io/r/base/paste.html)`(`\
`            ``"A workflow step that removes suffixes from molecule names by "``,`\
`            ``"splitting a string at the last underscore an retaining the part"``,`\
`            ``"of the string before the underscore."`\
`        ``)``,`\
`        predicted ``=`` ``"updated"``,`\
`        clean ``=`` ``FALSE``,`\
`        column_name ``=`` ``"V1"`\
`    ``)`\
`)`\
\
`# define method for new object`\
[`set_obj_method`](https://rdrr.io/pkg/struct/man/set_obj_method.html)`(`\
`    class_name ``=`` ``"remove_suffix"``,`\
`    method_name ``=`` ``"model_apply"``,`\
`    signature ``=`` `[`c`](https://rdrr.io/r/base/c.html)`(``"remove_suffix"``, ``"annotation_source"``)``,`\
`    definition ``=`` ``function``(``M``, ``D``)`` ``{`\
`        ``# get list of molecule names`\
`        ``x`` ``<-`` ``D``$``data``[[``M``$``column_name``]``]`\
\
`        ``# split string at last underscore`\
`        ``s`` ``<-`` `[`strsplit`](https://rdrr.io/r/base/strsplit.html)`(``x``, ``"_(?!.*_)"``, perl ``=`` ``TRUE``)`\
\
`        ``# get left hand side`\
`        ``s`` ``<-`` `[`lapply`](https://rdrr.io/r/base/lapply.html)`(``s``, ``"["``, ``1``)`\
\
`        ``# if clean replace existing column, otherwise new column`\
`        ``if`` ``(``M``$``clean``)`` ``{`\
`            ``D``$``data``[[``M``$``column_name``]``]`` ``<-`` `[`unlist`](https://rdrr.io/r/base/unlist.html)`(``s``)`\
`        ``}`` ``else`` ``{`\
`            ``D``$``data``$``name.fixed`` ``<-`` `[`unlist`](https://rdrr.io/r/base/unlist.html)`(``s``)`\
`        ``}`\
\
`        ``# update model object`\
`        ``M``$``updated`` ``<-`` ``D`\
\
`        ``# return object`\
`        `[`return`](https://rdrr.io/r/base/function.html)`(``M``)`\
`    ``}`\
`)`

For this model we included two input parameters and provided default
values in the `prototype`:

- column_name: the name of the column in the annotation table containing
  molecule names
- clean: a flag that will replace the old column if TRUE or add a new
  column if FALSE

Here is the step applied to a tiny table, first with `clean = FALSE`,
which adds a new `name.fixed` column and keeps the original names:

\
`toy`` ``<-`` `[`annotation_table`](https://computational-metabolomics.github.io/MetMashR/reference/annotation_table.md)`(`\
`    data ``=`` `[`data.frame`](https://rdrr.io/r/base/data.frame.html)`(`\
`        name ``=`` `[`c`](https://rdrr.io/r/base/c.html)`(``"Glucose_1"``, ``"Glucose_2"``, ``"Alanine"``)`\
`    ``)``,`\
`    id_column ``=`` ``NULL`\
`)`\
\
[`predicted`](https://rdrr.io/pkg/struct/man/predicted.html)`(`[`model_apply`](https://computational-metabolomics.github.io/MetMashR/reference/model_apply.md)`(`\
`    ``remove_suffix``(``column_name ``=`` ``"name"``, clean ``=`` ``FALSE``)``,`\
`    ``toy`\
`)``)``$``data`\
`#>        name .MetMashR_id name.fixed`\
`#> 1 Glucose_1            1    Glucose`\
`#> 2 Glucose_2            2    Glucose`\
`#> 3   Alanine            3    Alanine`

and then with `clean = TRUE`, which replaces the names in the existing
column:

\
[`predicted`](https://rdrr.io/pkg/struct/man/predicted.html)`(`[`model_apply`](https://computational-metabolomics.github.io/MetMashR/reference/model_apply.md)`(`\
`    ``remove_suffix``(``column_name ``=`` ``"name"``, clean ``=`` ``TRUE``)``,`\
`    ``toy`\
`)``)``$``data`\
`#>      name .MetMashR_id`\
`#> 1 Glucose            1`\
`#> 2 Glucose            2`\
`#> 3 Alanine            3`

## Metabolite mashing

Now that we have defined our new workflow steps we can use them in a
`model_seq` (workflow), alongside other existing steps to mash the
annotations with other tables containing identifiers and additional
information.

In this case we do the following:

1.  import the annotations from Metabolomics Workbench
2.  remove empty columns
3.  remove suffixes from molecule names, writing them to a new column so
    the original names are kept
4.  search the Metabolomics Workbench refmet database for identifiers
5.  use the PubChem REST API to search for compound CIDs using the names
    without suffixes
6.  combine the CID columns from refmet and pubchem into a single
    column, giving priority to refmet, to obtain a single column of
    identifiers we have confidence in for as many metabolites as
    possible
7.  use the PubChem REST API to obtain SMILES for each metabolite

For clarity in the workflow we import the refmet database before using
it in the workflow instead of importing it in-line.
[`mwb_refmet_database()`](https://computational-metabolomics.github.io/MetMashR/reference/mwb_refmet_database.md)
downloads the full RefMet database; here we read a stored copy of the
RefMet records for the names in this example. We also import stored
PubChem responses, which are used with `cache_mode = "offline"`.

\
`# stored RefMet records for this example`\
`refmet`` ``<-`` `[`rds_database`](https://computational-metabolomics.github.io/MetMashR/reference/rds_database.md)`(`\
`    source ``=`` `[`system.file`](https://rdrr.io/r/base/system.file.html)`(`\
`        ``"extdata/vignette_snapshots/RefMet_AN000465_subset.rds"``,`\
`        package ``=`` ``"MetMashR"`\
`    ``)``,`\
`    .writable ``=`` ``FALSE`\
`)`\
\
`pubchem_cid_cache`` ``<-`` `[`rds_database`](https://computational-metabolomics.github.io/MetMashR/reference/rds_database.md)`(`\
`    source ``=`` `[`file.path`](https://rdrr.io/r/base/file.path.html)`(`\
`        `[`system.file`](https://rdrr.io/r/base/system.file.html)`(``"cached"``, package ``=`` ``"MetMashR"``)``,`\
`        ``"pubchem_cid_cache.rds"`\
`    ``)`\
`)`\
`pubchem_smile_cache`` ``<-`` `[`rds_database`](https://computational-metabolomics.github.io/MetMashR/reference/rds_database.md)`(`\
`    source ``=`` `[`file.path`](https://rdrr.io/r/base/file.path.html)`(`\
`        `[`system.file`](https://rdrr.io/r/base/system.file.html)`(``"cached"``, package ``=`` ``"MetMashR"``)``,`\
`        ``"pubchem_smiles_cache.rds"`\
`    ``)`\
`)`

In practice it is better to create your own cache using
e.g. `rds_database`,
[`sqlite_database()`](https://computational-metabolomics.github.io/MetMashR/reference/sqlite_database.md)
or other `annotation_database` objects with write access.

\
`# prepare sequence`\
`M`` ``<-`` `[`import_source`](https://computational-metabolomics.github.io/MetMashR/reference/import_source.md)`(``)`` ``+`\
`    ``drop_empty_columns``(``)`` ``+`\
`    ``# keep the original names; the names without suffix are written to a`\
`    ``# new "name.fixed" column`\
`    ``remove_suffix``(`\
`        clean ``=`` ``FALSE``,`\
`        column_name ``=`` ``"metabolite_name"`\
`    ``)`` ``+`\
`    `[`database_lookup`](https://computational-metabolomics.github.io/MetMashR/reference/database_lookup.md)`(`\
`        query_column ``=`` ``"refmet_name"``,`\
`        database_column ``=`` ``"name"``,`\
`        database ``=`` ``refmet``,`\
`        suffix ``=`` ``"_mwb"``,`\
`        include ``=`` ``"pubchem_cid"`\
`    ``)`` ``+`\
`    `[`pubchem_compound_lookup`](https://computational-metabolomics.github.io/MetMashR/reference/pubchem_compound_lookup.md)`(`\
`        query_column ``=`` ``"name.fixed"``,`\
`        search_by ``=`` ``"name"``,`\
`        suffix ``=`` ``"_pc"``,`\
`        output ``=`` ``"cids"``,`\
`        records ``=`` ``"best"``,`\
`        delay ``=`` ``0.2``,`\
`        cache ``=`` ``pubchem_cid_cache``,`\
`        cache_mode ``=`` ``"offline"`\
`    ``)`` ``+`\
`    `[`prioritise_columns`](https://computational-metabolomics.github.io/MetMashR/reference/prioritise_columns.md)`(`\
`        column_names ``=`` `[`c`](https://rdrr.io/r/base/c.html)`(``"pubchem_cid_mwb"``, ``"CID_pc"``)``,`\
`        output_name ``=`` ``"pubchem_cid"``,`\
`        source_name ``=`` ``"pubchem_cid_source"``,`\
`        source_tags ``=`` `[`c`](https://rdrr.io/r/base/c.html)`(``"mwb"``, ``"pc"``)``,`\
`        clean ``=`` ``TRUE`\
`    ``)`` ``+`\
`    `[`pubchem_property_lookup`](https://computational-metabolomics.github.io/MetMashR/reference/pubchem_property_lookup.md)`(`\
`        query_column ``=`` ``"pubchem_cid"``,`\
`        search_by ``=`` ``"cid"``,`\
`        suffix ``=`` ``""``,`\
`        property ``=`` ``"ConnectivitySMILES"``,`\
`        delay ``=`` ``0.2``,`\
`        cache ``=`` ``pubchem_smile_cache``,`\
`        cache_mode ``=`` ``"offline"`\
`    ``)`\
\
`# apply sequence`\
`M`` ``<-`` `[`model_apply`](https://computational-metabolomics.github.io/MetMashR/reference/model_apply.md)`(``M``, ``mwb_source``(``source ``=`` ``"AN000465"``)``)`

Note that because the first step in the workflow is to import data from
Metabolomics Workbench, the source object we pass to `model_apply` only
needs to say which source to import; the data are read by the first
step.

The table below shows the result. For each annotation it gives the
original name, the name used for the PubChem search, the selected
PubChem CID, which search supplied that CID (`mwb` for RefMet, `pc` for
the PubChem name search) and the SMILES returned by PubChem.

\
`res`` ``<-`` `[`predicted`](https://rdrr.io/pkg/struct/man/predicted.html)`(``M``)``$``data`\
\
`# the cached PubChem responses may use the older property name`\
`# "CanonicalSMILES" for what PubChem now calls "ConnectivitySMILES"`\
`smiles_col`` ``<-`` `[`grep`](https://rdrr.io/r/base/grep.html)`(``"SMILES"``, `[`colnames`](https://rdrr.io/r/base/colnames.html)`(``res``)``, value ``=`` ``TRUE``)``[``1``]`\
\
[`data.frame`](https://rdrr.io/r/base/data.frame.html)`(`\
`    original_name ``=`` ``res``$``metabolite_name``,`\
`    search_name ``=`` ``res``$``name.fixed``,`\
`    pubchem_cid ``=`` ``res``$``pubchem_cid``,`\
`    cid_source ``=`` ``res``$``pubchem_cid_source``,`\
`    smiles ``=`` ``res``[[``smiles_col``]``]`\
`)`\
`#>                                                   original_name`\
`#> 1                           10-Deacetyl-2-debenzoylbaccatin III`\
`#> 2                                               10-Oxodecanoate`\
`#> 3                    1,1-Diethyl-2-hydroxy-2-nitrosohydrazine_1`\
`#> 4                    1,1-Diethyl-2-hydroxy-2-nitrosohydrazine_2`\
`#> 5                          1,2-Dihydroxynaphthalene-6-sulfonate`\
`#> 6                         12-trans-Hydroxy juvenile hormone III`\
`#> 7                                         14-Dihydroxycornestin`\
`#> 8                       1-(5-Phosphoribosyl)imidazole-4-acetate`\
`#> 9  1alpha,5alpha-Epidithio-17a-oxa-D-homoandrostan-3,17-dione_1`\
`#> 10 1alpha,5alpha-Epidithio-17a-oxa-D-homoandrostan-3,17-dione_2`\
`#>                                                   search_name pubchem_cid`\
`#> 1                         10-Deacetyl-2-debenzoylbaccatin III      443489`\
`#> 2                                             10-Oxodecanoate       79686`\
`#> 3                    1,1-Diethyl-2-hydroxy-2-nitrosohydrazine     6331148`\
`#> 4                    1,1-Diethyl-2-hydroxy-2-nitrosohydrazine     6331148`\
`#> 5                        1,2-Dihydroxynaphthalene-6-sulfonate     9543072`\
`#> 6                       12-trans-Hydroxy juvenile hormone III        <NA>`\
`#> 7                                       14-Dihydroxycornestin     5281160`\
`#> 8                     1-(5-Phosphoribosyl)imidazole-4-acetate      440341`\
`#> 9  1alpha,5alpha-Epidithio-17a-oxa-D-homoandrostan-3,17-dione    11954165`\
`#> 10 1alpha,5alpha-Epidithio-17a-oxa-D-homoandrostan-3,17-dione    11954165`\
`#>    cid_source                                                           smiles`\
`#> 1         mwb CC1=C2C(C(=O)C3(C(CC4C(C3C(C(C2(C)C)(CC1O)O)O)(CO4)OC(=O)C)O)C)O`\
`#> 2         mwb                                              C(CCCCC(=O)O)CCCC=O`\
`#> 3          pc                                                CCN(CC)N(N=O)[O-]`\
`#> 4          pc                                                CCN(CC)N(N=O)[O-]`\
`#> 5          pc                              C1=CC2=C(C=CC(=C2O)O)C=C1S(=O)(=O)O`\
`#> 6        <NA>                                                             <NA>`\
`#> 7         mwb                        CCCC1C(C(=O)CC(C(=CC)CC2=C1C(=O)OC2=O)O)O`\
`#> 8         mwb                     C1=C(N=CN1C2C(C(C(O2)COP(=O)(O)O)O)O)CC(=O)O`\
`#> 9          pc                   CC12CCC3C(C1CCC(=O)O2)CCC45C3(C(CC(=O)C4)SS5)C`\
`#> 10         pc                   CC12CCC3C(C1CCC(=O)O2)CCC45C3(C(CC(=O)C4)SS5)C`

## Summary

Extending the templates provided by `struct` to implement workflow steps
for metabolite mashing is straight forward using either the provided
on-the-fly functions or scripting more permanent solutions. All objects
that use the templates will be compatible with other workflow objects
provided you follow the templates. As well as new workflow steps
MetMashR can also be extended to include additional annotation sources.

## Session info

\
[`sessionInfo`](https://rdrr.io/r/utils/sessionInfo.html)`(``)`\
`#> R version 4.6.1 (2026-06-24)`\
`#> Platform: x86_64-pc-linux-gnu`\
`#> Running under: Ubuntu 24.04.5 LTS`\
`#> `\
`#> Matrix products: default`\
`#> BLAS:   /usr/lib/x86_64-linux-gnu/openblas-pthread/libblas.so.3 `\
`#> LAPACK: /usr/lib/x86_64-linux-gnu/openblas-pthread/libopenblasp-r0.3.26.so;  LAPACK version 3.12.0`\
`#> `\
`#> locale:`\
`#>  [1] LC_CTYPE=en_US.UTF-8       LC_NUMERIC=C              `\
`#>  [3] LC_TIME=en_US.UTF-8        LC_COLLATE=en_US.UTF-8    `\
`#>  [5] LC_MONETARY=en_US.UTF-8    LC_MESSAGES=en_US.UTF-8   `\
`#>  [7] LC_PAPER=en_US.UTF-8       LC_NAME=C                 `\
`#>  [9] LC_ADDRESS=C               LC_TELEPHONE=C            `\
`#> [11] LC_MEASUREMENT=en_US.UTF-8 LC_IDENTIFICATION=C       `\
`#> `\
`#> time zone: UTC`\
`#> tzcode source: system (glibc)`\
`#> `\
`#> attached base packages:`\
`#> [1] stats     graphics  grDevices utils     datasets  methods   base     `\
`#> `\
`#> other attached packages:`\
`#> [1] ggplot2_4.0.3                 metabolomicsWorkbenchR_1.23.0`\
`#> [3] MetMashR_1.7.3                struct_1.25.0                `\
`#> [5] BiocStyle_2.41.0             `\
`#> `\
`#> loaded via a namespace (and not attached):`\
`#>  [1] SummarizedExperiment_1.43.0 gtable_0.3.6               `\
`#>  [3] xfun_0.62                   bslib_0.12.0               `\
`#>  [5] httr2_1.3.0                 htmlwidgets_1.6.4          `\
`#>  [7] Biobase_2.73.2              lattice_0.23-1             `\
`#>  [9] vctrs_0.7.3                 tools_4.6.1                `\
`#> [11] generics_0.1.4              stats4_4.6.1               `\
`#> [13] tibble_3.3.1                pkgconfig_2.0.3            `\
`#> [15] Matrix_1.7-6                data.table_1.18.6.1        `\
`#> [17] RColorBrewer_1.1-3          S7_0.2.2                   `\
`#> [19] desc_1.4.3                  S4Vectors_0.51.10          `\
`#> [21] lifecycle_1.0.5             compiler_4.6.1             `\
`#> [23] farver_2.1.2                textshaping_1.0.5          `\
`#> [25] Seqinfo_1.3.2               htmltools_0.5.9            `\
`#> [27] sass_0.4.10                 yaml_2.3.12                `\
`#> [29] pkgdown_2.2.1.9000          pillar_1.11.1              `\
`#> [31] jquerylib_0.1.4             DelayedArray_0.39.8        `\
`#> [33] cachem_1.1.0                abind_1.4-8                `\
`#> [35] tidyselect_1.2.1            digest_0.6.39              `\
`#> [37] dplyr_1.2.1                 purrr_1.2.2                `\
`#> [39] bookdown_0.48               ggthemes_7.0.0             `\
`#> [41] fastmap_1.2.0               grid_4.6.1                 `\
`#> [43] cli_3.6.6                   SparseArray_1.13.4         `\
`#> [45] magrittr_2.0.5              S4Arrays_1.13.2            `\
`#> [47] withr_3.0.3                 scales_1.4.0               `\
`#> [49] rmarkdown_2.32              XVector_0.53.0             `\
`#> [51] httr_1.4.9                  matrixStats_1.5.0          `\
`#> [53] otel_0.2.0                  ragg_1.5.2                 `\
`#> [55] evaluate_1.0.5              knitr_1.52                 `\
`#> [57] GenomicRanges_1.65.4        IRanges_2.47.5             `\
`#> [59] MultiAssayExperiment_1.39.1 rlang_1.3.0                `\
`#> [61] Rcpp_1.1.2                  glue_1.8.1                 `\
`#> [63] BiocManager_1.30.27         xml2_1.6.0                 `\
`#> [65] BiocGenerics_0.59.12        jsonlite_2.0.0             `\
`#> [67] R6_2.6.1                    plyr_1.8.9                 `\
`#> [69] MatrixGenerics_1.25.0       systemfonts_1.3.2          `\
`#> [71] fs_2.1.0`
