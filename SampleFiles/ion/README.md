# Amazon Ion Test Data
The `iontestdata` folder contains samples of [Ion](http://amzn.github.io/ion-docs)
content for use by compatibility test suites.

Consumers must assume that additional nested subfolders may be added, and
should therefore recurse down from the desired folder if appropriate.

The content is partitioned as follows:

  * `bad`

      All files in this directory are invalid Ion and should fail parsing.
      Most files should include comments indicating the problem.

  * `good`

      All files in this directory are valid Ion.

  * `good/equivs`

      Each file in this directory consists of one or more top-level sequences
      (lists or sexps). Each top-level sequence contains at least two Ion
      values, all of which should be equivalent within the Ion data model.
      This equivalence constraint does not apply to child values of different
      top-level sequences.

  * `good/non-equivs`

      Each file in this directory consists of one or more top-level sequences
      (lists or sexps). Each top-level sequence contains at least two Ion
      values, all of which should NOT be equivalent within the Ion data model.
      This equivalence constraint does not apply to child values of different
      top-level sequences.

Additional constraints:

  * `good/timestamps`

      Each .ion file must have one or more top-level timestamp values,
      one per line.
      Comments must be //-style and start at the first column.
      Subdirectories do not necessarily follow this convention.

  * `bad/timestamps`

      Each .ion file must have a single invalid timestamp as the first line.
      Comments may follow on subsequent lines.
      Subdirectories do not necessarily follow this convention.
  * `good/equivs` and `good/non-equivs`

      If a top-level sequence is annotated with "embedded_documents", it denotes
      that each of its Ion values is to be parsed as a Ion string value, where
      its string value is to be parsed as a document.
      As such, each top-level sequence contains at least two documents, all of
      which should be equivalent or non-equivalent within the Ion data model,
      for the directories good/equivs and good/non-equivs respectively.
