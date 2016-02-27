### Version 0.1.0.9003

  * Fixed an issue where `column_transformation` does not correctly drop multiple
    columns.

### Version 0.1.0.9001-2

  * Define `all.equal` for object of S3 class "transformation".
  * Fix a bug in `column_transformation` wherein `NULL` return
    values corrupted the data.frame instead of dropping columns.

### Version 0.1.0.9000

  * Start development version.
  * Change `munge` function to use an [objectdiff](https://github.com/robertzk/objectdiff)-compatible
    check for the `data` element if passed an environment.

