### Version 0.1.0.9001

  * Fix a bug in `column_transformation` wherein `NULL` return
    values corrupted the data.frame instead of dropping columns.

### Version 0.1.0.9000

  * Start development version.
  * Change `munge` function to use an [objectdiff](https://github.com/robertzk/objectdiff)-compatible
    check for the `data` element if passed an environment.
