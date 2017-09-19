### Version 0.1.0.9016

* Explicitly use `base::` in `column_transformation` to avoid crazy
  scenarios where a global function exists with the same name (because
  not everyone knows what's in base).

### Version 0.1.0.9015

* Allow NULL values in lists passed to `munge` function.

### Version 0.1.0.9015

* Ensure that names are preserved for legacy mungebits too.

### Version 0.1.0.9014

* Ensure that names are preserved when munging using `munge`.

### Version 0.1.0.9013

* Add `is.transformation` helper.

### Version 0.1.0.9012

  * Remember the value of `trained` when duplicating a mungebit.

### Version 0.1.0.9010-9011

  * Fix `parse_mungepiece` for dual format with legacy functions.

### Version 0.1.0.9009

  * Modify `mungebit$run` to work correctly with shadowed versions
    of `exists`, as in [objectdiff](https://github.com/robertzk/objectdiff).

### Version 0.1.0.9004-8

  * Provide backwards compatibility with
    [legacy mungebits](https://github.com/robertzk/mungebits)
    so that using `munge` with mixed legacy and new mungepieces
    works as expected, including construction of mungebits
    from raw train and predict functions.

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

