Mungebits 2 [![Build Status](https://travis-ci.org/robertzk/mungebits2.svg?branch=master)](https://travis-ci.org/robertzk/mungebits2) [![Coverage Status](https://coveralls.io/repos/robertzk/mungebits2/badge.svg?branch=master)](https://coveralls.io/r/robertzk/mungebits2) [![Documentation](https://img.shields.io/badge/rocco--docs-%E2%9C%93-blue.svg)](http://robertzk.github.io/mungebits2/)
=========

A way of thinking about data preparation that couples the definition
of what happens in batch processing versus online prediction so that
both can be described by the same codebase.

A re-implementation of mungebits that removes the need for non-standard
evaluation, and provides better integration with
[stageRunner](https://github.com/robertzk/stagerunner).

The package has full test coverage and documentation, so you are
encouraged to peek into the internals.

## Installation

This package is not yet available from CRAN (as of October 26, 2015).
To install the latest development builds directly from GitHub, run this instead:

```R
if (!require("devtools")) install.packages("devtools")
devtools::install_github("robertzk/mungebits2")
```

### License

This project is licensed under the MIT License:

Copyright (c) 2015-2016 Robert Krzyzanowski

Permission is hereby granted, free of charge, to any person obtaining
a copy of this software and associated documentation files (the
"Software"), to deal in the Software without restriction, including
without limitation the rights to use, copy, modify, merge, publish,
distribute, sublicense, and/or sell copies of the Software, and to
permit persons to whom the Software is furnished to do so, subject to
the following conditions:

The above copyright notice and this permission notice shall be included
in all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.
IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY
CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT,
TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE
SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.

### Authors

This package was created by Robert Krzyzanowski, technoguyrob@gmail.com.
It is based on the original package [mungebits](https://github.com/robertzk/mungebits)
by the same author. 

