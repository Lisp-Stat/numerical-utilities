+++
title = "Numerical Utilities"
author = ["Steven Nunez"]
draft = false
+++

# Numerical Utilities Library, version 1.1

This library is a collection of packages useful in numerical
applications, each big enough to be its own package, but too small to
split out into a separate ASDF system. Included are:

-   `num=`, a comparison operator for floats
-   simple arithmetic functions, like `sum` and `l2norm`
-   element-wise operations for arrays and vectors
-   intervals
-   special matrices and shorthand for their input
-   sample statistics
-   Chebyshev polynomials
-   quadratures
-   univariate root finding

# Origin

This library is a fork of
[cl-num-utils](https://github.com/tpapp/cl-num-utils). Changes include:

1.  Make work with Genera
2.  Convert to [fiveam](https://github.com/sionescu/fiveam) for unit
    tests ([clunit](https://github.com/tgutu/clunit) seems abandoned)
3.  Restore quadrature tests
4.  Fix SBCL compiler warnings

Although the project has been renamed to distinguish it from the
original, the package names are the same and should work as a drop-in
replacement.

# Status

It appears that this library was in the midst of a reorganization when
released. As near as I can tell from the [github
history](https://github.com/tpapp/cl-num-utils/commit/a0f522b44b465fc071623f9662bdde0163be6467),
all the files in cl-num-utils were moved to src/old/, and then
selectively moved into src/ as they were cleaned up and unit tests
written. Some, such as data-frame, were moved by Papp into separate
projects. Several of the files in old/ could be dusted off and moved
into src/ to be used. Check in data-frames project first because I think
some were moved there without being removed from cl-num-utils.

There was a fair amount of code commented out by the original author.
Where we have commented code, we use the block comment syntax
`#| ... |#`, and noted the person performing the removal. We have also
added comments throughout the code to make it more readable, or to
include our research notes.

# Known Issues

## Test Failures

[Issue 1](https://github.com/Symbolics/num-utils/issues/1) describes a
problem with the wrapped-bivariate-to-array test in tests/matrix.lisp.
This fails under fiveam, but passes on clunit. I believe there to be a
bug, either in fiveam or num-utils (or, possibly, clunit). The [fiveam
reason-arg](https://common-lisp.net/project/fiveam/docs/api/macro_005FIT.BESE.FIVEAM_003A_003AIS.html)
is misleading, it will print the two values, and they are equal. However
this only happens after the *second* call. For example, given this code

``` {.commonlisp org-language="lisp"}
(is (num= (funcall op a b)
      (funcall op (funcall convert a) b));)
"Expected ~A to be equal to ~A" (funcall op a b) (funcall op (funcall convert a) b))
```

The sequence

``` {.commonlisp org-language="lisp"}
(funcall op a b)
(funcall op (funcall convert a) b)
```

is called twice, once as part of the test and once as part of the
reason-args output. The *first* time, the results are different. The
*second* time they are the same, making it appear that the result is a
false negative. It is not.

This test needs to be looked into further. The test code will not win
any \'most readable code of the year\' awards, and the answer is buried
below several layers of macro expansions, funcalls and currying.

Until this is resolved, use this function with caution.

Papp\'s [issue \#16](https://github.com/tpapp/cl-num-utils/issues/16) is
no longer a problem. The functionality was moved to `SELECT` (Papp\'s
`CL-SLICE`). It should have been closed but was not before Papp
abandoned the library.

## Implementation Support

Development is primarily done with SBCL and CCL on MS Windows. Issue
[papp15](https://github.com/tpapp/cl-num-utils/issues/15) reports that
generic function definitions may not work on other implementations.
Please report any such problems on the [Github issue
tracker](https://github.com/Symbolics/num-utils/issues).

## Symbol conflicts with alexandria

Importing both `num-utils` and `alexandria` will result in symbol
conflicts. There are two solutions for this: either import only parts of
`num-utils` (see the packages named in each file), or shadow some
symbols, e.g.

``` {.commonlisp org-language="lisp"}
(cl:defpackage #:my-package
  (:use #:cl
    #:alexandria
    #:num-utils)
  (:shadowing-import-from #:alexandria #:mean #:variance #:median))
```

# Reporting Bugs and Contributing

We welcome participants and contributors. When contributing to this
repository, please first discuss major changes to the existing code you
wish to make via a github issue.

## Pull Request Process

1.  Ensure any install or build dependencies are removed.
2.  Update the README.org with details of changes to the interface.
3.  Increase the version numbers in any examples files and the
    version.sexp file.
4.  Update README.org to the new version that the pull request would
    represent. The versioning scheme we use is described in the
    version.sexp file.

Bugs are tracked on Github, please [open an
issue](https://github.com/Common-Lisp-Statistics/numerical-utilities.cl/issues)
if you find one.

# TODO

## Tasks (by Nunez)

-   Silence SBCL compiler warnings for tests where we are expecting to
    signal an error
-   Test for more specific conditions with fiveam:signal test (now is
    generic error)
-   Investigate unit test failure in wrapped-bivariate-to-array
-   Restore old/arithmetic-types.lisp and write tests
-   Run doc generator over code

## Tasks (by Papp)

-   Finish histogram code and tests
-   Decide whether recursive indexes are practical.

The code for both of Papps\'s tasks is present, but commented out.
