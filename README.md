
<!-- PROJECT SHIELDS -->

[![Contributors][contributors-shield]][contributors-url]
[![Forks][forks-shield]][forks-url]
[![Stargazers][stars-shield]][stars-url]
[![Issues][issues-shield]][issues-url]
[![MS-PL License][license-shield]][license-url]
[![LinkedIn][linkedin-shield]][linkedin-url]



<!-- PROJECT LOGO -->
<br />
<p align="center">
  <a href="https://github.com/lisp-stat/numerical-utilities">
    <img src="http://www.lisp-stat.dev/images/stats-image.svg" alt="Logo" width="80" height="80">
  </a>

  <h3 align="center">Numerical Utilities</h3>

  <p align="center">
  For statistical computing and numerical methods
	<br />
    <a href="https://github.com/Lisp-Stat/numerical-utilities/blob/master/docs/numerical-utilities.md"><strong>Explore the docs »</strong></a>
    <br />
    <br />
    <a href="https://github.com/lisp-stat/numerical-utilities/issues">Report Bug</a>
    ·
    <a href="https://github.com/lisp-stat/numerical-utilities/issues">Request Feature</a>
    ·
	<a href="https://lisp-stat.github.io/numerical-utilities/">Reference Manual</a>
  </p>
</p>



<!-- TABLE OF CONTENTS -->
<details open="open">
  <summary><h2 style="display: inline-block">Table of Contents</h2></summary>
  <ol>
    <li>
      <a href="#about-the-project">About The Project</a>
      <ul>
        <li><a href="#built-with">Built With</a></li>
      </ul>
    </li>
    <li>
      <a href="#getting-started">Getting Started</a>
      <ul>
        <li><a href="#prerequisites">Prerequisites</a></li>
        <li><a href="#installation">Installation</a></li>
      </ul>
    </li>
    <li><a href="#usage">Usage</a></li>
    <li><a href="#roadmap">Roadmap</a></li>
	<li><a href="#resources">Resources</a></li>
    <li><a href="#contributing">Contributing</a></li>
    <li><a href="#license">License</a></li>
    <li><a href="#contact">Contact</a></li>
  </ol>
</details>



<!-- ABOUT THE PROJECT -->
## About the Project

  This library is a collection of packages useful in numerical
  applications, each big enough to be its own package, but too small
  to split out into a separate ASDF system. Included are:

  - `num=`, a comparison operator for floats
  - simple arithmetic functions, like `sum` and `l2norm`
  - element-wise operations for arrays and vectors
  - intervals
  - special matrices and shorthand for their input
  - sample statistics
  - Chebyshev polynomials
  - quadratures
  - univariate root finding


### Built With

* [anaphora](https://github.com/tokenrove/anaphora)
* [alexandria](https://gitlab.common-lisp.net/alexandria/alexandria)
* [array-operations](https://github.com/bendudson/array-operations)
* [select](https://github.com/Symbolics/select)
* [let-plus](https://github.com/sharplispers/let-plus)



<!-- GETTING STARTED -->
## Getting Started

To get a local copy up and running follow these steps:

### Prerequisites

An ANSI Common Lisp implementation. Developed and tested with
[SBCL](http://www.sbcl.org/) and
[CCL](https://github.com/Clozure/ccl).

### Installation

1. Clone the repository
   ```sh
   cd ~/quicklisp/local-projects &&
   git clone https://github.com/Lisp-Stat/numerical-utilities.git
   ```
2. Reset the ASDF source-registry to find the new system (from the REPL)
   ```lisp
   (asdf:clear-source-registry)
   ```
3. Load the system
   ```lisp
   (ql:quickload :num-utils)
   ```

### Documentation

The API documentation is in the `docs/` directory and is available in
emacs info format, PDF and HTML.  You can also [view the documentation
online](https://lisp-stat.github.io/numerical-utilities/).

<!-- USAGE EXAMPLES -->
## Usage

```lisp
(nu:median '(1 2 3 4 5 6 7 8 9 10)) ; -> 11/2
```

Note a [ratio](https://www.cs.cmu.edu/Groups/AI/html/cltl/clm/node18.html) is returned. This is a feature and Lisp-Stat leverages the
[Common Lisp numerical
tower](https://en.wikipedia.org/wiki/Numerical_tower).

For more examples, please refer to the [Reference Manual](https://lisp-stat/github.io/numerical-utilities/)


<!-- ROADMAP -->
## Roadmap

See the [open issues](https://github.com/lisp-stat/numerical-utilities/issues) for a list of proposed features (and known issues).


## Origin

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

## Status

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

## Known Issues

### Test Failures

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

### Implementation Support

Development is primarily done with SBCL and CCL on MS Windows. Issue
[papp15](https://github.com/tpapp/cl-num-utils/issues/15) reports that
generic function definitions may not work on other implementations.
Please report any such problems on the [Github issue
tracker](https://github.com/Lisp-Stat/numerical-utilities/issues).

### Symbol conflicts with alexandria

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

This is what the top-level `ls-user` package does.

## Resources

This system is part of the [Lisp-Stat](https://lisp-stat.dev/)
project; that should be your first stop for information. Also see the
[resources](https://lisp-stat.dev/resources) and
[community](https://lisp-stat.dev/community) pages for more
information.

<!-- CONTRIBUTING -->
## Contributing

Contributions are what make the open source community such an amazing place to be learn, inspire, and create. Any contributions you make are **greatly appreciated**. Please see [CONTRIBUTING.md](CONTRIBUTING.md) for details on our code of conduct, and the process for submitting pull requests.

<!-- LICENSE -->
## License

Distributed under the MS-PL License. See `LICENSE` for more information.



<!-- CONTACT -->
## Contact

Project Link: [https://github.com/lisp-stat/numerical-utilities](https://github.com/lisp-stat/numerical-utilities)



<!-- MARKDOWN LINKS & IMAGES -->
<!-- https://www.markdownguide.org/basic-syntax/#reference-style-links -->
[contributors-shield]: https://img.shields.io/github/contributors/lisp-stat/numerical-utilities.svg?style=for-the-badge
[contributors-url]: https://github.com/lisp-stat/numerical-utilities/graphs/contributors
[forks-shield]: https://img.shields.io/github/forks/lisp-stat/numerical-utilities.svg?style=for-the-badge
[forks-url]: https://github.com/lisp-stat/numerical-utilities/network/members
[stars-shield]: https://img.shields.io/github/stars/lisp-stat/numerical-utilities.svg?style=for-the-badge
[stars-url]: https://github.com/lisp-stat/numerical-utilities/stargazers
[issues-shield]: https://img.shields.io/github/issues/lisp-stat/numerical-utilities.svg?style=for-the-badge
[issues-url]: https://github.com/lisp-stat/numerical-utilities/issues
[license-shield]: https://img.shields.io/github/license/lisp-stat/numerical-utilities.svg?style=for-the-badge
[license-url]: https://github.com/lisp-stat/numerical-utilities/blob/master/LICENSE
[linkedin-shield]: https://img.shields.io/badge/-LinkedIn-black.svg?style=for-the-badge&logo=linkedin&colorB=555
[linkedin-url]: https://www.linkedin.com/company/symbolics/
