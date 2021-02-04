
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
  Numerical utilities for statistical computing.

    <br />
    <a href="https://github.com/lisp-stat/numerical-utilities/docs/numerical-utilities.md"><strong>Explore the docs »</strong></a>
    <br />
    <br />
    <a href="https://github.com/lisp-stat/numerical-utilities">View Demo</a>
    ·
    <a href="https://github.com/lisp-stat/numerical-utilities/issues">Report Bug</a>
    ·
    <a href="https://github.com/lisp-stat/numerical-utilities/issues">Request Feature</a>
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
## About The Project

  This library is a collection of packages useful in numerical
  applications, each big enough to be its own package, but too small
  to split out into a separate ASDF system. Included are:

  - =num==, a comparison operator for floats
  - simple arithmetic functions, like =sum= and =l2norm=
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

To get a local copy up and running follow these steps.

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
   ```
   (ql:quickload :num-utils)
   ```

<!-- USAGE EXAMPLES -->
## Usage

Use this space to show useful examples of how a project can be used. Additional screenshots, code examples and demos work well in this space. You may also link to more resources.

_For more examples, please refer to the [Documentation](https://lisp-stat.dev/)_



<!-- ROADMAP -->
## Roadmap

See the [open issues](https://github.com/lisp-stat/numerical-utilities/issues) for a list of proposed features (and known issues).

## Resources

See the Lisp-Stat [resources](https://lisp-stat.dev/resources) and
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
[contributors-shield]: https://img.shields.io/github/contributors/lisp-stat/repo.svg?style=for-the-badge
[contributors-url]: https://github.com/lisp-stat/repo/graphs/contributors
[forks-shield]: https://img.shields.io/github/forks/lisp-stat/repo.svg?style=for-the-badge
[forks-url]: https://github.com/lisp-stat/repo/network/members
[stars-shield]: https://img.shields.io/github/stars/lisp-stat/repo.svg?style=for-the-badge
[stars-url]: https://github.com/lisp-stat/repo/stargazers
[issues-shield]: https://img.shields.io/github/issues/lisp-stat/repo.svg?style=for-the-badge
[issues-url]: https://github.com/lisp-stat/repo/issues
[license-shield]: https://img.shields.io/github/license/lisp-stat/repo.svg?style=for-the-badge
[license-url]: https://github.com/lisp-stat/repo/blob/master/LICENSE.txt
[linkedin-shield]: https://img.shields.io/badge/-LinkedIn-black.svg?style=for-the-badge&logo=linkedin&colorB=555
[linkedin-url]: https://linkedin.com/in/lisp-stat
