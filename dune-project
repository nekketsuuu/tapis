(lang dune 1.11)
(name tapis)
(version 1.0.0)
(maintainers "Takuma Ishikawa <nekketsuuu@users.noreply.github.com>")
(authors "Takuma Ishikawa")
(license "MIT License")
(homepage "https://github.com/nekketsuuu/tapis")
(bug_reports "https://github.com/nekketsuuu/tapis/issues")
(source (github nekketsuuu/tapis))
(generate_opam_files true)

(package
  (name tapis)
  (synopsis "Termination analyzer for π-calculus processes")
  (description
    "\| *Tapis* is a tool to analyze termination of a π-calculus process
    "\| by using a reduction from termination of the process to 
    "\| termination of a sequential program.
    "\| Since termination of the program can be analyzed by
    "\| an existing termination analyzer for sequential programs,
    "\| we can utilize its advanced techniques to analyze
    "\| termination of π-calculus processes.
    "\| 
    "\| This tool is based on my theses.
    "\| See my graduation thesis and master thesis (in Japanese) for details of the background theory.
    ))

(using fmt 1.2)
(using menhir 2.0)
