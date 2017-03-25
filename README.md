# pdfinfo library for OCaml

A wrapper around the pdfinfo command (for collecting PDF file info).

```
match Pdfinfo.of_file ~path:"sample.pdf" () with
| Ok info ->
    ...
```

## Install

```
opam pin add pdfinfo https://github.com/kkazuo/pdfinfo.git
```
