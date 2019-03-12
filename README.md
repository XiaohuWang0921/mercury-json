# Mercury/JSON - A simple JSON module for Mercury

## Overview

A simple module for handling JSON data.

## Features

* You can create a JSON data structure or extract contents from it in Mercury.
* You can convert a JSON data structure to its string representation or write it into a file.
* You can parse a string into a JSON data structure.

## Warning

This module uses `list` and `assoc_list` for storing JSON arrays and objects, instead of the more efficient `array` and `hash_table`. The reason for this is that the unique modes in Mercury are really tricky to deal with and I am not yet very experienced in this. It somehow did not compile if I used these unique data structures. There might also be other performance issues, since I have only tested it on simple tasks.

## Contributing

As stated in the **Warning** section, this module is currently far from stable and there might also be new features added in the future. Therefore, any contribution or idea is welcome. The **TODO** section contains features I intend to add, which you might help me with. You can also report bugs in the **Issues** section of this repository.

## TODO

* Better documentation for the code.
* Deterministic versions of the `as_*` functions, which throw an exception if the data stored in the JSON structure is of a different type than the one to be extracted.
* Functions for direct access of elements from a JSON array or object.
* Support for parsing more advanced integer literals (e.g. binary, octal and hexadecimal literals).
* A more convenient way to specify the output format of JSON strings (e.g. by introducing a default format instead of having to specify it every single time).