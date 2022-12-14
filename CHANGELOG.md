# Revision history for text-compression

## 0.1.0.0 -- 2022-10-31

* First version. Released on an unsuspecting world.

## 0.1.0.1 -- 2022-10-31

* Updated documentation.

## 0.1.0.2 -- 2022-10-31

* Updated documentation.

## 0.1.0.3 -- 2022-10-31

* Updated documentation.

## 0.1.0.4 -- 2022-10-31

* Updated documentation.

## 0.1.0.5 -- 2022-11-01

* Removed requirement for avoiding input with the '$' character in both the toBWT and fromBWT functions (both toBWT and fromBWT functions are now polymorphic).

## 0.1.0.6 -- 2022-11-05

* Added helper functions to ease conversion of ByteStrings and Text to and from the BWT type.

## 0.1.0.7 -- 2022-11-07

* Added Run-length encoding (RLE) implementation.

## 0.1.0.8 -- 2022-11-11

* Optimized BWT and RLE implementations by switching out sequences with vectors.

## 0.1.0.9 -- 2022-11-12

* Switched back to sequences for maintainability (for now).

## 0.1.0.10 -- 2022-11-15

* Added Move-to-front transform (MTF) implementation.

## 0.1.0.11 -- 2022-11-16

* Adding more documentation for RLE and MTF implementations.

## 0.1.0.12 -- 2022-11-23

* Added Full-text Minute-space index (FM-index) implementation.

## 0.1.0.13 -- 2022-11-23

* Bug fixes to FM-index STRef functions.

## 0.1.0.14 -- 2022-11-23

* Added example FM-index output.

## 0.1.0.15 -- 2022-11-23

* Added C[c] table for FM-index, and reworked FM-index implementation to include the C[c] table.

## 0.1.0.16 -- 2022-11-28

* Added FM-index count operation and updated FM-index example text.
* Added contents to all internal modules.

## 0.1.0.17 -- 2022-12-02

* Added FM-index locate operation.

## 0.1.0.18 -- 2022-12-02

* Added additional FM-index functionality for the locate operation.

## 0.1.0.19 -- 2022-12-03

* Reworked FM-index functions.

## 0.1.0.20 -- 2022-12-03

* Added missing date to 0.1.0.19 entry in changelog.

## 0.1.0.21 -- 2022-12-03

* Optimized locate and count functions.

## 0.1.0.22 -- 2022-12-07

* Added parallel implementation for locate and count functions. Renamed serial version by adding 'S' at the end.

## 0.1.0.23 -- 2022-12-07

* Fixed grammar/spelling in FM-index documentation.

## 0.1.0.24 -- 2022-12-08

* Optimized parallel implementation for locate and count FM-index functions.

## 0.1.0.25 -- 2022-12-09

* Optimized FMIndexB and FMIndexT data types to include the SuffixArray of the respective input.
