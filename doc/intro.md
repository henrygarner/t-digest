# Introduction to t-digest

The t-digest is a new data structure for accurate on-line accumulation of rank-based statistics such as quantiles and trimmed means. The t-digest algorithm is a streaming algorithm making it a perfect fit for use with Clojure's transducers.

The t-digest was introduced in [this paper](https://github.com/tdunning/t-digest/blob/master/docs/t-digest-paper/histo.pdf).
