# ctest

This simple program can solve language C-Tests tests after being trained on a corpus of words. C-Tests are often given to learners of foreign languages and may look like this:

The dog b_____ the postman.
Most children agree, Pizza is a d______ dish.

Filling in the gaps, sometimes with a one letter hint, is a task well suited for machine learning algorithms. Moreover, the solving accuracy of the program can be used to gauge the difficulty of the C-test. The ctest program achieves this rather simply, using a mere bigram HMM internally as a language model.

#Formats

ctest reads the tests from standard input. One sentence per line, underscores indicate places to fill in. See the ctest.txt example in data/ for more.
The corpus consists of sentences, which are made up of one word/tag per line, sentences themselves being separated by two newlines. A line containing a word must be followed by a line containing the corresponding tag. A line containing a tag must be followed by either another line containing a word or two newlines, indicating the end of the sentence. There is no newline at the end of the file.
For an example see data/corpus.txt
Note that the supplied corpus is way too small. The program was originally run on a much larger excerpt of the tiger corpus, which is not included due to fear of licensing issues.

#Usage

ctestTakes ctests from standard input and prints solution to standard output.
Usage: ctest [-e GOLDSTANDARD] CORPUSFILE

Example:
ctest data/tiger < data/ctest.txt

Options
  -e - Evaluate; do not print solutions to stdout.
    Instead prints out performance statistics and a list of errorpairs (goldstandard and mistaken guess)
