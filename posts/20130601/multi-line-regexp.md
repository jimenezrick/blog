% Multi-line regexps: searching on plain text
% Ricardo Catalinas Jiménez
% 10 June 2013


  I always like to keep everything in plain text but sometimes it is a
pain in the ass the concept that content and presentation is mixed in
these files. If you keep lines wrapped to be under some column width,
that implies that using a regular expression to locate some group of
words is gonna be problematic, as regexp also have the pervasive concept
of line.

  That reminds me that I should read more about [structural regular
expressions](http://doc.cat-v.org/bell_labs/structural_regexps/) which
precisely try to fix this and other issues.

  So Vim to the rescue it is. GNU grep features `--perl-regexp` but the
doc states that it is experimental. On Vim, we have the pattern `\_`
which extends any character set with the new-line. Thus with the
whitespace character set `\s` we can search for a contiguos pair of
words which could occur in different consecutive lines:

	/hello\_s*world/

  “I define UNIX as 30 definitions of regular expressions living under
one roof.” —Donald Knuth
