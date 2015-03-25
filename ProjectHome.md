![http://cl-svg.googlecode.com/svn/wiki/teenyroots.png](http://cl-svg.googlecode.com/svn/wiki/teenyroots.png)

This is a basic library for producing SVG files.  It makes simple things easy (well, easier), but makes no attempt to offer the full freedom of SVG, for which you'd be better off using an XML library.

The [documentation](API_Basics.md) is done (though I'll gladly accept suggestions for improvements).

Current release (0.02) on July 3, 2008.  Known to work with recent versions of SBCL and OpenMCL on Linux, OSX and Solaris.

Changes in 0.02:
  * `<foreignObject>`s
  * `add-namespace`
  * one bug fix (which causes compiler grouching, not actual errors)
  * ghastly bug in `TRANSFORM` fixed