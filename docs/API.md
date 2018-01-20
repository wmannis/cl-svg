# Introduction

This library is not designed to provide full access to every corner of
SVG functionality.  For that you are better off using a complete XML
library.  My purpose with this library is to make it easy to create
SVG files that don't need to do anything too exotic.  All the shapes
and grouping elements are provided.

The library aims to produce readable XML.  This inflates the size of
the resulting somewhat, especially if it contains deeply nested
elements.  The formatting depends on your Lisp correctly implementing
the **`~<`** directive.  See the Formatting section below.

For more info on SVG see the [1.1](http://www.w3.org/TR/SVG11/)
Specification.  W3 Schools also has a good
[tutorial](http://www.w3schools.com/svg/default.asp).

## Attribute Handling

The library also does some extra sanity-checking on attributes.  When
an SVG shape element is missing certain attributes SVG viewers will
simply ignore the element.  Most of the time I want to know if I've
missed a necessary attribute.  There are, however, situations where
missing attributes can be inherited from an enclosing SVG element.
The macro **`WITHOUT-ATTRIBUTE-CHECK`** allows you to tell the library
to not check for missing attributes when necessary.

The authors of the SVG spec were evidently unable to decide between
naming attributes with lisp-style `dashed-names` or using `camelCase`.
This library will happily accept attribute names like `:marker-units`
and will convert that to `markerUnits` in the SVG output.  Because the
SVG standard is a mish-mash in this regard the name conversion is
table-driven.  If I've missed any attributes you can also use strings
instead of keywords as attribute names.

Finally, within SVG itself it is common to refer to other elements.
This is usually handled by referring to the element's `id` property.
Searching for what you want using some other property requires
Javascript.  Using **`:id :generate`** in element creation will result
in a random string for the `id` property, which you may extract with
the generic function `xlink-href`.  See the gradient or symbol
examples for expected use patterns.


## The Package

The package name is **`:cl-svg`** with a single nickname, **`:svg`**.

It depends on no other libraries.


## Conditions and Restarts

*condition*  
**`missing-attributes`**  
  Is signaled when the required attributes of an element are missing.
  If you're sure you don't need the missing attributes, you can skip
  the check with the macro **`WITHOUT-ATTRIBUTE-CHECK`**

There are no restarts.


## SVG Toplevel and Utilities

The **`svg-toplevel`** class (never accessed directly) handles XML
boilerplate as well as providing the outermost XML container for all
the graphical and support elements.


*function*  
**`make-svg-toplevel`**  *`class &rest attributes` => svg-toplevel object*  
 Creates the highest-level SVG container.  The first argument is a quoted
 class representing an SVG version.  At the moment the only option 
 is **`svg-1.1-toplevel`** (as of May 2008 version 1.2 is still in the
 works).  The attributes are keyword/value pairs, of which you need at
 the very least to specify **`:height`** and **`:width`**.

 The `xmlns` and `xmlns:xlink` namespace attributes are included by
 default.  The property `id` is set to `toplevel` unless you specify
 something else.


```lisp
(let ((scene (make-svg-toplevel 'svg-1.1-toplevel :height 300 :width 300)))
  (draw scene (:rect :x 5 :y 5 :height 30 :width 30))
...
```

*macro*  
**`with-svg-to-file`** *(svg &rest svg-attributes) (filename &rest open-options) &body body*  
 This simply wraps up SVG scene and file creation conveniently.  The
 `open-options` are passed to `WITH-OPEN-FILE`, though note that the
 macro provides `:direction :output` already.


```lisp
(with-svg-to-file
    (scene 'svg-1.1-toplevel :height 20 :width 20)
    (#p"pale-blue-dot.svg" :if-exists :supersede)
  (draw scene (:circle :cx 10 :cy 10 :r 5) :fill "cyan"))
```

*generic function*  
**`add-element`**  *`svg-element string`*  
 If you need for some reason to add hand-rolled XML strings to any
 SVG element, toplevel or otherwise, use this generic function.

*generic function*  
**`add-stylesheet`**  *`svg-toplevel stylesheet-url`*  
 Adds an XML stylesheet URL to the SVG file.  CSS may also be added
 inline with the function **`style`**.

*generic function*  
**`add-namespace`**  *`svg-toplevel prefix namespace-url`*  
 Adds an XML namespace to the SVG file.  The prefix will have "xmlns:"
 attached to it automatically:


```lisp
(add-namespace "clsvggui" "http://www.lingweenie.org/lisp/cl-svg-gui")
```

 will result in:


```xml
xmlns:clsvggui="http://www.lingweenie.org/lisp/cl-svg-gui"
```

*generic function*  
**`xlink-href`** *svg-element => url-string*  
 Extracts the `id` property from any element and generates an XML URL
 link to it, e.g., `url(#radialGradientOne)`.

*generic function*  
**`stream-out`** *stream svg-element*  
 This sends the XML representation of the SVG element to the output
 stream.


## Drawing Shape Elements

The syntax of the shape drawing macro is meant to highlight those
properties which must be defined for the shape to be rendered at all.
This is entirely an artifact of this library, since from the XML side
there is no special syntax that distinguishes these properties.

*macro*  
**`draw`** *`canvas (:SHAPE { required properties }) {other properties}` => svg-element object*  
 Adds a new shape to a canvas.  The shape keywords match the SVG
 element names exactly:

  |Shape       | Required attributes |
  |----------- |---------------------|
  |`:line`     | `:x1 :y1 :x2 :y2` |
  |`:rect`     | `:x :y :height :width` |
  |`:polyline` |`:points` |
  |`:polygon`  | `:points` |
  |`:ellipse`  | `:cx :cy :rx :ry` |
  |`:circle`   |`:cx :cy :r` |
  |`:path`     |`:d` |
  |`:use`      | `:xlink-href` |
  |`:image`    | `:x :y :height :width :xlink-href`|

 This will signal a **`missing-attribute`** condition if any of the
 required attributes are missing, unless used within the body of 
 the **`without-attribute-check`** macro.

 See the Paths section below for utilities to describe path data readably.

*macro*  
**`without-attribute-check`** *body*  
 Suppresses required attribute checking for all drawing commands in
 the body.


## Definition Elements

Elements which are not displayed directly but which are instead
referenced by other elements should be stored in an SVG `<defs>`
element.  This isn't actually a requirement of the standard, but a
polite suggestion.  This library, however, will shepherd a bunch of
group elements directly into a `<defs>` element.

These defined elements are referred to by the elements that need them
by means of the `:xlink-href` property.  Within this library the
`:xlink-href` property will frequently be followed by a call to the
generic function `xlink-href`.  See the examples below.

All of the functions and macros that create grouping elements that
contain shape elements (i.e., not color gradients or text elements)
make a convenience macro available:

*macro*  
**`draw*`** *(:SHAPE { required properties }) {other properties} => svg-element object*  
 This is just like `draw` except that you don't specify the canvas.
 The shape element will be placed in the group.  An error is signalled
 if you try to call this outside a group definition macro.

There are four grouping elements that this library puts into the
`<defs>` element.  Each must define an `:id` property.

*macro*  
**`make-svg-symbol`** *scene (&rest attributes) &body shapes => svg-element*  
 Creates an SVG *symbol*, a collection of shapes and groups that can be
 instantiated by `<use>` in the main SVG body.


```lisp
(let* ((scene (make-svg-toplevel 'svg-1.1-toplevel :height 700 :width 700))
       ;;; draw a nice picture
       (columns (make-svg-symbol scene (:id :generate :view-box "0 0 20 20")
                  (draw* (:rect :x 5 :y 5 :height 10 :width 3) :opacity 1.0)
                  (draw* (:rect :x 10 :y 5 :height 10 :width 3) :opacity 0.7)
                  (draw* (:rect :x 15 :y 5 :height 10 :width 3) :opacity 0.3))))
  ;;; Next, instantiate like mad.
  (dotimes (i 400)
    (let ((size (+ 5 (random 95))))
      (draw scene (:use :xlink-href (xlink-href columns))
            :x (- (random 750) 25)
            :y (- (random 700) 25)
            :height size :width size
            :stroke "rgb(232, 229, 148)"
            :stroke-width 0.3
            :fill "purple")))
  (title scene "SVG test: using symbols")
  ...
```

*macro*  
**`make-marker`** *scene (&rest attributes) &body shapes => svg-element*  
 Creates line markers.

*macro*  
**`make-pattern`** *scene (&rest attributes) &body shapes => svg-element*  
 Creates fill patterns.

*macro*  
**`make-mask`** *scene (&rest attributes) &body shapes => svg-element*  
 Creates alpha masks.

## Fill Gradients

Fill gradient elements are also inserted into the `<defs`> element,
but do not provide the `draw*` macro, which makes no sense here.  The
body of both gradient definitions should contain calls to `stop` which
defines color stops.

*macro*  
**`make-linear-gradient`** *scene (&rest attributes) &body color-stops* => svg-element  
 Creates linear gradient elements.  It must define the attributes
 `:id`, `:x1`, `:y2`, `:x2`, and `:y2`.

*macro*  
**`make-radial-gradient`** *scene (&rest attributes) &body color-stops* => svg-element  
 Creates radial gradient elements.  It must define the attributes
 `:id`, `:cx`, `:cy` and `:r`.

*macro*  
**`stop`** *&key color offset (opacity 1.0)*  
 Creates color stops for radial gradients.  It is only defined within
 the macros that creat gradients.

Here's an example of a linear gradient being used:

```lisp
(let* ((scene (make-svg-toplevel 'svg-1.1-toplevel :height 700 :width 700))
       (lg1 (make-linear-gradient scene (:id :generate
                                         :x1 "0%" :y1 "0%" :x2 "100%" :y2 "100%")
            (stop :color "black" :offset "0%")
            (stop :color "purple" :offset "50%")
            (stop :color "red" :offset "100%"))))
  (title scene "SVG test: gradients")
  (draw scene (:rect :x 10 :y 10 :height 200 :width 200)
        :fill (xlink-href lg1))
```


## Other Grouping Elements

These grouping elements are inserted into the SVG in the order they
are defined.  

*macro*  
**`make-group`** *scene (&rest attributes) &body shapes* => svg-element  
 Creates an SVG `<g>` group.  This is especially useful for collecting
 shapes so that they can be manipulated as a group.

*macro*  
**`make-foreign-object`** *scene (&rest attributes)* => svg-element  
 embeds non-SVG XML contents in the SVG file.  Be sure to consult the
 SVG [spec](http://www.w3.org/TR/SVG/extend.html#EmbeddingForeignObjects)
 before doing this, to understand the namespace issues.

*macro*  
**`link`** *scene (&rest attributes) &body shapes* => svg-element  
 Makes the elements defined within it clickable links.  The attribute
 `:xlink-href` must be defined with the URL to go to on click.

*macro*  
**`text`** *scene (&rest attributes) &body elements* => svg-element  
 Inserts text into the SVG document.  The `elements` must be either
 text strings or calls to `tspan`.

*macro*  
**`tspan`** *scene (&rest attributes) text* => svg-element  
 Is used to add styling changes to a subset of the text in a `text`
 element.  For example:


```lisp
(let* ((scene (make-svg-toplevel 'svg-1.1-toplevel :height 40 :width 250)))
  (draw scene (:rect :x 0 :y 0 :height 40 :width 250) :fill "#CCCCCC")
  (text scene (:x 25 :y 25)
    "Mouse over a "
    (tspan (:fill "orange" :font-weight "bold") "circle"))
  (draw scene (:circle :cx 200 :cy 20 :r 10) :fill "blue")
  (with-open-file (s #p"test.svg" :direction :output :if-exists :supersede)
    (stream-out s scene)))
```

 Produces:
![Text Example](https://github.com/wmannis/cl-svg/blob/master/docs/text-ex.png)

## Element Modifiers

Any shape or group element, including the SVG toplevel, may be
modified by the following:

*function*  
**`title`** *scene text*  
 Adds a title to the element.  The title of the SVG toplevel will
 usually be displayed in your browser's title bar.

*function*  
**`desc`** *scene text*  
 Adds an SVG `desc` ("description") element to the element.

*function*  
**`comment`** *scene text*  
 Adds an XML comment to the element.

*function*  
**`script`** *scene script*  
 Adds inline Javascript, correctly wrapped in `CDATA`.

*function*  
**`script-link`** *scene script-url*  
 Adds a link to an external script URL.

*function*  
**`style`** *scene css*  
 Adds inline CSS to an element.

Geometric transformations in SVG are cumulative: you can cram as many
of them into the `transform` attribute as you like and they'll be
combined sensibly.

*macro*  
**`transform`** *(transformations) element => svg-element*  
 Adds transformations to the `transform` property.  A single
 transformation has a shorter notation available:

```lisp
(transform (scale 33)
  (draw scene (:rect ...)))
```

 But:


```lisp
(transform ((scale 33))
  (draw scene (:rect ...)))
(transform ((rotate 90 15 15) (scale 1.36) (skew-x 30))
  (draw scene (:rect ...)))
```

 The transformation functions are:

*function*  
**`scale`** *sx &optional sy*  
 Scales the image.  If no `sy` is given it takes the value of `sx`.

*function*  
**`translate`** *tx &optional ty*  
 Translation.  If no `ty` is given it takes the value of `tx`.

*function*  
**`rotate`** *angle &optional (cx 0) (cy 0)*  
 Rotates the image `angle` degrees about the point `cx, cy`.  By
 default the rotation is about the origin of the current coordinate
 system, rarely what you want by default.  You have to calculate the
 center of the shape yourself.

*function*  
**`skew-x`** *angle*  
 Skew in the X axis `angle` degrees.

*function*  
**`skew-y`** *angle*  
 Skew in the Y axis `angle` degrees.

*function*  
**`matrix`** *a b c d e f*  
 Roll your own transformation matrix. See
 [7.4](http://www.w3.org/TR/SVG/coords.html#TransformMatrixDefined)
 "Transform Matrix Defined."


## The Path API

CL-SVG provides a simple API to produce SVG path
[data](http://www.w3.org/TR/SVG/paths.html#PathData) more readably.

*function*  
**`make-path`** *=> path-string*  
 Makes an adjustable string into which path commands are pushed.

*macro*  
**`with-path`** *path &body path-commands*  
 Adds path data to the `path`.  You can only use the path data
 commands below within the body.

*macro*  
**`path`** *&body path-commands => path-data-string*  
 This is a convenience macro equivalent to calling `make-path`, adding
 path data with `WITH-PATH` then returning the path data string.

All the path data commands except `close-path` have two versions, one
which uses absolute coordinates for points and one which uses
coordinates relative to the last point produced.  The function names
for the relative version end in `-r`.

*function*  
{ **`move-to`** | **`move-to-r`** } *x y => path-string*  
 Move to the coordinates without drawing anything.

*function*  
{ **`line-to`** | **`line-to-r`** } *x y => path-string*  
 Draw a line to the coordinates.

*function*  
{ **`horizontal-to`** | **`horizontal-to-r`** } *x => path-string*  
 Draw a horizontal line to `x`.

*function*  
{ **`vertical-to`** | **`vertical-to-r`** } *y => path-string*  
 Draw a vertical line to `y`.

*function*  
{ **`curve-to`** | **`curve-to-r`** } *control-x1 control-y1 control-x2 control-y2 x y => path-string*  
 Draw a cubic Bézier curve to `x, y` using `control-x1, control-y1` as
 the control point at the start of the curve and `control-x2,
 control-y2` at the end.

*function*  
{ **`smooth-curve-to`** | **`smooth-curve-to-r`** } *control-x2 control-y2 x y => path-string*  
 Same as `curve-to` but the first control point is the reflection
 of the second control point of a previous `curve-to` command.

*function*  
{ **`quadratic-curve-to`** | **`quadratic-curve-to-r`** } *control-x1 control-y1 x y => path-string*  
 Draw a quadratic Bézier curve to `x, y` using `control-x1, control-y1` as
 the control point.

*function*  
{ **`smooth-quadratic-curve-to`** | **`smooth-quadratic-curve-to-r`** } *x y => path-string*  
 Same as `quadratic-curve-to` but the control control point is the reflection
 of the second control point of a previous `quadratic-curve-to` command.

*function*  
{ **`arc-to` | `arc-to-r`** } *rx ry x-rotation large-arc-flag sweep-flag x y => path-string*  
 Draws an eliptical arc from the current point to `x, y`.  The size is
 determined by the two radii, `rx` and `ry`.  `x-axis-rotation`
 indicates the rotation of the arc relative to the current coordinate
 system.  Both `large-arc-flag` and `sweep-flag` may have the values
 '0' or '1'.

*function*  
**`close-path`** *=> path-string*  
 Closes the current subpath by drawing a line from the current point
 to the starting point of the subpath (*i.e.*, the closes previous
 `move-to`).

An example:

```lisp
(let* ((scene (make-svg-toplevel 'svg-1.1-toplevel :height 700 :width 700
                                 :viewbox "0 0 700 700")))
  (title scene "Path test")
  (draw scene (:path :d (path
                          (move-to 100 400)
                          (line-to-r 50 -25)
                          (arc-to-r 25 25 -30 0 1 50 -25)
                          (line-to-r 50 -25)
                          (arc-to-r 25 50 -30 0 1 50 -25)
                          (line-to-r 50 -25)
                          (arc-to-r 25 75 -30 0 1 50 -25)
                          (line-to-r 50 -25)
                          (arc-to-r 25 100 -30 0 1 50 -25)
                          (line-to-r 50 -25)
                          (vertical-to-r 50)))
               :fill "none" :stroke "blue" :stroke-width 5)
  (with-open-file (s #p"test.svg" :direction :output :if-exists :supersede)
    (stream-out s scene)))
```

Produces:

![Path example](https://github.com/wmannis/cl-svg/blob/master/docs/path-ex.png)


## How CL-SVG produces SVG's XML

The XML required by SVG is fairly trivial.  As a result CL-SVG doesn't
use one of the several more powerful XML libraries available for
Common Lisp, but instead spits out XML directly itself.  The output is
handled by `format-xml.lisp` and relies heavily on some of the more
exotic options offered by `FORMAT`.

CL-SVG tries hard to produce XML readable by you and your editor.  As
a consequence there's a *lot* of extra whitespace in the resulting SVG
files.  There is one special variable to get some control of this.

*special variable*  
**`*indent-spacing*`**  
  Controls how many spaces to indent at each level of XML nesting.  It
  defaults to 2.  Set it to zero for no indentation.

This simple code:

```lisp
(with-svg-to-file
    (scene 'svg-1.1-toplevel :height 20 :width 20)
    (#p"pale-blue-dot.svg" :if-exists :supersede)
  (draw scene (:circle :cx 10 :cy 10 :r 5) :fill "cyan"))
```

results in this XML:

```xml
<?xml version="1.0" standalone="no"?>
<!DOCTYPE svg PUBLIC "-//W3C//DTD SVG 1.1//EN" 
  "http://www.w3.org/Graphics/SVG/1.1/DTD/svg11.dtd">
<svg width="20" height="20" version="1.1" id="toplevel"
    xmlns="http://www.w3.org/2000/svg"
    xmlns:xlink="http://www.w3.org/1999/xlink">
  <circle cx="10" cy="10" r="5" fill="cyan"/>
</svg>
```

You can adjust the right margin setting using the CL special
variable `**PRINT-RIGHT-MARGIN**`, though certain bits of boilerplate
have newlines hardcoded.

```lisp
(let ((**print-right-margin** 200))
       (with-svg-to-file
           (scene 'svg-1.1-toplevel :height 20 :width 20)
           (#p"pale-blue-dot.svg" :if-exists :supersede)
         (draw scene (:circle :cx 10 :cy 10 :r 5) :fill "cyan")))
```

Results in:


```xml
<?xml version="1.0" standalone="no"?>
<!DOCTYPE svg PUBLIC "-//W3C//DTD SVG 1.1//EN" 
  "http://www.w3.org/Graphics/SVG/1.1/DTD/svg11.dtd">
<svg width="20" height="20" version="1.1" id="toplevel" xmlns="http://www.w3.org/2000/svg" xmlns:xlink="http://www.w3.org/1999/xlink">
  <circle cx="10" cy="10" r="5" fill="cyan"/>
</svg>
```

Finally, the pathing utilities insert newlines more often than is
probably necessary, but again I wanted to balance readability with a
suggestion in the SVG spec that lines shouldn't be longer than 255
characters.  The API currently offers no control over this.

### Floating Point Precision

By default this library restricts floating point precision to two
digits after the decimal point, again to keep down the size of the
resulting file.  Due to how some browsers cope with floating point
representations of zero (`0.0` and the like), this library will print
floating point zero as the integer `0`.  A value very close to zero
will be rounded to prevent representation as something like `0.00`
when formatted with a precision of two.

*function*
**`set-float-precision`** *`precision`*
 This sets the precision, and calculates a reasonable epsilon to test
 for near-zero values that might break some browsers' SVG number
 parsing.  It will give a warning if you chose a precision great than
 six (which, again, some browsers don't care for).

*special variable*  
**`*float-format-precision*`**  
  Controls how many digits after the decimal place to print in SVG
  element attributes.  It defaults to 2.  This variable remains open
  for backwards compatibility, but it is best to change this with the
  **`set-float-precision`** function above.
