# Introduction #

This library is not designed to provide full access to every corner of
SVG functionality.  For that you are better off using a complete XML
library.  My purpose with this library is to make it easy to create
SVG files that don't need to do anything too exotic.  All the shapes
and grouping elements are provided.

The library aims to produce readable XML.  This inflates the size of
the resulting somewhat, especially if it contains deeply nested
elements.  The formatting depends on your Lisp correctly implementing
the **`~<`** directive.  See also [Formatting](Formatting.md).

For more info on SVG see the [1.1](http://www.w3.org/TR/SVG11/)
Specification.  W3 Schools also has a good
[tutorial](http://www.w3schools.com/svg/default.asp).

## Attribute Handling ##

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


## The Package ##

The package name is **`:cl-svg`** with a single nickname, **`:svg`**.

It depends on no other libraries.


## Conditions and Restarts ##

_condition_
**`missing-attributes`**
> Is signaled when the required attributes of an element are missing.
> If you're sure you don't need the missing attributes, you can skip
> the check with the macro **`WITHOUT-ATTRIBUTE-CHECK`**

There are no restarts.


## SVG Toplevel and Utilities ##

The **`svg-toplevel`** class (never accessed directly) handles XML
boilerplate as well as providing the outermost XML container for all
the graphical and support elements.


_function_
**`make-svg-toplevel`**  _`class &rest attributes` => svg-toplevel object_
> Creates the highest-level SVG container.  The first argument is a quoted
> class representing an SVG version.  At the moment the only option
> is **`svg-1.1-toplevel`** (as of May 2008 version 1.2 is still in the
> works).  The attributes are keyword/value pairs, of which you need at
> the very least to specify **`:height`** and **`:width`**.

> The `xmlns` and `xmlns:xlink` namespace attributes are included by
> default.  The property `id` is set to `toplevel` unless you specify
> something else.

```
(let ((scene (make-svg-toplevel 'svg-1.1-toplevel :height 300 :width 300)))
  (draw scene (:rect :x 5 :y 5 :height 30 :width 30))
...
```

_macro_
**`with-svg-to-file`** _(svg &rest svg-attributes) (filename &rest open-options) &body body_
> This simply wraps up SVG scene and file creation conveniently.  The
> `open-options` are passed to `WITH-OPEN-FILE`, though note that the
> macro provides `:direction :output` already.

```
(with-svg-to-file
    (scene 'svg-1.1-toplevel :height 20 :width 20)
    (#p"pale-blue-dot.svg" :if-exists :supersede)
  (draw scene (:circle :cx 10 :cy 10 :r 5) :fill "cyan"))
```

_generic function_
**`add-element`**  _`svg-element string`_
> If you need for some reason to add hand-rolled XML strings to any
> SVG element, toplevel or otherwise, use this generic function.

_generic function_
**`add-stylesheet`**  _`svg-toplevel stylesheet-url`_
> Adds an XML stylesheet URL to the SVG file.  CSS may also be added
> inline with the function **`style`**.

_generic function_
**`add-namespace`**  _`svg-toplevel prefix namespace-url`_
> Adds an XML namespace to the SVG file.  The prefix will have "xmlns:"
> attached to it automatically:

```
(add-namespace "clsvggui" "http://www.lingweenie.org/lisp/cl-svg-gui")
```

> will result in:

```
xmlns:clsvggui="http://www.lingweenie.org/lisp/cl-svg-gui"
```

_generic function_
**`xlink-href`** _svg-element => url-string_
> Extracts the `id` property from any element and generates an XML URL
> link to it, e.g., `url(#radialGradientOne)`.

_generic function_
**`stream-out`** _stream svg-element_
> This sends the XML representation of the SVG element to the output
> stream.


## Drawing Shape Elements ##

The syntax of the shape drawing macro is meant to highlight those
properties which must be defined for the shape to be rendered at all.
This is entirely an artifact of this library, since from the XML side
there is no special syntax that distinguishes these properties.

_macro_
**`draw`** _`canvas (:SHAPE { required properties }) {other properties}` => svg-element object_
> Adds a new shape to a canvas.  The shape keywords match the SVG
> element names exactly:

| **Shape** | **Required attributes** |
|:----------|:------------------------|
| `:line`    |  `:x1 :y1 :x2 :y2`  |
| `:rect`    |  `:x :y :height :width` |
| `:polyline` | `:points`   |
| `:polygon`  | `:points`   |
| `:ellipse`  | `:cx :cy :rx :ry` |
| `:circle`   | `:cx :cy :r`  |
| `:path`     | `:d`    |
| `:use`      | `:xlink-href`  |
| `:image`    | `:x :y :height :width :xlink-href` |

> This will signal a **`missing-attribute`** condition if any of the
> required attributes are missing, unless used within the body of
> the **`without-attribute-check`** macro.

> See [Paths](Paths.md) for utilities to describe path data readably.

_macro_
**`without-attribute-check`** _body_
> Suppresses required attribute checking for all drawing commands in
> the body.


## Definition Elements ##

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

_macro_
**`draw*`** _(:SHAPE { required properties }) {other properties} => svg-element object_
> This is just like `draw` except that you don't specify the canvas.
> The shape element will be placed in the group.  An error is signalled
> if you try to call this outside a group definition macro.

There are four grouping elements that this library puts into the
`<defs>` element.  Each must define an `:id` property.

_macro_
**`make-svg-symbol`** _scene (&rest attributes) &body shapes => svg-element_
> Creates an SVG _symbol_, a collection of shapes and groups that can be
> instantiated by `<use>` in the main SVG body.

```
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

_macro_
**`make-marker`** _scene (&rest attributes) &body shapes => svg-element_
> Creates line markers.

_macro_
**`make-pattern`** _scene (&rest attributes) &body shapes => svg-element_
> Creates fill patterns.

_macro_
**`make-mask`** _scene (&rest attributes) &body shapes => svg-element_
> Creates alpha masks.

## Fill Gradients ##

Fill gradient elements are also inserted into the `<defs`> element,
but do not provide the `draw*` macro, which makes no sense here.  The
body of both gradient definitions should contain calls to `stop` which
defines color stops.

_macro_
**`make-linear-gradient`** _scene (&rest attributes) &body color-stops_ => svg-element
> Creates linear gradient elements.  It must define the attributes
> `:id`, `:x1`, `:y2`, `:x2`, and `:y2`.

_macro_
**`make-radial-gradient`** _scene (&rest attributes) &body color-stops_ => svg-element
> Creates radial gradient elements.  It must define the attributes
> `:id`, `:cx`, `:cy` and `:r`.

_macro_
**`stop`** _&key color offset (opacity 1.0)_
> Creates color stops for radial gradients.  It is only defined within
> the macros that creat gradients.

Here's an example of a linear gradient being used:

```
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



## Other Grouping Elements ##

These grouping elements are inserted into the SVG in the order they
are defined.

_macro_
**`make-group`** _scene (&rest attributes) &body shapes_ => svg-element
> Creates an SVG `<g>` group.  This is especially useful for collecting
> shapes so that they can be manipulated as a group.

_macro_
**`make-foreign-object`** _scene (&rest attributes)_ => svg-element
> embeds non-SVG XML contents in the SVG file.  Be sure to consult the
> SVG [spec](http://www.w3.org/TR/SVG/extend.html#EmbeddingForeignObjects)
> before doing this, to understand the namespace issues.

_macro_
**`link`** _scene (&rest attributes) &body shapes_ => svg-element
> Makes the elements defined within it clickable links.  The attribute
> `:xlink-href` must be defined with the URL to go to on click.

_macro_
**`text`** _scene (&rest attributes) &body elements_ => svg-element
> Inserts text into the SVG document.  The `elements` must be either
> text strings or calls to `tspan`.

_macro_
**`tspan`** _scene (&rest attributes) text_ => svg-element
> Is used to add styling changes to a subset of the text in a `text`
> element.  For example:

```
(let* ((scene (make-svg-toplevel 'svg-1.1-toplevel :height 40 :width 250)))
  (draw scene (:rect :x 0 :y 0 :height 40 :width 250) :fill "#CCCCCC")
  (text scene (:x 25 :y 25)
    "Mouse over a "
    (tspan (:fill "orange" :font-weight "bold") "circle"))
  (draw scene (:circle :cx 200 :cy 20 :r 10) :fill "blue")
  (with-open-file (s #p"test.svg" :direction :output :if-exists :supersede)
    (stream-out s scene)))
```

> Produces:
> ![http://cl-svg.googlecode.com/svn/wiki/text-ex.png](http://cl-svg.googlecode.com/svn/wiki/text-ex.png)

## Element Modifiers ##

Any shape or group element, including the SVG toplevel, may be
modified by the following:

_function_
**`title`** _scene text_
> Adds a title to the element.  The title of the SVG toplevel will
> usually be displayed in your browser's title bar.

_function_
**`desc`** _scene text_
> Adds an SVG `desc` ("description") element to the element.

_function_
**`comment`** _scene text_
> Adds an XML comment to the element.

_function_
**`script`** _scene script_
> Adds inline Javascript, correctly wrapped in `CDATA`.

_function_
**`script-link`** _scene script-url_
> Adds a link to an external script URL.

_function_
**`style`** _scene css_
> Adds inline CSS to an element.

Geometric transformations in SVG are cumulative: you can cram as many
of them into the `transform` attribute as you like and they'll be
combined sensibly.

_macro_
**`transform`** _(transformations) element => svg-element_
> Adds transformations to the `transform` property.  A single
> transformation has a shorter notation available:

```
(transform (scale 33)
  (draw scene (:rect ...)))
```

> But:

```
(transform ((scale 33))
  (draw scene (:rect ...)))
(transform ((rotate 90 15 15) (scale 1.36) (skew-x 30))
  (draw scene (:rect ...)))
```

> The transformation functions are:

_function_
**`scale`** _sx &optional sy_
> Scales the image.  If no `sy` is given it takes the value of `sx`.

_function_
**`translate`** _tx &optional ty_
> Translation.  If no `ty` is given it takes the value of `tx`.

_function_
**`rotate`** _angle &optional (cx 0) (cy 0)_
> Rotates the image `angle` degrees about the point `cx, cy`.  By
> default the rotation is about the origin of the current coordinate
> system, rarely what you want by default.  You have to calculate the
> center of the shape yourself.

_function_
**`skew-x`** _angle_
> Skew in the X axis `angle` degrees.

_function_
**`skew-y`** _angle_
> Skew in the Y axis `angle` degrees.

_function_
**`matrix`** _a b c d e f_
> Roll your own transformation matrix. See
> [7.4](http://www.w3.org/TR/SVG/coords.html#TransformMatrixDefined)
> "Transform Matrix Defined."