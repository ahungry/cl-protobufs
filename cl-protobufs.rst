.. raw:: LaTeX

   \setlength{\parindent}{0pt}
   \setlength{\parskip}{6pt plus 2pt minus 1pt}


=========================
Protobufs for Common Lisp
=========================


:Description: Protobufs for Common Lisp
:Author: Scott McKay <swm@google.com>
:Date: $Date: 2012-05-07 14:58:00 -0500 (Mon, 7 May 2012) $

.. contents::
..
    1  Introduction
      1.1  Implementation notes
      1.2  Model classes
    2  Defining a Protobufs schema
      2.1  .proto file to Lisp conversion
      2.2  CLOS classes to .proto conversion
      2.3  Using .proto files directly
      2.4  Using the Protobufs macros
        2.4.1  Protobufs types
        2.4.2  Protobufs service stubs
    3  Serializing and deserializing
      3.1  Wire format
      3.2  Text format
    4  Other API functions
      4.1 Extensions functions
      4.2 Initialization functions
      4.3 Python compatibility functions


Introduction
============

The Common Lisp Protobufs library provides a fully in-Lisp way for
Common Lisp programs to use existing, or define new Protobufs
"schemas", and serialize and deserialize objects to and from the
Protobufs wire and text formats.

To use it, first load the ASDF declaration file ``protobufs/cl-protobufs.asd``
and then use ASDF to load the library named ``:cl-protobufs``.


Implementation notes
--------------------

The Protobufs library defines a set of model classes that describes a
protobufs "schema" (i.e., one .proto file). These classes describe a
schema, its options, enums and enum values, messages and fields, and
services and methods.

Unlike the 'protobuf' library described at ``http://common-lisp.net/project/protobuf``,
this implementation is entirely written in Common Lisp. It provides
some things that the 'protobuf' library does not, in particular,
the ability to define Protobufs schemas entirely within Lisp and
the ability to "export" a set of CLOS classes as a Protobufs schema.

The library provides the means to convert several kinds of inputs into
the Protobufs models, including:

 - Parse an existing .proto file into a set of model objects.
 - Convert a set of related CLOS classes into a set of model objects.
 - Compile a ``proto:define-schema`` macro call into a set of model objects.

It also provides two ways to convert the model objects into outputs:

 - Print a set of model objects using the standard Protobufs v2 syntax.
 - Print a set of model objects using the Common Lisp syntax, defined below.

There are two formats for serialization and deserialization:

 - The wire format, which is compact and fast.
 - The text format, which is human readable.

Finally, there is a validator that takes an old version of a Protobufs
schema and a new version of the same schema and generates a set of
warnings that describes any incompatibilities between the old schema and
the new one.


Model classes
-------------

::

  proto:protobuf-schema                                         [Class]

The class the represents a Protobufs schema, i.e., one .proto file.
It has slots for the name, options, enums, messages and services. The
name is equal to the name of the .proto file, without the file type.

::

  proto:protobuf-option                                         [Class]

The class that represents a Protobufs option.
It has slots for a key and a value.

::

  proto:protobuf-enum                                           [Class]

The class that represents a Protobufs enum type.
It has slots for the enum name and its values.

::

  proto:protobuf-enum-value                                     [Class]

The class that represents one value in a Protobufs enum type.
It has slots for the value name and the value index.

::

  proto:protobuf-message                                        [Class]

The class that represents a Protobufs message.
It has slots for the name, options, nested enums and messages, and fields.

::

  proto:protobuf-field                                          [Class]

The class that represents one field in a Protobufs message.
It has slots for the name, type, index and options.

::

  proto:protobuf-service                                        [Class]

The class that represents a Protobufs service.
It has slots for the name, options and methods.

::

  proto:protobuf-method                                         [Class]

The class that represents one method description in a Protobufs service.
It has slots for the name, input type, output type and options.


Defining a Protobufs schema
===========================

There are several ways to define a Protobufs schema: convert a .proto
file to a .lisp source file and then use the Lisp file; convert a set
of Lisp classes to a Protobufs model, and then use either the .lisp or
the .proto representation of the model; use a .proto file directly in
an ASDF system; or use the Protobufs macros in a Lisp source file.


.proto file to Lisp conversion
------------------------------

If you have an existing .proto source file that you would like to
convert to Lisp classes (more precisely, to the macros defined by the
Protobufs library), you can use ``proto:parse-schema-from-file`` to
read the .proto file and then use ``proto:write-schema`` to write a
new .lisp file. (This is what that ASDF module type ``:protobuf-file``
does.)

::

  proto:parse-schema-from-file (pathname                        [Function]
                                &key name class conc-name)

Parses the contents of the file given by *pathname*, and returns the
Protobufs model (a set object objects rooted at ``proto:protobuf-schema``)
corresponding to the parsed file. The name of the Protobufs schema is
generated automatically from the file name.

*name*, *class* and *conc-name* are as for ``proto:parse-schema-from-stream``.
The defaults for *name* and *class* are produced by taking the name of the
file and generating a name string and a class name symbol.

::

  proto:parse-schema-from-stream (stream                        [Function]
                                  &key name class conc-name)

Parses the contents of the stream *stream*, and returns the Protobufs
schema corresponding to the parsed file.

If *name* is supplied, it gives the Protobufs name (a string) for the
schema. If *class* is supplied, it gives the Lisp name (a symbol). These
are only used for display purposes.

*conc-name* is the default "conc name" to use for all of the messages
in the file. The default is "", which has the effect of giving eponymous
slot accessors to all of the classes generating during the import process.

::

  proto:write-schema (schema &key stream type)                  [Function]

Pretty-prints the Protobufs schema *schema* onto the stream *stream*,
which defaults to ``*standard-output*``.

*type* can be either ``:proto`` or ``:lisp``.


CLOS classes to .proto conversion
---------------------------------

If you have an existing set of CLOS classes that you would like to
convert to a Protobufs schema, you can use ``proto:generate-schema-from-classes``.

Note that the Protobufs schema is an *approximation* of a good schema.
You should review it and, if necessary, change it (and probably the Lisp
classes as well) until you have a good Protobufs schema definition.

::

  proto:generate-schema-for-classes (classes                    [Function]
                                     &key name package lisp-package
                                          slot-filter type-filter enum-filter value-filter
                                          alias-existing-classes)

Given a list of class names *classes*, this generates a Protobufs schema
for the classes, generating any necessary enum types that correspond to
Lisp ``member`` types. The return value is the model, rooted at an instance
of ``proto:protobuf-schema``.

*name* and *package* can be supplied to give the Protobufs name and
package. *lisp-package* can be supplied to give the name of the Lisp
package, if it is different from *package*. (Note that you should
still use ``in-package`` at the top of .lisp files, and it should
match the value of *lisp-package*.)

*slot-filter*, *type-filter*, *enum-filter* and *value-filter* are
filtering functions that can be used to weed out things from the Lisp
classes that should not be included in the Protobufs schema.

*slot-filter* is a function of two arguments, a list of all the slots
in the class and the slot currently being processed, and should return
true if the slot is to be kept or ``nil`` if it to be discarded. For
example, if there are internal implementation slots in a class that
need not appear in the Protobufs description, it can be used to filter
them out.

*type-filter* is a function of one argument, the type (of a slot); it
should return a "transformed" type if any is required. For example,
complex ``and`` and ``or`` types can't be directly represented in
Protobufs; this can be used to substitute something workable.

*enum-filter* is a function of one argument, a list of all the values
of a ``member`` type; it should return the transformed values. For
example, there maybe be some enumeration values that don't make sense;
they can be discarded by the filter.

*value-filter* is a function of one argument, the value of a slot
initform. It should transform the value into a scalar value suitable
for Protobufs.

If *alias-existing-classes* is true (the default), the generated
code will include ``:alias-for`` so that there will be no clash
with the existing Lisp class.

::

  proto:write-schema-for-classes (classes                       [Function]
                                  &key stream type name package lisp-package
                                       slot-filter type-filter enum-filter value-filter
                                       alias-existing-classes)

Given a list of class names *classes*, this generates a Protobufs schema
for the classes, generating enum types as necessary, and then
pretty-prints the result onto *stream*. *type* can be either ``:proto``
(the default) or ``:lisp``; it controls which format the generated
code will be printed in. The return value is the model, rooted at an
instance of ``proto:protobuf-schema``.

*name* and *package* can be supplied to give the Protobufs name and
package. *lisp-package* can be supplied to give the name of the Lisp
package, if it is different from *package*.

*slot-filter*, *type-filter*, *enum-filter* and *value-filter* are
as for ``proto:generate-schema-for-classes``.

*alias-existing-classes* is as for ``proto:generate-schema-for-classes``.


Using .proto files directly
---------------------------

In addition to using the tools described above to convert between
.proto files and .lisp files, you can also use .proto files directly
in ASDF systems. Just use the ASDF module type ``:protobuf-file`` in
your system, and compile and load the system in the usual way. This
will create both the Protobufs model and the Lisp classes that
correspond to the Protobufs messages. (Note that it will also leave a
.lisp file having the same name as the .proto file in the file
system.)


Using the Protobufs macros
--------------------------

You can define a Protobufs schema entirely within Lisp by using the
following macros. For example::

  (proto:define-schema color-wheel
      (:package com.google.colorwheel
       :lisp-package color-wheel)
    (proto:define-message color-wheel
        (:conc-name color-wheel-)
      (name   :type string)
      (colors :type (proto:list-of color) :default ()))
    (proto:define-message color
        (:conc-name color-)
      (name    :type (or string null))
      (r-value :type integer)
      (g-value :type integer)
      (b-value :type integer)
      (proto:define-extension 1000 max))
    (proto:define-extend color ()
      ((opacity 1000) :type (or null integer)))
    (proto:define-message get-color-request ()
      (wheel :type color-wheel)
      (name  :type string))
    (proto:define-message add-color-request ()
      (wheel :type color-wheel)
      (color :type color))
    (proto:define-service color-wheel ()
      (get-color (get-color-request color)
        :options ("deadline" "1.0"))
      (add-color (add-color-request color)
        :options ("deadline" "1.0"))))

This will create the Protobufs model objects, Lisp classes and enum
types that correspond to the model. The .proto file of the same schema
looks something like this::

  syntax = "proto2";

  package com.google.colorwheel;

  option (lisp_package) = "color-wheel";

  message ColorWheel {
    required string name = 1;
    repeated Color colors = 2;
  }

  message Color {
    optional string name = 1;
    required int64 rValue = 2;
    required int64 gValue = 3;
    required int64 bValue = 4;
    extensions 1000 to max;
  }

  extend Color {
    optional int64 opacity = 1000;
  }

  message GetColorRequest {
    required ColorWheel wheel = 1;
    required string name = 2;
  }

  message AddColorRequest {
    required ColorWheel wheel = 1;
    required Color color = 2;
  }

  service ColorWheel {
    rpc GetColor (GetColorRequest) returns (Color) {
      option deadline = "1.0";
    }
    rpc AddColor (AddColorRequest) returns (Color) {
      option deadline = "1.0";
    }
  }

Note that Lisp types ``(or null <T>)`` turn into optional fields,
and Lisp types ``(proto:list-of <T>)`` and ``(proto:vector-of <T>)``
turn into repeated fields representing by lists or vectors,
respectively.

::

  proto:define-schema (type (&key name syntax import            [Macro]
                                  package lisp-package
                                  optimize options documentation)
                       &body messages)

Defines a Protobufs "schema" whose name is given by the symbol *type*,
corresponding to a .proto file of that name. By a "schema", we mean an
object that corresponds to the contents of one .proto file. If *name*
is not supplied, the Protobufs name of the schema is the camel-cased
rendition of *type* (e.g., ``color-wheel`` becomes ``ColorWheel``);
otherwise the Protobufs name is the string *name*.

*imports* is a list of pathname strings to be imported. This corresponds
to ``import`` in a .proto file. Note that ``proto:define-schema`` can
import both .proto files and .lisp files containing Protobufs macros,
but the generated .proto code will convert all of these to imports of
.proto files.

*syntax* and *package* are strings that give the Protobufs syntax and
package name. *lisp-package* can be supplied to give the name of the
Lisp package, if it is different from *package*. *package* corresponds
to ``package`` in a .proto file. If you want to specify a Lisp package
in a .proto file, you can use ``option (lisp_package)``.

*optimize* can be either ``:space`` (the default) or ``:speed``. When it
is ``:space`` the serialization methods generated for each message are
compact, but slower; when it is ``:speed``, the serialization methods
will be much faster, but will take more space. This corresponds to
``option optimize_for = CODE_SIZE|SPEED`` in a .proto file.

*options* is a property list whose keys and values are both strings,
for example, ``:option ("java_package" "com.yoyodyne.overthruster")``.
They are passed along unchanged to the generated .proto file.

*documentation* is a documentation string that is preserved as a comment
in the .proto file.

*body* consists of any number of calls to ``proto:define-enum``,
``proto:define-message``, ``proto:define-extend`` or ``proto:define-service``.

::

  proto:define-enum (type (&key name conc-name alias-for        [Macro]
                                options documentation)
                     &body values)

Defines a Protobufs enum type and a corresponding Lisp deftype whose name
is given by the symbol *type*. If *name* is not supplied, the Protobufs
name of the enum is the camel-cased rendition of *type*; otherwise the
Protobufs name is the string *name*. If *conc-name* is given, it will
be used as the prefix for all of the enum value names. In a .proto file,
you can use ``option (lisp_name)`` to override the default name for the
enum type in Lisp.

If *alias-for* is given, no Lisp deftype is defined. Instead, the enum
will be used as an alias for an enum type that already exists in Lisp.
You can use ``option (lisp_alias)`` in a .proto file to give the Lisp
alias for an enum type.

*options*  is a property list whose keys and values are both strings.

*documentation* is a documentation string that is preserved as a comment
in the .proto file.

*body* consists of the enum values, each of which is either a symbol
or a list of the form ``(name index)``. By default, the indexes start at
0 and are incremented by 1 for each new enum value.

``proto:define-enum`` can be used only within ``proto:define-schema``
or ``proto:define-message``.

::

  proto:define-message (type (&key name conc-name alias-for     [Macro]
                                   options documentation)
                        &body fields)

Defines a Protobuf message and a corresponding Lisp defclass whose name
is given by the symbol *type*. If *name* is not supplied, the Protobufs
name of the class is the camel-cased rendition of *type*; otherwise the
Protobufs name is the string *name*. If *conc-name* is given, it will
be used as the prefix for all of the slot accessor names. In a .proto
file, you can use ``option (lisp_name)`` to override the default name
for the class in Lisp.

If *alias-for* is given, no Lisp defclass is defined. Instead, the
message will be used as an alias for a class that already exists in
Lisp. This feature is intended to be used to define messages that will
be serialized from existing Lisp classes; unless you get the slot names,
readers and writers exactly right for each field, it will be the case
that trying to (de)serialize into a(n aliased) Lisp object won't work.
You can use ``option (lisp_alias)`` in a .proto file to give the Lisp
alias for the class corresponding to a message.

*options*  is a property list whose keys and values are both strings.

*documentation* is a documentation string that is preserved as a comment
in the .proto file.

The body *fields* consists of fields, ``proto:define-enum``,
``proto:define-message`` or ``proto:define-extension`` forms.

Fields take the form ``(slot &key type name default reader writer)``.
*slot* can be either a symbol giving the slot name or a list of the
form ``(slot index)``. By default, the field indexes start at 1 and
are incremented by 1 for each new field value. *type* is the type of
the slot. *name* can be used to override the defaultly generated
Protobufs field name (for example, ``color-name`` becomes
``colorName``). *default* is the default value for the slot. *reader*
is the name of a Lisp slot reader function to use to get the value during
serialization, as opposed to using ``slot-value``; this is meant to be
used when aliasing an existing class. *writer* can be similarly used
to name a Lisp slot writer function.

Note that the Protobufs does not support full Lisp type expressions in
the types of fields. The following type expressions are supported:

 - ``integer``, optionally with upper and lower bounds
 - ``signed-byte``, which correspond to ``proto:int32`` or ``proto:int64``
 - ``unsigned-byte``, which correspond to ``proto:uint32`` or ``proto:uint64``
 - ``float`` and ``double-float``
 - ``string``and ``character``
 - ``(array (unsigned-byte 8))``, which corresponds to ``proto:byte-vector``
 - ``boolean``
 - ``(member ...)``, where all the members are symbols or keywords or ``nil``
 - the name of a class that corresponds to another Protobufs message
 - ``(proto:list-of <T>)``, where ``<T>`` is any of the above types
 - ``(proto:vector-of <T>)``, where ``<T>`` is any of the above types
 - ``(or <T> null)``, where ``<T>`` is any of the above types

``member`` corresponds to a Protobufs ``enum``. ``(or <T> null)``
corresponds to an optional field. ``proto:list-of`` corresponds to a
repeated field, and the Lisp slot will be typed as a list. ``proto:vector-of``
corresponds to a repeated field, and the Lisp slot will be typed as an
adjustable array with a fill pointer. The other types correspond to
the various Protobufs scalar field types.

``proto:define-message`` can be used only within ``proto:define-schema``
or ``proto:define-message``.

::

  proto:define-extension (from to)                              [Macro]

Defines a field extension for the indexes from *from* to *to*.
*from* and *to* are positive integers ranging from 1 to 2^29 - 1.
*to* can also be the token ``max``, i.e., 2^29 - 1.

Once an extension to a message has been defined, you can use
``proto:define-extends`` to add new fields.

``proto:define-extension`` can be used only within ``proto:define-message``.

In non-Lisp implementations of Protobufs, you set and get the value
of an extension using functions like ``SetExtension()`` and
``GetExtension()``. For example, if you extended a ``Color`` message
to have an ``opacity`` field, you would set the field using something
like this::

  Color color;
  color.SetExtension(opacity, 0.5);

In Common Lisp Protobufs, you can just use an ordinary slot accessor::

  (let ((color (make-instance 'color)))
    (setf (color-opacity color) 0.5))

::

  proto:define-extend (type (&key name conc-name                [Macro]
                                  options documentation)
                       &body fields)

Defines a Protobuf ``extend``, that is, an extension to an existing
message (and corresponding Lisp class) that has additional fields that
were reserved by ``proto:define-extension``. *type* and *name* are as
for ``proto:define-message``. Note that no new Lisp class is defined;
the additional slots are implemented as getter and setter methods on
a closed-over variable. The other options, such as *conc-name* and
*alias-for* are take from the extended message.

*options*  is a property list whose keys and values are both strings.

*documentation* is a documentation string that is preserved as a comment
in the .proto file.

The body *fields* consists only of fields, which take the same form as
they do for ``proto:define-message``.

``proto:define-extend`` can be used only within ``proto:define-schema``
or ``proto:define-message``.

::

  proto:define-service (type (&key name                         [Macro]
                                   options documentation)
                        &body method-specs)

Defines a Protobufs service named *type* and corresponding Lisp generic
functions for all its methods. If *name* is not supplied, the Protobufs
name of the service is the camel-cased rendition of *type*; otherwise
the Protobufs name is the string *name*.

*options*  is a property list whose keys and values are both strings.

*documentation* is a documentation string that is preserved as a comment
in the .proto file.

The body is a set of method specs of the form
``(name (input-type output-type) &key options documentation)``.
*name* is a symbol naming the RPC method. *input-type* and
*output-type* may either be symbols or a list of the form ``(type &key name)``.

``proto:define-service`` can only be used within ``proto:define-schema``.


Protobufs types
---------------

The following types are defined in the ``protobufs`` package:

 - ``proto:int32``, which corresponds to the Protobufs ``int32`` type
 - ``proto:int64``, which corresponds to the Protobufs ``int64`` type
 - ``proto:uint32``, which corresponds to the Protobufs ``uint32`` type
 - ``proto:uint64``, which corresponds to the Protobufs ``uint64`` type
 - ``proto:sint32``, which corresponds to the Protobufs ``sint32`` type
 - ``proto:sint64``, which corresponds to the Protobufs ``sint64`` type
 - ``proto:fixed32``, which corresponds to the Protobufs ``fixed32`` type
 - ``proto:fixed64``, which corresponds to the Protobufs ``fixed64`` type
 - ``proto:sfixed32``, which corresponds to the Protobufs ``sfixed32`` type
 - ``proto:sfixed64``, which corresponds to the Protobufs ``sfixed32`` type
 - ``proto:byte-vector``, which corresponds to the Protobufs ``bytes`` type
 - ``proto:list-of``, which corresponds to a repeated field
 - ``proto:vector-of``, which corresponds to a repeated field

The following existing Lisp type correspond to other Protobufs types:

 - ``string`` is the Protobufs UTF-8 encoded ``string`` type
 - ``boolean``  is the Protobufs ``bool`` type
 - ``float``  is the Protobufs ``float`` type
 - ``double-float``  is the Protobufs ``double`` type
 - ``member`` of a set of keywords generates a Protobufs ``enum`` type

Note that ``(or <T> null)`` corresponds to an optional field.


Protobufs service stubs
-----------------------

When you use the ``proto:define-service`` macro to define a service
with some methods, the macro defines "stubs" (CLOS generic functions)
for each of the methods in the service. Each method gets a client stub
and a server stub whose signatures are, respectively::

  (rpc-channel input output &key callback) => output
  (rpc-channel input output) => output

The type of *rpc-channel* is unspecified, but is meant to be a
"channel" over which some sort of RPC call will be done. The types of
*input* and *output* are classes that were defined via
Protobufs. *callback* is a function of two arguments, the RPC channel
and the output; it is intended for use by asynchronous RPC calls.

For example, this fragment defines four stubs::

  (proto:define-service color-wheel ()
    (get-color (get-color-request color))
    (add-color (add-color-request color)))

The client stubs are ``get-color`` and ``add-color``, the server stubs
are ``do-get-color`` and ``do-add-color``. An RPC library will implement
a method for the client stub. You must fill in the server stub yourself;
it will implement the desired functionality.

It is beyond the scope of this Protobufs library to provide the RPC
service; that is the domain of another library.


Serializing and deserializing
=============================

You can serialize from Lisp objects or deserialize into Lisp objects
using either the fast and compact Protobufs wire format, or the
human-readable text format.


Wire format
-----------

::

  proto:serialize-object-to-stream (object type                 [Function]
                                    &key stream visited)

Serializes the object *object* of type *type* onto the stream *stream*
using the wire format. *type* is the Lisp name of a Protobufs message
(often the name of a Lisp class) or a ``proto:protobuf-message`` object.
*type* defaults to the class of *object*

The element type of *stream* must be ``(unsigned-byte 8)``.

*visited* is an ``eql`` hash table used to cache object sizes. If it is
supplied, it will be cleared before it is used; otherwise, a fresh table
will be created.

The returned value is a byte vector containing the serialized object.
If the stream is ``nil``, the buffer is not actually written anywhere.

::

  proto:serialize-object (object type buffer                    [Generic function]
                          &optional start visited)

Serializes the object *object* of type *type* into the byte array
*buffer* using the wire format. *type* is the Lisp name of a Protobufs
message (often the name of a Lisp class) or a ``proto:protobuf-message``
object. *type* defaults to the class of *object*. The buffer is assumed
to be large enough to hold the serialized object; if it is not, an
out-of-bounds condition may be signaled.

The object is serialized using the wire format into the byte array
(i.e., a vector whose type is ``(unsigned-byte 8)``) given by *buffer*,
starting at the fixnum index *start* .

*visited* is an ``eql`` hash table used to cache object sizes.

The returned values are the modified buffer containing the serialized
object and the index that points one past the last serialized byte in
the buffer, which will be the number of bytes required to serialize the
object if *start* was 0.

Note that ``proto:serialize-object`` will not correctly serialize a
set of objects that has cycles. You must resolve these yourself.

::

  proto:deserialize-object-from-stream (type &key stream)       [Function]

Deserializes an object of the given type *type* as a Protobuf object.
*type* is the Lisp name of a Protobufs message (usually the name of a
Lisp class) or a ``proto:protobuf-message``.

The element type of *stream* must be ``(unsigned-byte 8)``.

The returned value is the deserialized object.

::

  proto:deserialize-object (type buffer &optional start end)    [Generic function]

Deserializes an object of the given type *type* as a Protobufs object.
*type* is the Lisp name of a Protobufs message (usually the name of a
Lisp class) or a ``proto:protobuf-message``.

The encoded bytes come from the byte array given by *buffer*, starting
at the fixnum index *start* up to the end of the buffer, given by *end*.
*start* defaults to 0, *end*' defaults to the length of the buffer.

If a zero byte is encountered in in the "tag position" during
deserialization, this is interpreted as an "end of object" marker
and deserialization stops.

The returned values are the deserialized object and the index into the
buffer at which the deserialization ended.

::

  proto:object-size (object type &optional visited)             [Generic function]

Computes the size in bytes of the object *object* of type *type*.
*type* is the Lisp name of a Protobufs message (usually the name of a
Lisp class) or a ``proto:protobuf-message``. *type* defaults to the
class of *object*

*visited* is an ``eql`` hash table used to cache object sizes.

The returned value is the size of the serialized object in bytes.


Text format
-----------

::

  proto:print-text-format (object &optional type                [Function]
                           &key stream suppress-line-breaks)

Prints the object *object* of type *type* onto the stream *stream* using
the textual format. *type* defaults to the class of *object*.

If *suppress-line-breaks* is true, all the output is put on a single line.

::

  proto:parse-text-format (type &key stream)                    [Function]

Parses the textual format of an object of the given type *type*. *type*
is the Lisp name of a Protobufs message (usually the name of a Lisp
class) or a ``proto:protobuf-message``. The input is read from the
stream *stream*.

The returned value is the object.


Other API functions
===================

Extensions functions
--------------------

::

proto:get-extension (object slot)                               [Generic function]

Returns the value of the extended slot *slot* in the object *object*.

Since you can just use the ordinary slot reader function, you should
not need to call ``proto:get-extension``. It is included for compatibility
with other Protobufs APIs.

::

proto:set-extension (object slot value)                         [Generic function]

Sets the value of the extended slot *slot* in the object *object*
to *value*.

Since you can just use the ordinary slot writer function, you should
not need to call ``proto:set-extension``. It is included for compatibility
with other Protobufs APIs.

::

proto:has-extension (object slot)                               [Generic function]

Returns true iff the object *object* has any value for the extended
slot *slot*.

::

proto:clear-extension (object slot)                             [Generic function]

Removes the value for the extended slot *slot* in the object *object*.


Initialization functions
------------------------

::

  proto:object-initialized-p (object type)                      [Generic function]

Returns true iff all of the fields of *object* of type *type* are
initialized, i.e., there are no fields whose value is unbound.

::

  proto:slot-initialized-p (object type slot)                   [Generic function]

Returns true iff the field *slot* of *object* of type *type* is
initialized, i.e., there are no fields whose value is unbound.

::

  proto:reinitialize-object (object type)                       [Generic function]

Initializes all of the fields of *object* of type *type* to their
default values.


Python compatibility functions
------------------------------

By popular demand, the Protobufs library provides an API that is very
similar to the API of the Python Protobufs library.

::

  proto:is-initialized (object)                                 [Generic function]

Returns true iff all of the fields of *object* are initialized, i.e.,
there are no fields whose value is unbound.

::

  proto:has-field (object slot)                                 [Generic function]

Returns true iff the field *slot* is initialized in *object*.

::

  proto:clear (object)                                          [Generic function]

Initializes all of the fields of *object* to their default values.

::

  proto:serialize (object &optional buffer start end)           [Generic function]

Serializes *object* into *buffer* using the wire format, starting at the
index *start* and going no further than *end*. *object* is an object
whose Lisp class corresponds to a Protobufs message.

::

  proto:merge-from-array (object buffer &optional start end)    [Generic function]

Deserializes the object encoded in *buffer* into *object*, starting at
the index *start* and ending at *end*. *object* is an object whose Lisp
class corresponds to a Protobufs message.

::

  proto:octet-size (object)                                     [Generic function]

Returns the number of bytes required to serialize *object* using the
wire format. *object* is an object whose Lisp class corresponds to a
Protobufs message.
