.. raw:: LaTeX

   \setlength{\parindent}{0pt}
   \setlength{\parskip}{6pt plus 2pt minus 1pt}


=========================
Protobufs for Common Lisp
=========================


:Description: Protobufs for Common Lisp
:Author: Scott McKay <swm@google.com>
:Date: $Date: 2012-04-10 14:18:00 -0500 (Tue, 10 Apr 2012) $

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
    3  Serializing and deserializing
      3.1  Wire format
      3.1  Text format
    4  Python compatibility functions


Introduction
============

The Protobufs library provides a way for Common Lisp programs to use
existing (or define new) Protobufs "schemas", and serialize and
deserialize objects to and from the Protobufs wire and text formats.

To use it, first load the ASDF declaration file ``protobufs/protobufs.asd``
and then use ASDF to load the library.


Implementation notes
--------------------

The Protobufs library defines a set of model classes that describes a
protobufs "schema". There is a class that describes one .proto file
(i.e., one "schema"), options, enums and enum values, messages and
fields, and services and methods.

The library provides the means to convert several kinds of inputs into
the Protobufs models, including:

 - Parse an existing .proto file into a set of model objects.
 - Convert a set of related CLOS classes into a set of model objects.
 - Compile a ``proto:define-proto`` macro call into a set of model objects.

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

  proto:protobuf                                                [Class]

The class the represents a Protobufs schema, i.e., one .proto file.
It has slots for the name, options, enums, messages and services.


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
file to a .lisp source file and then using the Lisp file; convert a set
of Lisp classes to a Protobufs model, and then use either the .lisp or
the .proto representation of the model; use a .proto file directly in as
ASDF system; or use the Protobufs macros in a Lisp source file.


.proto file to Lisp conversion
------------------------------

If you have an existing .proto source file that you would like to
convert to Lisp classes (more precisely, to the macros defined by the
Protobufs library), you can use ``proto:parse-protobuf-from-file`` to read the
.proto file and then use ``proto:write-protobuf`` to write a new .lisp file.

::

  proto:parse-protobuf-from-file (filename)                     [Function]

Parses the contents of the file given by *filename*, and returns the
Protobufs model (a set object objects rooted at ``proto:protobuf``)
corresponding to the parsed file. The name of the Protobufs schema is
generated automatically from the file name.


::

  proto:parse-protobuf-from-stream (stream &key name class)     [Function]

Parses the contents of the stream *stream*, and returns the Protobufs
schema corresponding to the parsed file. If *name* is supplied, it gives
the Protobufs name for the schema. If *class* is supplied, it gives the
Lisp name.


::

  proto:write-protobuf (protobuf &key stream type)              [Function]

Pretty-prints the Protobufs schema *protobuf* onto the stream,
which defaults to ``*standard-output*``.

``type`` can be either ``:proto`` or ``:lisp``.


CLOS classes to .proto conversion
---------------------------------

If you have an existing set of CLOS classes that you would like to
convert to a Protobufs schema, you can use ``proto:generate-protobuf-schema-from-classes``.

Note that the Protobufs schema is an *approximation* of a good schema.
You should review it and, if necessary, change it (and probably the Lisp
classes as well) until you have a good Protobufs schema definition.

::

  proto:generate-protobuf-schema-for-classes (classes           [Function]
                                              &key name package lisp-package)

Given a list of class names *classes*, this generates a Protobufs schema
for the classes, generating any necessary enum types that correspond to
Lisp ``member`` types.

*name* and *package* can be supplied to give the Protobufs name and package.
*lisp-package* can be supplied to give the name of the Lisp package, if it
is different from *package*.


::

  proto:write-protobuf-schema-for-classes (classes              [Function]
                                           &key stream type name package lisp-package)

Given a list of class names *classes*, this generates a Protobufs schema
for the classes, generating enum types as necessary, and then
pretty-prints the result onto *stream*. *type* can be either ``:proto``
(the default) or ``:lisp``.

*name* and *package* can be supplied to give the Protobufs name and package.
*lisp-package* can be supplied to give the name of the Lisp package, if it
is different from *package*.


Using .proto files directly
---------------------------

In addition to using the tools described above to convert between .proto
files and .lisp files, you can also use .proto files directly in ASDF
systems. Just use the ASDF module type ``:proto`` in your system, and
compile and load the system in the usual way. This will create both the
Protobufs model and the Lisp classes that correspond to the Protobufs
messages.


Using the Protobufs macros
--------------------------

You can define a Protobufs schema entirely within Lisp by using the
following macros. For example::

  (proto:define-proto color-wheel
      (:package color-wheel)
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
    (proto:define-extends color ()
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
looks like this::

  syntax = "proto2";

  package color_wheel;

  import "net/proto2/proto/descriptor.proto"

  extend proto2.MessageOptions {
    optional string lisp_package = 195801;
    optional string lisp_name = 195802;
    optional string lisp_alias = 195803;
  }

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

  extends Color {
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


::

  proto:define-proto (type (&key name syntax import             [Macro]
                                 package lisp-package
                                 optimize options documentation)
                      &body messages)

Defines a Protobufs schema whose name is given by the symbol *type*,
corresponding to a .proto file of that name. If *name* is not supplied,
the Protobufs name of the schema is the camel-cased rendition of *type*
(e.g., ``color-wheel`` becomes ``ColorWheel``); otherwise the Protobufs
name is the string *name*.

*imports* is a list of pathname strings to be imported. This corresponds
to ``import`` in a .proto file.

*syntax* and *package* are strings that give the Protobufs syntax and
package name. *lisp-package* can be supplied to give the name of the
Lisp package, if it is different from *package*. *package* corresponds
to ``package`` in a .proto file. If you want to specify a Lisp package
in a .proto file, you can use ``option lisp_package``.

*optimize* can be either ``:space`` (the default) or ``:speed``. When it
is ``:space`` the serialization methods generated for each message are
compact, but slower; when it is ``:speed``, the serialization methods
will be much faster, but will take more space. This corresponds to
``option optimize_for CODE_SIZE|SPEED`` in a .proto file.

*options* is a property list whose keys and values are both strings,
for example, ``:option ("java_package" "com.yoyodyne.overthruster")``.
The are passed along unchanged to a generated .proto file.

*documentation* is a documentation string that is preserved as a comment
in the .proto file.

*body* consists of any number of calls to ``proto:define-enum``,
``proto:define-message``, ``proto:define-extends`` or ``proto:define-service``.


::

  proto:define-enum (type (&key name conc-name alias-for        [Macro]
                                options documentation)
                     &body values)

Defines a Protobufs enum type and a corresponding Lisp deftype whose name
is given by the symbol *type*. If *name* is not supplied, the Protobufs
name of the enum is the camel-cased rendition of *type*; otherwise the
Protobufs name is the string *name*. If *conc-name* is given, it will
be used as the prefix for all of the enum value names. In a .proto file,
you can use ``option lisp_name`` to override the default name for the
enum type in Lisp.

If *alias-for* is given, no Lisp deftype is defined. Instead, the enum
will be used as an alias for an enum type that already exists in Lisp.
You can use ``option lisp_alias`` in a .proto file to give the Lisp
alias for an enum type.

*options*  is a property list whose keys and values are both strings.

*documentation* is a documentation string that is preserved as a comment
in the .proto file.

*body* consists of the enum values, each of which is either a symbol
or a list of the form ``(name index)``. By default, the indexes start at
0 and are incremented by 1 for each new enum value.


::

  proto:define-message (type (&key name conc-name alias-for     [Macro]
                                   options documentation)
                        &body fields)

Defines a Protobuf message and a corresponding Lisp defclass whose name
is given by the symbol *type*. If *name* is not supplied, the Protobufs
name of the class is the camel-cased rendition of *type*; otherwise the
Protobufs name is the string *name*. If *conc-name* is given, it will
be used as the prefix for all of the slot accessor names. In a .proto
file, you can use ``option lisp_name`` to override the default name
for the class in Lisp.

If *alias-for* is given, no Lisp defclass is defined. Instead, the
message will be used as an alias for a class that already exists in
Lisp. This feature is intended to be used to define messages that will
be serialized from existing Lisp classes; unless you get the slot names
or readers exactly right for each field, it will be the case that trying
to (de)serialize into a Lisp object won't work.
You can use ``option lisp_alias`` in a .proto file to give the Lisp
alias for the class corresponding to a message.

*options*  is a property list whose keys and values are both strings.

*documentation* is a documentation string that is preserved as a comment
in the .proto file.

The body *fields* consists of fields, ``proto:define-enum``,
``proto:define-message`` or ``proto:define-extension`` forms.

Fields take the form ``(slot &key type name default reader writer)``.
*slot* can be either a symbol giving the slot name or a list of the
form ``(slot index)``. By default, the field indexes start at 1 and
are incremented by 1 for each new field value.  *type* is the type of
the slot. *name* can be used to override the defaultly generated
Protobufs field name (for example, ``color-name`` becomes
``colorName``).  *default* is the default value for the slot. *reader*
is a Lisp slot reader function to use to get the value during
serialization, as opposed to using ``slot-value``; this is meant to be
used when aliasing an existing class. *writer* can be similarly used
to give a Lisp slot writer function.


::

  proto:define-extends (type (&key name                         [Macro]
                                   options documentation)
                        &body fields)

Defines a Protobuf "extends", that is, an extension to an existing
message and Lisp class that has additional fields that were reserved
by ``proto:define-extension``. *type* and *name* are as for
``proto:define-message``. Note that no new Lisp class is defined; the
additional slots are implemented as getter and setter methods with a
closed-over variable. The other options, such as *conc-name* and
*alias-for* are take from the extended message.

*options*  is a property list whose keys and values are both strings.

*documentation* is a documentation string that is preserved as a comment
in the .proto file.

The body *fields* consists only of fields, which take the same form as
they do for ``proto:define-message``.


::

  proto:define-extension (from to)                              [Macro]

Defines a field extension for the indexes from *from* to *to*.
*from* and *to* are positive integers ranging from 1 to 2^29 - 1.
*to* can also be the token ``max``, i.e., 2^29 - 1.


::

  proto:define-service (type (&key name                         [Macro]
                                   options documentation)
                        &body method-specs)

Defines a Protobufs service named *type* and corresponding Lisp
defgenerics for all its methods. If *name* is not supplied, the Protobufs
name of the enum is the camel-cased rendition of *type*; otherwise the
Protobufs name is the string *name*.

*options*  is a property list whose keys and values are both strings.

*documentation* is a documentation string that is preserved as a comment
in the .proto file.

The body is a set of method specs of the form
``(name (input-type output-type) &key options documentation)``.
*name* is a symbol naming the RPC method. *input-type* and
*output-type* may either be symbols or a list of the form ``(type &key name)``.


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

*visited* is a hash table used to cache object sizes. If it is supplied,
it will be cleared before it is used; otherwise, a fresh table will be
created.

The returned values are a byte vector containing the serialized object
and the number of bytes required to serialize the object. If the stream
is ``nil``, the buffer is not actually written anywhere.


::

  proto:serialize-object (object type buffer                    [Generic function]
                          &optional start visited)

Serializes the object *object* of type *type* into the byte array
*buffer* using the wire format. *type* is the Lisp name of a Protobufs
message (often the name of a Lisp class) or a ``proto:protobuf-message``
object. *type* defaults to the class of *object*. The buffer is assumed
to be large enough to hold the serialized object; if it is not, an
out-of-bounds condition may be signalled.

The object is serialized into the byte array given by *buffer*, starting
at the fixnum index *start* using the wire format.

*visited* is a hash table used to cache object sizes.

The returned values are the modified buffer containing the serialized
object and the number of bytes required to serialize the object.


::

  proto:deserialize-object-from-stream (type &key stream)       [Function]

Deserializes an object of the given type *type* as a Protobuf object.
*type* is the Lisp name of a Protobufs message (usually the name of a
Lisp class) or a ``proto:protobuf-message``.

The returned value is the deserialized object.


::

  proto:deserialize-object (type buffer &optional start end)    [Generic function]

Deserializes an object of the given type *type* as a Protobufs object.
*type* is the Lisp name of a Protobufs message (usually the name of a
Lisp class) or a ``proto:protobuf-message``.

The encoded bytes come from the byte array given by *buffer*, starting
at the fixnum index *start* up to the end of the buffer, given by *end*.
If a zero byte is encountered in in the "tag position" during
deserialization, this is interpreted as an "end of object" marker.

The returned values are the deserialized object and the index into the
buffer at which the deserialization ended.


::

  proto:object-size (object type &optional visited)             [Generic function]

Computes the size in bytes of the object *object* of type *type*.
*type* is the Lisp name of a Protobufs message (usually the name of a
Lisp class) or a ``proto:protobuf-message``. *type* defaults to the
class of *object*

*visited* is a hash table used to cache object sizes.

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


Python compatibility functions
==============================

By popular demand, the Protobufs library provides an API that very
similar to the API of the Python Protobufs library.

::

  proto:clear (object)                                          [Generic function]

Initializes all of the fields of *object* to their default values.


::

  proto:is-initialized (object)                                 [Generic function]

Returns true iff all of the fields of *object* are initialized, i.e.,
there are no fields whose value is unbound.


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
