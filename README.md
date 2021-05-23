# emqx_widget

The `emqx_widget` is an application that manages configuration specs and runtime states
for components that need to be configured and manipulated from the emqx-dashboard.

It is intended to be used by resources, actions, acl, auth, backend_logics and more.

It reads the configuration spec from *.wgt (in HOCON format) and provide APIs for
creating, updating and destroying widget instances among all nodes in the cluster.

It handles the problem like storing the configs and runtime states for both widget
and widget instances, and how porting them between different emqx_widget versions.

It may maintain the config and data in JSON or HOCON files in data/ dir.

After restarting the emqx_widget, it re-creates all the widget instances.

There can be foreign references between widget instances via widget-id.
So they may find each other via this Id.

## Try it out

    $ ./demo.sh
    Eshell V11.1.8  (abort with ^G)
    1> == the demo log tracer <<"log_tracer_clientid_shawn">> started.
    config: #{<<"configs">> =>
                #{<<"bulk">> => <<"10KB">>,<<"cache_log_dir">> => <<"/tmp">>,
                    <<"condition">> => #{<<"clientid">> => <<"abc">>},
                    <<"level">> => <<"debug">>},
            <<"id">> => <<"log_tracer_clientid_shawn">>,
            <<"widget_type">> => <<"log_tracer">>}
    1> emqx_widget_instance:health_check(<<"log_tracer_clientid_shawn">>).
    == the demo log tracer <<"log_tracer_clientid_shawn">> is working well
    state: #{health_checked => 1,logger_handler_id => abc}
    ok

    2> emqx_widget_instance:health_check(<<"log_tracer_clientid_shawn">>).
    == the demo log tracer <<"log_tracer_clientid_shawn">> is working well
    state: #{health_checked => 2,logger_handler_id => abc}
    ok

    3> emqx_widget_instance:query(<<"log_tracer_clientid_shawn">>, get_log).
    == the demo log tracer <<"log_tracer_clientid_shawn">> received request: get_log
    state: #{health_checked => 2,logger_handler_id => abc}
    "this is a demo log messages..."

    4> emqx_widget_instance:remove(<<"log_tracer_clientid_shawn">>).
    == the demo log tracer <<"log_tracer_clientid_shawn">> stopped.
    state: #{health_checked => 0,logger_handler_id => abc}
    ok

    5> emqx_widget_instance:query(<<"log_tracer_clientid_shawn">>, get_log).
    ** exception error: {get_instance,{<<"log_tracer_clientid_shawn">>,not_found}}

## The *.wgt file

The widget uses the .wgt file as a specification to define the schema of its config file and the
request body of HTTP API.
The .wgt describes what fields the widget accepts and also the type of the fields.

Besides the validation, the .wgt file is also used by the dashboard to render the UI. For example
what fields should exist and what fields should not, and whether provide a textbox or textarea
(multi-line textbox) to the user. It also defines the order of the component displayed on the UI.
These functionalities are implement by some metadata fields in the .wgt file.

### The metadata for widget title and descriptions

The field `title` and `description` is used by the dashboard UI for show the title of the widget.
For example:

```
title: "this is an example title"
description: "write more detailed descriptions here"
```

### The metadata for layout of UI components

The .wgt file can defined the order of the UI components for the fields, by specifying the top-level
fields in an array:

```
layout: [field1, field2, ...]
```

It can also split the fields into different groups, by providing the `layout` some groups of fields.
The following example the UI will show two tab pages, one for the `group_name_1` and the other for
`group_name_2`:

```
layout: {
  group_name_1: [field1, field2]
  group_name_2: [field3]
}
```

### Schema for the fields

The syntax of the schema is similar to the `descriptors` described by
[yiminghe's async-validator](https://github.com/yiminghe/async-validator), but extends it with more
types and metadata.

A description of widget schema is defined by `field` object, and each field is defined also as an
object with the field name as the key:

```
fields: {
  field_name_1: {
    type: string
    default: "abc"
    title: "The short description for this field, used for displaying it on the UI"
    description: """Here is the more detailed description, e.g. how to use the field in different
use cases."""
  },
  field_name_2: {
    type: integer
    required: true
    title: "foo"
    description: """bar"""
  }
}
```
### Types

#### Commonly used types

- `string`: The optional field `len` can be used to define the exactly length of the string.
    And the optional `max` and `min` fields can be used to define a range of the length.
    The optional `pattern` can be used to define the regular expression for matching the string.

- `number`: The value Can be unsigned float or integer.

- `boolean`: The value must be true or false.

- `integer`: The value must be of type integer.

- `float`: The value must be of type floating point number.

- `array`: The value must be an array, the mandatory field `items` is used for defining the schema of each element in the array.

    If all the element are of the same type:

    ```
    urls: {
        type: 'array',
        required: true,
        items: {type: 'url'},
    }
    ```

    Or if the elements have different types, give all possible types to `items` in an array.
    e.g.
    ```
    roles: {
        type: 'array',
        required: true,
        len: 3,
        items: [
            {type: string},
            {type: integer}
        ]
    }
    ```

- `object`: The value must be an object, the mandatory field `fields` is used for defining the schema
    of each sub-field in the object.

    e.g.
    ```
    address: {
        type: 'object',
        required: true,
        fields: {
            street: { type: 'string', required: true },
            city: { type: 'string', required: true },
            zip: { type: 'string', required: true, len: 8, message: 'invalid zip' },
        }
    }
    ```

    The UI should use a textarea and prompt the user to input an JSON object string for type
    `object`. See also `object_table`.

- `enum`: Value must exist in the enum defined by the mandatory field `enum`.

- `date`: Value must be a valid date

- `url`: Same to the `string` but with the default regular expression validator to
    `https?:\/\/(www\.)?[-a-zA-Z0-9@:%._\+~#=]{1,256}\.[a-zA-Z0-9()]{1,6}\b([-a-zA-Z0-9()@:%_\+.~#?&//=]*)`

#### Extended types

- `size`: Same to `string`, but with the default regular expression validator to `^[. 0-9]+(B|KB|MB|GB)$`
- `long_string`: Same to `string`, but indicates the UI to use the `textarea` not `text` component.
- `password`: Same to `string`, but indicates the UI to use the `password` textbox.
- `file`: Same to `string`, but indicates the UI to use the component for uploading files.
- `table_object`: Same to `string`, but indicates the UI to use the table component.

### Metadata that confine the types

- `required`: Boolean value that indicates that the field must exist.
- `pattern`: Indicates a regular expression that the value must match to pass validation.
    Only when the type is of type `string`, `long_string` or `password`.
- `max_len` and `min_len`: To validate an max/min length of a string or an array.
- `max` and `min`: Indicates the range of a number. Value >= max && Value <= min.

### Metadata for field visibility

- `visible`: defined rules indicting this field is visible or not.

  `Rules` are consist of one or more `conditions`.

  A condition is of format:
  ```
  {field: "the_field_name", equals: "some_value"}
  ```

  The above condition is evaluated to true when the field with name `the_field_name` exists and its
  value equals to `some_value`.

  A rule is a combination of one or multiple rules using `and` or `or` keywords:

  ```
  {
    and: [
        or: [
            {field: "enable_cache", equals: "true"},
            {field: "cache_logs_in", equals: "file"}
        ]
        {field: "force_enable", equals: "true"}
    ]
  }
  ```

  The above rule can be described as `(enable_cache || cache_logs_in) && force_enable` in most
  programming languages.
