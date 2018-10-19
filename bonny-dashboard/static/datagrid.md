# bonny-datagrid

```
<bonny-datagrid data-resource="/api/endpoint"></bonny-datagrid>
```

## Attributes

| Name       | Type    | Default               | Description
----------------------------------------------------------------------------------------------------
| resource   | string  |                       | URL to fetch the JSON array that will populate the datagrid
| page-size  | integer | 5                     | Number of elements to display per page
| page-sizes | array   | [5, 20, 50, 100, 300] | Possible values for page-size (populates `bonny-page-size-selector`)

## Children

| Element             | Number | Description
----------------------------------------------------------------------------------------------------
* `bonny-header`      | any    |
* `bonny-column`      | any    |
* `bonny-footer`      | any    |
* `bonny-placeholder` | one    |
* `bonny-loader`      | one    |

# bonny-column

```
<bonny-column data-name="Mane Color" data-value="color">
  <span class="color-marker">{color}</span>
</bonny-column>
```

## Attributes

| Name       | Type           | Default  | Description
----------------------------------------------------------------------------------------------------
| name       | string         |          | Text to display in the column head
| value      | string         |          | Property to read from the displayed resource
| sortable   | boolean        | false    | Should this column be sortable?
| filterable | boolean        | false    | Should this column be filterable?
| type       | string / array | string   | What kind of filters are available on the value?


## `type` attribute details

The possible values for `type` are:
* "string": The value can be filtered using string operations
* "boolean": The value can be True or False
* "['red', 'green', 'blue']": The value can be anything present in the given array

Specifying the `type` attribute allows the datagrid to render a custom `bonny-filter-adder` for some columns.
For example, if you have a color property in your resources, by default, the filter adder treats
it as a string, so you will get filters like the following:
- Color starts with "gr"
- Color is "blue"
- Color does not contain "dark"

If you want to replace this behavior with custom filters instead, specify the type as an array with possible values:
```
<bonny-column data-value="color" data-type="['pink', 'purple', 'rainbow']"></bonny-color>
```
The filters would now look like this:
- Color is pink
- Color is not purple

# bonny-filter-adder
# bonny-filter-list
# bonny-result-count
# bonny-page-size-selector
# bonny-pagination
