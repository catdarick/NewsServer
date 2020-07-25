# Draft

Data type that describing the draft.

Field | Type |Description
---------- | ------------- | ---------
__id__ | integer | Draft ID
__title__ | string | Draft name
__content__ | string | Draft content
__tags__ | [[Tag](tag.md)] | Draft content
__category__ | [[Category](category.md)] | Draft content
__creation_time__ | string | Draft's creation time
main_picture | string | Draft's main picture URL
additional_pictures | [string] | Draft's additional pictures URL

**Note:**
`creation_time` contains ISO 8601 formatted time as `YYYY-MM-DDThh:mm:ss`.

**Data example:**

```json
{
    "category":{
        "name":"World",
        "id":13
    },
    "creation_time":"2020-07-26T00:49:07",
    "content":"The WHO explains how it keeps a vigil for outbreaks like Covid-19",
    "id":25,
    "title":"Using AI to spot the next pandemic",
    "tags":[
        {
        "name":"health",
        "id":1
        }
    ]
}
```
