# News

Data type that describing the news.

Field | Type |Description
---------- | ------------- | ---------
__id__ | integer | News ID
__title__ | string | News name
__content__ | string | News content
__tags__ | [[Tag](tag.md)] | News content
__category__ | [[Category](category.md)] | News category
__creation_time__ | string | News creation time
author | [Author](author.md) | News author
main_picture | string | News main picture URL
additional_pictures | [string] | News additional pictures URL

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
    "author":{
        "user":{
            "first_name":"Ivan",
            "creation_time":"2020-07-16T04:34:41",
            "last_name":"Ivanov",
            "is_admin":false,
            "id":1,
            "login":"user1"
        },
        "id":2,
        "description":"The best author in the world!"
    },
    "tags":[
        {
        "name":"health",
        "id":1
        }
    ]
}
```
