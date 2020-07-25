# Category

Data type that describing the category.

Field | Type |Description
---------- | ------------- | ---------
__id__ | integer | Category ID
__name__ | string | Category name
childs | [[Category](category.md)] | Category childs



**Data example:**

```json
{
    "name":"World",
    "id":1,
    "childs":[
        {
            "name":"Europe",
            "id":3,
            "childs":[
                {
                "name":"Germany",
                "id":4
                }
            ]
        },
        {
            "name":"Africa",
            "id":2
        }
    ]
}
```
