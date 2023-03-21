# {{ self.title() }}

{% match content %}
    {% when DocumentContent::Empty %}
    {% when DocumentContent::Single with (item) %}
    {% when DocumentContent::Constants with (items) %}
    {% when DocumentContent::OverloadedFunctions with (items) %}
{% endmatch %}
