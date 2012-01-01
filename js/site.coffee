$(document).ready ->
    path_pattern = /^\/([^\.\/]*)/
    href_pattern = /^(?:http:\/\/[^\/]+)?\/([^\.\/]*)/
    path_component = window.location.pathname.match(path_pattern)[1]
    $('#menu a').each ->
        $(this).addClass('active') if this.href.match(href_pattern)[1] is path_component

window.bookmark = ->
    url = 'http://www.reaktor42.de'
    description = 'reaktor42 - a personal platform'
    window.external.AddFavorite(url, description)
