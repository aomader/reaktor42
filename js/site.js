$(function() {
    var path_pattern = /^\/([^\.\/]*)/,
        href_pattern = /^(?:http:\/\/[^\/]+)?\/([^\.\/]*)/,
        path_component = window.location.pathname.match(path_pattern)[1];
    $('#menu a').each(function() {
        if (this.href.match(href_pattern)[1] === path_component) {
            $(this).addClass('active');
        }
    });
});

window.bookmark = function() {
    var url = 'http://www.reaktor42.de',
        description = 'reaktor42 - a personal platform';
    window.external.AddFavorite(url, description);
};
