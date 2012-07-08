(function( $ ) {

    var itemTemplate = '<div class="foton-item"><span class="foton-item-title"><%= title %></span><span class="foton-item-content"><%= content %></span></div>';

    $.fn.fotonCMS = function( options ) {
        
        var fotonElem = this;

        var itemTempl = options && options.itemTemplate || itemTemplate;

        var settings = $.extend({
            host: '46.149.20.92',
            port: 80,
            account: 'fotoncms',
            feed: 'news'
        }, options);

        $.ajax({
            url: 'http://' + settings.host + ':' + settings.port + '/feeds/' + settings.account + '/' + settings.feed,
            success: function(data) {
                var content = _.foldl(data.items, function(acc, item) {
                        return acc + _.template(itemTempl, item);
                    }, '');

                fotonElem.html(content);
            }
        });

        return this;
    };
})( jQuery );
