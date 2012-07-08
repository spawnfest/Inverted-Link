(function( $ ) {

    var itemTemplate = '<div class="foton-item"><span class="foton-item-title"><%= title %></span><span class="foton-item-content"><%= content %></span></div>';

    $.fn.fotonCMS = function( options ) {
        
        var fotonElem = this;

        var settings = $.extend({
            account: 'gabriel',
            feed: 'test'
        }, options);

        $.ajax({
            url: '/feeds/' + settings.account + '/' + settings.feed,
            success: function(data) {
                var content = _.foldl(data.items, function(acc, item) {
                        return acc + _.template(itemTemplate, item);
                    }, '');

                fotonElem.html(content);
            }
        });

        return this;
    };
})( jQuery );