
            jQuery(document).ready(function () {

    var $gtranslate = $('#gtranslate-wrapper');
    if ($gtranslate !== undefined) {
        $(document).bind('click', function (e) {
            hideTranslate($gtranslate);
        });

        $gtranslate.children('.translate-toggle').bind('click', function (e) {
            if ($gtranslate.children('.translate-content').is(':visible')) {
                hideTranslate($gtranslate);
            } else {
                showTranslate($gtranslate);
            }
            e.stopPropagation(); // This is the preferred method.
            return false;
        });

        $gtranslate.children('.translate-content').bind('click.openTranslate', function (e) {
            console.log(e.target);
            if (!$(e.target).is('img') && !$(e.target).is('a')) {
                e.stopPropagation(); // This is the preferred method.
                return false;
            }
        });
    }

    showTranslate = function ($wrapper) {
        $wrapper.addClass('translate-visible');
        $("a.goog-te-menu-value").attr('tabindex', '0');
        $wrapper.children('.translate-content').fadeIn();
    }

    hideTranslate = function ($wrapper) {
        $wrapper.removeClass('translate-visible');
        $wrapper.children('.translate-content').fadeOut();
    }
    
    
                });