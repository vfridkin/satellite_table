"use strict";

$(function(){

    $('#launch_button').on("click"
      , function(){
        $('.body__image').addClass('image-up');
        $('.header__text-box').addClass('header-up');
        // $('#launch_button').addClass('fade-away');
        $('#launch_button').fadeOut(2000);
        $('.statistic_container').addClass('fade-in');
        $('.body__help-box').addClass('fade-in');
        $('.body__help-image').addClass('btn');
      }
    );


});
