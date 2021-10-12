"use strict";

$(function(){

    $('#launch_button').on("click"
      , function(){
        $('.body__image').addClass('image-up');
        $('.header__text-box').addClass('header-up');
        $('#launch_button').addClass('fade-away');
        $('.statistic_container').addClass('fade-in');
      }
    );


});
