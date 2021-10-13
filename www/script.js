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

Shiny.addCustomMessageHandler('change-slider-label', function(slider) {

  const label_id = `${slider.id}-label`;
  const el = document.getElementById(label_id);
  el.innerHTML = `
    <div>
      <i class = "fa fa-filter" role="presentation" aria-label="filter icon">
      </i>
      ${slider.label}
    </div>
  `;



});

// #main-value_slider-label


// main-value_slider-label
// main-value_slider-label
