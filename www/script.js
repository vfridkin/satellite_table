"use strict";

$(function(){

  // Activate post start animations and table view
  $('#launch_button').on("click"
    , function(){
      $('.body__image').addClass('image-up');
      $('.header__text-box').addClass('header-up');
      $('#launch_button').fadeOut(2000);
      $('.statistic_container').addClass('fade-in');
      $('.body__help-box').addClass('fade-in');
      $('.body__help-image').addClass('btn');
    }
  );

  // Handle double click on table controls - used for selecting slider definition
  $('.table_controls').on('dblclick'
    , function(event){
      const item = event.target.closest('.item');
      if(item){
        const input_id = $(item.parentNode).find('input').attr('id');
        if(input_id){
          const value = item.dataset.value;
          Shiny.onInputChange(
            'main-double_click_selectize_item'
            , {container: input_id, value: value}
            , {priority: "event"}
          );
        }
      }
    }
  );

  // Handle table double click events - used for filtering factors
  $('#main-statistic_rt').on('dblclick'
    , function(event){
      const cell = event.target.closest('.rt-td > div > div');
      if(cell){
          Shiny.onInputChange(
            'main-double_click_cell'
            , {col_name: cell.dataset.colName, value: cell.innerText}
            , {priority: "event"}
          );
      }
    }
  );




});

// Hide/show table controls
Shiny.addCustomMessageHandler(
  'view_controls_switch'
  , function(visible) {
      visible
      ? $('.table_controls').show(500)
      : $('.table_controls').hide(500)
})

// Workaround for icon in slider label - without it the label shows [object object] on update
Shiny.addCustomMessageHandler(
  'change-slider-label'
  , function(slider) {

    const label_id = `${slider.id}-label`;
    const el = document.getElementById(label_id);
    el.innerHTML = `
      <div>
        <i class = "fa fa-filter" role="presentation" aria-label="filter icon">
        </i>
        ${slider.label}
      </div>
    `;
  }
);
