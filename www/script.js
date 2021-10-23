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

  // Handle setting circle clicks
  $('#main-setting_circle_ui').on('click'
    , function(event){
        const el = event.target.closest('.setting-circle');
        if(el){
          const id = $(el).data('id');
          Shiny.setInputValue(
            'main-setting_circle'
            , id
            , {priority: "event"}
          );
        }
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
});

// Hide/show if message is null
Shiny.addCustomMessageHandler(
  'control_visibility'
  , function(input){
      const id = `#${input.id}`;
      console.log(id, input.visible);
      if(input.visible){
        $(id).show(100)
      } else {
        $(id).hide(100)
      }
});


// Set local storage
Shiny.addCustomMessageHandler(
  'set_local_storage'
  , function(input) {
      localStorage.setItem(input.id, input.data);
});

// Set local storage
Shiny.addCustomMessageHandler(
  'get_local_storage'
  , function(id) {
      const message = localStorage.getItem(id);
      Shiny.setInputValue(
        'main-local_storage'
        , message
        , {priority: "event"}
      );
});
