"use strict";

$(function(){

  // Activate post start animations and table view
  $('#launch_button').on("click"
    , function(){
        $('.body__image').addClass('image-up');
        $('.header__text-box').addClass('header-up');
        $('#launch_button').fadeOut(2000);
        $('.main_container').addClass('fade-in');
        $('.body__help-box').addClass('fade-in');
        $('.body__help-image').addClass('btn');
        $('.help__splash-background').fadeIn(500);
        $('.help__splash-box').delay(500).fadeIn(500);

    }
  );

  // Handle clicks for dynamically created elements
  $('body').on("click",
    function(event){

      const splash_box = event.target.closest('.help__splash-box');
      if(splash_box) return;

      const moon = event.target.closest('.body__help-image');
      const splash = event.target.closest('.help__splash-background');

      if(moon || splash){
        $('.help__splash-background').fadeOut(500);
        $('.help__splash-box').fadeOut(500);
        $('.filled_circle').hide(0);
      }

    }

  )



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
  $('#main-main_rt').on('dblclick'
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

  $("#sw-content-main-more_settings-settings_dropdown").on('dblclick'
    , function(event){
      const item = event.target.closest('.item');
      const icon = $(item).children('i')
      if(icon){
        const icon_class = $(icon).attr('class');
        const down_class = "fa fa-arrow-down";
        const up_class = "fa fa-arrow-up";

        // Check icon has an up or down class
        const valid_class = icon_class.includes(down_class) || icon_class.includes(up_class)
        if(!valid_class) return;

        const new_class = icon_class.includes(down_class) ? up_class : down_class;
        $(icon).removeClass(icon_class).addClass(new_class);

        const order = new_class.replace('fa fa-arrow-','');

        Shiny.setInputValue(
          'main-more_settings-sort_item_change'
          , {item: $(item).data('value'), order: order}
          , {priority: "event"}
        );
      }
    }
  )


  // Set sort order
  Shiny.addCustomMessageHandler(
    'set_sort_order'
    , function(sort_message) {

        if(sort_message == null) return;

        const selected = sort_message.selected;
        const order = sort_message.order;

        const selected_array = Array.isArray(selected) ? selected : [selected];
        const order_array = Array.isArray(order) ? order : [order];

        selected_array.forEach(
          function(selection, index){
            const icon = $(document).find(`.item[data-value='${selection}'] > i`);

            const down_class = "fa fa-arrow-down";
            const up_class = "fa fa-arrow-up";

            const is_up = order_array[index] === 1;
            const new_class = is_up ? up_class : down_class;

            $(icon).removeClass(down_class).removeClass(up_class);
            $(icon).addClass(new_class);

            console.log("Index:", index);
            console.log("Is up: ", is_up, " .. if true, this should be 1: ", order_array[index]);
            console.log(new_class);
            console.log(order_array[index]);

          }
        );

  });


});

// Hide/show table controls
Shiny.addCustomMessageHandler(
  'view_controls_switch'
  , function(visible) {
      visible
      ? $('.table_controls').slideDown(500)
      : $('.table_controls').slideUp(500);

      visible
      ? $('.more_settings').delay(500).show("slide", { direction: "left" }, 100)
      : $('.more_settings').hide("slide", { direction: "left" }, 100);
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

// Get local storage
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




// Get sort order
Shiny.addCustomMessageHandler(
  'get_sort_order'
  , function(x) {
      const parent_element = $("#main-more_settings-sort_by").parent();
      const sort_orders = $(parent_element).find('i').map(
          function() {
            return $(this).attr('class').replace('fa fa-arrow-','');
          }).get();

      Shiny.setInputValue(
        'main-more_settings-sort_order'
        , sort_orders
        , {priority: "event"}
      );
});
