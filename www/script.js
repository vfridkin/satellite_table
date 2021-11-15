"use strict";

$(function(){

  // Activate post start animations and table view
  $('#launch_button').on("click"
    , function(){
        $('.body__image').addClass('image-up saved-info-opener');
        $('.header__text-box').addClass('header-up');
        $('#launch_button').fadeOut(2000);
        $('.main_container').addClass('fade-in')
        $('.body__help-box').addClass('fade-in');
        $('.body__help-image').addClass('btn');

        const splash_check = localStorage.getItem('satellite_table_shiny_appsplash_check');
        if(splash_check !== "[true]"){
          $('.screen-overlay').fadeIn(500);
          $('.bubble-moon').delay(500).fadeIn(1000);
        } else {
          $('.filled_circle').hide(0);
          $('.filled_circle2').hide(0);
        }
    }
  );

  // Handle clicks for dynamically created elements
  $('body').on("click",
    function(event){

      const bubble_moon = event.target.closest('.bubble-moon');
      const bubble_satellite = event.target.closest('.bubble-satellite');
      const info_box = event.target.closest(".info-box")
      const cell = event.target.closest('.rt-td > div > div');

      const moon_button = event.target.closest('.moon-button');
      const satellite_button = event.target.closest('.satellite-button');

      if(bubble_moon && !moon_button) return;
      if(bubble_satellite && !satellite_button) return;

      const satellite = event.target.closest('.saved-info-opener');
      const moon = event.target.closest('.body__help-image');
      const screen_overlay = event.target.closest('.screen-overlay');

      if(moon_button){
        $('.bubble-moon').fadeOut(500);
        $('.bubble-satellite').delay(500).fadeIn(500);
        $('.body__image').css("z-index", 11)
        $('.filled_circle2').css("display", "inline-block");
        $('.filled_circle2').show(0);
        $('.filled_circle').hide(0);
        $('.body__help-box').css("z-index", 9);
      }

      if(moon || screen_overlay || satellite_button){
        $('.screen-overlay').fadeOut(500);
        $('.bubble-moon').fadeOut(500);
        $('.filled_circle').hide(0);
        $('.filled_circle2').hide(0);
        $('.saved-info').fadeOut(500);

        if($('.bubble-satellite').is(":visible")){
          $('.bubble-satellite').fadeOut(500);
          Shiny.setInputValue('splash_close', 1, {priority: "event"});
        }
      };

      if(moon){
        Shiny.setInputValue('start_help', 1, {priority: "event"});
        return;
      };

      if(satellite){
        $('.screen-overlay').fadeIn(250);
        $('.saved-info').fadeIn(250);
        Shiny.setInputValue('saved-refresh_table', 1, {priority: "event"});
        return;
      }

      if(info_box || cell) return;
      Shiny.setInputValue('main-click_background', 1, {priority: "event"});

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
          Shiny.setInputValue(
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
          Shiny.setInputValue(
            'main-double_click_cell'
            , {col_name: cell.dataset.colName, value: cell.innerText}
            , {priority: "event"}
          );
      }
    }
  );

  // Detect changes in sort order in main table
  $("#main-main_rt").on("click",
    function(event){
      const el = event.target.closest("[role='columnheader']");
      // Return sort order of all columns
      if(el){
          Shiny.setInputValue(
            'main-sort_state_change'
            , 1
            , {priority: "event"}
        );
      }

      // Single click on cell - used for info box
      const cell = event.target.closest('.rt-td > div > div');
      if(cell){
          Shiny.setInputValue(
            'main-click_cell'
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

// Get all local storage
Shiny.addCustomMessageHandler(
  'get_local_storage_multi'
  , function(ids) {

      const message = ids.map(
        function(id){
          const res = localStorage.getItem(id);
          return res ? res : 0;
        }
      )

      Shiny.setInputValue(
        'saved-local_storage_multi'
        , message
        , {priority: "event"}
      );
});

// Clear local storage for id
Shiny.addCustomMessageHandler(
  'remove_local_storage'
  , function(input) {
      localStorage.removeItem(input);
});

// Change filter icon class if filters applied
Shiny.addCustomMessageHandler(
  'filters_applied'
  , function(filter_count){
    filter_count.factor > 0
    ? $(".factor-filter-select i").addClass("active")
    : $(".factor-filter-select i").removeClass("active");

    console.log("filter_count.measure: ", filter_count.measure)
    filter_count.measure > 0
    ? $(".measure-filter-select i").addClass("active")
    : $(".measure-filter-select i").removeClass("active");
  }
)


// Get sort order
Shiny.addCustomMessageHandler(
  'get_sort_order'
  , function(input) {
      const headers = $("#main-main_rt").find(".-header [ role='columnheader']").get();
      const sort_order = headers.map(
        function(el){
          return el.getAttribute("aria-sort");
        }
      );

      Shiny.setInputValue(
        'main-sort_order'
        , sort_order
        , {priority: "event"}
      );
});

Shiny.addCustomMessageHandler(
  'spaceman'
  , function(has_data){
    has_data ? $(".space-man").fadeOut(100) : $(".space-man").delay(1000).fadeIn(1000);
  }
)

Shiny.addCustomMessageHandler(
  'show_info_box'
  , function(is_visible){
    console.log(is_visible);
    is_visible
    ? $('.info-box').fadeIn(250)
    : $('.info-box').fadeOut(250);
  }
)



