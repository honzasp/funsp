$(function() {
  var frame = 0;
  var frames$ = $("#demo .frames img");
  var step$ = $("#ban-demo");

  $(document).keydown(function(evt) {
    if(evt.which == 83 || evt.which == 115) { // s
      frame += 1;
      refresh();
    } else if(evt.which == 65 || evt.which == 97) { // a
      frame -= 1;
      refresh();
    }
  });

  refresh();

  function refresh() {
    if(frame < 0) { frame = 0; }
    if(frame >= frames$.length) { frame = frames$.length - 1 }

    frames$.removeClass("active");
    $(frames$[frame]).addClass("active");
  }
});
