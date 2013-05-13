$(function(){
  var frames = $("#castle-anim .frames img");
  var frame_count = frames.length;
  var frame = 0;

  refresh();

  $("#castle-anim .backward").click(function() {
    frame -= 1;
    refresh();
  });

  $("#castle-anim .forward").click(function() {
    frame += 1;
    refresh();
  });

  var timer = null;

  $("#castle-anim .play").click(function() {
    if(timer) {
      clearInterval(timer);
      timer = null;
    } else {
      timer = setInterval(function() {
        if($("#ban-anim").hasClass("active")) {
          frame += 1;
          refresh();
        }
      }, 500);
    }
  });

  function refresh() {
    if(frame < 0) { frame = frame_count - 1; }
    if(frame >= frame_count) { frame = 0; }

    $("#castle-anim .frames img").removeClass("active");
    $($("#castle-anim .frames img")[frame]).addClass("active");
  }
});
